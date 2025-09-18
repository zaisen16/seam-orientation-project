# Load packages
library(forecast)
library(caret)
library(xgboost)
library(dplyr)     
library(ggplot2)
library(pdp)
library(patchwork)
library(FNN)

# Loading Data
CCBL_Combined_2025 <- read.csv("~/Downloads/CCBL_2025_Master_w_RV_Fixed.csv")
CCBL_3d_spin <- CCBL_Combined_2025 %>% filter(!is.na(SpinAxis3dSeamOrientationBallAngleHorizontalAmb1),
                                              !is.na(SpinAxis3dSeamOrientationBallAngleVerticalAmb1),
                                              !is.na(InducedVertBreak), !is.na(HorzBreak)) %>% filter(HomeTeam != "BRE_WHI")



###################
# HorzBreak Model #
###################

# --- 1. Data Preparation ---
set.seed(0)

# Split into training and validation sets
train_size <- 0.5 * nrow(CCBL_3d_spin)
train_index <- sample(x = 1:nrow(CCBL_3d_spin), size = train_size)

train_set <- CCBL_3d_spin[train_index, ] %>% filter(TaggedPitchType == "Slider", PitcherThrows == "Right")
valid_set <- CCBL_3d_spin[-train_index, ] %>% filter(TaggedPitchType == "Slider", PitcherThrows == "Right")

# Define features (X) and target (y) for training
x <- train_set[, c("SpinRate", "SpinAxis", "SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3", "SpinAxis3dSeamOrientationBallAngleVerticalAmb3")]

HorzBreak <- train_set$HorzBreak

# Validation features
x_valid <- valid_set[, c("SpinRate", "SpinAxis", "SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3", "SpinAxis3dSeamOrientationBallAngleVerticalAmb3")]


# --- 2. Model Training ---
horz_break_model <- xgboost(
  data        = as.matrix(x),
  label       = HorzBreak,
  max_depth   = 7,
  nrounds     = 100,
  eta         = 0.1,
  early_stopping_rounds = 20,
  objective   = "reg:squarederror",
  eval_metric = "rmse",
  verbose     = 1
)


# --- 3. Model Evaluation ---
# Feature importance
var_imp_matrix <- xgb.importance(model = horz_break_model)
xgb.plot.importance(var_imp_matrix)

# Predictions on validation set
new_data <- xgb.DMatrix(data = as.matrix(x_valid))
valid_set$horz_predictions <- predict(horz_break_model, new_data)

# Plot actual vs predicted Horz Break
valid_set %>% ggplot(aes(HorzBreak, horz_predictions)) + 
  geom_point(na.rm = TRUE)





# --- 5. Partial Dependence (PDP) on seam-orientation features ---

# Seam-orientation features
feat_H1 <- "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1"
feat_V1 <- "SpinAxis3dSeamOrientationBallAngleVerticalAmb1"
feat_H2 <- "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3"
feat_V2 <- "SpinAxis3dSeamOrientationBallAngleVerticalAmb3"


# 5.1 predict function that pdp will use (xgboost needs a numeric matrix)
pred_fun_xgb <- function(object, newdata) {
  predict(object, as.matrix(newdata))
}

# HorzMovement based on different seam orientations(sweeping across vert angles)
# Essentailly means: What is the horz break on a pitch with everything constant, except sweeping through every option for seam orientation

# Create "sweep" plots, how much movement does it add at every possibly "seam angle"

# Uses existing objects: horz_break_model, x, pred_fun_xgb
make_cice_plot <- function(feature, title_prefix = "Sweep Style Plot, Horz Break vs ") {
  pd <- partial(
    object          = horz_break_model,
    pred.var        = feature,
    pred.fun        = pred_fun_xgb,
    train           = as.data.frame(x),
    grid.resolution = 50,
    ice             = TRUE,
    center          = TRUE
  )
  autoplot(pd, alpha = 0.15) +
    labs(
      title = paste0(title_prefix, feature),
      x     = feature,
      y     = "Δ Predicted HorzBreak (inches)"
    )
}

# Vertical (Amb1)
p_V1_cice <- make_cice_plot(feat_V1)

# Horizontal (Amb1)
p_H1_cice <- make_cice_plot(feat_H1)

# Plot side by side
p_H1_cice | p_V1_cice




# Function for putting in one pitcher/pitch

### Predictor helper
feat_cols <- c(
  "SpinRate",
  "SpinAxis",
  "SpinAxis3dSpinEfficiency",
  "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1",
  "SpinAxis3dSeamOrientationBallAngleVerticalAmb1",
  "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3",
  "SpinAxis3dSeamOrientationBallAngleVerticalAmb3"
)

feat_H1 <- "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1"
feat_V1 <- "SpinAxis3dSeamOrientationBallAngleVerticalAmb1"

pred_fun_xgb <- function(object, newdata) predict(object, as.matrix(newdata))

sweep_pitcher_feature <- function(model,
                                  source_df,
                                  pitcher,
                                  pitch_type,
                                  feature,                             # feat_H1 or feat_V1
                                  id_cols   = c("Pitcher","TaggedPitchType","GameDate"),
                                  grid.resolution = 60,
                                  range_from = c("subset","train"),     # where the sweep range comes from
                                  train_X_df = NULL,                    # REQUIRED if range_from="train" or support_check=TRUE
                                  title_prefix = "Sweep Style Plot, Horz Break vs ",
                                  add_avg_vline = TRUE,                 # median line
                                  band = c("none","sd","iqr"),
                                  support_check = TRUE,                 # turn on KNN support gating
                                  support_k = 20,
                                  support_quantile = 0.95,              # 95th pct cutoff of train self-distances
                                  sweep_values = NULL,                  # explicit sweep values (optional)
                                  sweep_pad = 0) {                      # widen min/max by degrees (optional)
  
  range_from <- match.arg(range_from)
  band <- match.arg(band)
  
  # 1) Subset to this pitcher & pitch type
  sub <- source_df %>%
    filter(.data[[id_cols[1]]] == pitcher,
           .data[[id_cols[2]]] == pitch_type) %>%
    select(any_of(c(id_cols, feat_cols, "HorzBreak"))) %>%
    tidyr::drop_na(all_of(feat_cols))
  if (nrow(sub) < 2) stop("Not enough rows for that pitcher + pitch type.")
  
  # 2) Numeric feature frame for ICE
  sub_X <- sub[, feat_cols, drop = FALSE] %>%
    mutate(across(everything(), as.numeric))
  sub_X$.id <- seq_len(nrow(sub_X))   # keep an id that matches pdp's .id
  
  # 3) Build sweep grid (values for the chosen feature)
  if (!is.null(sweep_values)) {
    vals <- as.numeric(sweep_values)
  } else {
    base_vec <- if (range_from == "subset") {
      sub_X[[feature]]
    } else {
      if (is.null(train_X_df)) stop("Provide train_X_df when range_from='train'.")
      train_X_df[[feature]]
    }
    rng <- range(base_vec, na.rm = TRUE)
    rng <- c(rng[1] - sweep_pad, rng[2] + sweep_pad)
    vals <- seq(rng[1], rng[2], length.out = grid.resolution)
  }
  pred.grid <- data.frame(setNames(list(vals), feature))
  
  # 4) ICE + PDP (raw predictions)
  pd <- pdp::partial(
    object     = model,
    pred.var   = feature,
    pred.fun   = pred_fun_xgb,
    train      = as.data.frame(sub_X[, feat_cols, drop = FALSE]), # each row -> one ICE curve
    pred.grid  = pred.grid,
    ice        = TRUE,
    center     = FALSE
  )
  
  # 5) Identify the id column from pdp::partial
  id_col <- if (".id" %in% names(pd)) ".id" else if ("yhat.id" %in% names(pd)) "yhat.id" else {
    stop("Could not find an id column in partial() output (expected .id or yhat.id).")
  }
  pd$id <- pd[[id_col]]
  
  # 6) Compute robust reference for vline/band
  p50    <- stats::median(sub[[feature]], na.rm = TRUE)
  sd_val <- stats::sd(sub[[feature]], na.rm = TRUE)
  q      <- stats::quantile(sub[[feature]], probs = c(.25,.75), na.rm = TRUE)
  
  # 7) ------- SUPPORT CHECK (KNN to training manifold) -------
  if (isTRUE(support_check)) {
    if (is.null(train_X_df)) stop("support_check=TRUE requires train_X_df.")
    # scale training features
    train_mat <- as.matrix(train_X_df[, feat_cols, drop = FALSE])
    s_train   <- scale(train_mat)
    cen <- attr(s_train, "scaled:center")
    scl <- attr(s_train, "scaled:scale")
    
    # baseline: each training point to its k-NN in training (exclude self)
    d_train   <- FNN::knnx.dist(s_train, s_train, k = support_k + 1)
    base_dist <- rowMeans(d_train[, -1, drop = FALSE])    # drop self-distance
    cutoff    <- stats::quantile(base_dist, probs = support_quantile, na.rm = TRUE)
    
    # build the full set of counterfactual rows in the same order as pd
    base_by_id <- sub_X[match(pd$id, sub_X$.id), feat_cols, drop = FALSE]
    newX_all   <- as.matrix(base_by_id)
    newX_all[, feature] <- pd[[feature]]
    
    # scale with training parameters
    s_new <- scale(newX_all, center = cen, scale = scl)
    
    # distance of each counterfactual to training manifold (mean of k-NN)
    d_new     <- FNN::knnx.dist(s_train, s_new, k = support_k)
    pd$dist   <- rowMeans(d_new)
    pd$in_sup <- pd$dist <= as.numeric(cutoff)
  } else {
    pd$in_sup <- TRUE
  }
  
  # 8) Build PDP (average line)
  pd_avg <- pd %>%
    group_by(.data[[feature]]) %>%
    summarise(yhat = mean(.data[["yhat"]], na.rm = TRUE), .groups = "drop")
  
  # 9) Segment ICE lines so in/out-of-support can have different alphas
  pd2 <- pd %>%
    arrange(id, .data[[feature]]) %>%
    group_by(id) %>%
    mutate(seg = cumsum(c(FALSE, diff(in_sup) != 0))) %>%
    ungroup() %>%
    mutate(group = paste(id, seg, sep = "_"))
  
  # 10) Plot: faint for out-of-support, darker for in-support + PDP + vline/band
  p <- ggplot() +
    geom_path(data = subset(pd2, !in_sup),
              aes(x = .data[[feature]], y = .data[["yhat"]], group = group),
              color = "black", alpha = 0.08) +
    geom_path(data = subset(pd2, in_sup),
              aes(x = .data[[feature]], y = .data[["yhat"]], group = group),
              color = "black", alpha = 0.45) +
    geom_line(data = pd_avg,
              aes(x = .data[[feature]], y = .data[["yhat"]]),
              color = "red", linewidth = 1) +
    labs(
      title    = paste0(title_prefix, feature),
      subtitle = paste0(id_cols[1], ": ", pitcher, "   |   ", id_cols[2], ": ", pitch_type),
      # subtitle = paste0(id_cols[1], ": Anonymous Pitcher", "   |   ", id_cols[2], ": ", pitch_type),
      x = feature,
      y = "Predicted HorzBreak (inches)"
    )
  
  # Optional band
  if (band == "sd" && is.finite(sd_val)) {
    p <- p + annotate("rect",
                      xmin = p50 - sd_val, xmax = p50 + sd_val,
                      ymin = -Inf, ymax = Inf, alpha = 0.06)
  } else if (band == "iqr" && all(is.finite(q))) {
    p <- p + annotate("rect",
                      xmin = q[1], xmax = q[2],
                      ymin = -Inf, ymax = Inf, alpha = 0.06)
  }
  # Median marker
  if (add_avg_vline && is.finite(p50)) {
    p <- p +
      geom_vline(xintercept = p50, linetype = 2, linewidth = 0.8, color = "steelblue") +
      annotate("label", x = p50, y = Inf, vjust = 1.2,
               label = sprintf("p50 = %.1f°", p50),
               size = 3, label.size = 0.25, fill = "white")
  }
  
  list(
    data = pd2,
    plot = p,
    subset_n = nrow(sub),
    p50 = p50,
    support_params = list(k = support_k, cutoff_quantile = support_quantile)
  )
}



### Wrapper for plots side by side
sweep_pitcher_H1_V1 <- function(model,
                                source_df,
                                pitcher,
                                pitch_type,
                                grid.resolution = 60,
                                range_from = "train",      # <- use league/train distribution by default
                                train_X_df = NULL,         # pass as.data.frame(x)
                                id_cols = c("Pitcher","TaggedPitchType","GameDate"),
                                add_avg_vline = TRUE,
                                band = c("none","sd","iqr"),
                                support_check = TRUE,
                                support_k = 20,
                                support_quantile = 0.95,
                                sweep_values_H1 = NULL,
                                sweep_values_V1 = NULL,
                                sweep_pad = 0) {
  
  band <- match.arg(band)
  
  out_H <- sweep_pitcher_feature(model, source_df, pitcher, pitch_type,
                                 feature = feat_H1,
                                 id_cols = id_cols,
                                 grid.resolution = grid.resolution,
                                 range_from = range_from,
                                 train_X_df = train_X_df,
                                 add_avg_vline = add_avg_vline,
                                 band = band,
                                 support_check = support_check,
                                 support_k = support_k,
                                 support_quantile = support_quantile,
                                 sweep_values = sweep_values_H1,
                                 sweep_pad = sweep_pad)
  
  out_V <- sweep_pitcher_feature(model, source_df, pitcher, pitch_type,
                                 feature = feat_V1,
                                 id_cols = id_cols,
                                 grid.resolution = grid.resolution,
                                 range_from = range_from,
                                 train_X_df = train_X_df,
                                 add_avg_vline = add_avg_vline,
                                 band = band,
                                 support_check = support_check,
                                 support_k = support_k,
                                 support_quantile = support_quantile,
                                 sweep_values = sweep_values_V1,
                                 sweep_pad = sweep_pad)
  
  out_H$plot | out_V$plot
}


### Example call of wrapper function
# Use your original subset (IDs intact)
source_df <- CCBL_3d_spin

# Training features frame (for sweep range + support check)
train_X_df <- as.data.frame(x)  # x = your training feature matrix (as.data.frame)

sweep_pitcher_H1_V1(
  model        = horz_break_model,
  source_df    = source_df,
  pitcher      = "Marsten, Duncan",
  pitch_type   = "Slider",
  grid.resolution = 80,
  range_from   = "train",       # sweep on league range (not just this pitcher)
  train_X_df   = train_X_df,
  band         = "iqr",
  support_check = TRUE,         # fade out-of-support regions
  support_k     = 20,
  support_quantile = 0.95,
  sweep_pad      = 5            # optional: widen min/max by ±5°
)




### Now test a new orientation here(also need trained IVB model)

test <- CCBL_3d_spin %>% filter(Pitcher == "Marsten, Duncan", TaggedPitchType == "Slider")

# Avg Horz & Vert break of original pitch(sanity check)
c(mean(test$InducedVertBreak), mean(test$HorzBreak))
# Tilt of original pitch(sanity check)
test$SpinAxis3dTilt
# edit test df to have only what is needed for model
test <- test[, c("SpinRate", "SpinAxis","SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3", "SpinAxis3dSeamOrientationBallAngleVerticalAmb3")]

# More sanity check, Amb1 & Amb3 points, prior to adjusting
mean(test$SpinAxis3dSeamOrientationBallAngleHorizontalAmb1)
mean(test$SpinAxis3dSeamOrientationBallAngleVerticalAmb1)
mean(test$SpinAxis3dSeamOrientationBallAngleHorizontalAmb3)
mean(test$SpinAxis3dSeamOrientationBallAngleVerticalAmb3)


example_pitch <- data.frame(
  SpinRate = mean(test$SpinRate),
  SpinAxis = mean(test$SpinAxis),
  SpinAxis3dSpinEfficiency = mean(test$SpinAxis3dSpinEfficiency), # Use mean spin efficiency of original pitch
  # SpinAxis3dSpinEfficiency = 0.15,       # Select your own spin efficiency (option)
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 = 100,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb1 = -50 ,    # These two are referring to points on the mollweide plot
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb3 = -70,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb3 = 50    # These two are referring to points on the mollweide plot
)

# Create matrix(format needed to put through xgboost model)
example_pitch <- as.matrix(example_pitch)

# Put new pitch through xgboost model, observe newly predicted Horz & Vert breaks
predict(vert_break_model, example_pitch)
predict(horz_break_model, example_pitch)

# Optional: Creates a df that contains the rotations, which can be put into the TexasLeaguers spin visualization tool
w_rotations <- test[, c("SpinRate", "SpinAxis","SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb2", "SpinAxis3dSeamOrientationBallAngleVerticalAmb2", "SpinAxis3dSeamOrientationRotationX", "SpinAxis3dSeamOrientationRotationY", "SpinAxis3dSeamOrientationRotationZ", "SpinAxis3dTilt")]


