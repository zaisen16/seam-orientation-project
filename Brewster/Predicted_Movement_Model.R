library(forecast)
library(caret)
library(xgboost)
library(rpart)
library(dplyr)     
library(ggplot2)
library(pdp)        # partial dependence / ICE




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
x <- train_set[, c("SpinRate", "SpinAxis", "SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb2", "SpinAxis3dSeamOrientationBallAngleVerticalAmb2")]

HorzBreak <- train_set$HorzBreak

# Validation features
x_valid <- valid_set[, c("SpinRate", "SpinAxis", "SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb2", "SpinAxis3dSeamOrientationBallAngleVerticalAmb2")]


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




# --- 4. SHAP: local explanations on the validation set ---

# 4.1 Get SHAP contributions (already as you had it)
shap_valid_mat <- predict(horz_break_model, newdata = new_data, predcontrib = TRUE)
colnames(shap_valid_mat) <- c(colnames(x_valid), "BIAS")

# 4.2 (optional) Keep a tidy table for later use (actual/pred + raw features)
shap_valid <- as.data.frame(shap_valid_mat) %>%
  mutate(
    HorzBreak_actual = valid_set$HorzBreak,
    HorzBreak_pred   = valid_set$horz_predictions
  ) %>%
  bind_cols(as.data.frame(x_valid))   # okay to bind here, we won't use names for importance

# Build a SHAP table
shap_valid <- as.data.frame(shap_valid_mat) %>%
  mutate(
    HorzBreak_actual = valid_set$HorzBreak,
    HorzBreak_pred   = valid_set$horz_predictions
  )

# Add RAW features with a suffix to avoid name collisions
raw_feats <- as.data.frame(x_valid)
colnames(raw_feats) <- paste0(colnames(raw_feats), "_raw")

shap_valid <- dplyr::bind_cols(shap_valid, raw_feats)

# 4.4 Now we can safely select the SHAP columns by the original feature names
shap_importance <- shap_valid %>%
  dplyr::select(dplyr::all_of(colnames(x_valid))) %>%   # these are the SHAP columns
  dplyr::summarise(dplyr::across(everything(), ~mean(abs(.x), na.rm = TRUE))) %>%
  tidyr::pivot_longer(everything(), names_to = "feature", values_to = "mean_abs_shap") %>%
  dplyr::arrange(dplyr::desc(mean_abs_shap))

# Plot Importance
ggplot(shap_importance, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "HorzBreak — Global SHAP Importance (Validation)",
    x = "Feature",
    y = "Mean |SHAP| (inches contribution)"
  )





# --- 5. Partial Dependence (PDP) on seam-orientation features ---

# 5.1 A predict function that pdp will use (xgboost needs a numeric matrix)
pred_fun_xgb <- function(object, newdata) {
  predict(object, as.matrix(newdata))
}


# HorzMovement based on different seam orientations(sweeping across vert angles)
# Essentailly means: What is the horz break on a pitch with everything constant, except sweeping through every option for seam orientation
pdp_HV <- partial(
  object    = horz_break_model,
  pred.var  = c(feat_H1, feat_V1),
  pred.fun  = pred_fun_xgb,
  train     = as.data.frame(x),
  grid.resolution = 31
)

# Create "rolling" plots, how much movement does it add at every possibly "seam angle"

# Seam-orientation features
feat_H1 <- "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1"
feat_V1 <- "SpinAxis3dSeamOrientationBallAngleVerticalAmb1"
feat_H2 <- "SpinAxis3dSeamOrientationBallAngleHorizontalAmb2"
feat_V2 <- "SpinAxis3dSeamOrientationBallAngleVerticalAmb2"

# Uses your existing objects: horz_break_model, x, pred_fun_xgb
make_cice_plot <- function(feature, title_prefix = "Centered ICE + PDP — HorzBreak vs ") {
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

library(patchwork)  # install.packages("patchwork") if needed
p_H1_cice | p_V1_cice





# Draw a contour/surface
pdp::levelplot(pdp_HV, 
               xlab = feat_H1, ylab = feat_V1,
               main = "2D PDP — HorzBreak vs (H1, V1) seam angles")

# Same as above, but with every pitch starting at one point. Better visual
# THE BEST ONE RN, try to keep going with this idea
p_V1_cice <- partial(
  horz_break_model, pred.var = feat_V1, pred.fun = pred_fun_xgb,
  train = as.data.frame(x), grid.resolution = 50,
  ice = TRUE, center = TRUE
)
autoplot(p_V1_cice, alpha = 0.15) +
  labs(title = "Centered ICE + PDP — HorzBreak vs VerticalAmb1",
       x = feat_V1, y = "Δ Predicted HorzBreak (inches)")


# didnt work
library(lattice)

library(ggplot2)

df2 <- as.data.frame(pdp_HV)  # columns: <feat_H1>, <feat_V1>, yhat

ggplot(df2, aes(x = .data[[feat_H1]], y = .data[[feat_V1]], z = yhat)) +
  geom_raster(aes(fill = yhat), interpolate = TRUE) +
  geom_contour(color = "white", alpha = 0.8) +
  scale_fill_viridis_c(name = "Pred HB (in)") +
  labs(
    title = "2D PDP — HorzBreak vs (H1, V1) seam angles",
    x = feat_H1, y = feat_V1
  )



library(ggplot2)

# kinda makes sense, but looks terrible idk
df2 <- as.data.frame(pdp_HV)  # columns: feat_H1, feat_V1, yhat

ggplot(df2, aes_string(x = feat_H1, y = feat_V1, z = "yhat")) +
  geom_raster(aes(fill = yhat), interpolate = TRUE) +
  geom_contour(color = "white", alpha = 0.7) +
  scale_fill_viridis_c(name = "Pred HB (in)") +
  labs(title = "2D PDP — HorzBreak vs (H1, V1)",
       x = feat_H1, y = feat_V1)


# --- 1. Data Preparation ---

# Define features (X) and target (y) for training
x <- train_set[, c("SpinRate", "SpinAxis", "SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb2", "SpinAxis3dSeamOrientationBallAngleVerticalAmb2")]

VertBreak <- train_set$InducedVertBreak

# Validation features
x_valid <- valid_set[, c("SpinRate", "SpinAxis", "SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb2", "SpinAxis3dSeamOrientationBallAngleVerticalAmb2")]


# --- 2. Model Training ---
vert_break_model <- xgboost(
  data        = as.matrix(x),
  label       = VertBreak,
  max_depth   = 7,
  nrounds     = 100,
  alpha       = 0.01,
  objective   = "reg:squarederror",
  eval_metric = "rmse",
  verbose     = 1
)


# --- 3. Model Evaluation ---
# Feature importance
var_imp_matrix <- xgb.importance(model = vert_break_model)
xgb.plot.importance(var_imp_matrix)

# Predictions on validation set
new_data <- xgb.DMatrix(data = as.matrix(x_valid))
valid_set$vert_predictions <- predict(vert_break_model, new_data)

# Plot actual vs predicted Horz Break
valid_set %>% ggplot(aes(InducedVertBreak, vert_predictions)) + 
  geom_point(na.rm = TRUE)




# ------- Evaluating both models----------

# Root mean squared error
RMSE(valid_set$HorzBreak, valid_set$horz_predictions)
RMSE(valid_set$InducedVertBreak, valid_set$vert_predictions)

# mean absolute error
MAE(valid_set$HorzBreak, valid_set$horz_predictions)
MAE(valid_set$InducedVertBreak, valid_set$vert_predictions) 

# r squared
r_squared <- 1 - sum((valid_set$HorzBreak - valid_set$horz_predictions)^2) / sum((valid_set$HorzBreak - mean(valid_set$HorzBreak))^2)
r_squared <- 1 - sum((valid_set$InducedVertBreak - valid_set$vert_predictions)^2) / sum((valid_set$InducedVertBreak - mean(valid_set$InducedVertBreak))^2)

# correlation
cor(valid_set$HorzBreak, valid_set$horz_predictions, method = "pearson")
cor(valid_set$InducedVertBreak, valid_set$vert_predictions, method = "pearson")


