
# Load in dataset of all 3D spin tracked pitches
CCBL_3d_spin <- CCBL_Combined_2025 %>% filter(!is.na(SpinAxis3dSeamOrientationBallAngleHorizontalAmb1),
                                              !is.na(SpinAxis3dSeamOrientationBallAngleVerticalAmb1),
                                              !is.na(InducedVertBreak), !is.na(HorzBreak))


########################
### Horz Break Model ###
########################

# --- 1. Data Preparation ---

set.seed(0)

# Split into training and validation sets
train_size <- 0.8 * nrow(CCBL_3d_spin)
train_index <- sample(x = 1:nrow(CCBL_3d_spin), size = train_size)

train_set <- CCBL_3d_spin[train_index, ] # %>% filter(AutoPitchType == "Slider", PitcherThrows == "Right")
valid_set <- CCBL_3d_spin[-train_index, ] # %>% filter(AutoPitchType == "Slider", PitcherThrows == "Right")



# Define features (X) and target (y) for training
x <- train_set[, c("SpinRate", "SpinAxis3dTransverseAngle", "SpinAxis3dLongitudinalAngle",
                   "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1",
                   "SpinAxis3dSeamOrientationBallAngleVerticalAmb1",
                   "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3",
                   "SpinAxis3dSeamOrientationBallAngleVerticalAmb3")]

y <- train_set$HorzBreak

# Validation features
x_valid <- valid_set[, c("SpinRate", "SpinAxis3dTransverseAngle", "SpinAxis3dLongitudinalAngle",
                         "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1",
                         "SpinAxis3dSeamOrientationBallAngleVerticalAmb1",
                         "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3",
                         "SpinAxis3dSeamOrientationBallAngleVerticalAmb3")]


# --- 2. Model Training ---
horz_break_model <- xgboost(data        = as.matrix(x),
                            label       = y,
                            eta         = 0.075,
                            max_depth   = 5,
                            nrounds     = 100,
                            subsample   = 0.6,
                            min_child_weight = 5,
                            early_stopping_rounds = 30,
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
  geom_point(na.rm = TRUE) + coord_equal()


# RMSE(Root Mean Squared Error)
sqrt(mean((valid_set$horz_predictions - valid_set$HorzBreak)^2))

# MAE(Mean Absolute Error)
mean(abs(valid_set$horz_predictions - valid_set$HorzBreak))

# R-squared value
1 - sum((valid_set$HorzBreak - valid_set$horz_predictions)^2) / sum((valid_set$HorzBreak - mean(valid_set$HorzBreak))^2)

# RMSE on training data(check for overfitting)
new_data <- xgb.DMatrix(data = as.matrix(x))
train_set$horz_predictions <- predict(horz_break_model, new_data)
sqrt(mean((train_set$horz_predictions - train_set$HorzBreak)^2))

