
# Load in dataset of all 3D spin tracked pitches
CCBL_3d_spin <- CCBL_Combined_2025 %>% filter(!is.na(SpinAxis3dSeamOrientationBallAngleHorizontalAmb1),
                                              !is.na(SpinAxis3dSeamOrientationBallAngleVerticalAmb1),
                                              !is.na(InducedVertBreak), !is.na(HorzBreak))


########################
### Vert Break Model ###
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

y <- train_set$InducedVertBreak

# Validation features
x_valid <- valid_set[, c("SpinRate", "SpinAxis3dTransverseAngle", "SpinAxis3dLongitudinalAngle",
                         "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1",
                         "SpinAxis3dSeamOrientationBallAngleVerticalAmb1",
                         "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3",
                         "SpinAxis3dSeamOrientationBallAngleVerticalAmb3")]



# Train model(Induced Vertical Break Model)
vert_break_model <- xgboost(data        = as.matrix(x), 
                            label       = y,
                            eta         = 0.075,
                            max_depth   = 5,
                            nrounds     = 100,
                            subsample   = 0.6,
                            min_child_weight = 5,
                            early_stopping_rounds = 30,
                            objective   = "reg:squarederror",
                            eval_metric = "rmse",
                            verbose     = 1)

# Importance plot
var_imp_matrix <- xgb.importance(model = vert_break_model)
xgb.plot.importance(var_imp_matrix)

# Make predictions on valid set using model
new_data <- xgb.DMatrix(data = as.matrix(x_valid))
valid_set$vert_predictions <- predict(vert_break_model, new_data)

# Plot actual vs predicted Horz Break
valid_set %>% ggplot(aes(InducedVertBreak, vert_predictions)) + 
  geom_point(na.rm = TRUE)


# RMSE(Root Mean Squared Error)
sqrt(mean((valid_set$vert_predictions - valid_set$InducedVertBreak)^2))

# MAE(Mean Absolute Error)
mean(abs(valid_set$vert_predictions - valid_set$InducedVertBreak))

# R-squared value
1 - sum((valid_set$InducedVertBreak - valid_set$vert_predictions)^2) / sum((valid_set$InducedVertBreak - mean(valid_set$InducedVertBreak))^2)

# RMSE on training data(check for overfitting)
new_data <- xgb.DMatrix(data = as.matrix(x))
train_set$vert_predictions <- predict(vert_break_model, new_data)
sqrt(mean((train_set$vert_predictions - train_set$InducedVertBreak)^2))

