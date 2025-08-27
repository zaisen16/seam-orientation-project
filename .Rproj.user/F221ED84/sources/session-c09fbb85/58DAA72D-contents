

NCAA_3D_Spin <- read.csv("~/Downloads/3DSpin2025.csv")
CCBL_Combined_2025 <- read.csv("~/Downloads/CCBL_2025_Master_w_RV_Fixed.csv")


CCBL_3d_spin <- CCBL_Combined_2025 %>% filter(!is.na(SpinAxis3dSeamOrientationBallAngleHorizontalAmb1),
                                              !is.na(SpinAxis3dSeamOrientationBallAngleVerticalAmb1),
                                              !is.na(InducedVertBreak), !is.na(HorzBreak)) %>% filter(HomeTeam != "BRE_WHI")

# Returns a list of pitchers with the biggest sample size
print(CCBL_3d_spin %>% 
        # filter(PitcherTeam == "BRE_WHI") %>%
        count(Pitcher, sort = TRUE))

library(rpart)
library(randomForest)
library(forecast)
library(caret)
library(xgboost)


# Adjusting to make model based on a specific pitch type
train_size <- 0.7 * nrow(CCBL_3d_spin)
set.seed(0)
train_index <- sample(x = 1:nrow(CCBL_3d_spin), size = train_size)
train_set <- CCBL_3d_spin[train_index, ] %>% filter(AutoPitchType == "Four-Seam", PitcherThrows == "Right")
valid_set <- CCBL_3d_spin[-train_index, ] %>% filter(AutoPitchType == "Four-Seam", PitcherThrows == "Right")

CCBL_3d_spin$SpinAxis3dLongitudinalAngle
x <- train_set[, c("SpinRate", "SpinAxis","SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1")]
y <- train_set$HorzBreak


x_valid <- valid_set[, c("SpinRate", "SpinAxis","SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1")]

# Train model(Horizontal Break Model)
horz_break_model <- xgboost(data = as.matrix(x), 
                         label = y,
                         max_depth = 5,
                         nrounds = 100,
                         alpha = 0.01,
                         objective = "reg:squarederror",
                         eval_metric = 'rmse',
                         verbose = 1)

# Importance plot
var_imp_matrix <- xgb.importance(model = horz_break_model)
xgb.plot.importance(var_imp_matrix)

# Make predictions on valid set using model
new_data <- xgb.DMatrix(data = as.matrix(x_valid))
valid_set$predictions <- predict(horz_break_model, new_data)
# Predict onto training set
train_set$probabilities <- predict(whiff_model, as.matrix(x))


### testing validity of the model
# Plot actual vs predicted Horz Break
valid_set %>% ggplot(aes(HorzBreak, predictions)) + 
  geom_point(na.rm = TRUE)

RMSE(valid_set$HorzBreak, predictions)




# Train model(Induced Vertical Break Model)
y <- train_set$InducedVertBreak
vert_break_model <- xgboost(data = as.matrix(x), 
                                  label = y,
                                  max_depth = 5,
                                  nrounds = 100,
                                  alpha = 0.01,
                                  objective = "reg:squarederror",
                                  eval_metric='rmse',
                                  verbose = 1)

# Importance plot
var_imp_matrix <- xgb.importance(model = vert_break_model)
xgb.plot.importance(var_imp_matrix)

# Make predictions on valid set using model
new_data <- xgb.DMatrix(data = as.matrix(x_valid))
valid_set$predictions <- predict(vert_break_model, new_data)
# Predict onto training set
train_set$probabilities <- predict(vert_break_model, as.matrix(x))

# Plot actual vs predicted Horz Break
valid_set %>% ggplot(aes(InducedVertBreak, predictions)) + 
  geom_point(na.rm = TRUE)



#################################
### Testing pitch adjustments ###
#################################

test <- CCBL_3d_spin %>% filter(Pitcher == "Seid, Spencer", TaggedPitchType == "Fastball")
c(mean(test$InducedVertBreak), mean(test$HorzBreak))
test$SpinAxis3dTilt
test <- test[, c("SpinRate", "SpinAxis","SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1")]

mean(test$SpinAxis3dSeamOrientationBallAngleHorizontalAmb1)
mean(test$SpinAxis3dSeamOrientationBallAngleVerticalAmb1)

example_pitch <- data.frame(
  SpinRate = mean(test$SpinRate),
  SpinAxis = mean(test$SpinAxis),
  SpinAxis3dSpinEfficiency = mean(test$SpinAxis3dSpinEfficiency),
  # SpinAxis3dSpinEfficiency = 0.85,       # Select your own spin efficiency option
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 = -90,  # These two are referring to points on the mollweide plot
  SpinAxis3dSeamOrientationBallAngleVerticalAmb1 = 45     # Should be 45 for LHP, -45 for RHP(For sweeper)
)
example_pitch <- as.matrix(example_pitch)

predict(vert_break_model, example_pitch)
predict(horz_break_model, example_pitch)










###############################################
###Plots for evaluating patters/correlations###
###############################################

### Horz break plotted by its spin axis, for sliders
CCBL_3d_spin %>% filter(AutoPitchType == "Slider", PitcherThrows == "Right") %>% mutate(HorzBreakAbs = abs(HorzBreak)) %>% 
  ggplot(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb1, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb1, color = HorzBreakAbs)) +
  # scale_color_manual(values = TMcolors) + 
  scale_color_gradient(low = "white", high = "navy") + 
  geom_path(data = spline_curve, aes(x = lon, y = lat), color = "red", linewidth = 1.5) +
  geom_point(size = 1.5, alpha = 0.75) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb2, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb2, color = HorzBreakAbs)) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb3, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb3, color = HorzBreakAbs)) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb4, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb4, color = HorzBreakAbs)) +
  coord_map("mollweide") +  # projection type
  scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 45)) +
  scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
  labs(
    title = paste0("Seam Orientation: Sliders"),
    subtitle = "Using TrackMan 3D Spin",
    x = "Horizontal Angle (°)",
    y = "Vertical Angle (°)",
    color = "Horz Break
(Absolute Value)"
  ) +
  theme_minimal()


# 4-seam FB spin orientation by vertical break
CCBL_3d_spin %>% filter(AutoPitchType == "Four-Seam") %>% mutate(HorzBreakAbs = abs(HorzBreak)) %>% 
  ggplot(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb1, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb1, color = InducedVertBreak)) +
  # scale_color_manual(values = TMcolors) + 
  scale_color_gradient(low = "white", high = "blue") + 
  geom_path(data = spline_curve, aes(x = lon, y = lat), color = "red", linewidth = 1.5) +
  geom_point(size = 1.5, alpha = 0.75) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb2, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb2, color = InducedVertBreak)) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb3, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb3, color = InducedVertBreak)) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb4, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb4, color = InducedVertBreak)) +
  coord_map("mollweide") +  # projection type
  scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 45)) +
  scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
  labs(
    title = paste0("Seam Orientation"),
    subtitle = "Using TrackMan 3D Spin",
    x = "Horizontal Angle (°)",
    y = "Vertical Angle (°)",
    color = "Vert Break"
  ) +
  theme_minimal()


# Sinkers & HorzBreak
CCBL_3d_spin %>% filter(AutoPitchType == "Sinker", SpinAxis3dSpinEfficiency > 0.8, SpinAxis3dSpinEfficiency < 0.9) %>% mutate(HorzBreakAbs = abs(HorzBreak)) %>% 
  ggplot(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb1, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb1, color = HorzBreakAbs)) +
  # scale_color_manual(values = TMcolors) + 
  scale_color_gradient(low = "white", high = "navy") + 
  geom_path(data = spline_curve, aes(x = lon, y = lat), color = "red", linewidth = 1.5) +
  geom_point(size = 1.5, alpha = 0.75) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb2, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb2, color = HorzBreakAbs)) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb3, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb3, color = HorzBreakAbs)) +
  geom_point(aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb4, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb4, color = HorzBreakAbs)) +
  coord_map("mollweide") +  # projection type
  scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 45)) +
  scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
  labs(
    title = paste0("Seam Orientation"),
    subtitle = "Using TrackMan 3D Spin",
    x = "Horizontal Angle (°)",
    y = "Vertical Angle (°)",
    color = "Vert Break"
  ) +
  theme_minimal()




c("SpinRate", "SpinAxis", "SpinAxis3dSpinEfficiency", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1")

CCBL_3d_spin %>% mutate(HorzBreakAbs = abs(HorzBreak)) %>% filter(AutoPitchType == "Slider") %>% ggplot(aes(HorzBreakAbs, SpinAxis3dSeamOrientationBallAngleHorizontalAmb1)) + geom_smooth(aes(HorzBreakAbs)) + theme_light()

CCBL_3d_spin %>% filter(AutoPitchType == "Four-Seam") %>% ggplot(aes(InducedVertBreak, SpinAxis3dSpinEfficiency)) + geom_smooth(aes(InducedVertBreak)) + theme_light()



### IDK
CCBL_3d_spin %>% mutate(HorzBreakAbs = abs(HorzBreak)) %>% filter(TaggedPitchType == "Slider", 
                                                                  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 > 85, 
                                                                  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 < 95,
                                                                  SpinAxis3dSeamOrientationBallAngleVerticalAmb1 < -30, 
                                                                  SpinAxis3dSeamOrientationBallAngleVerticalAmb1 > -60) %>% ggplot(aes(HorzBreakAbs, SpinAxis3dSpinEfficiency)) + geom_smooth(aes(HorzBreakAbs)) + theme_light()

CCBL_3d_spin[, c("TaggedPitchType", "HorzBreak", "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1", "SpinAxis3dSeamOrientationBallAngleVerticalAmb1")] %>% filter(TaggedPitchType == "Slider") %>% arrange((HorzBreak))


