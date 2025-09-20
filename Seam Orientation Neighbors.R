library(FNN)

test_pitch_neighbors <- function(pitcher, pitch_type, H1, V1, H3, V3){

pitch_actual <- CCBL_3d_spin %>% filter(Pitcher == pitcher, TaggedPitchType == pitch_type)

example_pitch <- data.frame(
  SpinRate = mean(pitch_actual$SpinRate),
  SpinAxis = mean(pitch_actual$SpinAxis),
  SpinAxis3dSpinEfficiency = mean(pitch_actual$SpinAxis3dSpinEfficiency), # Use mean spin efficiency of original pitch
  # SpinAxis3dSpinEfficiency = 0.15,       # Select your own spin efficiency (option)
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 = H1,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb1 = V1 ,    # These two are referring to points on the mollweide plot
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb3 = H3,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb3 = V3    # These two are referring to points on the mollweide plot
)





test <- CCBL_3d_spin %>% filter(Pitcher == "Marsten, Duncan", TaggedPitchType == "Slider")

example_pitch <- data.frame(
  SpinRate = mean(test$SpinRate, na.rm = TRUE),
  SpinAxis = mean(test$SpinAxis, na.rm = TRUE),
  SpinAxis3dSpinEfficiency = mean(test$SpinAxis3dSpinEfficiency, na.rm = TRUE), # Use mean spin efficiency of original pitch
  # SpinAxis3dSpinEfficiency = 0.15,       # Select your own spin efficiency (option)
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 = 100,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb1 = -50 ,    # These two are referring to points on the mollweide plot
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb3 = -70,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb3 = 50    # These two are referring to points on the mollweide plot
)


feat_cols <- c(
  "SpinRate",
  "SpinAxis",
  "SpinAxis3dSpinEfficiency",
  "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1",
  "SpinAxis3dSeamOrientationBallAngleVerticalAmb1",
  "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3",
  "SpinAxis3dSeamOrientationBallAngleVerticalAmb3"
)

# Feature weights(care more about finding matching seam orientations than spin rates)
w <- c(
  SpinRate = 1.0,
  SpinAxis = 2.0,
  SpinAxis3dSpinEfficiency = 1.5,
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 = 3.0,
  SpinAxis3dSeamOrientationBallAngleVerticalAmb1   = 3.0,
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb3 = 3.0,
  SpinAxis3dSeamOrientationBallAngleVerticalAmb3   = 3.0
)

# 2) standardize
X <- CCBL_3d_spin[feat_cols]
X_scaled <- scale(X)
center <- attr(X_scaled, "scaled:center")
scale_  <- attr(X_scaled, "scaled:scale")

# 3) apply sqrt-weights
ws <- sqrt(w[feat_cols])
X_w <- sweep(X_scaled, 2, ws, "*")

# 4) prepare the query pitch (same scaling + same weights)
q_raw <- example_pitch[feat_cols]
q_scaled <- (q_raw - center) / scale_
q_w <- as.matrix(q_scaled * ws)

# 5) KNN search on the weighted space
neighbors <- get.knnx(data = X_w, query = q_w, k = 10)

# neighbors indecies, to refer to other dataset
neighbors_idx <- neighbors$nn.index[1,]
# Create dataset of top 10 neighbors
out <- cbind(CCBL_3d_spin[neighbors_idx, c("Pitcher", "TaggedPitchType", "InducedVertBreak","HorzBreak")], CCBL_3d_spin[neighbors_idx, feat_cols])
# Make into gt table 538 theme
out %>% gt() %>% gt_theme_538(quiet = TRUE)

}

test_pitch_neighbors("Last Name, First Name", "Slider", 100, -50, -70, 50)

