library(FNN)
library(gt)
library(gtExtras)
library(scales)

test_pitch_neighbors <- function(pitcher, pitch_type, H1, V1, H3, V3){

pitch_actual <- CCBL_3d_spin %>% filter(Pitcher == pitcher, TaggedPitchType == pitch_type)

example_pitch <- data.frame(
  SpinRate = mean(pitch_actual$SpinRate),
  SpinAxis3dTransverseAngle = mean(pitch_actual$SpinAxis3dTransverseAngle),
  SpinAxis3dLongitudinalAngle = mean(pitch_actual$SpinAxis3dLongitudinalAngle), # Use mean spin efficiency of original pitch
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb1 = H1,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb1 = V1 ,    # These two are referring to points on the mollweide plot
  SpinAxis3dSeamOrientationBallAngleHorizontalAmb3 = H3,  # Adjustment of seam orientation
  SpinAxis3dSeamOrientationBallAngleVerticalAmb3 = V3    # These two are referring to points on the mollweide plot
)


feat_cols <- c(
  "SpinRate",
  "SpinAxis3dTransverseAngle",
  "SpinAxis3dLongitudinalAngle",
  "SpinAxis3dSeamOrientationBallAngleHorizontalAmb1",
  "SpinAxis3dSeamOrientationBallAngleVerticalAmb1",
  "SpinAxis3dSeamOrientationBallAngleHorizontalAmb3",
  "SpinAxis3dSeamOrientationBallAngleVerticalAmb3"
)

# Feature weights(care more about finding matching seam orientations than spin rates)
w <- c(
  SpinRate = 1.0,
  SpinAxis3dTransverseAngle = 3,
  SpinAxis3dLongitudinalAngle = 1,
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
out <- cbind(CCBL_3d_spin[neighbors_idx, c("Pitcher", "TaggedPitchType", "InducedVertBreak","HorzBreak", "SpinAxis3dSpinEfficiency", "SpinAxis3dTilt")], CCBL_3d_spin[neighbors_idx, feat_cols])

out %>% summarize(
  TaggedPitchType = TaggedPitchType,
  IVB = round(InducedVertBreak, 1),
  HB = round(HorzBreak, 1),
  SpinRate = round(SpinRate, 0),
  SpinDir = SpinAxis3dTilt,
  SpinEfficiency = percent(round(SpinAxis3dSpinEfficiency, 4)),
  H1 = round(SpinAxis3dSeamOrientationBallAngleHorizontalAmb1, 0),
  V1 = round(SpinAxis3dSeamOrientationBallAngleVerticalAmb1, 0),
  H3 = round(SpinAxis3dSeamOrientationBallAngleHorizontalAmb3, 0),
  V3 = round(SpinAxis3dSeamOrientationBallAngleVerticalAmb3, 0)
) %>% gt() %>% tab_spanner(
  label = "Inputted Spin & Seam Orientation",
  columns = c(SpinRate, SpinDir, SpinEfficiency, H1, V1, H3, V3)) %>% tab_spanner(
      label = "Movement",
      columns = c(IVB, HB)) %>% gt_theme_538(quiet = TRUE)

}

test_pitch_neighbors("Last Name, First Name", "Slider", 100, -50, -70, 50)

