
# Load libraries
library(ggplot2)
library(dplyr)
library(ggforce)



# Defining pitch type coloring
TMcolors <- c("4-Seam Fastball" = "#8E8E8E",
              "Fastball" = "#8E8E8E",
              "Cutter" = "purple",
              "Sinker" = "#E50E00",
              "Slider" = "#4595FF",
              "Sweeper" = "#6ACDE5",
              "Slurve" = "#D38B31",
              "Changeup" = "#009A09",
              "ChangeUp" = "#009A09",
              "Split-Finger" = "#11C67B",
              "Splitter" = "#11C67B",
              "Curveball" = "orange",
              "Knuckle Curve" = "orange",
              "Screwball" = "#ECE400",
              "Forkball" = "#00F9AC",
              "Knuckleball" = "aquamarine")


# Load your TrackMan dataset
# If you want to put in a single game csv
# data_unfiltered <- read.csv("~/Downloads/20250718-CCBLChatham-1_unverified.csv")

# All of CCBL data
data_unfiltered <- CCBL_Combined_2025

# Creating a function to make it easier
plot_mollweide <- function(name){
# Choose a pitcher
pitcher_name <- name
# Prepare data for plotting
# Filter out any rows without 3D spin data, filter for pitcher selected
data <- data_unfiltered %>%
  filter(!is.na(SpinAxis3dSeamOrientationBallAngleHorizontalAmb1),
         !is.na(SpinAxis3dSeamOrientationBallAngleVerticalAmb1)) %>% filter(Pitcher == pitcher_name)


# # Find abs value of sum of angles, as described in TrackMan's documentation, this is how to find correct Amb to use
# data$angle_sum1 <- abs(data$SpinAxis3dSeamOrientationBallAngleHorizontalAmb1) + abs(data$SpinAxis3dSeamOrientationBallAngleVerticalAmb1)
# data$angle_sum2 <- abs(data$SpinAxis3dSeamOrientationBallAngleHorizontalAmb2) + abs(data$SpinAxis3dSeamOrientationBallAngleVerticalAmb2)
# data$angle_sum3 <- abs(data$SpinAxis3dSeamOrientationBallAngleHorizontalAmb3) + abs(data$SpinAxis3dSeamOrientationBallAngleVerticalAmb3)
# data$angle_sum4 <- abs(data$SpinAxis3dSeamOrientationBallAngleHorizontalAmb4) + abs(data$SpinAxis3dSeamOrientationBallAngleVerticalAmb4)
# 
# # Create a matrix of the four angle sums
# angle_sums <- data[, c("angle_sum1", "angle_sum2", "angle_sum3", "angle_sum4")]
# 
# # Apply across rows to get min index (1 = Amb1, 2 = Amb2, etc.)
# data$min_index <- apply(angle_sums, 1, which.min)


# # Use the min_index column to reference which Column(Amb1, Amb2, etc.) to use
# # accounting for both "entry" and "exit" points of the spin axis
# data <- data %>%
#   mutate(
#     SpinAxis_h = case_when(
#       min_index == 1 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb1,
#       min_index == 2 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb2,
#       min_index == 3 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb3,
#       min_index == 4 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb4
#     ),
#     SpinAxis_v = case_when(
#       min_index == 1 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb1,
#       min_index == 2 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb2,
#       min_index == 3 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb3,
#       min_index == 4 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb4
#     ),
#     SpinAxis_h_2 = case_when(
#       min_index == 1 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb3,
#       min_index == 2 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb4,
#       min_index == 3 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb1,
#       min_index == 4 ~ SpinAxis3dSeamOrientationBallAngleHorizontalAmb2
#     ),
#     SpinAxis_v_2 = case_when(
#       min_index == 1 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb3,
#       min_index == 2 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb4,
#       min_index == 3 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb1,
#       min_index == 4 ~ SpinAxis3dSeamOrientationBallAngleVerticalAmb2
#     )
#   )



# My defined seam control points
seam_points <- data.frame(
  lon = c(-157, -90, 0, 90, 157, 90, 0, -90, -157),
  lat = c(   0,  45, 22.5, 45,   0, -45, -22.5, -45,   0)
)

# Step 1: Create a parametric variable t (like time)
t <- seq_along(seam_points$lon)

# Step 2: Interpolate lon and lat as functions of t
interp_n <- 300
t_interp <- seq(min(t), max(t), length.out = interp_n)

lon_spline <- spline(t, seam_points$lon, xout = t_interp)$y
lat_spline <- spline(t, seam_points$lat, xout = t_interp)$y

spline_curve <- data.frame(lon = lon_spline, lat = lat_spline)




# Plot Mollweide Projection of seam orientation
ggplot(data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb1, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb1, color = TaggedPitchType)) +
  scale_color_manual(values = TMcolors) + 
  geom_path(data = spline_curve, aes(x = lon, y = lat), color = "red", linewidth = 1.5) +
  geom_point(size = 1.5, alpha = 0.75) +
  geom_point(data = data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb2, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb2, color = TaggedPitchType)) +
  geom_point(data = data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb3, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb3, color = TaggedPitchType)) +
  geom_point(data = data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb4, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb4, color = TaggedPitchType)) +
  coord_map("mollweide") +  # projection type
  scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 45)) +
  scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
  labs(
    title = paste0("Seam Orientation: ", pitcher_name),
    subtitle = "Using TrackMan 3D Spin",
    x = "Horizontal Angle (°)",
    y = "Vertical Angle (°)",
    color = "Pitch Type"
  ) +
  theme_minimal()
}


plot_mollweide("Stephens, Jordan")
data_unfiltered$Pitcher
