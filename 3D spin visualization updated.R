
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
  
  seam_curve <- data.frame(lon = lon_spline, lat = lat_spline)
  
  
  
  
  # Plot Mollweide Projection of seam orientation
  ggplot(data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb1, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb1)) +
    geom_path(data = seam_curve, aes(x = lon, y = lat), color = "red", linewidth = 1.5, inheret.aes = FALSE) +
    geom_point(size = 1.5, aes(fill = TaggedPitchType), color = "black", shape = 21) +
    geom_point(data = data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb2, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb2, fill = TaggedPitchType), color = "black", shape = 21, size = 1.5) +
    geom_point(data = data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb3, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb3, fill = TaggedPitchType), color = "black", shape = 21, size = 1.5) +
    geom_point(data = data, aes(x = SpinAxis3dSeamOrientationBallAngleHorizontalAmb4, y = SpinAxis3dSeamOrientationBallAngleVerticalAmb4, fill = TaggedPitchType), color = "black", shape = 21, size = 1.5) +
    scale_fill_manual(values = TMcolors) + 
    coord_map("mollweide") +  # projection type
    scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 45)) +
    scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
    labs(
      title = paste0("Seam Orientation: ", pitcher_name),
      # title = paste0("Seam Orientation:  Anonymous Pitcher"),
      subtitle = "Using TrackMan 3D Spin",
      x = "Horizontal Angle (°)",
      y = "Vertical Angle (°)",
      color = "Pitch Type"
    ) +
    theme_minimal()
}


plot_mollweide("Last Name, First Name")
