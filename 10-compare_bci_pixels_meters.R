rm(list=ls())
#load pipe operator
library(magrittr)

#load data 
load("mes_dat_final_vid.RData")
load("vid_dat_final.RData")


#join mes_dat_final_vid with vid_dat_final based on column video_name_long
mes_dat_final <- dplyr::left_join(mes_dat_final_vid, vid_dat_final, by = "video_name_long") # IMPORTANT use left_join to keep all observations in x (mes_dat_final_vid)

mes_dat_final <- mes_dat_final %>% 
  dplyr::select(c(video_name_long, type_individual, mean_l_nb_pixels, mean_w_nb_pixels, drone_alt, sensor_height, sensor_width, focal_length, video_width, video_height, mean_body_condition))


# Convert variables to numeric variables 
mes_dat_final$mean_l_nb_pixels <- as.numeric(mes_dat_final$mean_l_nb_pixels)
mes_dat_final$mean_w_nb_pixels <- as.numeric(mes_dat_final$mean_w_nb_pixels)
mes_dat_final$drone_alt <- as.numeric(mes_dat_final$drone_alt)
mes_dat_final$sensor_height <- as.numeric(mes_dat_final$sensor_height)
mes_dat_final$sensor_width <- as.numeric(mes_dat_final$sensor_width)
mes_dat_final$focal_length <- as.numeric(mes_dat_final$focal_length)
mes_dat_final$video_width <- as.numeric(mes_dat_final$video_width)
mes_dat_final$video_height <- as.numeric(mes_dat_final$video_height)


# Create new columns to store the calculated lengths and widths
mes_dat_final$mean_l_meters <- NA
mes_dat_final$mean_w_meters <- NA
mes_dat_final$mean_body_condition_meters <- NA
mes_dat_final$diff_body_condition <- NA


# Loop through each row of the dataframe
for (i in 1:nrow(mes_dat_final)) {
  # Check if any of the required columns have NA
  if (is.na(mes_dat_final$mean_l_nb_pixels[i]) ||
      is.na(mes_dat_final$mean_w_nb_pixels[i]) ||
      is.na(mes_dat_final$drone_alt[i]) ||
      is.na(mes_dat_final$sensor_height[i]) ||
      is.na(mes_dat_final$sensor_width[i]) ||
      is.na(mes_dat_final$focal_length[i]) ||
      is.na(mes_dat_final$video_width[i]) ||
      is.na(mes_dat_final$video_height[i])) {
    # Skip to the next iteration if any value is NA
    next
  }
  
  # Extract the required values from the dataframe
  mean_l_nb_pixels <- mes_dat_final$mean_l_nb_pixels[i]
  mean_w_nb_pixels <- mes_dat_final$mean_w_nb_pixels[i]
  drone_alt <- mes_dat_final$drone_alt[i]
  sensor_height <- mes_dat_final$sensor_height[i]
  sensor_width <- mes_dat_final$sensor_width[i]
  focal_length <- mes_dat_final$focal_length[i]
  video_width <- mes_dat_final$video_width[i]
  video_height <- mes_dat_final$video_height[i]
  
  
  # Calculate GSD width and height from https://visionaerial.com/what-is-ground-sample-distance/
  gsd_width <- (drone_alt * sensor_width) / (focal_length * video_width) # in meter/pixel
  gsd_height <- (drone_alt * sensor_height) / (focal_length * video_height) # in meter/pixel
  gsd <- (gsd_width + gsd_height) / 2  # in meter/pixel
  
  
  # Calculate real length and width of dugong
  mean_l_meters <- mean_l_nb_pixels * gsd # in meters
  mean_w_meters <- mean_w_nb_pixels * gsd # in meters 
  
  
  # Calculate body condition meters
  mean_body_condition_meters <- (mean_w_meters * pi) / mean_l_meters
  
  
  # Store the calculated values back in the dataframe
  mes_dat_final$mean_l_meters[i] <- mean_l_meters
  mes_dat_final$mean_w_meters[i] <- mean_w_meters
  mes_dat_final$mean_body_condition_meters[i] <- mean_body_condition_meters
  
  
  # Calculate the difference between mean_body_condition and mean_body_condition_meters
  if (!is.na(mes_dat_final$mean_body_condition[i])) {
    mes_dat_final$diff_body_condition[i] <- mes_dat_final$mean_body_condition[i] - mean_body_condition_meters
  }
  
}

nrow(mes_dat_final)
table(is.na(mes_dat_final$mean_body_condition_meters)) #24 indiv with BCI both in meters and in pixels

#linear regression
mod = lm(mean_body_condition ~ mean_body_condition_meters, data = mes_dat_final)
summary(mod) #R2 0.9999


#plot 
ggplot2::ggplot(data = mes_dat_final, ggplot2::aes(x = mean_body_condition, y = mean_body_condition_meters)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = lm, colour = "dark grey") +
  ggplot2::xlim(c(0.6, 0.9)) +
  ggplot2::ylim(c(0.6, 0.9)) +
  ggplot2::xlab("Relative BCI (pixels)") +
  ggplot2::ylab("Absolute BCI (meters)") +
  ggplot2::theme_light() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 15),
                 axis.text = ggplot2::element_text(size = 13))

ggplot2::ggsave(here::here("outputs", "10-compare_bci_pixels_meters.R", "plot_body_condition_pixels_VS_meters.png"), width = 7, height = 5)





