# Function to perform slope corrections based on blanks
correct_slopes <- function(output_plate_data, air_blank_positions, nitrogen_positions) {
  combined_plate_data <- dplyr::bind_rows(output_plate_data)

  # Calculate the mean of 'SlopeRaw' for air blank and nitrogen positions, grouped by 'Bay' and 'Day'
  mean_slope_raw_blanks <- aggregate(SlopeRaw ~ Q2Bay + Day, data = combined_plate_data[combined_plate_data$Well %in% air_blank_positions, ], FUN = mean, na.rm = TRUE)
  mean_slope_raw_nitrogen <- aggregate(SlopeRaw ~ Q2Bay + Day, data = combined_plate_data[combined_plate_data$Well %in% nitrogen_positions, ], FUN = mean, na.rm = TRUE)

  mean_slope_raw_blanks <- mean_slope_raw_blanks %>% rename(mean_slope_air_blank = SlopeRaw)
  mean_slope_raw_nitrogen <- mean_slope_raw_nitrogen %>% rename(mean_slope_raw_nitrogen = SlopeRaw)

  # Merge the means back to the combined data frame
  combined_plate_data <- merge(combined_plate_data, mean_slope_raw_blanks, by = c("Q2Bay", "Day"), all.x = TRUE)
  combined_plate_data <- merge(combined_plate_data, mean_slope_raw_nitrogen, by = c("Q2Bay", "Day"), all.x = TRUE)

  # Calculate range per bay
  mean_O2_Air_blank <- aggregate(MeanO2 ~ Q2Bay + Day, data = combined_plate_data[combined_plate_data$Well %in% air_blank_positions, ], FUN = mean, na.rm = TRUE)
  mean_O2_Nitrogen_blank <- aggregate(MeanO2 ~ Q2Bay + Day, data = combined_plate_data[combined_plate_data$Well %in% nitrogen_positions, ], FUN = mean, na.rm = TRUE)
  mean_O2_Air_blank <- mean_O2_Air_blank %>% rename(mean_O2_Air_blank = MeanO2)
  mean_O2_Nitrogen_blank <- mean_O2_Nitrogen_blank %>% rename(mean_O2_Nitrogen_blank = MeanO2)
  combined_plate_data <- merge(combined_plate_data, mean_O2_Air_blank, by = c("Q2Bay", "Day"), all.x = TRUE)
  combined_plate_data <- merge(combined_plate_data, mean_O2_Nitrogen_blank, by = c("Q2Bay", "Day"), all.x = TRUE)

  # Calculate adjusted slopes and ranges
  combined_plate_data <- combined_plate_data %>%
    mutate(
      Q2Range = mean_O2_Air_blank - mean_O2_Nitrogen_blank,
      slopeRawAdj = SlopeRaw + mean_slope_raw_nitrogen + mean_slope_air_blank,
      slopeRawAdjRan = SlopeRaw / Q2Range,
      slopeRawAdjSloRan = slopeRawAdj / Q2Range
    )

  return(combined_plate_data)
}
