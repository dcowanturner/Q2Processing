#' correct_slopes_for_periods
#'
#' New version that calculate_slopes_for_periods
#'
#' @param output_plate_data,air_blank_positions,nitrogen_positions Define overlapping time windows
#' @return final_corrected_data
#' @export

correct_slopes_for_periods <- function(output_plate_data, air_blank_positions, nitrogen_positions) {
  # Combine data from all plates
  combined_plate_data <- dplyr::bind_rows(output_plate_data)

  # Get a list of slope periods from column names
  slope_periods <- grep("SlopeRaw_slope_period_", names(combined_plate_data), value = TRUE)

  # Initialize an empty list to store results for each slope period
  corrected_data_list <- list()

  # Process each slope period
  for (slope_period in slope_periods) {
    # Extract the slope period-specific columns
    slope_col <- slope_period
    mean_o2_col <- gsub("SlopeRaw", "MeanO2", slope_period)

    # Filter relevant columns for the current slope period
    current_data <- combined_plate_data %>%
      select(all_of(c("Well", "Q2Bay","Q2plate", "Day", slope_col, mean_o2_col)), everything())

    # Calculate mean air and nitrogen blanks for the current slope period
    mean_slope_raw_blanks <- current_data %>%
      filter(Well %in% air_blank_positions) %>%
      group_by(Q2Bay, Day) %>%
      summarise(mean_slope_air_blank = mean(!!sym(slope_col), na.rm = TRUE), .groups = "drop")

    mean_slope_raw_nitrogen <- current_data %>%
      filter(Well %in% nitrogen_positions) %>%
      group_by(Q2Bay, Day) %>%
      summarise(mean_slope_raw_nitrogen = mean(!!sym(slope_col), na.rm = TRUE), .groups = "drop")

    # Calculate mean O₂ blanks
    mean_o2_air_blank <- current_data %>%
      filter(Well %in% air_blank_positions) %>%
      group_by(Q2Bay, Day) %>%
      summarise(mean_o2_air_blank = mean(!!sym(mean_o2_col), na.rm = TRUE), .groups = "drop")

    mean_o2_nitrogen_blank <- current_data %>%
      filter(Well %in% nitrogen_positions) %>%
      group_by(Q2Bay, Day) %>%
      summarise(mean_o2_nitrogen_blank = mean(!!sym(mean_o2_col), na.rm = TRUE), .groups = "drop")

    # Merge blank means back to the current data
    slope_data <- current_data %>%
      left_join(mean_slope_raw_blanks, by = c("Q2Bay", "Day")) %>%
      left_join(mean_slope_raw_nitrogen, by = c("Q2Bay", "Day")) %>%
      left_join(mean_o2_air_blank, by = c("Q2Bay", "Day")) %>%
      left_join(mean_o2_nitrogen_blank, by = c("Q2Bay", "Day"))

    # Apply corrections
    slope_data <- slope_data %>%
      mutate(
        Q2Range = mean_o2_air_blank - mean_o2_nitrogen_blank,
        slopeRawAdj = !!sym(slope_col) + mean_slope_raw_nitrogen + mean_slope_air_blank,
        slopeRawAdjRan = !!sym(slope_col) / Q2Range,
        slopeRawAdjSloRan = slopeRawAdj / Q2Range
      )

    # Rename corrected columns with slope period suffix
    # slope_data <- slope_data %>%
    #   rename_with(~ paste0(.x, "_", gsub("SlopeRaw_slope_period_", "", slope_period)),
    #               c("slopeRawAdj", "slopeRawAdjRan", "slopeRawAdjSloRan"))
    #
    slope_data <- slope_data %>%
      rename_with(
        ~ paste0(.x, "_", gsub("SlopeRaw_slope_period_", "", slope_period)),
        c("slopeRawAdj", "slopeRawAdjRan", "slopeRawAdjSloRan", "mean_o2_air_blank", "mean_o2_nitrogen_blank")
      )

    # Store results for merging
    corrected_data_list[[slope_period]] <- slope_data %>%
      select(Well, Q2Bay, Q2plate, Day, starts_with(slope_col), starts_with("slopeRawAdj"), starts_with("mean_o2"))



    # Store results for merging
    # corrected_data_list[[slope_period]] <- slope_data %>%
    #   select(Well, Q2Bay,Q2plate, Day, starts_with(slope_col), starts_with("slopeRawAdj"))
  }

  # Combine all corrected data into a single data frame
  corrected_slopes <- Reduce(function(x, y) merge(x, y, by = c("Well", "Q2Bay","Q2plate", "Day"), all = TRUE), corrected_data_list)

  # Add metadata and other columns back to the combined data
  final_corrected_data <- combined_plate_data %>%
    select(-starts_with("SlopeRaw_slope_period_")) %>%
    left_join(corrected_slopes, by = c("Well", "Q2Bay", "Q2plate","Day"))

  return(final_corrected_data)
}
