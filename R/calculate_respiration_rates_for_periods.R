#' calculate_respiration_rates_for_periods
#'
#' test test
#'
#' @param combined_plate_data,R,O2_fraction test test
#' @return final_combined_data
#' @export

calculate_respiration_rates_for_periods <- function(combined_plate_data, R, O2_fraction) {
  # Get a list of slope periods from column names
  slope_periods <- grep("SlopeRaw_slope_period_", names(combined_plate_data), value = TRUE)

  # Initialize an empty list to store results for each slope period
  respiration_data_list <- list()

  # Process each slope period
  for (slope_period in slope_periods) {
    # Dynamically construct the corrected slope column names
    slope_col <- slope_period
    slope_adj_col <- gsub("SlopeRaw_slope_period_","slopeRawAdj_",slope_period)
    slope_adj_ran_col <- gsub("SlopeRaw_slope_period_","slopeRawAdjRan_",slope_period)
    slope_adj_slo_ran_col <- gsub("SlopeRaw_slope_period_","slopeRawAdjSloRan_",slope_period)


    # Filter relevant columns for calculations
    current_data <- combined_plate_data %>%
      select(Well, Q2Bay,Q2plate, Day, all_of(slope_col), all_of(slope_adj_col),
             all_of(slope_adj_ran_col), all_of(slope_adj_slo_ran_col),
             Pressure_kPa, tubeVol, Temperature, LeafArea, LeafDryMass)

    # Apply respiration calculations for raw and adjusted slopes
    current_data <- current_data %>%
      mutate(
        SlopeMolar = Pressure_kPa * (O2_fraction * !!sym(slope_col)) * (tubeVol * 0.001) /
          (R * (273.15 + Temperature)) * 1e+06,
        SlopeMolarAdjSlo = Pressure_kPa * (O2_fraction * !!sym(slope_adj_col)) * (tubeVol * 0.001) /
          (R * (273.15 + Temperature)) * 1e+06,
        slopeMolarAdjRan = Pressure_kPa * (O2_fraction * !!sym(slope_adj_ran_col)) * (tubeVol * 0.001) /
          (R * (273.15 + Temperature)) * 1e+06,
        slopeMolarAdjSloRan = Pressure_kPa * (O2_fraction * !!sym(slope_adj_slo_ran_col)) * (tubeVol * 0.001) /
          (R * (273.15 + Temperature)) * 1e+06,
        RespirationAreaRaw = SlopeMolar * (10000 / LeafArea) / (60 * 60),
        RespirationAreaAdjSlo = SlopeMolarAdjSlo * (10000 / LeafArea) / (60 * 60),
        RespirationAreaAdjRan = slopeMolarAdjRan * (10000 / LeafArea) / (60 * 60),
        RespirationAreaAdjSloRan = slopeMolarAdjSloRan * (10000 / LeafArea) / (60 * 60),
        RespirationMassRaw = (SlopeMolar / LeafDryMass) / (60 * 60) * 1000,
        RespirationMassAdjSlo = (SlopeMolarAdjSlo / LeafDryMass) / (60 * 60) * 1000,
        RespirationMassAdjRan = (slopeMolarAdjRan / LeafDryMass) / (60 * 60) * 1000,
        RespirationMassAdjSloRan = (slopeMolarAdjSloRan / LeafDryMass) / (60 * 60) * 1000
      )

    # Rename calculated columns with slope period suffix
    current_data <- current_data %>%
      rename_with(~ paste0(.x, "_", slope_period),
                  starts_with("SlopeMolar") | starts_with("Respiration"))

    # Store results for merging
    respiration_data_list[[slope_period]] <- current_data %>%
      select(Well, Q2Bay,Q2plate, Day, starts_with("SlopeMolar"), starts_with("Respiration"))
  }

  # Combine all respiration data into a single data frame
  respiration_results <- Reduce(function(x, y) merge(x, y, by = c("Well", "Q2Bay","Q2plate", "Day"), all = TRUE), respiration_data_list)

  # Combine the respiration results with original metadata
  final_combined_data <- combined_plate_data %>%
    select(-starts_with("SlopeMolar"), -starts_with("Respiration")) %>%
    left_join(respiration_results, by = c("Well", "Q2Bay","Q2plate", "Day"))

  return(final_combined_data)
}
