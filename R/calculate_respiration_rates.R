#' calculate_respiration_rates
#'
#' Function to calculate respiration rates
#'
#' @param combined_plate_data Test
#' @return combined_plate_data
#' @export
calculate_respiration_rates <- function(combined_plate_data, R, O2_fraction) {
  combined_plate_data <- combined_plate_data %>%
    mutate(
      SlopeMolar = Pressure_kPa * (O2_fraction * SlopeRaw) * (tubeVol * 0.001) / (R * (273.15 + Temperature)) * 1000000,
      SlopeMolarAdjSlo = Pressure_kPa * (O2_fraction * slopeRawAdj) * (tubeVol * 0.001) / (R * (273.15 + Temperature)) * 1000000,
      slopeMolarAdjRan = Pressure_kPa * (O2_fraction * slopeRawAdjRan) * (tubeVol * 0.001) / (R * (273.15 + Temperature)) * 1000000,
      slopeMolarAdjSloRan = Pressure_kPa * (O2_fraction * slopeRawAdjSloRan) * (tubeVol * 0.001) / (R * (273.15 + Temperature)) * 1000000,

      # Convert to area-based respiration rate (µmol m-2 s-1)
      RespirationAreaRaw = SlopeMolar * (10000 / LeafArea) / (60 * 60),
      RespirationAreaAdjSlo = SlopeMolarAdjSlo * (10000 / LeafArea) / (60 * 60),
      RespirationAreaAdjRan = slopeMolarAdjRan * (10000 / LeafArea) / (60 * 60),
      RespirationAreaAdjSloRan = slopeMolarAdjSloRan * (10000 / LeafArea) / (60 * 60),

      # Convert to dry mass-based respiration rate (nmol g DM s-1)
      RespirationMassRaw = (SlopeMolar / LeafDryMass) / (60 * 60) * 1000,
      RespirationMassAdjSlo = (SlopeMolarAdjSlo / LeafDryMass) / (60 * 60) * 1000,
      RespirationMassAdjRan = (slopeMolarAdjRan / LeafDryMass) / (60 * 60) * 1000,
      RespirationMassAdjSloRan = (slopeMolarAdjSloRan / LeafDryMass) / (60 * 60) * 1000
    )

  return(combined_plate_data)
}
