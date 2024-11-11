#' create_plate_metadata
#'
#' Creates metadata for each plate - adds standards to each plate.
#'
#' @param input_sample_metadata,input_plate_data,universal_settings,expected_positions
#' @return input_plate_metadata
#' @export

create_plate_metadata <- function(input_sample_metadata, input_plate_data, universal_settings, expected_positions) {
  input_plate_metadata <- list()

  for (i in names(input_plate_data)) {
    metadata <- as.data.frame(input_sample_metadata[[i]])
    metadata <- metadata %>% rename(Well = Well_Position, LeafArea = Area_cm2)
    Standards <- as.data.frame(universal_settings[["Standards"]])
    Standards$Temperature <- metadata$Temperature[1]
    Standards$LeafArea <- 0
    Standards$LeafDryMass <- 0
    Standards$Pressure_kPa <- metadata$Pressure_kPa[1]
    Standards$Elevation <- metadata$Elevation[1]
    Standards$start.time <- metadata$start.time[1]
    Standards$end.time <- metadata$end.time[1]
    Standards$tubeVol <- metadata$tubeVol[1]
    Standards$Day <- metadata$Day[1]
    Standards$Q2plate <- metadata$Q2plate[1]

    metadata <- metadata %>%
      mutate(SampleName = as.character(SampleName))
    Standards <- Standards %>%
      mutate(SampleName = as.character(SampleName))
    metadata <- bind_rows(metadata, Standards)

    # Reorder based on well position.
    metadata$Well <- factor(metadata$Well, levels = expected_positions)
    metadata <- metadata %>% arrange(Well)

    input_plate_metadata[[i]] <- metadata
  }
  return(input_plate_metadata)
}
