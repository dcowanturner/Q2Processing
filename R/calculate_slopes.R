#' calculate_slopes
#'
#' Wrapper around Q2_Calculate_slopes - that allows runs Q2_Calculate_slopes on a loop over plates.
#'
#' @param Input.plate.data test
#' @return output_plate_data
#' @export
calculate_slopes <- function(Input.plate.data, Input_plate_metadata) {
  output_plate_data <- list()

  for (i in names(Input.plate.data)) {
    raw_data <- as.data.frame(Input.plate.data[[i]])
    raw_data_clean <- Q2_Clean(raw_data)

    metadata <- as.data.frame(Input_plate_metadata[[i]])
    metadata <- metadata %>% select(-Well)

    output <- cbind(Q2_Calculate_slopes(raw_data_clean, metadata), metadata)
    output_plate_data[[i]] <- output
  }
  return(output_plate_data)
}


