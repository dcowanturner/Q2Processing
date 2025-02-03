#' calculate_Low_O2
#'
#' Function that finds 5% O2 point and adds to DF.  Needs to be 5% of total. Might be depreciated...
#'
#'
#' @param Input.plate.data,Input.plate.metadata test test
#' @return output_plate_data
#' @export

calculate_Low_O2 <- function (Input.plate.data, Input.plate.metadata)
{
  output_plate_data <- list()
  for (i in names(Input.plate.data)) {
    raw_data <- as.data.frame(Input.plate.data[[i]])
    raw_data_clean <- Q2_Clean(raw_data)
    metadata <- as.data.frame(Input.plate.metadata[[i]])
    metadata <- metadata %>% select(-Well)

    output <- cbind(Q2_Calculate_slopes(raw_data_clean, metadata), metadata)
    output_plate_data[[i]] <- output
  }
  return(output_plate_data)
}


