#' calculate_slopes_plate_loop
#'
#' Fixed  030225 - Issue with well matching
#'
#' @param raw.data.clean,metadata,time_windows TEST
#' @return output_plate_data
#' @export


calculate_slopes_plate_loop <- function (Input.plate.data, Input.plate.metadata,time_windows)
{
  output_plate_data <- list()
  for (i in names(Input.plate.data)) {
    raw_data <- as.data.frame(Input.plate.data[[i]])
    raw_data_clean <- Q2_Clean(raw_data)
    metadata <- as.data.frame(Input.plate.metadata[[i]])
    # metadata <- metadata %>% mutate(metaWell=Well)  %>% select(-Well)


    output <- calculate_slopes_for_periods(raw_data_clean,
                                           metadata,time_windows)


    output <- left_join(metadata,Testout,by="Well")

    output_plate_data[[i]] <- output
  }
  return(output_plate_data)
}
