#' add_bay_to_metadata
#'
#' Add bay to metadata - might merge this into another function.
#'
#' @param input_plate_metadata,bays Test test test
#' @return input_plate_metadata
#' @export
add_bay_to_metadata <- function(input_plate_metadata, bays) {
  input_plate_metadata <- lapply(input_plate_metadata, function(df) {
    df <- merge(df, bays, by = "Q2plate", all.x = TRUE)
    return(df)
  })
  return(input_plate_metadata)
}
