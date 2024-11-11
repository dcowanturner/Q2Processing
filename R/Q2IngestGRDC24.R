#' Q2IngestGRDC24
#'
#' Q2IngestGRDC24
#'
#' @param InputRawData Test test test
#' @return all_sheets
#' @export
Q2IngestGRDC24 <- function(InputRawData) {
  # Check if the file exists
  if (!file.exists(InputRawData)) {
    stop("Error: The specified file does not exist!")
  }

  # Import all sheets into a single named list
  all_sheets <- excel_sheets(InputRawData) %>%
    set_names() %>%
    map(~ read_excel(InputRawData, sheet = .x))

  return(all_sheets)
}
