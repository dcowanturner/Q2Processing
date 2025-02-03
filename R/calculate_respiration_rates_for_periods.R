#' IngestAndMergeXLS_DP22
#'
#' test test
#'
#' @param combined_plate_data,R,O2_fraction test test
#' @return final_combined_data
#' @export


IngestAndMergeXLS_DP22 <- function(InputDir) {
  # Check if the input directory exists
  if (!dir.exists(InputDir)) {
    stop("Error: Please input a real directory!")
  }

  # List all .xls files in the directory (excluding temporary files starting with '~')
  xls_files <- list.files(path = InputDir, pattern = "^[^~].*.xls$", full.names = TRUE)

  # Check if there are any .xls files in the directory
  if (length(xls_files) == 0) {
    stop("Error: No .xls files found in the directory!")
  }

  # Read and merge data from all files
  merged_data <- map_df(xls_files, function(Filename) {
    Sheets <- excel_sheets(Filename)
    FileNameBase <- tools::file_path_sans_ext(basename(Filename))

    # Read data from each sheet within the file
    map_df(Sheets, function(sheet) {
      Temp <- read_excel(Filename, skip = 20, sheet = sheet)

      # Add a new column with the filename
      Temp <- Temp %>%
        mutate(SourceFile = FileNameBase)

      return(Temp)
    })
  })

  return(merged_data)
}
