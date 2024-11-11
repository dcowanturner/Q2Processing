#' IngestAndMergeXLS
#'
#' Import all .xls files in the directory, process sheets, and merge into a single data frame
#'
#' @param InputDir
#' @return merged_data
#' @export

IngestAndMergeXLS <- function(InputDir) {

  # Check if the directory exists
  if (!dir.exists(InputDir)) {
    stop("Error: Please input a real directory!")
  }

  # List all .xls files in the directory
  xls_files <- list.files(path = InputDir, pattern = "^[^~].*.xls$", full.names = TRUE)

  if (length(xls_files) == 0) {
    stop("Error: No .xls files found in the directory!")
  }

  # Pull in all run files and merge them into a single data frame
  merged_data <- map_df(xls_files, function(Filename) {
    Sheets <- excel_sheets(Filename)
    FileNameBase <- tools::file_path_sans_ext(basename(Filename))

    map_df(Sheets, function(sheet) {
      Temp <- read_excel(Filename, skip = 20, sheet = sheet)
    })
  })

  return(merged_data)
}
