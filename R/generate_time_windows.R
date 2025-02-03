#' generate_time_windows
#'
#' Define overlapping time windows
#'
#' @param start,end Define overlapping time windows
#' @return time_windows
#' @export

# Define overlapping time windows
generate_time_windows <- function(start, end, step = 0.5, window_size = 2) {
  time_windows <- list()
  current_start <- start
  while (current_start + window_size <= end) {
    current_end <- current_start + window_size
    time_windows <- append(time_windows, list(c(current_start, current_end)))
    current_start <- current_start + step
  }
  return(time_windows)
}
