#' calculate_slopes_for_periods
#'
#' Find the lowest point in each slope period - we can use this to work out the point at which 5% O2 is reached
#'
#' @param raw.data.clean,metadata,time_windows Define overlapping time windows
#' @return final_results
#' @export

# Define overlapping time windows
calculate_slopes_for_periods <- function(raw.data.clean, metadata, time_windows) {
  # Initialize an empty list to store results for each time window
  results <- list()

  # Loop through each time window
  for (time_window in time_windows) {
    start.time <- time_window[1]
    end.time <- time_window[2]

    # Filter the raw data for the current time window
    slopeRange <- subset(raw.data.clean, HRS > start.time & HRS < end.time)

    # Skip if no data is available in the range
    if (nrow(slopeRange) == 0) next

    # Calculate slopes, R² values, lowest O₂, and mean O₂ for each well
    slopes_r_squared <- sapply(slopeRange[, 7:ncol(slopeRange)], function(column) {
      model <- lm(column ~ slopeRange$HRS)
      slope <- abs(coef(model)[2])
      r_squared <- summary(model)$r.squared
      lowest_o2 <- min(column, na.rm = TRUE)
      mean_o2 <- mean(column, na.rm = TRUE)
      return(c(slope = slope, r_squared = r_squared, lowest_o2 = lowest_o2, mean_o2 = mean_o2))
    })

    # Convert the results to a data frame
    slopes_r_squared_df <- as.data.frame(t(slopes_r_squared))
    colnames(slopes_r_squared_df) <- c("SlopeRaw", "R_Squared", "LowestO2", "MeanO2")
    slopes_r_squared_df$Well <- colnames(slopeRange)[7:ncol(slopeRange)]

    # Add the time period suffix to column names
    time_suffix <- paste0("_slope_period_", start.time, "_", end.time)
    colnames(slopes_r_squared_df)[1:4] <- paste0(colnames(slopes_r_squared_df)[1:4], time_suffix)

    # Append results to the list
    results[[time_suffix]] <- slopes_r_squared_df
  }

  # Combine all results into a single data frame
  final_results <- Reduce(function(x, y) merge(x, y, by = "Well", all = TRUE), results)

  return(final_results)
}
