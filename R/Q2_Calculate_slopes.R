#Calculate slopes and R-squared values
Q2_Calculate_slopes <- function(raw.data.clean,metadata) {
  start.time<- metadata$start.time[1]
  end.time<-  metadata$end.time[1]
  slopeRange<-subset(raw.data.clean, HRS > start.time & HRS < end.time)
  data <- slopeRange

  slopes_r_squared <- sapply(data[, 7:ncol(data)], function(column) {
    model <- lm(column ~ data$HRS)
    slope <- abs(coef(model)[2])
    r_squared <- summary(model)$r.squared
    meanO2 <- mean(column, na.rm = TRUE)
    return(c(slope = slope, r_squared = r_squared,meanO2=meanO2))
  })

  # Transpose and convert to data frame
  slopes_r_squared_df <- as.data.frame(t(slopes_r_squared))

  # Rename columns for better readability
  colnames(slopes_r_squared_df) <- c("SlopeRaw", "R_Squared", "MeanO2")

  # Add a Well column for sample names
  slopes_r_squared_df$Well <- colnames(data)[7:ncol(data)]

  # Reorder the columns to have Well first
  slopes_r_squared_df <- slopes_r_squared_df[, c("Well", "SlopeRaw", "R_Squared", "MeanO2")]


  return(slopes_r_squared_df)
}
