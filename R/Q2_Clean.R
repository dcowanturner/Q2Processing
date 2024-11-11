#' Q2_Clean
#'
#' Cleans Q2 data - might be removed, not doing much
#'
#' @param raw.data
#' @return Clean Q2 data
#' @export


Q2_Clean <-function(raw.data) {
  n.raw.data<-nrow(raw.data)
  raw.data.clean<-raw.data[-(n.raw.data),]
  return(raw.data.clean)}
