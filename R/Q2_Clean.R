#Q2 Data Processing Functions
Q2_Clean <-function(raw.data) {
  n.raw.data<-nrow(raw.data)
  raw.data.clean<-raw.data[-(n.raw.data),]
  return(raw.data.clean)}
