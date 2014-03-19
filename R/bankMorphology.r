
bankMorphology <- function(x){
  
  data <- subset(x, AnalyteName %in% c("Bank Stability", "Bankfull Height", "Bankfull Width") &
                   ResQualCode == "=")
  if(nrow(data) == 0)return(data.frame("SampleID"=NULL,
                                       "metric"=NULL, 
                                       "mean"=NULL,
                                       "sd"=NULL,
                                       "count"=NULL))
  data$Result <- as.numeric(data$Result)
  data$Location2 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])

  stability <- subset(data, AnalyteName == "Bank Stability", c(SampleID, Location2, VariableResult))  
  if(nrow(stability) == 0) {
    stability_result <- data.frame("SampleID"=NULL,
                                   "metric"=NULL, 
                                   "mean"=NULL,
                                   "sd"=NULL,
                                   "count"=NULL)
  } else {
    
    
    metrics <- c(PBM_S = function(x)100 * mean(x$VariableResult == 'stable'),
                 PBM_V = function(x)100 * mean(x$VariableResult == 'vulnerable'),
                 PBM_E = function(x)100 * mean(x$VariableResult == 'eroded'))
    
    stability_result <- metricCalc(NULL)(stability, metrics)
  }
  bankdata <- subset(data, AnalyteName %in% c("Bankfull Height", "Bankfull Width"))
  if(nrow(bankdata) > 0){
  bankfull <- dcast(bankdata,
                    SampleID + Location2 ~ AnalyteName, value.var="Result", mean)
  if(is.null(bankfull$'Bankfull Height'))bankfull$'Bankfull Height' <- NA
  if(is.null(bankfull$'Bankfull Width'))bankfull$'Bankfull Width' <- NA
  
  bankfull_result <- metricCalc(NULL)(bankfull, c(XBKF_H = function(x)mean(x$'Bankfull Height', na.rm=TRUE),
                                                  XBKF_W = function(x)mean(x$'Bankfull Width', na.rm=TRUE)))
  } else
    bankfull_result <- data.frame("SampleID"=NULL,
                                 "metric"=NULL, 
                                 "mean"=NULL,
                                 "sd"=NULL,
                                 "count"=NULL)
  rbind(stability_result, bankfull_result)
}