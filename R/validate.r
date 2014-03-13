#' Check validty of PHAB data
#'
#' @param phabdata A data frame of SWAMP formatted PHAB data
#' @param returnPurged If false (default), return data frame
#' listing problematic rows. If true, return data frame where 
#' SampleID/AnalyteName combinations that contain problematic
#' rows have been removed.
#' @export

validatePHAB <- function(data, returnPurged = FALSE) {
  requiredCols <- c("StationCode", "SampleID", "LocationCode",
                    "AnalyteName", "VariableResult", "Result",
                    "FractionName", "ResQualCode")
  missingCols <- requiredCols[!(requiredCols %in% names(data))]
  if(length(missingCols) > 0){
    mis <- paste(missingCols, sep="", collapse=" ")
    stop(paste("The following required fields are missing:", mis))
  }
  
  bad <- !is.na(data$Result) & as.numeric(data$Result) < 0
  
  if(returnPurged){
    badsamples <- unique(data[bad, c("SampleID", "AnalyteName")])
    good <- sapply(1:nrow(badsamples), function(i){
      which(data$SampleID == badsamples$SampleID[i] &
              data$AnalyteName == badsamples$AnalyteName[i])
    })
    good <- Reduce(union, good)
    data[!(data$SampleID %in% badsamples$SampleID & 
             data$AnalyteName %in% badsamples$AnalyteName), ]
  }
  else
    data[bad, c("StationCode", "SampleID", "LocationCode", "AnalyteName",
                "VariableResult", "Result")]
}