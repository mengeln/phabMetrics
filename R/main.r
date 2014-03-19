#' Calculate SWAMP PHAB metrics
#'
#' @param phabdata A data frame of SWAMP formatted PHAB data
#' @param detailed When set to true, returns a list with 1. a data frame of
#' the results, and 2. a list of character vectors containing the metrics that
#' were not calculated for each SampleID.
#' @param verbose 0 (default) is silent, 1 prints SampleIDs, 2 prints metrics
#' @return A data frame with the metric values, standard deviations, and
#' sample size for all successfully calculated metrics
#' @export


metricsPHAB <- function(phabdata,
                        detailed = FALSE,
                        verbose = 0){
  metricFun <- function (data) {
    
    
    metrics <- c("algae", "bankMorphology", "channelMorphology", "habitatComplexity",
                 "humanDisturbance", "riparianVegetation", "slope", 
                 "substrate", "flow", "misc") 
    
    resultList <- lapply(metrics, function(f){
      if(verbose >= 2)print(f)
      fun <- get(f)
      tryCatch(fun(data), error = function(e){
        print(paste("Problem with", 
                    unique(data$SampleID),
                    "in", f, "metrics; discarding"))
        NULL})})
    resultList <- resultList[sapply(resultList, is.data.frame)]
    result <- rbind.fill(resultList)
    if(detailed){
      notCalc <- metricCodes[!metricCodes %in% result$metric] 
      list(result, notCalc)
    } else{
      result
    }
    
  }
  datasplit <- split(phabdata, phabdata$SampleID)
  results <- lapply(datasplit, function(d){
    if(verbose >= 1)print(unique(d$SampleID))
    metricFun(d)})
  
  if(detailed){
    rdata <- lapply(results, '[[', 1)
    resultsFinal <- rbind.fill(rdata)[, 1:5]
    notCalc <- lapply(results, '[[', 2)
    list(results = resultsFinal, notCalculated = notCalc)
  } else {
    rbind.fill(results)[, 1:5]
  }
}







