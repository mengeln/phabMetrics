
slope <- function(x){
  
  analytes <- c("Proportion", "Elevation Difference", "Length, Segment", "Slope", "Bearing")
  data <- subset(x, AnalyteName %in% analytes)
  if(nrow(data) == 0 || !all(analytes %in% names(data)))
    return(data.frame("SampleID"=NULL,
                      "metric"=NULL, 
                      "mean"=NULL,
                      "sd"=NULL,
                      "count"=NULL))
  data$Result <- as.numeric(data$Result)
  
  
  data$Location2 <- paste0(data$LocationCode, data$FractionName)
  slope <- dcast(data, SampleID + Location2 + FractionName ~ AnalyteName, value.var="Result")
  
  if(is.null(slope$Slope))slope$Slope <- rep(NA, nrow(slope))
  
  if(any(is.na(slope$Slope)) & (is.null(slope$"Elevation Difference")|is.null(slope$"Length, Segment")))
    return(data.frame("SampleID"=NULL,
                      "metric"=NULL, 
                      "mean"=NULL,
                      "sd"=NULL,
                      "count"=NULL))


  

  slope$Slope[is.na(slope$Slope)] <- (slope$"Elevation Difference"[is.na(slope$Slope)])/(slope$"Length, Segment"[is.na(slope$Slope)])
  slope$product <- slope$Slope * slope$Proportion/100

  slope$xbearing <- slope$Bearing * slope$Proportion/100

  slope$Radians <- 2*pi *slope$Bearing/360
  slope$sin <- sin(slope$Radians) * slope$"Length, Segment"
  slope$cos <- cos(slope$Radians) * slope$"Length, Segment"

  metricCalc(NULL, "Sin <- sum(l$sin);
                    Cos <- sum(l$cos);
                    bearingR <- weighted.mean(l$Bearing, l$Proportion);
                    Total <- sum(l$'Length, Segment')")(slope, c(XSLOPE = function(x)mean(x$product),
                            SLOPE_0 = function(x)100*sum(100*x$product[x$Slope == 0])/sum(x$Proportion),
                            SLOPE_0_5 = function(x)100*sum(100*x$product[x$Slope <= 0.5])/sum(x$Proportion),
                            SLOPE_01 = function(x)100*sum(100*x$product[x$Slope <= 1])/sum(x$Proportion),
                            SLOPE_02 = function(x)100*sum(100*x$product[x$Slope <= 2])/sum(x$Proportion),
                            XBEARING = function(x)parent.frame(6)$bearingR,
                            SINU = function(x)parent.frame(6)$Total/(parent.frame(6)$Cos^2 + parent.frame(6)$Sin^2)^(0.5)))
}