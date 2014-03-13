
flow <- function(x){
  data <- subset(x, ResQualCode != "NR" &
                   MethodName %in% c("Velocity Area", "Neutral Buoyant Object")) 
  
  if(nrow(data) == 0)return(data.frame("SampleID"=NULL,
                                       "metric"=NULL, 
                                       "mean"=NULL,
                                       "sd"=NULL,
                                       "count"=NULL))
  fdata <- data[data$MethodName == "Velocity Area", ]
  if(nrow(fdata) != 0 & all(c("Distance","StationWaterDepth", "Velocity") %in% 
                              names(fdata$AnalyteName))){
    flow <- dcast(fdata, SampleID + Replicate ~ AnalyteName, value.var="Result")
    
    distBetween <- function(x){
      l <- length(x)
      diag(outer(x[2:l], x[1:(l-1)], "-"))
    }
    
    flow <- ddply(flow, .(SampleID), function(df){
      df <- df[order(df$Distance), ]
      df$Distance2 <- c(0, distBetween(df$Distance))
      df
    })
    
    
    flow$discharge <- flow$Distance2 * flow$StationWaterDepth * flow$Velocity * 0.00107639104
    flow$Location2 <- flow$Replicate
    res <- metricCalc(NULL, "result <- sum(l$discharge); max <- max(l$Velocity)")(flow, c(
      FL_Q_F = function(x)sum(parent.frame(6)$result),
      FL_Q_M = function(x)sum(parent.frame(6)$result)*0.0283168466,
      XWV_F = function(x)mean(x$Velocity),
      XWV_M = function(x)mean(x$Velocity)/3.28084,
      MWVM_F = function(x)parent.frame(6)$max,
      MWVM_M = function(x)parent.frame(6)$max/3.28084,
      PWVZ = function(x)mean(x$Velocity == 0)*100
    ))
  } else {
    res <- data.frame(metric = NULL,
                      mean = NULL,
                      sd = NULL,
                      count = NULL)
  }
  flow2 <- data[data$MethodName == "Neutral Buoyant Object", ]
  if(nrow(flow2) == 0)
    res2 <- data.frame(metric = NULL,
                       mean = NULL,
                       sd = NULL,
                       count = NULL)
  else {
    flow2$trans <- sapply(strsplit(flow2$LocationCode, " "), "[", 3)
    flow2$trans[is.na(flow2$trans)] <- "X"
    flow2$trans <- gsub(",", "", flow2$trans)
    flow2$Result[flow2$AnalyteName == "StationWaterDepth"] <- flow2$Result[flow2$AnalyteName == "StationWaterDepth"]/100
    
    flow22 <- dcast(flow2, SampleID + AnalyteName + Replicate ~  trans, value.var="Result", mean)
    
    if(!all(c("TransLower", "TransMiddle", "TransUpper") %in% flow22))
      res2 <- data.frame(metric = NULL,
                         mean = NULL,
                         sd = NULL,
                         count = NULL)
    else {
      View(flow22)
      res2 <- ddply(flow22, .(SampleID), function(df){
        vel <- mean(df$X[df$AnalyteName == "Distance, Float"]/df$X[df$AnalyteName == "Float Time"], na.rm=TRUE)
        xsecs <- apply(df[, c("TransLower", "TransMiddle", "TransUpper")], 2, prod, na.rm=TRUE)
        xsec <- mean(xsecs, na.rm=TRUE)
        res <- vel * xsec
        data.frame(metric = c("FL_N_M", "FL_N_F"),
                   mean = c(res, res*35.3147),
                   sd = NA,
                   count = rep(length(xsecs), 2))
      })
    }
  }
  rbind.fill(res, res2)
}