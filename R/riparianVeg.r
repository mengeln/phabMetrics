
riparianVegetation <- function(datum){
  data <- subset(datum, AnalyteName %in% c("Riparian Upper Canopy All Trees", "Riparian Lower Canopy All Vegetation",
                                           "Riparian GroundCover NonWoody Plants", "Riparian GroundCover Barren",
                                           "Riparian GroundCover Woody Shrubs") 
                 & ResQualCode == "=")
  if(nrow(data) == 0)return(data.frame("SampleID"=NULL,
                                       "metric"=NULL, 
                                       "mean"=NULL,
                                       "sd"=NULL,
                                       "count"=NULL))
  
  data$Result2 <- with(data, ifelse(VariableResult == 0, 0, ifelse(
    VariableResult == 1, 5, ifelse(
      VariableResult == 2, 25, ifelse(
        VariableResult == 3, 57.5, ifelse(
          VariableResult == 4, 87.5, NA))))))
  
  
  data <- dcast(data, SampleID + StationCode + SampleDate + LocationCode ~ AnalyteName, value.var="Result2", mean, na.rm=TRUE)

  data$Location2 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
  
  metrics <- c()
  
  rgnp <- data$"Riparian GroundCover NonWoody Plants"
  rgws <- data$"Riparian GroundCover Woody Shrubs"
  
  
  if(!(is.null(rgnp) | is.null(rgws))){
    data$XPGVEG <- mapply(function(x,y)(x > 0)|(y > 0), rgnp, rgws)
    data$XPMGVEG <- mapply(function(x,y)(x > 5)|(y > 5), rgnp, rgws)
    metrics <- c("XGB" = function(x)mean(x$"Riparian GroundCover Barren"),
                 "XGH" = function(x)mean(x$"Riparian GroundCover NonWoody Plants"),
                 "XGW" = function(x)mean(x$"Riparian GroundCover Woody Shrubs"),
                 "XG" = function(x)sum(mean(x$"Riparian GroundCover NonWoody Plants"), mean(x$"Riparian GroundCover Woody Shrubs")),
                 "XPGVEG" = function(x)mean(x$XPGVEG),
                 "XPMGVEG" = function(x)mean(x$XPMGVEG))             
  }
  else {
    data$XPGVEG <- rep(NA, nrow(data))
    data$XPMGVEG <- rep(NA, nrow(data))
  }
  
  ruct <- data$"Riparian Upper Canopy All Trees"
  rlav <- data$"Riparian Lower Canopy All Vegetation"
  
  if(!(is.null(ruct)|is.null(rlav))) {
    data$XPCM <- mapply(function(x,y)(x > 0)&(y > 0), data$"Riparian Upper Canopy All Trees", data$"Riparian Lower Canopy All Vegetation")
    metrics <- c(metrics,
                 "XM" = function(x)mean(x$"Riparian Lower Canopy All Vegetation"),
                 "XC" = function(x)mean(x$"Riparian Upper Canopy All Trees"),
                 "XCM" = function(x)sum(mean(x$"Riparian Upper Canopy All Trees"), mean(x$"Riparian Lower Canopy All Vegetation")),
                 "XPMID" = function(x)mean(x$"Riparian Lower Canopy All Vegetation" != 0),
                 "XPCAN" = function(x)mean(x$"Riparian Upper Canopy All Trees" != 0),
                 "XPCM" = function(x)mean(x$XPCM),
                 "XPCMG" = function(x)mean(x$XPCMG)
    )}
  else
    data$XPCM <- rep(NA, nrow(data))
  
  if(length(metrics) == 13)metrics <- c(metrics, "XCMG" = function(x)sum(mean(x$"Riparian Upper Canopy All Trees"), mean(x$"Riparian Lower Canopy All Vegetation"), sum(mean(x$"Riparian GroundCover NonWoody Plants"), mean(x$"Riparian GroundCover Woody Shrubs"))))
  
  data$XPCMG <- mapply(function(x,y)(x > 0)&(y > 0), data$XPGVEG, data$XPCM)
  
  if(length(metrics) == 0)return(data.frame("SampleID"=NULL,
                                            "metric"=NULL, 
                                            "mean"=NULL,
                                            "sd"=NULL,
                                            "count"=NULL))

  result <- metricCalc(NULL)(data, metrics)
  
  canopy <- subset(datum, AnalyteName == "Canopy Cover")
  if(nrow(canopy) == 0){
    canopy_result <- data.frame("SampleID"=NULL,
                                "metric"=NULL, 
                                "mean"=NULL,
                                "sd"=NULL,
                                "count"=NULL)
  } else {
    canopy$Result <- as.numeric(canopy$Result)
    canopy$Result2 <- canopy$Result * (100/17)
    canopy$Location2 <- sapply(strsplit(as.character(canopy$LocationCode), ","), function(x)x[1])
    canopy$Location3 <- sapply(strsplit(as.character(canopy$LocationCode), ","), function(x)x[2])
    canopy$MidLoc <- grepl("ctr", canopy$Location3) | grepl("Ctr", canopy$Location3)
    
    canopy <- dcast(canopy, SampleID + Location2 ~ MidLoc, value.var="Result2", fun.aggregate=mean, na.rm=TRUE)
    canopy_result <- metricCalc(NULL)(canopy, c("XCDENMID" = function(x)mean(x$'TRUE', na.rm=TRUE),
                                                "XCDENBK" = function(x)mean(x$'FALSE', na.rm=TRUE))
    )
  }
  rbind(result, canopy_result)
}


