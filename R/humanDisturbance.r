
humanDisturbance <- function(x){
  data <- subset(x, AnalyteName %in% c("Riparian Bridges/Abutments",
                                          "Riparian Buildings",
                                          "Riparian Landfill/Trash",
                                          "Riparian Logging",
                                          "Riparian Mining",
                                          "Riparian Orchards/Vineyards",
                                          "Riparian Park/Lawn",
                                          "Riparian Pasture/Range",
                                          "Riparian Pavement",
                                          "Riparian Pipes",
                                          "Riparian Road",
                                          "Riparian Row Crops",
                                          "Riparian Vegetation Management",
                                          "Riparian Wall/Dike"
                                          ) &
                   ResQualCode == "=")
  if(nrow(data) == 0)return(data.frame("SampleID"=NULL,
                                       "metric"=NULL, 
                                       "mean"=NULL,
                                       "sd"=NULL,
                                       "count"=NULL))
  data$Location1 <- sapply(strsplit(as.character(data$LocationCode), ", "), function(x)x[2])
  data$Location1[is.na(data$Location1)] <- "Channel"
  loc1 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
  data$Location2 <- substr(loc1, 5, nchar(loc1))
  
  if(nrow(data) == 0)
    return(data.frame("SampleID"=NULL,
                      "metric"=NULL, 
                      "mean"=NULL,
                      "sd"=NULL,
                      "count"=NULL))
 
  if(any(duplicated(data[, c("SampleID", "Location2", "Location1", "AnalyteName")])))
    stop("Problem parsing location")
  hdist <- dcast(data, SampleID + Location2 + AnalyteName ~ Location1,
                 value.var="VariableResult")
  
  
  
  hdist$Channel <- hdist$Location2
  hdist$Channel[is.na(hdist$Channel)] <- "N"
  
  convert <- function(x, y){
    ifelse(y == "Y", 1.5, ifelse(
      x == "B", 1.5, ifelse(
        x == "C", 1, ifelse(
          x == "P", 0.667, 0))))
  }
  hdist$ResultLeft <- mapply(convert, hdist$Left, hdist$Channel)
  hdist$ResultRight <- mapply(convert, hdist$Right, hdist$Channel)

  hdistm <- function(dat, x){
    mean(c(dat[dat$AnalyteName %in% x, 'ResultRight'], dat[dat$AnalyteName %in% x, 'ResultLeft']))
  }
  

  metrics <- c(W1H_BRDG = function(x)hdistm(x, 'Riparian Bridges/Abutments'),
               W1H_BLDG = function(x)hdistm(x, 'Riparian Buildings'),
               W1H_LDFL = function(x)hdistm(x, 'Riparian Landfill/Trash'),
               W1H_LOG = function(x)hdistm(x, 'Riparian Logging'),
               W1H_MINE = function(x)hdistm(x, 'Riparian Mining'),
               W1H_ORVY = function(x)hdistm(x, 'Riparian Orchards/Vineyards'),
               W1H_PARK = function(x)hdistm(x, 'Riparian Park/Lawn'),
               W1H_PSTR = function(x)hdistm(x, 'Riparian Pasture/Range'),
               W1H_PVMT = function(x)hdistm(x, 'Riparian Pavement'),
               W1H_PIPE = function(x)hdistm(x, 'Riparian Pipes'),
               W1H_ROAD = function(x)hdistm(x, 'Riparian Road'),
               W1H_CROP =  function(x)hdistm(x, 'Riparian Row Crops'),
               W1H_VEGM = function(x)hdistm(x, 'Riparian Vegetation Management'),
               W1H_WALL = function(x)hdistm(x, 'Riparian Wall/Dike')
               )
  hdistMetrics <- metricCalc(NULL)
  result <- hdistMetrics(hdist, metrics)
  hall <- ddply(result, "SampleID", function(df)
    data.frame(metric = c("W1_HALL_SWAMP", "W1_HALL_EMAP"),
               mean = c(sum(df$mean), sum(df$mean[!(df$metric %in% c("W1H_BRDG", "W1H_ORVY", "W1H_VEGM"))])),
               sd = NA,
               count = unique(df$count)))

  result <- rbind(result, hall)
  result$count <- result$count/7
  result
}






