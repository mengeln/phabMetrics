
substrate <- function(data){
  data <- subset(data, AnalyteName %in% c("Substrate Size Class",
                                          "Embeddedness",
                                          "CPOM") &
                   ResQualCode == "=")
  
  if(nrow(data[data$AnalyteName == "Substrate Size Class", ]) == 0) {
    result1  <- data.frame("SampleID"=NULL,
                           "metric"=NULL, 
                           "mean"=NULL,
                           "sd"=NULL,
                           "count"=NULL)
  } else {
    data$Result <- as.numeric(data$Result)
    data$Location2 <- sapply(strsplit(as.character(data$LocationCode), ","), function(x)x[1])
    
    substrate <- subset(data, AnalyteName == "Substrate Size Class")
    substrate$VariableResult <- toupper(as.character(substrate$VariableResult))  
    substrate$VariableResult2 <- as.character(cut(substrate$Result, breaks=c(0, 0.06, 2, 16, 64, 250, 1000, 4000), 
                                                  labels=c("FN", "SA", "GF", "GC", "CB", "SB", "XB")))
    substrate$VariableResult2 <- mapply(function(x,y)ifelse(!is.na(x), x, y), substrate$VariableResult, substrate$VariableResult2)
    
    substrate$Result2 <- with(substrate, ifelse(VariableResult == "RS", 5660, ifelse(
      VariableResult=="RR", 5660, ifelse(
        VariableResult=="XB", 2500, ifelse(
          VariableResult=="SB", 625, ifelse(
            VariableResult=="CB", 157, ifelse(
              VariableResult=="GC", 9, ifelse(
                VariableResult=="GF", 9, ifelse(
                  VariableResult=="SA", 1.03, ifelse(
                    VariableResult=="FN", 0.03, ifelse(
                      VariableResult=="HP", 5660, ifelse(
                        VariableResult=="RC", 5660, NA))))))))))))
    
    substrate$Result2 <- mapply(function(x,y)ifelse(!is.na(x), x, y), substrate$Result2, substrate$Result)
    if(any(substrate$Result2[!is.na(substrate$Result2)] < 0 ))
      stop("Negative values not allowed for substrate size")
    
    metrics <- c(Shannon_Substrate = function(x)parent.frame(6)$diverse,
                 PCT_RS = function(x)100 * sum(x$VariableResult2 == 'RS', na.rm=T)/x$total[1],
                 PCT_RR = function(x)100 * sum(x$VariableResult2 == 'RR', na.rm=T)/x$total[1],
                 PCT_RC = function(x)100 * sum(x$VariableResult2 == 'RC', na.rm=T)/x$total[1],
                 PCT_XB = function(x)100 * sum(x$VariableResult2 == 'XB', na.rm=T)/x$total[1],
                 PCT_SB = function(x)100 * sum(x$VariableResult2 == 'SB', na.rm=T)/x$total[1],
                 PCT_CB = function(x)100 * sum(x$VariableResult2 == 'CB', na.rm=T)/x$total[1],
                 PCT_GC = function(x)100 * sum(x$VariableResult2 == 'GC', na.rm=T)/x$total[1],
                 PCT_GF = function(x)100 * sum(x$VariableResult2 == 'GF', na.rm=T)/x$total[1],
                 PCT_SA = function(x)100 * sum(x$VariableResult2 == 'SA', na.rm=T)/x$total[1],
                 PCT_FN = function(x)100 * sum(x$VariableResult2 == 'FN', na.rm=T)/x$total[1],
                 PCT_HP = function(x)100 * sum(x$VariableResult2 == 'HP', na.rm=T)/x$total[1],
                 PCT_WD = function(x)100 * sum(x$VariableResult2 == 'WD', na.rm=T)/x$total[1],
                 PCT_OT = function(x)100 * sum(x$VariableResult2 == 'OT', na.rm=T)/x$total[1],
                 PCT_BDRK = function(x)sum(x$VariableResult2 %in% c('RR', 'RS'))/x$total[1],
                 PCT_BIGR = function(x)sum(x$VariableResult2 %in% c('RR', 'RS', 'XB', 'SB', 'CB', 'GC'))/x$total[1],
                 PCT_SFGF = function(x)sum(x$VariableResult2 %in% c('SA', 'FN', 'GF'))/x$total[1],
                 PCT_SAFN = function(x)sum(x$VariableResult2 %in% c('SA', 'FN'))/x$total[1],
                 XSDGM = function(x)10^(sum(log10(x$Result2))/x$total[1]),
                 XSPDGM = function(x)10^(sum(log10(x$Result2[x$Result2 <= 2500]))/x$total[1]),
                 SB_PT_D50 = function(x)parent.frame(6)$quantAll["50%"],
                 SB_PT_D10 = function(x)parent.frame(6)$quantAll["10%"],
                 SB_PT_D25 = function(x)parent.frame(6)$quantAll["25%"],
                 SB_PT_D75 = function(x)parent.frame(6)$quantAll["75%"],
                 SB_PT_D90 = function(x)parent.frame(6)$quantAll["90%"],
                 SB_PP_D50 = function(x)parent.frame(6)$quantPart["50%"],
                 SB_PP_D10 = function(x)parent.frame(6)$quantPart["10%"],
                 SB_PP_D25 = function(x)parent.frame(6)$quantPart["25%"],
                 SB_PP_D75 = function(x)parent.frame(6)$quantPart["75%"],
                 SB_PP_D90 = function(x)parent.frame(6)$quantPart["90%"]
    )
    
    substrateMetrics <- metricCalc("d$total <- sum(!is.na(d$VariableResult2))",
                                   "quantAll <- quantile(l$Result2, c(0.5, 0.1, 0.25, 0.75, 0.9), na.rm=T)
                                  quantPart <- quantile(l$Result2[l$Result2 <= 2500], c(0.5, 0.1, 0.25, 0.75, 0.9), na.rm=T)
                                 diverse <- diversity(table(l$VariableResult[!(l$VariableResult %in% c('RS', 'RR'))]))")
    
    result1 <- substrateMetrics(substrate, metrics)
    result1$metric[grep("SB_P", result1$metric)] <- 
      substr(result1$metric[grep("SB_P", result1$metric)], 1,
             nchar(result1$metric[grep("SB_P", result1$metric)])-4)
  }
  
  cpom <- subset(data, AnalyteName == "CPOM")
  if(nrow(cpom) == 0) {
    result2  <- data.frame("SampleID"=NULL,
                           "metric"=NULL, 
                           "mean"=NULL,
                           "sd"=NULL,
                           "count"=NULL)
  } else {
    cpomMetric <- metricCalc(NULL)
    result2 <- cpomMetric(cpom, c("PCT_CPOM" = function(x)sum(x$VariableResult=='Present')/sum(x$VariableResult %in% c('Present', 'Absent'))))
    result2 <- ddply(result2, .(SampleID), function(df){
      df$count <- nrow(cpom[cpom$VariableResult != "Dry" & cpom$SampleID == unique(df$SampleID), ])
      df
    })
  }
  
  embed <- subset(data, AnalyteName == "Embeddedness")
  if(nrow(embed) == 0) {
    result3  <- data.frame("SampleID"=NULL,
                           "metric"=NULL, 
                           "mean"=NULL,
                           "sd"=NULL,
                           "count"=NULL)
  } else {
    result3 <- data.frame("SampleID"=unique(embed$SampleID),
               "metric"="XEMBED", 
               "mean"=mean(embed$Result, na.rm=TRUE),
               "sd"=sd(embed$Result, na.rm=TRUE),
               "count"=length(embed$Result[!is.na(embed$Result)]))
  }
  
  rbind(result1, result2, result3)
}