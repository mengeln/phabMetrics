
misc <- function(data){
  fieldMeasures <- subset(data, AnalyteName %in% c("Temperature", "pH", "Oxygen", "SpecificConductivity", "Alkalinity as CaCO3",
                                          "Salinity", "Turbidity") & ResQualCode == "=")
  fieldCondition <- subset(data, AnalyteName %in% c("Dominant Land Use", "Evidence of Fire",
                                                    "Evidence of Recent Rainfail") & ResQualCode == "=")
  
  res1 <- ddply(fieldCondition, .(SampleID, AnalyteName), function(df){
    data.frame(mean = unique(df$VariableResult),
               sd = NA,
               count = nrow(df))

  })
  res1$mean <- as.character(res1$mean)
  res2 <- ddply(fieldMeasures, .(SampleID, AnalyteName), function(df){
    data.frame(mean = mean(df$Result, na.rm=TRUE),
               sd = sd(df$Result, na.rm=TRUE),
               count = nrow(df))
  })

  res <- rbind(res1, res2)
  names(res)[names(res) == "AnalyteName"] <- "metric"
  res
  
}