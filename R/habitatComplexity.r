
habitatComplexity <- function(x){
  
  analytes <- c("AQM" = 'Fish Cover Macrophytes',
                "HUM" = 'Fish Cover Artificial Structures',
                "RCK" = 'Fish Cover Boulders',
                "ALG" = 'Fish Cover Filamentous Algae',
                "LWD" = 'Fish Cover Woody Debris >0.3 m',
                "LTR" = 'Fish Cover Live Trees/Roots',
                "OHV" = 'Fish Cover Overhang.Veg',
                "BRS" = 'Fish Cover Woody Debris <0.3 m',
                "UCB" = 'Fish Cover Undercut Banks')
  data <- subset(x, AnalyteName %in% analytes & ResQualCode == '=')
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
  data$Location2 <- data$LocationCode
  
  habitat <- dcast(data, SampleID + Location2 ~ AnalyteName, value.var="Result2", mean)
  
  missing <- analytes[!analytes %in% names(habitat)]  
  
  metrics <- c(Shannon_Habitat = function(x)parent.frame(6)$diverse,
               XFC_AQM = function(x)mean(x$'Fish Cover Macrophytes'),
               XFC_HUM = function(x)mean(x$'Fish Cover Artificial Structures'),
               XFC_RCK = function(x)mean(x$'Fish Cover Boulders'),
               XFC_ALG = function(x)mean(x$'Fish Cover Filamentous Algae'),
               XFC_LWD = function(x)mean(x$'Fish Cover Woody Debris >0.3 m'),
               XFC_LTR = function(x)mean(x$'Fish Cover Live Trees/Roots'),
               XFC_OHV = function(x)mean(x$'Fish Cover Overhang.Veg'),
               XFC_BRS = function(x)mean(x$'Fish Cover Woody Debris <0.3 m'),
               XFC_UCB = function(x)mean(x$'Fish Cover Undercut Banks'),
               XFC_BIG = function(x)sum(c(parent.frame(6)$HUM2,
                                        parent.frame(6)$RCK2,
                                        parent.frame(6)$LWD2,
                                        parent.frame(6)$UCB2)),
               XFC_NAT_EMAP = function(x)sum(c(parent.frame(6)$LWD2,
                                               parent.frame(6)$RCK2,
                                               parent.frame(6)$OHV2,
                                               parent.frame(6)$UCB2,
                                               parent.frame(6)$BRS2)),
               XFC_NAT_SWAMP = function(x)sum(c(parent.frame(6)$LWD2,
                                                parent.frame(6)$RCK2,
                                                parent.frame(6)$OHV2,
                                                parent.frame(6)$UCB2,
                                                parent.frame(6)$BRS2,
                                                parent.frame(6)$LTR2,
                                                parent.frame(6)$AQM2)),
               CFC_AQM = function(x)parent.frame(6)$AQM,
               CFC_HUM = function(x)parent.frame(6)$HUM,
               CFC_RCK = function(x)parent.frame(6)$RCK,
               CFC_ALG = function(x)parent.frame(6)$ALG,
               CFC_LWD = function(x)parent.frame(6)$LWD,
               CFC_LTR = function(x)parent.frame(6)$LTR,
               CFC_OHV = function(x)parent.frame(6)$OHV,
               CFC_BRS = function(x)parent.frame(6)$BRS,
               CFC_UCB = function(x)parent.frame(6)$UCB
               )
  
  if(length(missing) > 0){
    mCols <- sapply(missing, function(x)rep(NA, nrow(habitat)))
    colnames(mCols) <- missing
    habitat <- cbind(habitat, mCols)
    metrics <- metrics[!names(metrics) %in% c("XFC_BIG", "XFC_NAT_EMAP", "XFC_NAT_SWAMP")]
    metrics <- metrics[!(sapply(names(metrics), function(x){
      any(sapply(names(missing), grepl, x))
    }))]
  }
  
  metricCalc(NULL, "diverse <- diversity(apply(l[, 3:ncol(l)], 2, sum, na.rm=TRUE));
             AQM = sum(l$'Fish Cover Macrophytes' > 0, na.rm=TRUE);
             HUM = sum(l$'Fish Cover Artificial Structures' > 0, na.rm=TRUE);
             RCK = sum(l$'Fish Cover Boulders' > 0, na.rm=TRUE);
             ALG = sum(l$'Fish Cover Filamentous Algae' > 0, na.rm=TRUE);
             LWD = sum(l$'Fish Cover Woody Debris >0.3 m' > 0, na.rm=TRUE);
             LTR = sum(l$'Fish Cover Live Trees/Roots' > 0, na.rm=TRUE);
             OHV = sum(l$'Fish Cover Overhang.Veg' > 0, na.rm=TRUE);
             BRS = sum(l$'Fish Cover Woody Debris <0.3 m' > 0, na.rm=TRUE);
             UCB = sum(l$'Fish Cover Undercut Banks' > 0, na.rm=TRUE);
             AQM2 = mean(l$'Fish Cover Macrophytes', na.rm=TRUE);
             HUM2 = mean(l$'Fish Cover Artificial Structures', na.rm=TRUE);
             RCK2 = mean(l$'Fish Cover Boulders', na.rm=TRUE);
             ALG2 = mean(l$'Fish Cover Filamentous Algae', na.rm=TRUE);
             LWD2 = mean(l$'Fish Cover Woody Debris >0.3 m', na.rm=TRUE);
             LTR2 = mean(l$'Fish Cover Live Trees/Roots', na.rm=TRUE);
             OHV2 = mean(l$'Fish Cover Overhang.Veg', na.rm=TRUE);
             BRS2 = mean(l$'Fish Cover Woody Debris <0.3 m', na.rm=TRUE);
             UCB2 = mean(l$'Fish Cover Undercut Banks', na.rm=TRUE);")(habitat, metrics)
}