
trans <- function(x) UseMethod("trans", x)
trans.numeric <- function(x)t(x)
trans.matrix <- function(x)x

helperfn <- function(x) UseMethod("helperfn", x)
helperfn.NULL <- function(x){}
helperfn.character <- function(x)eval(parse(text=x), parent.frame(n=1))

metricCalc <- function(helpers, outerhelpers=NULL){
  function(data, metrics){
    data.l <- lapply(split(data, data$SampleID), function(l){
      helperfn(outerhelpers)
      sample <- lapply(split(l, l$Location2), function(d){
        helperfn(helpers)
        sapply(metrics, function(code){
          code(d)
        })
      })
      sample <- trans(Reduce(rbind, sample))

      if(storage.mode(sample) == "list"){ ###fixes weird data that turn into a list
        tab <- outer(1:nrow(sample), 1:ncol(sample), 
                     Vectorize(function(x,y)is.null(unlist(sample[x, y])) | 
                                 length(unlist(sample[x, y])) != 1))
        sample[tab] <- NA
        storage.mode(sample) <- "numeric"
      }

      cbind(apply(sample, 2, mean, na.rm=T), apply(sample, 2, sd, na.rm=T), nrow(l))
    })
    result <- Reduce(rbind, data.l)
    rnames <- row.names(result)
    row.names(result) <- NULL
    result <- as.data.frame(result)
    result$SampleID <- rep(names(data.l), each=length(metrics))
    result$metric <- rnames
    names(result)[1:3] <- c("mean", "sd", "count")
    result <- result[,c("SampleID", "metric", "mean", "sd", "count")]
    result
  }
}

metricCalc2 <- function(helpers, outerhelpers=NULL, x){
  function(data, metrics){
    data.l <- lapply(split(data, data$SampleID), function(l){
      helperfn(outerhelpers)
      splt <- split(l, l$Location2)
      sample <- lapply(splt[sample.int(length(splt), ifelse(x <= length(splt), x, length(splt)))], function(d){
        helperfn(helpers)
        sapply(metrics, function(code){
          code(d)
        })
      })
      sample <- trans(Reduce(rbind, sample))
      cbind(apply(sample, 2, mean, na.rm=T), apply(sample, 2, sd, na.rm=T), nrow(l))
    })
    result <- Reduce(rbind, data.l)
    rnames <- row.names(result)
    row.names(result) <- NULL
    result <- as.data.frame(result)
    result$SampleID <- rep(names(data.l), each=length(metrics))
    result$metric <- rnames
    names(result)[1:3] <- c("mean", "sd", "count")
    result <- result[,c("SampleID", "metric", "mean", "sd", "count")]
    result
  }
}

