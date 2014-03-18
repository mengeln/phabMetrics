\name{metricsPHAB}
\alias{metricsPHAB}
\title{Calculate SWAMP PHAB metrics}
\usage{
metricsPHAB(phabdata, detailed = FALSE, verbose = 0)
}
\arguments{
  \item{phabdata}{A data frame of SWAMP formatted PHAB
  data}

  \item{verbose}{0 (default) is silent, 1 prints SampleIDs,
  2 prints metrics}
}
\value{
A data frame with the metric values, standard deviations,
and sample size for all successfully calculated metrics
}
\description{
Calculate SWAMP PHAB metrics
}

