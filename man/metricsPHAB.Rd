\name{metricsPHAB}
\alias{metricsPHAB}
\title{Calculate SWAMP PHAB metrics}
\usage{
metricsPHAB(phabdata, detailed = FALSE, verbose = 0)
}
\arguments{
  \item{phabdata}{A data frame of SWAMP formatted PHAB
  data}

  \item{detailed}{When set to true, returns a list with 1.
  a data frame of the results, and 2. a list of character
  vectors containing the metrics that were not calculated
  for each SampleID.}

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

