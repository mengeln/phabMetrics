\name{validatePHAB}
\alias{validatePHAB}
\title{Check validty of PHAB data}
\usage{
validatePHAB(data, returnPurged = FALSE)
}
\arguments{
  \item{phabdata}{A data frame of SWAMP formatted PHAB
  data}

  \item{returnPurged}{If false (default), return data frame
  listing problematic rows. If true, return data frame
  where SampleID/AnalyteName combinations that contain
  problematic rows have been removed.}
}
\description{
Check validty of PHAB data
}

