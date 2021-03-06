% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BIEN_public_AP.R
\name{BIEN.list.county}
\alias{BIEN.list.county}
\title{Species list from a given county (and state)}
\usage{
BIEN.list.county(state, county, cultivated = FALSE, only.new.world = TRUE)
}
\arguments{
\item{state}{Accepts single states or vectors}

\item{county}{Accepts single counties or vectors}

\item{cultivated}{cultivated = Return cultivated records as well?  Default is FALSE.}

\item{only.new.world}{only.new.world = Return only records from the New World?  Default is TRUE.}
}
\description{
Species list from a given county (and state)
}
\note{
Requires both state and county be supplied
}
\examples{
BIEN.list.county("Michigan","Kent")
BIEN.list.county(state="Michigan", county="Kent")
county_vector<-c("Kent","Kalamazoo")
BIEN.list.county(state="Michigan",county=county_vector)
}
\author{
Brian Maitner
}

