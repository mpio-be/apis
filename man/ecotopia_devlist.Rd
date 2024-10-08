% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/druid.R
\name{ecotopia_devlist}
\alias{ecotopia_devlist}
\title{List all devices for the given login profile}
\usage{
ecotopia_devlist(logstring, verbose = TRUE)
}
\arguments{
\item{logstring}{login string as returned by \link{ecotopia_login}}

\item{verbose}{print \link[httr2:request]{httr2::request} string. Default to TRUE.}
}
\value{
a data.table
}
\description{
List all devices for the given login profile
}
\examples{
\dontrun{
crd = config::get(config = "druid_api")
logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
dl = ecotopia_devlist(logstring)
}

}
