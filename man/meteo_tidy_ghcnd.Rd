% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_ghcnd.R
\name{meteo_tidy_ghcnd}
\alias{meteo_tidy_ghcnd}
\title{Create a tidy GHCND dataset from a single monitor}
\usage{
meteo_tidy_ghcnd(
  stationid,
  keep_flags = FALSE,
  var = "all",
  date_min = NULL,
  date_max = NULL
)
}
\arguments{
\item{stationid}{(character) A character vector giving the identification of
the weather stations for which the user would like to pull data. To get a full
and current list of stations, the user can use the \code{\link[=ghcnd_stations]{ghcnd_stations()}}
function. To identify stations within a certain radius of a location, the
user can use the \code{\link[=meteo_nearby_stations]{meteo_nearby_stations()}} function.}

\item{keep_flags}{TRUE / FALSE for whether the user would like to keep all
the flags for each weather variable. The default is to not keep the
flags (FALSE). See the note below for more information on these flags.}

\item{var}{A character vector specifying either \code{"all"} (pull all
available weather parameters for the site) or the weather parameters to
keep in the final data (e.g., \code{c("TMAX", "TMIN")} to only keep
maximum and minimum temperature). Example choices for this argument
include:
\itemize{
\item \code{PRCP}: Precipitation, in tenths of millimeters
\item \code{TAVG}: Average temperature, in tenths of degrees Celsius
\item \code{TMAX}: Maximum temperature, in tenths of degrees Celsius
\item \code{TMIN}: Minimum temperature, in tenths of degrees Celsius
}

A full list of possible weather variables is available in NOAA's README
file for the GHCND data
(https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt).
Most weather stations will only have a small subset of all the possible
weather variables, so the data generated by this function may not include
all of the variables the user specifies through this argument.}

\item{date_min}{A character string giving the earliest
date of the daily weather time series that the user would
like in the final output. This character string should be formatted as
"yyyy-mm-dd". If not specified, the default is to keep all daily data for
the queried weather site from the earliest available date.}

\item{date_max}{A character string giving the latest
date of the daily weather time series that the user would
like in the final output. This character string should be formatted as
"yyyy-mm-dd". If not specified, the default is to keep all daily data for
the queried weather site through the most current available date.}
}
\value{
A data frame of daily weather data for a single weather monitor,
converted to a tidy format. All weather variables may not exist for all
weather stations. Examples of variables returned are:
\itemize{
\item \code{id}: Character string with the weather station site id
\item \code{date}: Date of the observation
\item \code{prcp}: Precipitation, in tenths of mm
\item \code{tavg}: Average temperature, in degrees Celsius
\item \code{tmax}: Maximum temperature, in degrees Celsius
\item \code{tmin}: Minimum temperature, in degrees Celsius
\item \code{awnd}: Average daily wind speed, in meters / second
\item \code{wsfg}: Peak gust wind speed, in meters / second
}

There are other possible weather variables in the Global Historical
Climatology Network; see
http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt for a full
list. The variables \code{prcp}, \code{tmax}, \code{tmin}, and \code{tavg}
have all been converted from tenths of their metric to the metric (e.g.,
from tenths of degrees Celsius to degrees Celsius). All other variables
are in the units specified in the linked file.
}
\description{
This function inputs an object created by \code{\link{ghcnd}} and cleans up
the data into a tidy form.
}
\note{
The weather flags, which are kept by specifying
\code{keep_flags = TRUE} are:
\itemize{
\item \verb{*_mflag}: Measurement flag, which gives some information on how
the observation was measured.
\item \verb{*_qflag}: Quality flag, which gives quality information on the
measurement, like if it failed to pass certain quality checks.
\item \verb{*_sflag}: Source flag. This gives some information on the
weather collection system (e.g., U.S. Cooperative Summary of the Day,
Australian Bureau of Meteorology) the weather observation comes from.
}

More information on the interpretation of these flags can be found in the
README file for the NCDC's Daily Global Historical Climatology Network's
data at http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
}
\examples{
\dontrun{
# One station in Australia is ASM00094275
meteo_tidy_ghcnd(stationid = "ASN00003003")
meteo_tidy_ghcnd(stationid = "ASN00003003", var = "tavg")
meteo_tidy_ghcnd(stationid = "ASN00003003", date_min = "1989-01-01")
}

}
\seealso{
\code{\link[=meteo_pull_monitors]{meteo_pull_monitors()}}
}
\author{
Brooke Anderson \email{brooke.anderson@colostate.edu}
}
