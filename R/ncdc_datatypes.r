#' Get possible data types for a particular dataset
#'
#' From the NOAA API docs: Describes the type of data, acts as a label. If it's 64
#' degrees out right now, then the data type is Air Temperature and the data is 64.
#'
#' @export
#' @template rnoaa
#' @template rnoaa2
#' @template datatypes
#' @param datacategoryid Optional. Accepts a valid data category id or a chain of data
#'    category ids seperated by ampersands (although it is rare to have a data type
#'    with more than one data category). Data types returned will be associated with
#'    the data category(ies) specified
#' @return A \code{data.frame} for all datasets, or a list of length two, each with
#'    a data.frame.
#' @examples \dontrun{
#' # Fetch available data types
#' ncdc_datatypes()
#'
#' # Fetch more information about the ACMH data type id
#' ncdc_datatypes(datatypeid="ACMH")
#'
#' # Fetch data types with the air temperature data category
#' ncdc_datatypes(datacategoryid="TEMP", limit=56)
#'
#' # Fetch data types that support a given set of stations
#' ncdc_datatypes(stationid=c('COOP:310090','COOP:310184','COOP:310212'))
#' }

ncdc_datatypes <- function(datasetid=NULL, datatypeid=NULL, datacategoryid=NULL,
  stationid=NULL, locationid=NULL, startdate=NULL, enddate=NULL, sortfield=NULL,
  sortorder=NULL, limit=25, offset=NULL, token=NULL,
  dataset=NULL, page=NULL, filter=NULL, ...)
{
  calls <- names(sapply(match.call(), deparse))[-1]
  calls_vec <- c("dataset", "page", "filter") %in% calls
  if(any(calls_vec))
    stop("The parameters dataset, page, and filter \n  have been removed, and were only relavant in the old NOAA API v1. \n\nPlease see documentation for ?ncdc_datatypes")

  token <- check_key(token)

  if(!is.null(datatypeid)){
    url <- sprintf("http://www.ncdc.noaa.gov/cdo-web/api/v2/datatypes/%s", datatypeid)
  } else { url <- "http://www.ncdc.noaa.gov/cdo-web/api/v2/datatypes" }

  args <- noaa_compact(list(datasetid=datasetid, datacategoryid=datacategoryid,
                       locationid=locationid, stationid=stationid, startdate=startdate,
                       enddate=enddate, sortfield=sortfield, sortorder=sortorder,
                       limit=limit, offset=offset))
  args <- as.list(unlist(args))
  names(args) <- gsub("[0-9]+", "", names(args))

  temp <- GET(url, query=args, add_headers("token" = token), ...)
  out <- check_response(temp)
  if(is(out, "character")){
    all <- list(meta=NULL, data=NULL)
  } else {
    if(!is.null(datatypeid)){
      dat <- data.frame(out, stringsAsFactors=FALSE)
      metadat <- NULL
      all <- list(data = dat, meta = metadat)
    } else
    {
      dat <- do.call(rbind.fill, lapply(out$results, function(x) data.frame(x, stringsAsFactors=FALSE)))
      metadat <- data.frame(out$metadata$resultset, stringsAsFactors=FALSE)
      all <- list(meta = metadat, data = dat)
    }
  }
  class(all) <- "ncdc_datatypes"
  return( all )
}
