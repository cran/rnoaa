#' Search NOAA datasets
#'
#' From the NOAA API docs: All of our data are in datasets. To retrieve any data
#' from us, you must know what dataset it is in.
#' 
#' @export
#'
#' @template rnoaa
#' @template rnoaa2
#' @template datasets
#' @return A data.frame for all datasets, or a list of length two, each with a data.frame.
#' @examples \dontrun{
#' # Get a table of all datasets
#' ncdc_datasets()
#'
#' # Get details from a particular dataset
#' ncdc_datasets(datasetid='ANNUAL')
#'
#' # Get datasets with Temperature at the time of observation (TOBS) data type
#' ncdc_datasets(datatypeid='TOBS')
#'
#' # Get datasets with data for a series of the same parameter arg, in this case
#' # stationid's
#' ncdc_datasets(stationid=c('COOP:310090','COOP:310184','COOP:310212'))
#'
#' # Multiple datatypeid's
#' ncdc_datasets(datatypeid=c('ACMC','ACMH','ACSC'))
#' ncdc_datasets(datasetid='ANNUAL', datatypeid=c('ACMC','ACMH','ACSC'))
#' }

ncdc_datasets <- function(datasetid=NULL, datatypeid=NULL, stationid=NULL, locationid=NULL,
  startdate=NULL, enddate=NULL, sortfield=NULL, sortorder=NULL, limit=25, offset=NULL,
  token=NULL, dataset=NULL, page=NULL, year=NULL, month=NULL, ...)
{
  calls <- names(sapply(match.call(), deparse))[-1]
  calls_vec <- c("dataset", "page", "year", "month") %in% calls
  if(any(calls_vec))
    stop("The parameters dataset, page, year, and month \n  have been removed, and were only relavant in the old NOAA API v1. \n\nPlease see documentation for ?ncdc_datasets")

  token <- check_key(token)

  url <- "http://www.ncdc.noaa.gov/cdo-web/api/v2/datasets"
  if(!is.null(datasetid))
    url <- paste(url, "/", datasetid, sep="")
  args <- noaa_compact(list(datatypeid=datatypeid,
                       locationid=locationid, stationid=stationid, startdate=startdate,
                       enddate=enddate, sortfield=sortfield, sortorder=sortorder,
                       limit=limit, offset=offset))
  args <- as.list(unlist(args))
  names(args) <- gsub("[0-9]+", "", names(args))

  temp <- GET(url, query=args, add_headers("token" = token), ...)
  tt <- check_response(temp)
  if(is(tt, "character")){
    all <- list(meta=NULL, data=NULL)
  } else {
    if(!is.null(datasetid)){
      dat <- data.frame(tt, stringsAsFactors=FALSE)
      all <- list(meta = NULL, data = dat)
    } else
    {
      dat <- do.call(rbind.fill, lapply(tt$results, function(x) data.frame(x, stringsAsFactors=FALSE)))
      all <- list(meta = tt$metadata$resultset, data = dat)
    }
  }
  structure(all, class="ncdc_datasets")
}
