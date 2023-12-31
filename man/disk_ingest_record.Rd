% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disk_ingest_record.R
\name{disk_ingest_record}
\alias{disk_ingest_record}
\title{disk_ingest_record}
\usage{
disk_ingest_record(
  in_file = NULL,
  path_to_ras_dbase = NULL,
  code_to_place_in_source = NULL,
  proj_override = NULL,
  vdat_trans = FALSE,
  quiet = FALSE,
  verbose = FALSE,
  quick_check = FALSE,
  quick_hull = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{in_file}{PARAM_DESCRIPTION, Default: NULL}

\item{code_to_place_in_source}{PARAM_DESCRIPTION, Default: NULL}

\item{proj_override}{PARAM_DESCRIPTION, Default: NULL}

\item{vdat_trans}{PARAM_DESCRIPTION, Default: FALSE}

\item{quiet}{PARAM_DESCRIPTION, Default: FALSE}

\item{verbose}{PARAM_DESCRIPTION, Default: FALSE}

\item{quick_check}{PARAM_DESCRIPTION, Default: FALSE}

\item{quick_hull}{PARAM_DESCRIPTION, Default: FALSE}

\item{overwrite}{PARAM_DESCRIPTION, Default: FALSE}

\item{ras_dbase}{PARAM_DESCRIPTION, Default: NULL}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[glue]{glue}}
 \code{\link[utils]{glob2rx}}
 \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_write}}
 \code{\link[stringr]{str_sub}}, \code{\link[stringr]{str_detect}}
 \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{rbindlist}}, \code{\link[data.table]{fwrite}}
 \code{\link[sfheaders]{sf_linestring}}
 \code{\link[lwgeom]{st_startpoint}}
 \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
 \code{\link[holyhull]{holyhull}}
 \code{\link[nhdplusTools]{get_nhdplus}}
 \code{\link[AOI]{aoi_get}}
 \code{\link[arrow]{write_parquet}}
}
