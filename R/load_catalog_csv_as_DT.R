#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param path_to_csv PARAM_DESCRIPTION
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [fread][data.table::fread]
#' @rdname load_catalog_csv_as_DT
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom data.table fread

load_catalog_csv_as_DT <- function(path_to_csv,quiet=FALSE) {
  if(!quiet){ print('Loading generated ras database') }
  ras_catalog_dbase <- data.table::fread(path_to_csv, colClasses=c("nhdplus_comid" = "character",
                                                                   "model_name" = "character",
                                                                   "units" = "character",
                                                                   "crs" = "character",
                                                                   "final_name_key" = "character"))
  return(ras_catalog_dbase)
}
