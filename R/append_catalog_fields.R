#' @title append_catalog_fields
#' @description Append calculated fields and helper values to new catalog
#' @param path_to_ras_dbase The path to the top level folder of the ras_catalog structure
#' @param out_name PARAM_DESCRIPTION, Default: 'OWP_ras_catalog.csv'
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  # RRASSLER::append_catalog_fields(path_to_ras_dbase="/home/rstudio/g/data/ras_dbase",out_name="OWP_ras_catalog.csv",overwrite=FALSE,quiet=FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[sf]{st_transform}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}
#' @rdname append_catalog_fields
#' @export
#' @importFrom glue glue
#' @importFrom sf st_transform st_read st_crs

append_catalog_fields <- function(path_to_ras_dbase,out_name="OWP_ras_catalog.csv",dat_path="/home/rstudio/g/Dropbox/root/database/hosted/water/HUC8.fgb",overwrite=FALSE,quiet=FALSE) {

  # https://github.com/NOAA-OWP/ras2fim/issues/34

  # sinew::moga(file.path(getwd(),"R/append_catalog_fields.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()
  # path_to_ras_dbase="/home/rstudio/g/data/ras_dbase"
  # overwrite=TRUE
  # quiet=FALSE

  ### -- Start --
  if(!quiet) { print(glue::glue("Appending catalog at {path_to_ras_dbase} into {out_name}")) }

  if(file.exists(file.path(path_to_ras_dbase,out_name,fsep = .Platform$file.sep))) {
    if(!overwrite) {
      print_warning_block()
      print("Alert: file already exists and overwrite is set to FALSE")
      stop()
    }
    unlink(file.path(path_to_ras_dbase,out_name,fsep = .Platform$file.sep))
  }
  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), quiet = quiet)

  # Date as YYYYMMDD
  ras_catalog_dbase <- ras_catalog_dbase %>%
    dplyr::mutate(date = as.Date(as.POSIXct(last_modified, origin = "1970-01-01"), "%Y%m%d") %>% format("%Y%m%d"))

  # HUC as pythonic list
  sf::sf_use_s2(FALSE)
  ras_catalog_dbase[,hucs:=character()]
  list_vec <- c()
  template_hucs <- sf::st_transform(sf::st_read(file.path(dat_path,fsep=.Platform$file.sep),quiet=quiet),sf::st_crs("EPSG:4326"))
  for (row in 1:nrow(ras_catalog_dbase)) {
    if(!quiet) { print(glue::glue("Processing row:{row} of {nrow(ras_catalog_dbase)}")) }
    if(is.na(ras_catalog_dbase[row,final_name_key]) || !file.exists(file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row,final_name_key],"hull.fgb",fsep = .Platform$file.sep))) {
      print_warning_block()
      ras_catalog_dbase[row,hucs:=noquote(paste0("{}"))]
    } else {
      footprint <- sf::st_read(file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row,final_name_key],"hull.fgb",fsep = .Platform$file.sep),quiet=TRUE)
      val <- template_hucs[footprint,]$huc8
      ras_catalog_dbase[row,hucs:=noquote(paste0("{",paste(noquote(val) %>% sQuote(), collapse = ";"),"}"))]
    }
  }
  sf::sf_use_s2(TRUE)

  # Status field
  ras_catalog_dbase <- ras_catalog_dbase[, status:="Active"]

  data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,out_name,fsep = .Platform$file.sep), row.names = FALSE)

  return(TRUE)
}
