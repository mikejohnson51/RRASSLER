#' @title scraper
#' @description silly handle around unscraped databases
#' @param path_to_ras_dbase The path to the top level folder of the ras_catalog structure, Default: TRUE
#' @return a new set of model footprints to map
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{as.data.table}}, \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{rbindlist}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[arrow]{write_parquet}}
#'  \code{\link[sf]{st_write}}
#' @rdname scraper
#' @export
#' @importFrom data.table as.data.table data.table rbindlist fwrite
#' @importFrom arrow write_parquet
#' @importFrom sf st_write
scraper <- function(path_to_ras_dbase=TRUE) {

  # sinew::moga(file.path(getwd(),"R/scraper.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  #
  # devtools::load_all()
  #
  # path_to_ras_dbase=TRUE

  ## -- Start --
  names <- c("source","name","model_name","lat","long","notes")

  ras_catalog_dbase <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(ras_catalog_dbase) <- names
  ras_catalog_dbase <- data.table::as.data.table(ras_catalog_dbase)

  new_row <- data.table::data.table("Colorado Water Conservation Board",
                                    "Colorado Water Conservation Board",
                                    "https://coloradohazardmapping.com/data",
                                    30,
                                    40,
                                    "no scraper yet")
  names(new_row) <- names
  ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

  # # https://www.arcgis.com/apps/dashboards/1e98f1e511fc40d3b08790a4251a64ee
  # 12080005 2D
  # 12080004 2D
  # 12080003 2D

  dir.create(file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
  file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,rasmap_files),
            file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep))

  arrow::write_parquet(extrated_pts[[1]],file.path(path_to_ras_dbase,"models",current_final_name_key,"ras_xyz.parquet",fsep = .Platform$file.sep))
  sf::st_write(ahull_poly,file.path(path_to_ras_dbase,"models",current_final_name_key,"hull.fgb",fsep = .Platform$file.sep))
  data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), row.names = FALSE)

  return(TRUE)
}
