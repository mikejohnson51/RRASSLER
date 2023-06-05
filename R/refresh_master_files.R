#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param path_to_ras_dbase PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [glob2rx][utils::glob2rx]
#'  [glue][glue::glue]
#'  [st_read][sf::st_read], [st_set_crs][sf::st_set_crs], [st_crs][sf::st_crs], [st_write][sf::st_write]
#'  [read_parquet][arrow::read_parquet], [write_parquet][arrow::write_parquet]
#'  [as.data.table][data.table::as.data.table], [rbindlist][data.table::rbindlist]
#'  [sf_linestring][sfheaders::sf_linestring]
#' @rdname refresh_master_files
#' @export
#' @importFrom utils glob2rx
#' @importFrom glue glue
#' @importFrom sf st_read st_set_crs st_crs st_write
#' @importFrom arrow read_parquet write_parquet
#' @importFrom data.table as.data.table rbindlist
#' @importFrom sfheaders sf_linestring

refresh_master_files <- function(path_to_ras_dbase) {

  print("(re)merging database outputs")

  # path_to_ras_dbase = "H:/ras_dbase"
  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep))

  # Remerge master features
  xyz_files <- list.files(path_to_ras_dbase, pattern = utils::glob2rx("*ras_xyz.parquet$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE) %>% sort()
  hull_files <- list.files(path_to_ras_dbase, pattern = utils::glob2rx("*hull.fgb$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE) %>% sort()

  if(!(length(xyz_files)==length(hull_files))) {
    print("Alert, something is off here...")
    stop()
  }

  point_concat <- c()
  xs_concat <- c()
  hull_concat <- c()
  master_id <- 0
  rows_in_table <- length(xyz_files)
  for(index in 1:rows_in_table) {
    print(glue::glue("Processing {index} of {rows_in_table}"))
    # index = 1

    final_folder_name <- basename(dirname(hull_files[index]))
    row <- ras_catalog_dbase[ras_catalog_dbase$final_name_key==final_folder_name,]
    hull <- sf::st_read(hull_files[index],quiet = TRUE)

    hull$start_master_id <- master_id + 1

    point_data <- arrow::read_parquet(xyz_files[index],as_data_frame = TRUE) %>%
      data.table::as.data.table()
    point_data[, master_id := xid + master_id]
    point_concat <- data.table::rbindlist(list(point_concat, point_data))
    master_id <- max(point_data[,master_id])

    hull$Name <- row$model_name
    hull$crs <- row$crs
    hull$units <- row$units
    hull$path <- row$final_name_key
    hull$source <- row$source
    hull$end_master_id <- master_id
    hull_concat <- rbind(hull_concat,hull)
  }

  xs_lines <- sfheaders::sf_linestring(
    obj = point_concat,
    x = "x",
    y = "y",
    # z = "z",
    linestring_id = "master_id",
    keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

  unlink(file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))
  unlink(file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))
  unlink(file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep))

  arrow::write_parquet(point_concat,file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))
  sf::st_write(xs_lines,file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))
  sf::st_write(hull_concat,file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep))

  return(TRUE)
}
