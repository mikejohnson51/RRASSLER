#' @title disk_ingest_record
#' @description FUNCTION_DESCRIPTION
#' @param in_file PARAM_DESCRIPTION, Default: NULL
#' @param ras_dbase PARAM_DESCRIPTION, Default: NULL
#' @param code_to_place_in_source PARAM_DESCRIPTION, Default: NULL
#' @param proj_override PARAM_DESCRIPTION, Default: NULL
#' @param vdat_trans PARAM_DESCRIPTION, Default: FALSE
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param quick_check PARAM_DESCRIPTION, Default: FALSE
#' @param quick_hull PARAM_DESCRIPTION, Default: FALSE
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_write}}
#'  \code{\link[stringr]{str_sub}}, \code{\link[stringr]{str_detect}}
#'  \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{rbindlist}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[holyhull]{holyhull}}
#'  \code{\link[nhdplusTools]{get_nhdplus}}
#'  \code{\link[AOI]{aoi_get}}
#'  \code{\link[arrow]{write_parquet}}
#' @rdname disk_ingest_record
#' @export
#' @importFrom glue glue
#' @importFrom utils glob2rx
#' @importFrom sf st_crs st_set_crs st_coordinates st_as_sf st_transform st_write
#' @importFrom stringr str_sub str_detect
#' @importFrom data.table data.table rbindlist fwrite
#' @importFrom sfheaders sf_linestring
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom dplyr group_by distinct
#' @importFrom holyhull holyhull
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom arrow write_parquet
disk_ingest_record <- function(in_file = NULL,
                               path_to_ras_dbase = NULL,
                               code_to_place_in_source = NULL,
                               proj_override = NULL,
                               vdat_trans = FALSE,
                               quiet = FALSE,
                               verbose = FALSE,
                               quick_check = FALSE,
                               quick_hull = FALSE,
                               overwrite = FALSE) {
  # sinew::moga(file.path(getwd(),"R/disk_ingest_record.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()
  #
  # ras_dbase = "G:/data/ras_catalog/"
  # top_of_dir_to_scrape = "G:/data/ras_catalog/_temp/BLE/12090301/12090301_models"
  # list_of_prj_files <- list.files(top_of_dir_to_scrape, pattern = glob2rx("*.prj$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  # x <- list_of_prj_files[1]
  # code_to_place_in_source = "FEMA Region 6:12090301"
  # proj_override = "EPSG:2277"
  # vdat_trans = FALSE
  # quiet = FALSE
  # verbose = TRUE
  # quick_check = FALSE
  # quick_hull = FALSE
  # overwrite = FALSE
  # refresh = FALSE

  # What is in the database at this very moment?
  all_scrape_names <- basename(list.dirs(path = file.path(path_to_ras_dbase,"models",fsep = .Platform$file.sep), full.names = TRUE, recursive = TRUE))
  inital_scrape_names <- all_scrape_names[grepl("^unknown_", all_scrape_names)]
  processed_scrape_names <- all_scrape_names[!grepl("^unknown_", all_scrape_names)]

  file <- in_file

  # Per-model constants
  dir_of_file <- dirname(file)
  current_model_name <- gsub('.{4}$', '', basename(file))
  current_nhdplus_comid = NA
  current_model_units = NA
  current_model_projection = NA
  current_last_modified = as.integer(as.POSIXct(file.info(file)$mtime))

  # Files to copy around
  g_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.g??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  ghdf_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.g??.hdf$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  f_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.f??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  h_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.h??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  v_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.v??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  o_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.o??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  r_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.r??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  u_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.u??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  x_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.x??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  rasmap_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.rasmap$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  prj_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.prj$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  p_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.p??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  xml_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.xml$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  # pdf_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.pdf$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  p_files <- p_files[!p_files %in% prj_files]
  list_of_files <- c(g_files ,ghdf_files ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files)

  if(length(g_files) == 0) {
    next()
  }

  # populate what we can from a projection file and project file
  for(potential_file in prj_files) {
    # potential_file <- prj_files[1]
    file_text <- read.delim(potential_file, header = FALSE)

    if(any(c('PROJCS','GEOGCS','DATUM','PROJECTION') == file_text)) {
      if(!quiet) { message('found a projection') }
      current_model_projection = sf::st_crs(potential_file)
    } else if(grepl("SI Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "Meter"
    } else if(grepl("English Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "Foot"
    }
  }

  if(is.na(current_model_projection) & !is.null(proj_override)) {
    current_model_projection = proj_override
  }

  # For each geometric realization of the model
  for(g_file in g_files) {
    # g_file <- g_files[1]
    current_g_value <- stringr::str_sub(g_file,-3,-1)

    # Useful if we are re-scraping a large ingest source
    if(quick_check) {
      if(stringr::str_sub(basename(g_file), end = -5) %in% unglue::unglue(all_scrape_names,"{}_{x}_{}")) {
        next()
      }
    }

    # Do we have enough info to parse the file at this point?
    cond1 = !is.na(current_model_units)
    cond2 = !is.na(current_model_projection)
    cond3 = file.exists(paste0(g_file,".hdf"))

    # We don't know either the projection or the units and can't parse this yet
    if(!(cond1 & cond2)) {
      current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
      current_final_name_key <- NA

      if(sum(stringr::str_detect(inital_scrape_names, current_initial_name)) > 0) {
        if(overwrite) {
          # Model already processed into _unprocessed {current_initial_name}, overwriting
          # List of files to delete
          files_to_delete <- list.files(file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
          for(old_file in files_to_delete) {
            unlink(old_file)
          }
        } else {
          # Model already processed into _unprocessed {current_initial_name}
          next()
        }
      }

      new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_initial_name,current_final_name_key,"unparsed_units_proj")
      names(new_row) <- names

      data.table::fwrite(new_row,file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,"RRASSLER_metadata.csv",fsep = .Platform$file.sep), row.names = FALSE)

      files_to_copy <- c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files)
      file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files),
                file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep))

      next()
    }

    if(!quiet) { message(glue::glue("Parsing:{g_file}")) }
    extrated_pts <- try({
      parse_model_to_xyz(geom_path = g_file,
                         units = current_model_units,
                         proj_string = current_model_projection,
                         in_epoch_override = as.integer(as.POSIXct(Sys.time())),
                         out_epoch_override = as.integer(as.POSIXct(Sys.time())),
                         vdat_trans = FALSE,
                         quiet = TRUE,
                         default_g = FALSE,
                         try_both = TRUE)
    })

    if(isFALSE(extrated_pts[[1]]) | (c("try-error") %in% class(extrated_pts))) {
      current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
      current_final_name_key <- NA

      current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
      current_final_name_key <- NA

      if(sum(stringr::str_detect(inital_scrape_names, current_initial_name)) > 0) {
        if(overwrite) {
          # Model already processed into _unprocessed {current_initial_name}, overwriting
          # List of files to delete
          files_to_delete <- list.files(file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
          for(old_file in files_to_delete) {
            unlink(old_file)
          }
        } else {
          next()
        }
      }

      new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_initial_name,current_final_name_key,"unparsed_units_proj")
      names(new_row) <- names

      data.table::fwrite(new_row,file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,"RRASSLER_metadata.csv",fsep = .Platform$file.sep), row.names = FALSE)

      files_to_copy <- c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files)
      file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files),
                file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep))

      next()
    }

    # Footprint the points we extracted
    ls = sfheaders::sf_linestring(
      obj = extrated_pts[[1]]
      , x = "x"
      , y = "y"
      # , z = "z"
      , linestring_id = "xid"
      , keep = FALSE
    ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

    # Use full data frame or just end points
    if(quick_hull) {
      end_points <- c(ls %>% lwgeom::st_endpoint(), ls %>% lwgeom::st_startpoint())
      end_points <- sf::st_coordinates(end_points) %>%
        as.data.frame(extrated_pts[[1]]) %>%
        dplyr::group_by(X,Y) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("X","Y")) %>%
        sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
        sf::st_transform(sf::st_crs("EPSG:4326"))
    } else {
      end_points <- sf::st_coordinates(ls) %>%
        as.data.frame(extrated_pts[[1]]) %>%
        dplyr::group_by(X,Y) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("X","Y")) %>%
        sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
        sf::st_transform(sf::st_crs("EPSG:4326"))
    }

    ahull_poly = holyhull::holyhull(sf_frame=end_points, method='convave', alpha_value=0.01, concavity = 2, length_threshold = 0)

    flowline_list <- try({
      nhdplusTools::get_nhdplus(AOI::aoi_get(ahull_poly),realization = "flowline")
    })

    # Join to comids
    if(c("try-error") %in% class(flowline_list)) {
      current_nhdplus_comid = 3
    } else if(isFALSE(flowline_list)) {
      current_nhdplus_comid = 1
    } else if(length(flowline_list) == 0) {
      current_nhdplus_comid = 2
    } else {
      current_nhdplus_comid = flowline_list[flowline_list$streamorde == max(flowline_list$streamorde),][1,]$comid
    }

    current_initial_name = paste0(current_nhdplus_comid,"_",current_model_name,"_",current_g_value,"_",current_last_modified)
    current_final_name_key = current_initial_name

    if(sum(stringr::str_detect(processed_scrape_names, current_final_name_key)) > 0) {
      if(overwrite) {
        # Model already processed into {current_final_name_key}, overwriting
        files_to_delete <- list.files(file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
        for(old_file in files_to_delete) {
          unlink(old_file)
        }

      } else {
        # Model already processed into {current_final_name_key}
        next()
      }
    }

    new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_initial_name,current_final_name_key,"unparsed_units_proj")
    names(new_row) <- names
    data.table::fwrite(new_row,file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_metadata.csv",fsep = .Platform$file.sep), row.names = FALSE)

    arrow::write_parquet(extrated_pts[[1]],file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_cs_pts.parquet",fsep = .Platform$file.sep))

    sf::st_write(ahull_poly,file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_hull.fgb",fsep = .Platform$file.sep),quiet = TRUE)

    file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,rasmap_files),
              file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep))

  }
  # print(ras_catalog_dbase)
  return(TRUE)
}
