#' @title ingest_into_database
#' @description FUNCTION_DESCRIPTION
#' @param path_to_ras_dbase PARAM_DESCRIPTION
#' @param top_of_dir_to_scrape PARAM_DESCRIPTION
#' @param code_to_place_in_source PARAM_DESCRIPTION
#' @param proj_override PARAM_DESCRIPTION, Default: NULL
#' @param vdat_trans PARAM_DESCRIPTION, Default: FALSE
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param quick_check PARAM_DESCRIPTION, Default: FALSE
#' @param quick_hull PARAM_DESCRIPTION, Default: FALSE
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param free_treads PARAM_DESCRIPTION, Default: 4
#' @param refresh PARAM_DESCRIPTION, Default: TRUE
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
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[parallel]{detectCores}}, \code{\link[parallel]{makeCluster}}
#'  \code{\link[doParallel]{registerDoParallel}}
#'  \code{\link[foreach]{foreach}}
#' @rdname ingest_into_database
#' @export
#' @import magrittr
#' @import foreach
#' @importFrom glue glue
#' @importFrom stringr str_sub
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
ingest_into_database <- function(path_to_ras_dbase,
                                 top_of_dir_to_scrape,
                                 code_to_place_in_source,
                                 proj_override = NULL,
                                 vdat_trans = FALSE,
                                 quiet = FALSE,
                                 verbose = FALSE,
                                 quick_check = FALSE,
                                 quick_hull = FALSE,
                                 overwrite = FALSE,
                                 free_treads = 4,
                                 refresh = TRUE) {
  # sinew::moga(file.path(getwd(),"R/ingest_into_database.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()
  #
  # path_to_ras_dbase="/home/rstudio/g/data/ras_dbase"
  # top_of_dir_to_scrape="/home/rstudio/g/Dropbox/root/projects/floodmapping/methods/ras2fim/sample_data"
  # code_to_place_in_source="ras2fim_test_data"
  # proj_override="EPSG:26915"

  # path_to_ras_dbase="G:/data/ras_catalog"
  # top_of_dir_to_scrape="G:/data/ras_catalog/_temp/BLE/12010005/"
  # code_to_place_in_source="FEMA 6 BLE"
  # proj_override="ESRI:102739"
  # vdat_trans=FALSE
  # quiet=FALSE
  # chatty = TRUE
  # quick_check=TRUE
  # overwrite = FALSE
  # refresh = TRUE

  # path_to_ras_dbase = "G:/data/ras_catalog"
  # top_of_dir_to_scrape = "G:/data/ras_catalog/_temp/BLE/12090301/12090301_models"
  # code_to_place_in_source = "FEMA Region 6:12090301"
  # proj_override = "EPSG:2277"
  # vdat_trans = FALSE
  # quiet = FALSE
  # verbose = TRUE
  # quick_check = FALSE
  # quick_hull = FALSE
  # overwrite = FALSE
  # refresh = FALSE

  # gmailr::gm_auth_configure(path = "C:/Users/jimma/Desktop/client_secret_765662520275-iduoi88oke14pqst3ebukn5rb2qf0895.apps.googleusercontent.com.json")

  ## -- Start --
  fn_time_start <- Sys.time()
  if(!quiet) {
    message(glue::glue("Parsing {top_of_dir_to_scrape} to place in {path_to_ras_dbase}"))
  }

  # Global constants
  names <- c("nhdplus_comid","model_name","g_file","last_modified","source","units","crs","initial_scrape_name","final_name_key","notes")

  # Input sanitize
  code_to_place_in_source <- as.character(code_to_place_in_source)

  # Cloud or disk?
  cloud <- FALSE
  if(stringr::str_sub(path_to_ras_dbase,1,2) %in% c('s3','ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, .Platform$file.sep)[[1]]
    ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <- paste0("s3://",ls_srt_folders_file[2],"/")
    cloud <- TRUE
  }

  # Find a list of all the .prj files
  list_of_prj_files <- list.files(top_of_dir_to_scrape, pattern = glob2rx("*.prj$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  n_files_to_process <- length(list_of_prj_files)
  if(!quiet) { message(glue::glue("Found {n_files_to_process} potential ras files")) }

  # Set up par proc
  no_cores <- parallel::detectCores() - free_treads
  if(no_cores < 1) { no_cores = 1 }
  doParallel::registerDoParallel(cores = no_cores)
  cl <- parallel::makeCluster(no_cores)

  if(cloud) {
    foreach::foreach(x = list_of_prj_files) %do% cloud_ingest_record(file = x,
                                                                        ras_dbase = path_to_ras_dbase,
                                                                        root_bucket = path_to_root_bucket,
                                                                        code_to_place_in_source = code_to_place_in_source,
                                                                        proj_override = NULL,
                                                                        vdat_trans = FALSE,
                                                                        quiet = FALSE,
                                                                        verbose = FALSE,
                                                                        quick_check = FALSE,
                                                                        quick_hull = FALSE,
                                                                        overwrite = FALSE)
  } else {
    foreach::foreach(x = list_of_prj_files) %dopar% disk_ingest_record(file = x,
                                                                       ras_dbase = path_to_ras_dbase,
                                                                       code_to_place_in_source = code_to_place_in_source,
                                                                       proj_override = NULL,
                                                                       vdat_trans = FALSE,
                                                                       quiet = FALSE,
                                                                       verbose = FALSE,
                                                                       quick_check = FALSE,
                                                                       quick_hull = FALSE,
                                                                       overwrite = FALSE)
  }

  parallel::stopCluster(cl)

  # (re) build key files
  if(refresh) {
    refresh_master_files(path_to_ras_dbase,verbose = !quiet)
  }

  # Wrap up
  if(verbose) {
    runtime <- Sys.time() - fn_time_start
    units(runtime) <- "hours"
    message(paste("RAS Library appended in",round(runtime, digits = 3),"hours"))
  }

  return(TRUE)
}
