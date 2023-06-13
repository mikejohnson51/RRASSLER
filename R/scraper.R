scraper <- function(path_to_ras_dbase=TRUE) {

  # sinew::moga(file.path(getwd(),"R/hello.R"),overwrite = TRUE)
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



  dir.create(file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
  file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,rasmap_files),
            file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep))

  arrow::write_parquet(extrated_pts[[1]],file.path(path_to_ras_dbase,"models",current_final_name_key,"ras_xyz.parquet",fsep = .Platform$file.sep))
  sf::st_write(ahull_poly,file.path(path_to_ras_dbase,"models",current_final_name_key,"hull.fgb",fsep = .Platform$file.sep))
  data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), row.names = FALSE)

  return(TRUE)
}
