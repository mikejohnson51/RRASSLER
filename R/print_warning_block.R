#' @title print_warning_block
#' @description A warning block helper
#' @return print output
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  RRASSLER::print_warning_block()
#'  }
#' }
#' @rdname print_warning_block
#' @export

print_warning_block <- function() {

  # sinew::moga(file.path(getwd(),"R/print_warning_block.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  #
  # devtools::load_all()

  ## -- Start --
  print("   --   Warning Wizard   --    ")
  print("                                                 ")
  print("                             /\\                  ")
  print("                            /  \\                 ")
  print("                           |    |                ")
  print("                         --:'''':--              ")
  print("                           :'_' :                ")
  print("                           _:  :\\___             ")
  print("            ' '      ____.' :::     '._          ")
  print("           . *=====<<=)           \\    :         ")
  print("            .  '      '-'-'\\_      /'._.'        ")
  print("                             \\====:_             ")
  print("                            .'     \\            ")
  print("                           :       :             ")
  print("                          /   :    \\             ")
  print("                         :   .      '.           ")
  print("         ,. _            :  : :      :           ")
  print("      '-'    ).          :__:-:__.;--'           ")
  print("    (        '  )        '-'   '-'               ")
  print(" ( -   .00.   - _                                ")
  print("(    .'  _ )     )                               ")
  print("'-  ()_.\\,\\,   -                                 ")

  return(TRUE)
}
