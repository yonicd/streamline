#' @importFrom rstudioapi getSourceEditorContext navigateToFile
#' @importFrom tools file_path_sans_ext file_ext
clean_ctl_addin <- function(){
  adc <- rstudioapi::getSourceEditorContext()
  file_name <- basename(adc$path)
  
  tf <- sprintf('%s_clean.%s',tools::file_path_sans_ext(file_name),tools::file_ext(file_name))
  
  tf <- file.path(tempdir(),tf)
  
  file.create(tf,showWarnings = FALSE)
  
  cat( ctl_style( paste0(adc$contents,collapse = '\n') ), file=tf)
  
  rstudioapi::navigateToFile(tf)  
}
