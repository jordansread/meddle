
#' get metadata for computing environment
#' 
#' environment metadata returned as a string
#' 
#' @details 
#' create a string from sessionInfo() that can be used 
#' for `Native Data Set Environment`
#' @export
environment_metadata <- function(){
  
  sess_list <- sessionInfo()
  template_string <- 'This dataset was generated using open source tools available in the R programming language (%s).
  The computing platform for generating data and metadata was %s. 
  R packages loaded into this environment: %s.'
  
  packages_string <- paste(sapply(sess_list$otherPkgs, function(x) paste0(x$Package, ", version: ", x$Version)), collapse = '; ')
  sess_string <- sprintf(template_string, sess_list$R.version$version.string, sess_list$R.version$platform, packages_string)
  
  return(sess_string)
}