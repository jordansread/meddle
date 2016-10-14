
#' render xml from data and template
#'
#' create metadata file from input data or yaml file
#'
#' @param data filepath or list
#' @param filename name of file to write metadata to
#' @param \dots additional lists to include or other arguments passed to methods
#' (e.g., \code{template="metadata.mustache"})
#'
#' @export
render <- function(data, filename, ...){
  UseMethod("render")
}

#' @importFrom yaml yaml.load_file
#' @export
render.character <- function(data, filename, ...){

  config.text <- yaml::yaml.load_file(config)
  render(data = config.text, filename = filename, ...)
}

#' render from a list
#'
#' render text to a file from a list
#'
#' @describeIn render
#' @param template a character or filepath
#' @details template can be missing, a filepath to a *.mustache file, or a string
#' @importFrom whisker whisker.render
#' @keywords internal
#' @examples
#' render(list('dogname'='fred','catname'='midred'),
#'    filename=NULL, list('dogname'='betty'), template="my dog's name is: {{dogname}}")
#' @export
render.list <- function(data, filename, ..., template){
  if (missing(template)){
    template <- system.file(package=packageName(),'extdata', "FGDC_template.mustache")
  }
  text <- append_list_replace(data, ...)
  template <- as.template(template)
  output <- whisker::whisker.render(template, text)
  if (is.null(filename)){
    return(output)
  } else {
    cat(output, file = filename)
  }

}
