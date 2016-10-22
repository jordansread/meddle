
#' render xml from data and template
#'
#' create metadata file from input data or yaml file
#'
#' @param data filepath or list
#' @param filename name of file to write metadata to
#' @param \dots additional lists to include or other arguments passed to methods
#' (e.g., \code{template="metadata.mustache"})
#' @param template a character or filepath
#' @details template can be missing, a filepath to a *.mustache file, or a string
#' @keywords internal
#' @examples
#' render(list('dogname'='fred','catname'='midred'),
#'    filename=NULL, list('dogname'='betty'), template="my dog's name is: {{dogname}}")
#'
#' @seealso \code{\link[whisker]{whisker.render}}
#'
#' @export
render <- function(data, filename, ...){
  UseMethod("render")
}

#' @describeIn render render text to a file from a yaml file
#' @importFrom yaml yaml.load_file
#' @export
render.character <- function(data, filename, ..., template){
  stopifnot(file.exists(data))
  config.text <- yaml::yaml.load_file(data)
  render(data = config.text, filename = filename, ...)
}


#' @describeIn render render text to a file from a list
#' @export
#' @importFrom whisker whisker.render
#' @importFrom utils packageName
render.list <- function(data, filename, ..., template){
  if (missing(template)){
    template <- system.file(package=packageName(),'extdata', "FGDC_template.mustache")
  }
  # do special things for `ADD-CONTENT` blocks?
  # parse(text=as.character(d$`spatial`$`ADD-CONTENT`), keep.source=FALSE)
  text <- append_list_replace(data, ...)
  template <- as.template(template)
  output <- whisker::whisker.render(template, text)
  if (is.null(filename)){
    return(output)
  } else {
    cat(output, file = filename)
  }

}

eval_content <- function(x){
  if (grepl('`r ',  x)){
    exp <- gsub("`r (.*?)`", "\\1", x)
    data = eval(parse(text=as.character(exp)))
  } else {
    data <- x
  }
  return(data)
}
