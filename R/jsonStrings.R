#' @useDynLib jsonStrings, .registration=TRUE
#' @importFrom Rcpp evalCpp
NULL

#' @title Access an element in a JSON string
#' @description Extract an element in a JSON string by giving a path of keys or 
#'   indices.
#'
#' @param jsonString a JSON string representing an array or an object
#' @param path path to the element to extract; either a character vector of 
#'   keys, an integer vector of indices, or a list made of keys and indices
#'
#' @return A JSON string.
#' @export
#'
#' @examples jstring <- "[1,[\"a\",99],{\"x\":[2,3,4],\"y\":42}]"
#' jsonAt(jstring, 1)
#' jsonAt(jstring, list(2, "x"))
#' jsonAt(jstring, list(2, "z"))
jsonAt <- function(jsonString, path){
  if(is.list(path)){
    if(!checkPath(path)){
      stop("Invalid path.", call. = TRUE)
    }
    keys <- unlist(path)
    indices <- suppressWarnings(as.integer(keys))
    isIndex <- vapply(path, is.numeric, FUN.VALUE = logical(1L))
  }else if(is.numeric(path)){
    keys <- ""
    indices <- as.integer(path)
    isIndex <- rep(TRUE, length(path))
  }else if(is.character(path)){
    keys <- path
    indices <- 0L
    isIndex <- rep(FALSE, length(path))
  }else{
    stop("Invalid path.", call. = TRUE)
  }
  if(any(indices < 0L, na.rm = TRUE)){
    stop("Negative indices found in path.", call. = TRUE)
  }
  cpp_jsonAt(jsonString, keys = keys, indices = indices, isIndex = isIndex)
}