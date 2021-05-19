#' @useDynLib jsonStrings, .registration=TRUE
#' @importFrom Rcpp evalCpp Module
NULL


#' @title JSON string
#' @description Create a JSON string.
#'
#' @param string a character string
#'
#' @return A JSON string.
#' @export
#'
#' @examples jsonString("[1,[\"a\",99],{\"x\":[2,3,4],\"y\":42}]")
jsonString <- function(string){
  jsonModule <- Module("jsonModule", "jsonStrings")
  json <- jsonModule$JSON
  new(json, string)$jsonPointer()
}

#' @title Access an element in a JSON string
#' @description Extract an element in a JSON string by giving a path of keys or 
#'   indices.
#'
#' @param jsonstring a JSON string representing an array or an object
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
jsonAt <- function(jsonstring, path){
  if(is.character(jsonstring)){
    jsonstring <- jsonString(jsonstring)
  }
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
  jsonptrModule <- Module("jsonptrModule", "jsonStrings")
  jsonptr <- jsonptrModule$JSONPTR
  new(jsonptr, jsonstring)$at(keys = keys, indices = indices, isIndex = isIndex)
}


#' @title Does key exist?
#' @description Checks whether a key is present in a JSON string.
#'
#' @param jsonstring a JSON string
#' @param key character string
#'
#' @return \code{TRUE} if the given key is present in the JSON string, 
#'   \code{FALSE} otherwise.
#' @export
#'
#' @examples jsonHasKey("{\"a\":[1,2,3],\"b\":\"hello\"}", "b")
#' jsonHasKey("[1,2,3]", "a")
jsonHasKey <- function(jsonstring, key){
  if(is.character(jsonstring)){
    jsonstring <- jsonString(jsonstring)
  }
  jsonptrModule <- Module("jsonptrModule", "jsonStrings")
  jsonptr <- jsonptrModule$JSONPTR
  new(jsonptr, jsonstring)$hasKey(key)
}


#' @title Add new property
#' @description Add a new property to a JSON string.
#'
#' @param jsonstring JSON string representing an object
#' @param key character string, the key of the new property
#' @param value JSON string, value of the new property
#'
#' @return A JSON string.
#' @export
#'
#' @examples jsonAddProperty("{\"a\":[1,2,3],\"b\":\"hello\"}", "c", "[1,2]")
jsonAddProperty <- function(jsonstring, key, value){
  if(is.character(jsonstring)){
    jsonstring <- jsonString(jsonstring)
  }
  if(is.character(value)){
    value <- jsonString(value)
  }
  jsonptrModule <- Module("jsonptrModule", "jsonStrings")
  jsonptr <- jsonptrModule$JSONPTR
  new(jsonptr, jsonstring)$addProperty(key, value)
}

#' @title JSON string to character string
#' @description Convert a JSON string to a character string.
#'
#' @param jsonstring JSON string
#'
#' @return A character string.
#' @export
as.character.jsonString <- function(jsonstring){
  jsonptrModule <- Module("jsonptrModule", "jsonStrings")
  jsonptr <- jsonptrModule$JSONPTR
  new(jsonptr, jsonstring)$jsonString()
}

#' @title Print JSON string
#' @description Print a JSON string.
#'
#' @param jsonstring JSON string
#'
#' @export
print.jsonString <- function(jsonstring){
  print(as.character(jsonstring))
}
