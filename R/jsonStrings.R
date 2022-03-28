#' @useDynLib jsonStrings, .registration=TRUE
#' @importFrom Rcpp evalCpp setRcppClass 
#' @importFrom methods new
NULL

JsonString <- setRcppClass("JsonString")


#' @title R6 class representing a JSON string
#' @description R6 class representing a JSON string.
#'
#' @importFrom R6 R6Class
#' @export 
jsonString <- R6Class(
  "jsonString",
  
  lock_class = TRUE,
  
  cloneable = FALSE,
  
  private = list(
    .prettyPrint = TRUE,
    .jsonString = NULL,
    .ptrinit = function(ptr){
      json <- jsonString$new("{}")
      json[[".__enclos_env__"]][["private"]][[".jsonString"]] <- 
        new("JsonString", ptr, 0L)
      json
    }
  ),
  
  active = list(
    #' @field prettyPrint get or set the value of \code{prettyPrint}
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' )
    #' jstring$prettyPrint
    #' jstring
    #' jstring$prettyPrint <- FALSE
    #' jstring
    prettyPrint = function(value) {
      if(missing(value)) {
        private[[".prettyPrint"]]
      } else {
        stopifnot(
          isBoolean(value)
        )
        private[[".prettyPrint"]] <- value
      }
    }
  ),
  
  public = list(
    
    #' @description Creates a new \code{jsonString} object.
    #'
    #' @param string a string representing a JSON object
    #'
    #' @return A \code{jsonString} object.
    #'
    #' @examples
    #' jstring <- "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' jsonString$new(jstring)
    initialize = function(string){
      private[[".jsonString"]] <- JsonString$new(string)
    },
    
    #' @description Print a \code{jsonString} object.
    #' @param ... ignored
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' )
    #' jstring
    #' jstring$prettyPrint <- FALSE
    #' jstring
    print = function(...){
      private[[".jsonString"]]$print(pretty = private[[".prettyPrint"]])
    },
    
    #' @description Converts a \code{jsonString} to a character string.
    #' @param pretty Boolean, whether to get a pretty string
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' )
    #' cat(jstring$asString())
    #' cat(jstring$asString(pretty = TRUE))
    asString = function(pretty = FALSE){
      stopifnot(isBoolean(pretty))
      private[[".jsonString"]]$asString(pretty)
    },

    #' @description Extract an element in a JSON string by giving a path of 
    #'   keys or indices.
    #' @param ... the elements forming the path, integers or strings
    #' @return A \code{jsonString} object.
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' )
    #' jstring$at(1)
    #' jstring$at(2, "x")
    at = function(...){
      ptr <- private[[".jsonString"]]$at(list(...))
      private[[".ptrinit"]](ptr)
    }
    
  )
  
)

#' @title JSON string
#' @description Create a JSON string.
#'
#' @param string a character string
#'
#' @return A JSON string (external pointer).
#' @export
#'
#' @examples jsonString("[1,[\"a\",99],{\"x\":[2,3,4],\"y\":42}]")
jString <- function(string){
  new(JSON, string)$jsonPointer()
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
  new(JSONPTR, jsonstring)$at(keys = keys, indices = indices, isIndex = isIndex)
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
  new(JSONPTR, jsonstring)$hasKey(key)
}

#' @title Add new property
#' @description Add a new property to a JSON string.
#'
#' @param jsonstring a JSON string representing an object
#' @param key a character string, the key of the new property
#' @param value a JSON string, the value of the new property
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
  new(JSONPTR, jsonstring)$addProperty(key, value)
}

#' @title Erase property or element
#' @description Erase an object property or an array element from a JSON string.
#'
#' @param jsonstring a JSON string representing an object or an array
#' @param at either a character string, the key of the property to be erased, 
#'   or an integer, the index of the array element to be erased
#'
#' @return A JSON string.
#' @export
#'
#' @examples jsonErase("{\"a\":[1,2,3],\"b\":\"hello\"}", "a")
jsonErase <- function(jsonstring, at){
  if(is.character(jsonstring)){
    jsonstring <- jsonString(jsonstring)
  }
  if(is.character(at)){
    new(JSONPTR, jsonstring)$eraseProperty(at)
  }else if(is.numeric(at) && at >= 0){
    new(JSONPTR, jsonstring)$eraseElement(as.integer(at))
  }else{
    stop("Invalid `at` argument", call. = TRUE)
  }
}

#' @title Number of elements
#' @description Number of elements in a JSON string.
#'
#' @param jsonstring a JSON string 
#'
#' @return An integer.
#' @export
#'
#' @examples jsonSize("{\"a\":[1,2,3],\"b\":\"hello\"}")
jsonSize <- function(jsonstring){
  if(is.character(jsonstring)){
    jsonstring <- jsonString(jsonstring)
  }
  new(JSONPTR, jsonstring)$size()
}

#' @title Update object
#' @description Update a JSON string.
#'
#' @param jsonstring1 a JSON string representing an object
#' @param jsonstring2 a JSON string representing an object
#'
#' @return A JSON string.
#' @export
#'
#' @examples jstring1 <- jsonString("{\"a\":[1,2,3],\"b\":\"hello\"}")
#' jstring2 <- jsonString("{\"a\":[4,5,6],\"c\":\"goodbye\"}")
#' jsonUpdate(jstring1, jstring2)
jsonUpdate <- function(jsonstring1, jsonstring2){
  if(is.character(jsonstring1)){
    jsonstring1 <- jsonString(jsonstring1)
  }
  if(is.character(jsonstring2)){
    jsonstring2 <- jsonString(jsonstring2)
  }
  new(JSONPTR, jsonstring1)$update(jsonstring2)
}

#' @title Patch a JSON string
#' @description Apply a JSON patch to a JSON string.
#'
#' @param jsondoc a JSON string representing an object or an array
#' @param jsonpatch a JSON patch, a JSON string representing an array (see 
#'   the link in details)
#'
#' @return A JSON string.
#' @export
#' 
#' @details See \href{http://jsonpatch.com/}{jsonpatch.com}.
#'
#' @examples jsondoc <- jsonString("{\"a\":[1,2,3],\"b\":\"hello\"}")
#' jsonpatch <- jsonString("[
#'   {\"op\": \"remove\", \"path\": \"/a\"},
#'   {\"op\": \"replace\", \"path\": \"/b\", \"value\": null}
#' ]")
#' jsonPatch(jsondoc, jsonpatch)
jsonPatch <- function(jsondoc, jsonpatch){
  if(is.character(jsondoc)){
    jsondoc <- jsonString(jsondoc)
  }
  if(is.character(jsonpatch)){
    jsonpatch <- jsonString(jsonpatch)
  }
  new(JSONPTR, jsondoc)$patch(jsonpatch)
}

#' @title Merge JSON strings
#' @description Merge two JSON strings.
#'
#' @param jsonstring1 a JSON string 
#' @param jsonstring2 a JSON string 
#'
#' @return A JSON string.
#' @export
#'
#' @examples jstring1 <- jsonString("{\"a\":[1,2,3],\"b\":\"hello\"}")
#' jstring2 <- jsonString("{\"a\":[4,5,6],\"c\":\"goodbye\"}")
#' jsonMerge(jstring1, jstring2)
jsonMerge <- function(jsonstring1, jsonstring2){
  if(is.character(jsonstring1)){
    jsonstring1 <- jsonString(jsonstring1)
  }
  if(is.character(jsonstring2)){
    jsonstring2 <- jsonString(jsonstring2)
  }
  new(JSONPTR, jsonstring1)$merge(jsonstring2)
}

#' @title Append an element
#' @description Append an element to a JSON string.
#'
#' @param jsonstring1 JSON string representing an array
#' @param jsonstring2 JSON string 
#'
#' @return A JSON string.
#' @export
#'
#' @examples jstring1 <- jsonString("[1,2,3]")
#' jstring2 <- jsonString("{\"a\":\"hello\",\"b\":\"goodbye\"}")
#' jsonPush(jstring1, jstring2)
jsonPush <- function(jsonstring1, jsonstring2){
  if(is.character(jsonstring1)){
    jsonstring1 <- jsonString(jsonstring1)
  }
  if(is.character(jsonstring2)){
    jsonstring2 <- jsonString(jsonstring2)
  }
  new(JSONPTR, jsonstring1)$push(jsonstring2)
}

#' @title Check type of JSON string 
#' @description Check the type of a JSON string.
#'
#' @param jsonstring a JSON string
#' @param type the type to be checked, one of \code{"array"}, \code{"object"}, 
#'   \code{"string"}, \code{"number"}, \code{"integer"}, \code{"null"}, 
#'   \code{"boolean"}
#'
#' @return A logical value.
#' @export
#' 
#' @examples jsonIs("[1,2]", "array")
jsonIs <- function(jsonstring, type){
  types <- 
    c("array", "object", "number", "integer", "string", "null", "boolean")
  type <- match(match.arg(type, types), types)
  if(is.character(jsonstring)){
    jsonstring <- jsonString(jsonstring)
  }
  new(JSONPTR, jsonstring)$is(type)
}

#' @title Type of JSON string 
#' @description The type of a JSON string.
#'
#' @param jsonstring a JSON string
#'
#' @return A character string indicating the type of the JSON string.
#' @export
#' 
#' @examples jsonType("[1,2]")
jsonType <- function(jsonstring){
  if(is.character(jsonstring)){
    jsonstring <- jsonString(jsonstring)
  }
  new(JSONPTR, jsonstring)$type()
}

# #' @title JSON string to character string
# #' @description Convert a JSON string to a character string.
# #'
# #' @param x a JSON string
# #' @param pretty logical value, whether to pretty-format the string
# #' @param ... ignored
# #' 
# #' @return A character string.
# #' @export
#' as.character.jsonString <- function(x, pretty = FALSE, ...){
#'   new(JSONPTR, x)$jsonString(pretty = pretty)
#' }
#' 
# #' @title Print JSON string
# #' @description Print a JSON string.
# #'
# #' @param x a JSON string
# #' @param pretty logical value, whether to pretty-print
# #' @param ... ignored
# #'
# #' @export
#' print.jsonString <- function(
#'   x, pretty = getOption("jsonStrings.prettyPrint", FALSE), ...
#' ){
#'   if(pretty){
#'     cat(as.character(x, pretty = TRUE))
#'   }else{
#'     print(as.character(x))
#'   }
#' }
