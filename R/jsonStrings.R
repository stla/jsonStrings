#' @useDynLib jsonStrings, .registration=TRUE
#' @importFrom Rcpp evalCpp setRcppClass 
#' @importFrom methods new
NULL

JsonString <- setRcppClass("JsonString")

Xptr <- function(jstring){
  jstring[[".__enclos_env__"]][["private"]][[".jsonString"]][["ptr"]]
}

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
    },
    
    
    #' @description Checks whether a key exists in a JSON string.
    #' @param key a string
    #' @return A Boolean value.
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' )
    #' jstring$hasKey("x")
    #' jstring <- jsonString$new(
    #'   "{\"x\": [2,3,4], \"y\": 42}"
    #' )
    #' jstring$hasKey("x")
    hasKey = function(key){
      stopifnot(isString(key))
      private[[".jsonString"]]$hasKey(key)
    },
    
    #' @description Add a new property to the reference JSON string (if it 
    #'   represents an object).
    #'
    #' @param key a character string, the key of the new property
    #' @param value a JSON string, the value of the new property
    #'
    #' @return Nothing, this updates the reference JSON string.
    #' 
    #' @examples 
    #' jstring <- jsonString$new("{\"a\":[1,2,3],\"b\":\"hello\"}")
    #' ppty <- jsonString$new("[9, 99]")
    #' jstring$addProperty("c", ppty)
    #' jstring
    addProperty = function(key, value){
      stopifnot(isString(key))
      stopifnot(isJsonString(value))
      ptr <- Xptr(value)
      private[[".jsonString"]]$addProperty(key, ptr)
    },
    
    #' @description Erase an object property or an array element from a JSON string.
    #'
    #' @param jsonstring a JSON string representing an object or an array
    #' @param at either a character string, the key of the property to be erased, 
    #'   or an integer, the index of the array element to be erased
    #'
    #' @return Nothing, this updates the reference JSON string.
    #'
    #' @examples 
    #' jstring <- jsonString$new("{\"a\":[1,2,3],\"b\":\"hello\"}")
    #' jstring$erase("b")
    #' jstring
    #' jstring <- jsonString$new("[1, 2, 3, 4, 5]")
    #' jstring$erase(2)
    #' jstring
    erase = function(at){
      if(isString(at)){
        private[[".jsonString"]]$eraseProperty(at)
      }else if(isPositiveInteger(at)){
        private[[".jsonString"]]$eraseElement(as.integer(at))
      }else{
        stop("Invalid `at` argument.")
      }
    },
    
    #' @description Number of elements in the reference JSON string.
    #' 
    #' @return An integer.
    #' 
    #' @examples 
    #' jstring <- jsonString$new("{\"a\":[1,2,3],\"b\":\"hello\"}")
    #' jstring$size()
    size = function(){
      private[[".jsonString"]]$size()
    },
    
    #' @description Update the reference JSON string (if it 
    #'   represents an object).
    #'
    #' @param jstring a JSON string representing an object
    #'
    #' @return Nothing, this updates the reference JSON string.
    #' 
    #' @examples 
    #' jstring <- jsonString$new("{\"a\":[1,2,3],\"b\":\"hello\"}")
    #' jstring2 <- jsonString$new("{\"a\":[4,5,6],\"c\":\"goodbye\"}")
    #' jstring$update(jstring2)
    #' jstring
    update = function(jstring){
      stopifnot(isJsonString(jstring))
      ptr <- Xptr(jstring)
      private[[".jsonString"]]$update(ptr)
    }
    
  )
  
)





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
