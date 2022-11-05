#' @useDynLib jsonStrings, .registration=TRUE
#' @importFrom Rcpp evalCpp setRcppClass 
#' @importFrom methods new
NULL

JsonString <- setRcppClass("JsonString")

Xptr <- function(jstring){
  jstring[[".__enclos_env__"]][["private"]][[".jsonString"]][["ptr"]]
}

#' @title R6 class to represent a JSON string
#' @description R6 class to represent a JSON string.
#'
#' @importFrom R6 R6Class
#' @export 
jsonString <- R6Class(
  "jsonString",
  
  lock_class = TRUE,
  
  cloneable = FALSE,
  
  private = list(
    .prettyPrint = TRUE,
    .jsonString = NULL
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
    #' @param string a string representing a JSON object, or the path to a 
    #'   JSON file
    #'
    #' @return A \code{jsonString} object.
    #'
    #' @examples
    #' jstring <- "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' jsonString$new(jstring)
    initialize = function(string) {
      # initialization from a pointer is hidden to the user
      if(inherits(string, "externalptr")) {
        private[[".jsonString"]] <- JsonString$new(string, 0L)
        return(invisible(self))
      }
      stopifnot(isString(string))
      if(file.exists(string)) {
        ptr <- read_json(path.expand(string))
        private[[".jsonString"]] <- JsonString$new(ptr, 0L)
      } else {
        private[[".jsonString"]] <- JsonString$new(string)
      }
      invisible(self)
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
    print = function(...) {
      private[[".jsonString"]]$print(pretty = private[[".prettyPrint"]])
    },
    
    #' @description Converts a \code{jsonString} to a character string.
    #' @param pretty Boolean, whether to get a pretty string
    #' @return A string.
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' )
    #' cat(jstring$asString())
    #' cat(jstring$asString(pretty = TRUE))
    asString = function(pretty = FALSE) {
      stopifnot(isBoolean(pretty))
      private[[".jsonString"]]$asString(pretty)
    },

    #' @description Extract an element in a JSON string by giving a path of 
    #'   keys or indices.
    #' @param ... the elements forming the path, integers or strings; an 
    #'   integer represents an index in an array (starting at 0), a string
    #'   represents a key in an object
    #' @return A \code{jsonString} object.
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "[1, [\"a\", 99], {\"x\": [2,3,4], \"y\": 42}]"
    #' )
    #' jstring$at(1)
    #' jstring$at(2, "x")
    at = function(...) {
      ptr <- private[[".jsonString"]]$at(list(...))
      jsonString$new(ptr)
    },
    
    #' @description Checks whether a key exists in the reference JSON string.
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
    hasKey = function(key) {
      stopifnot(isString(key))
      private[[".jsonString"]]$hasKey(key)
    },

    #' @description Get the keys of the reference JSON string (if it represents 
    #'   an object).
    #' @return A character vector.
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"x\": [2,3,4], \"y\": 42}"
    #' )
    #' jstring$keys()
    keys = function() {
      private[[".jsonString"]]$keys()
    },
    
    #' @description Add a new property to the reference JSON string (if it 
    #'   represents an object).
    #'
    #' @param key a character string, the key of the new property
    #' @param value a JSON string, either a \code{jsonString} object or a 
    #'   string 
    #'
    #' @return This updates the reference JSON string and this returns it 
    #'  invisibly.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' ppty <- jsonString$new("[9, 99]")
    #' jstring$addProperty("c", ppty)
    #' jstring
    #' jstring$addProperty("d", "null")
    #' jstring
    addProperty = function(key, value) {
      stopifnot(isString(key))
      if(isJsonString(value)) {
        ptr <- Xptr(value)
      } else if(isString(value)) {
        ptr <- toJSONXptr(value)
      } else {
        stop("Invalid `value` argument.")
      }
      private[[".jsonString"]]$addProperty(key, ptr)
      invisible(self)
    },
    
    #' @description Erase an object property or an array element from the 
    #'   reference JSON string.
    #'
    #' @param at either a character string, the key of the property to be erased, 
    #'   or an integer, the index of the array element to be erased
    #'
    #' @return This invisibly returns the updated reference JSON string.
    #'
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jstring$erase("b")
    #' jstring
    #' jstring <- jsonString$new("[1, 2, 3, 4, 5]")
    #' jstring$erase(2)
    #' jstring
    erase = function(at) {
      if(isString(at)) {
        private[[".jsonString"]]$eraseProperty(at)
        invisible(self)
      }else if(isPositiveInteger(at)) {
        private[[".jsonString"]]$eraseElement(as.integer(at))
        invisible(self)
      } else {
        stop("Invalid `at` argument.")
      }
    },
    
    #' @description Number of elements in the reference JSON string.
    #' 
    #' @return An integer.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jstring$size()
    size = function() {
      private[[".jsonString"]]$size()
    },
    
    #' @description Update the reference JSON string (if it 
    #'   represents an object).
    #'
    #' @param jstring a JSON string representing an object, either a 
    #'   \code{jsonString} object or a string 
    #'
    #' @return This invisibly returns the updated reference JSON string.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jstring2 <- "{\"a\":[4,5,6],\"c\":\"goodbye\"}"
    #' jstring$update(jstring2)
    #' jstring
    update = function(jstring) {
      if(isJsonString(jstring)) {
        ptr <- Xptr(jstring)
      } else if(isString(jstring)) {
        ptr <- toJSONXptr(jstring)
      } else {
        stop("Invalid `jstring` argument.")
      }
      private[[".jsonString"]]$update(ptr)
      invisible(self)
    },

    #' @description Merge the reference JSON string (if it 
    #'   represents an object).
    #'
    #' @param jstring a JSON string, either a \code{jsonString} object or a 
    #'   string representing a JSON object
    #'
    #' @return This invisibly returns the updated reference JSON string.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jstring2 <- "{\"a\":[4,5,6],\"c\":\"goodbye\"}"
    #' jstring$merge(jstring2)
    #' jstring
    merge = function(jstring) {
      if(isJsonString(jstring)) {
        ptr <- Xptr(jstring)
      } else if(isString(jstring)) {
        ptr <- toJSONXptr(jstring)
      } else {
        stop("Invalid `jstring` argument.")
      }
      private[[".jsonString"]]$merge(ptr)
      invisible(self)
    },
    
    
    #' @description Apply a JSON patch to the reference JSON string (if it 
    #'   represents an array or an object).
    #'
    #' @param jspatch a JSON patch, a JSON string representing an array (see 
    #'   the link in details); it could be either a \code{jsonString} object or 
    #'   a string 
    #'
    #' @return A new \code{jsonString} object.
    #' 
    #' @details See \href{https://jsonpatch.com/}{jsonpatch.com}.
    #'
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jspatch <- "[
    #'   {\"op\": \"remove\", \"path\": \"/a\"},
    #'   {\"op\": \"replace\", \"path\": \"/b\", \"value\": null}
    #' ]"
    #' jstring$patch(jspatch)
    patch = function(jspatch) {
      if(isJsonString(jspatch)) {
        ptrpatch <- Xptr(jspatch)
      } else if(isString(jspatch)) {
        ptrpatch <- toJSONXptr(jspatch)
      } else {
        stop("Invalid `jspatch` argument.")
      }
      ptr <- private[[".jsonString"]]$patch(ptrpatch)
      jsonString$new(ptr)
    },
    
    #' @description Append an element to the reference JSON string (if it 
    #'   represents an array).
    #'
    #' @param jstring a JSON string, either a \code{jsonString} object or a 
    #'   string representing a JSON object
    #'
    #' @return This invisibly returns the updated reference JSON string.
    #' 
    #' @examples 
    #' jstring <- jsonString$new("[1, 2, 3, 4, 5]")
    #' jstring2 <- jsonString$new(
    #'   "{\"a\":[4,5,6],\"c\":\"goodbye\"}"
    #'  )
    #' jstring$push(jstring2)
    #' jstring
    push = function(jstring) {
      if(isJsonString(jstring)) {
        ptr <- Xptr(jstring)
      } else if(isString(jstring)) {
        ptr <- toJSONXptr(jstring)
      } else {
        stop("Invalid `jstring` argument.")
      }
      private[[".jsonString"]]$push(ptr)
      invisible(self)
    },
    
    #' @description Check the type of the reference JSON string.
    #'
    #' @param type the type to be checked, one of \code{"array"}, \code{"object"}, 
    #'   \code{"string"}, \code{"number"}, \code{"integer"}, \code{"float"}, 
    #'   \code{"null"}, \code{"boolean"}
    #'
    #' @return A Boolean value.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jstring$is("object")
    #' jstring$is("array")
    #' jstring <- jsonString$new("999")
    #' jstring$is("integer")
    #' jstring$is("number")
    #' jstring$is("float")
    is = function(type) {
      types <-
        c("array",
          "object",
          "number",
          "integer",
          "string",
          "float",
          "null",
          "boolean")
      type <- match.arg(type, types)
      private[[".jsonString"]]$is(type) 
    },
    
    #' @description Get the type of the reference JSON string.
    #'
    #' @return A character string indicating the type of the JSON string.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jstring$type()
    #' jstring <- jsonString$new("999")
    #' jstring$type()
    type = function() {
      private[[".jsonString"]]$type() 
    },
    
    #' @description Flatten the reference JSON string.
    #'
    #' @return A new \code{jsonString} object.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":{\"x\":\"hello\",\"y\":\"hi\"}}"
    #' )
    #' jstring$flatten()
    flatten = function() {
      ptr <- private[[".jsonString"]]$flatten()
      jsonString$new(ptr)
    },

    #' @description Unflatten the reference JSON string (if it is flattened).
    #'
    #' @return A new \code{jsonString} object.
    #' 
    #' @examples 
    #' folder <- system.file(package = "jsonStrings")
    #' files <- list.files(folder, recursive = TRUE)
    #' sizes <- file.size(file.path(folder, files))
    #' files <- sprintf('"%s"', paste0("/", files))
    #' string <- sprintf("{%s}", paste0(files, ":", sizes, collapse = ","))
    #' jstring <- jsonString$new(string)
    #' jstring$unflatten()
    unflatten = function() {
      ptr <- private[[".jsonString"]]$unflatten()
      jsonString$new(ptr)
    },
    
    #' @description Write the reference JSON string to a file.
    #' 
    #' @param filename name of the file
    #'
    #' @return Nothing.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' jsonfile <- tempfile(fileext = ".json")
    #' jstring$writeFile(jsonfile)
    #' cat(readLines(jsonfile), sep = "\n")
    #' jsonString$new(jsonfile)
    writeFile = function(filename) {
      stopifnot(isString(filename))
      private[[".jsonString"]]$writeFile(filename)
    },
    
    #' @description Copy the reference JSON string.
    #' 
    #' @return A new \code{jsonString} object.
    #' 
    #' @examples 
    #' jstring <- jsonString$new(
    #'   "{\"a\":[1,2,3],\"b\":\"hello\"}"
    #' )
    #' copy <- jstring$copy()
    #' copy$erase("b")
    #' jstring
    #' naive_copy <- jstring
    #' naive_copy$erase("b")
    #' jstring
    copy = function() {
      jsonString$new(Xptr(self))
    }
    
  )
  
)
