isScalar <- function(x){
  is.null(dim(x)) && length(x) == 1L && !is.na(x)
}

isBoolean <- function(x){
  is.logical(x) && isScalar(x)
}

isString <- function(x){
  is.character(x) && isScalar(x)
}

isPositiveInteger <- function(x){
  is.numeric(x) && isScalar(x) && floor(x) == x
}

isJsonString <- function(x){
  inherits(x, "jsonString")
}
