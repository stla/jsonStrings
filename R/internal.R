checkPath <- function(path){
  all(vapply(path, FUN = function(x){
    length(x) == 1L && (is.numeric(x) || is.character(x))
  }, FUN.VALUE = logical(1L)))
}