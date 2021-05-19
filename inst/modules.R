library(jsonStrings)
jsonModule <- Rcpp::Module("jsonModule", "jsonStrings")
json <- jsonModule$JSON
jsonPointer <- new(json, "{\"a\":2}")$jsonPointer()
jsonptrModule <- Rcpp::Module("jsonptrModule", "jsonStrings")
jsonptr <- jsonptrModule$JSONPTR
x <- new(jsonptr, jsonPointer)
x$getkey("a")
