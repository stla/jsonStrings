library(jsonStrings)
mod_json <- Rcpp::Module("json_module", "jsonStrings")
jj <- mod_json$JSON
js <- new(jj, "{\"a\":2}")
jsptr <- js$jsonPTR()
mod_jsonptr <- Rcpp::Module("jsonptr_module", "jsonStrings")
jjptr <- mod_jsonptr$JSONPTR
x <- new(jjptr, jsptr)
x$getkey("a")
