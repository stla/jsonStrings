#ifndef _JSONSTRINGSHEADER_
#define _JSONSTRINGSHEADER_

#include "nlohmann/json_cran.h"
using json = nlohmann::json;
#include <Rcpp.h>

typedef Rcpp::XPtr<json> jsonXptr;

#endif
