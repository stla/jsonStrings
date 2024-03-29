// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/jsonStrings.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// toJSONXptr
jsonXptr toJSONXptr(const std::string& string);
RcppExport SEXP _jsonStrings_toJSONXptr(SEXP stringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type string(stringSEXP);
    rcpp_result_gen = Rcpp::wrap(toJSONXptr(string));
    return rcpp_result_gen;
END_RCPP
}
// read_json
jsonXptr read_json(std::string filename);
RcppExport SEXP _jsonStrings_read_json(SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    rcpp_result_gen = Rcpp::wrap(read_json(filename));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_class_JsonString();

static const R_CallMethodDef CallEntries[] = {
    {"_jsonStrings_toJSONXptr", (DL_FUNC) &_jsonStrings_toJSONXptr, 1},
    {"_jsonStrings_read_json", (DL_FUNC) &_jsonStrings_read_json, 1},
    {"_rcpp_module_boot_class_JsonString", (DL_FUNC) &_rcpp_module_boot_class_JsonString, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_jsonStrings(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
