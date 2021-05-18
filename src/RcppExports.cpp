// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// jsonArrayAt
std::string jsonArrayAt(std::string jsonString, int i);
RcppExport SEXP _jsonStrings_jsonArrayAt(SEXP jsonStringSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type jsonString(jsonStringSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(jsonArrayAt(jsonString, i));
    return rcpp_result_gen;
END_RCPP
}
// jsonObjectAt
std::string jsonObjectAt(std::string jsonString, std::string key);
RcppExport SEXP _jsonStrings_jsonObjectAt(SEXP jsonStringSEXP, SEXP keySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type jsonString(jsonStringSEXP);
    Rcpp::traits::input_parameter< std::string >::type key(keySEXP);
    rcpp_result_gen = Rcpp::wrap(jsonObjectAt(jsonString, key));
    return rcpp_result_gen;
END_RCPP
}
// cpp_jsonAt
std::string cpp_jsonAt(std::string jsonString, std::vector<std::string> keys, Rcpp::IntegerVector indices, std::vector<bool> isIndex);
RcppExport SEXP _jsonStrings_cpp_jsonAt(SEXP jsonStringSEXP, SEXP keysSEXP, SEXP indicesSEXP, SEXP isIndexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type jsonString(jsonStringSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< std::vector<bool> >::type isIndex(isIndexSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_jsonAt(jsonString, keys, indices, isIndex));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_jsonStrings_jsonArrayAt", (DL_FUNC) &_jsonStrings_jsonArrayAt, 2},
    {"_jsonStrings_jsonObjectAt", (DL_FUNC) &_jsonStrings_jsonObjectAt, 2},
    {"_jsonStrings_cpp_jsonAt", (DL_FUNC) &_jsonStrings_cpp_jsonAt, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_jsonStrings(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
