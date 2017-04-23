// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// numUnpack
IntegerMatrix numUnpack(IntegerVector pack);
RcppExport SEXP GGIR_numUnpack(SEXP packSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type pack(packSEXP);
    rcpp_result_gen = Rcpp::wrap(numUnpack(pack));
    return rcpp_result_gen;
END_RCPP
}
// resample
NumericMatrix resample(NumericMatrix raw, NumericVector rawTime, NumericVector time, int stop);
RcppExport SEXP GGIR_resample(SEXP rawSEXP, SEXP rawTimeSEXP, SEXP timeSEXP, SEXP stopSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type raw(rawSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rawTime(rawTimeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type time(timeSEXP);
    Rcpp::traits::input_parameter< int >::type stop(stopSEXP);
    rcpp_result_gen = Rcpp::wrap(resample(raw, rawTime, time, stop));
    return rcpp_result_gen;
END_RCPP
}
