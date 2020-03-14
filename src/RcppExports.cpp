// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// geomcpp_convert_wkt
CharacterVector geomcpp_convert_wkt(SEXP data);
RcppExport SEXP _geom_geomcpp_convert_wkt(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(geomcpp_convert_wkt(data));
    return rcpp_result_gen;
END_RCPP
}
// geomcpp_convert_wkb
List geomcpp_convert_wkb(SEXP data);
RcppExport SEXP _geom_geomcpp_convert_wkb(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(geomcpp_convert_wkb(data));
    return rcpp_result_gen;
END_RCPP
}
// geos_wkt_is_parseable
LogicalVector geos_wkt_is_parseable(CharacterVector wkt);
RcppExport SEXP _geom_geos_wkt_is_parseable(SEXP wktSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    rcpp_result_gen = Rcpp::wrap(geos_wkt_is_parseable(wkt));
    return rcpp_result_gen;
END_RCPP
}
// geos_wkb_is_parseable
LogicalVector geos_wkb_is_parseable(List wkb);
RcppExport SEXP _geom_geos_wkb_is_parseable(SEXP wkbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type wkb(wkbSEXP);
    rcpp_result_gen = Rcpp::wrap(geos_wkb_is_parseable(wkb));
    return rcpp_result_gen;
END_RCPP
}
// geos_version_impl
std::string geos_version_impl();
RcppExport SEXP _geom_geos_version_impl() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(geos_version_impl());
    return rcpp_result_gen;
END_RCPP
}
// geos_test_throw_error
void geos_test_throw_error();
RcppExport SEXP _geom_geos_test_throw_error() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    geos_test_throw_error();
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_geom_geomcpp_convert_wkt", (DL_FUNC) &_geom_geomcpp_convert_wkt, 1},
    {"_geom_geomcpp_convert_wkb", (DL_FUNC) &_geom_geomcpp_convert_wkb, 1},
    {"_geom_geos_wkt_is_parseable", (DL_FUNC) &_geom_geos_wkt_is_parseable, 1},
    {"_geom_geos_wkb_is_parseable", (DL_FUNC) &_geom_geos_wkb_is_parseable, 1},
    {"_geom_geos_version_impl", (DL_FUNC) &_geom_geos_version_impl, 0},
    {"_geom_geos_test_throw_error", (DL_FUNC) &_geom_geos_test_throw_error, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_geom(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
