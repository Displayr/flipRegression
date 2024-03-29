// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// shapleyImportance
NumericVector shapleyImportance(Eigen::MatrixXd& corr_regressors, Eigen::VectorXd& corr_xy, List combinations_list);
RcppExport SEXP _flipRegression_shapleyImportance(SEXP corr_regressorsSEXP, SEXP corr_xySEXP, SEXP combinations_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd& >::type corr_regressors(corr_regressorsSEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd& >::type corr_xy(corr_xySEXP);
    Rcpp::traits::input_parameter< List >::type combinations_list(combinations_listSEXP);
    rcpp_result_gen = Rcpp::wrap(shapleyImportance(corr_regressors, corr_xy, combinations_list));
    return rcpp_result_gen;
END_RCPP
}
