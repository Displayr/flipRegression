// This file was automatically generated by 'Kmisc::registerFunctions()'

#include <R.h>
#include <Rinternals.h>

#include <R_ext/Rdynload.h>

SEXP _flipRegression_shapleyImportance(SEXP corr_regressorsSEXP, SEXP corr_xySEXP, SEXP combinationsSEXP);

R_CallMethodDef callMethods[]  = {
  {"_flipRegression_shapleyImportance", (DL_FUNC) &_flipRegression_shapleyImportance, 3},
  {NULL, NULL, 0}
};

void R_init_flipRegression(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

