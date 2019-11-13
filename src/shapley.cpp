#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;

NumericVector createConditionalVarInd(int n_indep, int omitted_ind)
{
    NumericVector conditional_var_ind(n_indep - 1);
    int c = 0;
    for (int i = 0; i < n_indep; i++)
    {
        if (i != omitted_ind)
        {
            conditional_var_ind[c] = i;
            c++;
        }
    }
    return conditional_var_ind;
}

// still need to implement caching
double get_rsquared_of_regressor_subset(NumericVector combination_ind,
                                        Eigen::MatrixXd & corr_regressors,
                                        Eigen::VectorXd & corr_xy)
{
    return 1;
}

// [[Rcpp::export]]
NumericVector shapleyImportance(Eigen::MatrixXd & corr_regressors,
                                Eigen::VectorXd & corr_xy,
                                List combinations,
                                NumericVector repeats_factor)
{
    int n_indep = corr_regressors.cols();
    NumericVector importance(n_indep);

    for (int i = 0; i < n_indep; i++) // i is index of regressor of interest
    {
        NumericVector conditional_var_ind = createConditionalVarInd(n_indep, i);
        double summed_rsquares = 0;

        for (int j = 0; j < n_indep; j++) // j is number of conditional variables in each combination
        {


            double r_squared_conditionals_and_x = get_rsquared_of_regressor_subset();
            double r_squared_conditionals = get_rsquared_of_regressor_subset();
            summed_rsquares += repeats_factor[j] * (r_squared_conditionals_and_x - r_squared_conditionals);
        }

        importance[i] = summed_rsquares;
    }

    importance = as<NumericVector>(combinations[1]);
    return importance;
}

