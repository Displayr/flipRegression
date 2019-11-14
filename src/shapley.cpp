#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;

NumericVector createConditionalVarIndices(int n_indep, int omitted_ind)
{
    NumericVector conditional_var_indices(n_indep - 1);
    int c = 0;
    for (int i = 0; i < n_indep; i++)
    {
        if (i != omitted_ind)
        {
            conditional_var_indices[c] = i;
            c++;
        }
    }
    return conditional_var_indices;
}

NumericVector combinationIndices(NumericMatrix combinations, int k, NumericVector items)
{
    int combination_size = combinations.nrow();
    NumericVector combination_indices(combination_size);
    for (int i = 0; i < combination_size; i++)
        combination_indices[i] = items[combinations(i, k)];
    return combination_indices;
}

NumericVector appendToCombination(NumericVector combination_indices, int index)
{
    int combination_size = combination_indices.size();
    NumericMatrix combination_indices_and_i(combination_size + 1);
    for (int i = 0; i < combination_size; i++)
        combination_indices_and_i[i] = combination_indices[i];
    combination_indices_and_i[combination_size] = index;
    return(combination_indices_and_i);
}

// still need to implement caching
double regressorSubsetRsquared(NumericVector combination_indices,
                               Eigen::MatrixXd & corr_regressors,
                               Eigen::VectorXd & corr_xy)
{
    int combination_size = combination_indices.size();
    Eigen::MatrixXd corr_regressors_submatrix = Eigen::MatrixXd::Zero(combination_size,
                                                                      combination_size);
    Eigen::VectorXd corr_xy_subvector(combination_size);

    for (int i = 0; i < combination_size; i++)
        for (int j = 0; j < combination_size; j++)
            corr_regressors_submatrix(i, j) = corr_regressors(combination_indices[i],
                                                              combination_indices[j]);
    for (int i = 0; i < combination_size; i++)
        corr_xy_subvector[i] = corr_xy[combination_indices[i]];

    // corr_regressors_submatrix.llt().solve(corr_xy_subvector)

    return 1;
}

// [[Rcpp::export]]
NumericVector shapleyImportance(Eigen::MatrixXd & corr_regressors,
                                Eigen::VectorXd & corr_xy,
                                List combinations_list,
                                NumericVector repeats_factor)
{
    int n_indep = corr_regressors.cols();
    NumericVector importance(n_indep);

    for (int i = 0; i < n_indep; i++) // i is index of regressor of interest
    {
        NumericVector conditional_var_indices = createConditionalVarIndices(n_indep, i);
        double summed_rsquares = 0;

        for (int j = 0; j < n_indep; j++) // j is the combination size
        {
            NumericMatrix combinations = combinations_list[j];
            int n_combinations = combinations.ncol();
            for (int k = 0; k < n_combinations; k++)
            {
                // NumericVector combination_indices = combinationIndices(combinations, k,
                //                                                        conditional_var_indices);
                // NumericVector combination_indices_and_i = appendToCombination(combination_indices, i);
                //
                // double r_squared_conditionals = regressorSubsetRsquared(combination_indices,
                //                                                         corr_regressors,
                //                                                         corr_xy);
                // double r_squared_conditionals_and_i = regressorSubsetRsquared(combination_indices_and_i,
                //                                                               corr_regressors, corr_xy);
                //
                // summed_rsquares += repeats_factor[j] * (r_squared_conditionals_and_i - r_squared_conditionals);
            }
        }

        importance[i] = summed_rsquares;
    }

    importance = as<NumericVector>(combinations_list[1]);
    return importance;
}

