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
        combination_indices[i] = items[combinations(i, k) - 1];
    return combination_indices;
}

NumericVector appendToCombination(NumericVector combination_indices, int index)
{
    int combination_size = combination_indices.size();
    NumericVector combination_indices_and_i(combination_size + 1);
    for (int i = 0; i < combination_size; i++)
        combination_indices_and_i[i] = combination_indices[i];
    combination_indices_and_i[combination_size] = index;
    return(combination_indices_and_i);
}

int combinationKey(NumericVector combination_indices)
{
    int key;
    int combination_size = combination_indices.size();
    for (int i = 0; i < combination_size; i++)
        key += pow(2, combination_indices[i]);
    return key;
}

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

    double temp = (double)corr_regressors_submatrix.llt().solve(corr_xy_subvector).dot(corr_xy_subvector);
    return 0;
    // return corr_regressors_submatrix.llt().solve(corr_xy_subvector).dot(corr_xy_subvector);
}

NumericVector initializeCache(int n_indep)
{
    int cache_size = pow(2, n_indep);
    NumericVector cache(cache_size);
    for (int i = 0; i < cache_size; i++)
        cache[i] = NA_REAL;
    return cache;
}

// [[Rcpp::export]]
NumericVector shapleyImportance(Eigen::MatrixXd & corr_regressors,
                                Eigen::VectorXd & corr_xy,
                                List combinations_list,
                                NumericVector repeats_factor)
{
    int n_indep = corr_regressors.cols();
    int key;
    NumericVector importance(n_indep);

    NumericVector rsquared_cache = initializeCache(n_indep);

    for (int i = 0; i < n_indep; i++) // i is index of regressor of interest
    {
        NumericVector conditional_var_indices = createConditionalVarIndices(n_indep, i);
        double summed_rsquares = 0;

        NumericVector combination_i = NumericVector::create(i);
        key = combinationKey(combination_i);
        double rsquared_i;
        if (NumericVector::is_na(rsquared_cache[key]))
        {
            rsquared_i = regressorSubsetRsquared(combination_i,
                                                 corr_regressors,
                                                 corr_xy);
            rsquared_cache[key] = rsquared_i;
        }
        else
            rsquared_i = rsquared_cache[key];

        summed_rsquares += repeats_factor[0] * rsquared_i;

        for (int j = 1; j < n_indep; j++) // j is the combination size
        {
            NumericMatrix combinations = as<NumericMatrix>(combinations_list[j - 1]);
            int n_combinations = combinations.ncol();
            for (int k = 0; k < n_combinations; k++)
            {
                NumericVector combination_indices = combinationIndices(combinations, k,
                                                                       conditional_var_indices);
                NumericVector combination_indices_and_i = appendToCombination(combination_indices, i);

                key = combinationKey(combination_indices);
                double rsquared_conditionals;
                if (NumericVector::is_na(rsquared_cache[key]))
                {
                    rsquared_conditionals = regressorSubsetRsquared(combination_indices,
                                                                    corr_regressors,
                                                                    corr_xy);
                    rsquared_cache[key] = rsquared_conditionals;
                }
                else
                    rsquared_conditionals = rsquared_cache[key];

                key = combinationKey(combination_indices_and_i);
                double rsquared_conditionals_and_i;
                if (NumericVector::is_na(rsquared_cache[key]))
                {
                    rsquared_conditionals_and_i = regressorSubsetRsquared(combination_indices_and_i,
                                                                          corr_regressors,
                                                                          corr_xy);
                    rsquared_cache[key] = rsquared_conditionals_and_i;
                }
                else
                    rsquared_conditionals_and_i = rsquared_cache[key];

                summed_rsquares += repeats_factor[j] * (rsquared_conditionals_and_i - rsquared_conditionals);
            }
        }

        // importance[i] = summed_rsquares;
        importance[i] = 1;
    }

    return importance;
}

