#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;

NumericVector computeRepeatsFactor(int n_predictors)
{
    NumericVector n_predictors_vec = rep(NumericVector::create(n_predictors), n_predictors);
    NumericVector seq_vec(n_predictors);
    for (int i = 0; i < n_predictors; i++)
        seq_vec[i] = i;
    return 1 / (choose(n_predictors_vec , seq_vec) * (n_predictors - seq_vec));
}

NumericVector createConditionalVarIndices(int n_predictors, int omitted_ind)
{
    NumericVector conditional_var_indices(n_predictors - 1);
    int c = 0;
    for (int i = 0; i < n_predictors; i++)
    {
        if (i != omitted_ind)
        {
            conditional_var_indices[c] = i;
            c++;
        }
    }
    return conditional_var_indices;
}

// Extract the kth combination from the combination matrix and express it in
// terms of the item indices.
NumericVector combinationIndices(NumericMatrix combinations, int k,
                                 NumericVector item_indices)
{
    int combination_size = combinations.nrow();
    NumericVector combination_indices(combination_size);
    for (int i = 0; i < combination_size; i++)
        combination_indices[i] = item_indices[combinations(i, k) - 1];
    return combination_indices;
}

// Append index to the end of combination_indices
NumericVector appendToCombination(NumericVector combination_indices, int index)
{
    int combination_size = combination_indices.size();
    NumericVector combination_indices_and_i(combination_size + 1);
    combination_indices_and_i[seq(0, combination_size - 1)] = combination_indices;
    combination_indices_and_i[combination_size] = index;
    return(combination_indices_and_i);
}

// Obtain the key corresponding to a combination.
// The key is used to index the rsquared_cache for a combination.
// The key is sum(2 ^ combination_indices).
int combinationKey(NumericVector combination_indices)
{
    int key = 0;
    int combination_size = combination_indices.size();
    for (int i = 0; i < combination_size; i++)
        key += pow(2, combination_indices[i]);
    return key;
}

// Compute the rsquared for a linear regresion with subset of predictors
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

    // v'M^1v, where v = corr_xy_subvector and M = corr_regressors_submatrix
    return (double)(corr_regressors_submatrix.llt().solve(corr_xy_subvector).dot(corr_xy_subvector));
}

// Initialize the R-squared cache with NAs
NumericVector initializeCache(int n_predictors)
{
    int cache_size = pow(2, n_predictors);
    NumericVector cache(cache_size);
    for (int i = 0; i < cache_size; i++)
        cache[i] = NA_REAL;
    return cache;
}

// corr_regressors is the correlation matrix of the regressors.
// corr_xy is a vector of correlations between the regressors and the outcome variable.
// combinations_list is a list of matrices from combn containing all the combinations
//                   of 1, ..., n_predictors out of n_predictors.
// [[Rcpp::export]]
NumericVector shapleyImportance(Eigen::MatrixXd & corr_regressors,
                                Eigen::VectorXd & corr_xy,
                                List combinations_list)
{
    int n_predictors = corr_regressors.cols();
    int key;
    NumericVector repeats_factor = computeRepeatsFactor(n_predictors);
    NumericVector importance(n_predictors);
    NumericVector rsquared_cache = initializeCache(n_predictors);

    for (int i = 0; i < n_predictors; i++) // i is index of regressor of interest
    {
        NumericVector conditional_var_indices = createConditionalVarIndices(n_predictors, i);
        double summed_rsquares = 0;

        // Deal with j = 0 case here separately (see for loop over j below)
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

        for (int j = 1; j < n_predictors; j++) // j is the combination size
        {
            NumericMatrix combinations = as<NumericMatrix>(combinations_list[j - 1]);
            int n_combinations = combinations.ncol();
            for (int k = 0; k < n_combinations; k++)
            {
                NumericVector combination_indices = combinationIndices(combinations, k,
                                                                       conditional_var_indices);
                NumericVector combination_indices_and_i = appendToCombination(combination_indices, i);

                // Get rsquared_conditionals
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

                // Get rsquared_conditionals_and_i
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

                // Increment by the difference in rsquared between linear
                // regressions using a subset of predictors that includes i
                // and a subset of the same predictors without i, weighted
                // by the repeats_factor for j.
                summed_rsquares += repeats_factor[j] * (rsquared_conditionals_and_i - rsquared_conditionals);
            }
        }

        importance[i] = summed_rsquares;
    }

    return importance;
}

