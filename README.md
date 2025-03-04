[![](https://travis-ci.org/Displayr/flipRegression.svg?branch=master)](https://travis-ci.org/Displayr/flipRegression/)
[![Coverage Status](https://coveralls.io/repos/github/Displayr/flipRegression/badge.svg?branch=master)](https://coveralls.io/github/Displayr/flipRegression?branch=master)
# flipRegression

Estimates standard regression models

## Installation

To install from GitHub:
```
require(devtools)
install_github("Displayr/flipTime", dependencies = NA, force = TRUE)
install_github("Displayr/flipFormat", dependencies = NA, force = TRUE)
install_github("Displayr/flipStatistics", dependencies = NA, force = TRUE)
install_version("RcppEigen", version = "0.3.3.9.4", repos = "http://cran.us.r-project.org")
install_github("Displayr/flipRegression", dependencies = NA, force = TRUE)
```
IMPORTANT: when you execute the last code above, you will be asked what dependencies you would like to update. You MUST SELECT “None”. Otherwise, it will update RcppEigen to version higher than ‘0.3.3.9.4’, and the installation process will fail.

If you have not set up a GitHub Personal Access Token, you will likely need to do so to avoid 
GitHub rate limits, which will manifest as 403 errors when downloading packages via
`install_github`. Please see the documentation in the `usethis` package or see the 
instructions [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) and [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).

If you are using Windows, you will need to have a version of Rtools installed that matches your
version of R in order to build packages from source. Rtools can be downloaded from
[here](https://cran.r-project.org/bin/windows/Rtools/).

Specifying `dependencies = NA` in `install_github` will not install packages listed
in `Suggests` in the `DESCRIPTION` file (some of which may be proprietary and unavailable for download).

## Submitting a bug report

If you encounter a problem using the package, please open an [issue](https://github.com/Displayr/flipRegression/issues). To achieve a resolution as quickly as possible, please include a minimal, reproducible example of the bug, along with the exact error message or output you receive and the behavior you expect. Including the output of `sessionInfo()` in R can be helpful to reproduce the issue. Please see this [FAQ](https://community.rstudio.com/t/faq-whats-a-reproducible-example-reprex-and-how-do-i-create-one/5219), which has a number of useful tips on creating great reproducible examples. 

[![Displayr logo](https://mwmclean.github.io/img/logo-header.png)](https://www.displayr.com)
