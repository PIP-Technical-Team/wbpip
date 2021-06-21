
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wbpip

<!-- badges: start -->
<!-- badges: end -->

The goal of `{wbpip}` is to twofold. First it contains the
methodological formulations currently used by the World Bank to estimate
and monitor global poverty and to calculate cross-country and time
comparable socioeconomic measures. Second, it provides a suit of
user-friendly functions that allows anybody to estimate poverty and
inequality measures with their own data, using the methodological
approach of the World Bank.

## Installation

<span style="color:red">*Not available as of now (2021-06-21)*</span>

<!-- You can install the released version of wbpip from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("wbpip") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->
<!-- ``` r -->
<!-- # install.packages("devtools") -->
<!-- devtools::install_github("PIP-Technical-Team/wbpip") -->
<!-- ``` -->

# Basic end-user workflows

Basic workflows are based on single data files (micro, grouped, bin, or
synthetic data). The assumption for these workflows is that they can be
satisfied by feeding a data file to different functions.

Functions in `{wbpip}` are divided in two main groups: computational
functions and end-user functions. Computational functions are the
internal functions of the package that do the calculations. End-user
functions are wrappers of the computational functions that fit well in
data science workflows such as the tidyverse.

Most computational functions follow this convention: dd\_compute\_ss(),
where **dd** stands for data type and **ss** stands from statistic to
compute. Data type could **md** for microdata or **gd** from grouped
data. Even though there are others type of data such as bin or synthetic
data, the computational procedure is the same as of micro data. The
statistic to compute is self-explanatory. It could be poverty, gini,
mean log deviation (mld), among others. Other computational functions
have the prefix `prod_`, which refers to production. These functions
make use of lower-level computational functions to create production
tables used in the Poverty and Inequality Platform.

End-user functions are prefixed with `get_`. These functions are
intended to be used as part of any data science workflow using pipes
like the tidyverse. This is why, the first argument of these functions
is the data frame with the data. For convenience, some functions return
one single stat, whereas others return a compendium of related stats:

## Single stats

-   `get_poverty_rate(df, povline)`
    `compute_poverty_rate(welfare, weight, povline)`
-   `get_number_poor(df, povline, pop)`
-   `get_poverty_gap(df, povline)`
-   `get_poverty_severity(df, povline)`
-   `get_watts_index(df, povline)`
-   `get_mean(df)`
-   `get_median(df)`
-   `get_gini(df)`
-   `get_polarization(df)`
-   `get_mld(df)`
-   `get_quantile(n = 10)`

# Multiple stats

-   `get_distributional_stats()`
-   `get_poverty_stats()`

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wbpip)
## basic example code
```
