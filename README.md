
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

You can install the released version of wbpip from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("wbpip")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/wbpip")
```

# Basic end-user workflows

Basic workflows are based on single data files (micro- or grouped-data).
The assumption for these workflows is that they can be satisfied by
feeding a single micro- / grouped-data file to different functions.

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

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
