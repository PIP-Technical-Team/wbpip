# wbpip 0.1.4

* update webpage

# wbpip 0.1.3

## main changes
1. Key values: removed as individual arguments, instead supplied as one list using argument `key_values` created using `gd_lq_key_values`

2. Remove unnecessary `dd` argument from `gd_compute_mld_*` and `gd_compute_watts_*`

3. This is potentially too much information, but here it is. The function `check_curve_validity_lq()` is called in `DEV` in the following ways. It is used a) line 115 in gd_compute_dist_stats.R in `gd_estimate_dist_stats_lq()`, where the r argument is squared as the input; b) line 784 in gd_compute_pip_stats_lq.R in `gd_estimate_lq()` where the r argument is not squared; c) in line 137 of gd_compute_poverty_stats.R in function `gd_estimate_poverty_stats_lq()` where the r argument is not squared; d) line 103  of prod_gd_compute_pip_stats_lq().R in function `prod_gd_estimate_lq()` where the r argument is not squared. Therefore, out of the four times it is called it is only the first where the r argument is squared. The paper (Villasenor & Arnold, 1989) uses the square root for the `r` argument, similar (although with a denominator) to `r` calculated in `gd_lq_key_values`. However, the other three calls of `check_curve_validity_lq()` mentioned above do not calculate the square root for r prior to the function call , which is why they just use the `r` argument directly. What has been done here is to i) use the square root for `r` in `key_values` list; ii) but then square the `r` argument within `check_curve_validity_lq()`. However, `r` is never actually used anywhere in `check_curve_validity_lq()` except in `if(r 0) {return(...)}` as an early check. This requires a bit more investigation to see whether the `r` argument is necessary here. 

4. `{collapse}` - use functions directly, and use `max()` and `sum()` rather than `fmax()` and `fsum()` because base functions are more efficient.

5. quantile functions in `md_compute_quantiles.R` that are efficient, utilize `lorenz` argument, and wrap around `md_compute_lorenz()`

6. Improvements in efficiency are made to `gd_compute_mld_lq()` and `gd_compute_quantile_lq()`. However, there are no changes in relation to the bug fixes yet. 

7. Removed the functions in md_quantile_functions.R. First ensured that a corresponding function that served the same purpose could be found in md_compute_quantiles.R

8. increase efficiency in md_compute_polarization and correct the tests. The benchmarks in the tests were slightly off (at around the fourth decimal). When using the formula to calculate the benchmarks analytically, the tests pass.

## Some additional/general comments:
* Exported many additional functions.
* Some functions have been improved/vectorized. The new functions have taken the names of the old functions, and the old functions have the prefix "old_"

# wbpip 0.1.2

No relevant changes to the package in this version.

# wbpip 0.1.1

* Add new functions to compute the following indicators:  
  - Number of poor
  - Average shortfall
  - Total shortfall
  - Income gap ratio
  - Palma ratio
  - Top 10 / Bottom 10 ratio

# wbpip 0.1.0

## New features
* Updated SPL function
* Small optimization

# wbpip 0.0.4

## New features

## Enhancements

* Make local file path more robust in test-suite
* Fix non-monotonicity bugs for grouped distributions
* Remove `purrr` dependency
* add test for `sd_create_synth_vector`
* create a separate function (`weighted_average_poverty_stats`) for repetitive calculation in adjust poverty stats and add corresponding test case for it. 

## Bug fixes
* Fix duplicated values being returned for group data when poverty line when no
Lorenz fit was successful

# wbpip 0.0.3

Initial release used in the PIP soft-launch on February 9, 2022

## New features

## Enhancements

## Bug fixes

* Fix the selection of Lorenz curve for distributional statistics by adding ad-hoc function to compute correct SSE
* Change creation of synth vectors

# wbpip 0.0.2

# wbpip 0.0.1

# wbpip 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
