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
