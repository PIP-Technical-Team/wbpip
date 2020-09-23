# Basic end-user workflows
Basic workflows are based on single data files (micro- or grouped-data).
The assumption for these workflows is that they can be satisfied by feeding a 
single micro- / grouped-data file to different functions.

## Single stats
* `get_poverty_rate(df)`
* `get_number_poor(df, pop)`
* `get_mean(df)`
* `get_poverty_gap(df)`
* `get_poverty_severity(df)`
* `get_watts_index(df)`
* `get_gini(df)`
* `get_median(df)`
* `get_mld(df)`
* `get_quantile(n = 10)`

# Multiple stats
* `get_distributional_stats()`
* `get_poverty_stats()`
