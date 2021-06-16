## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(wbpip)


# Load three different kinds of data
pry <- pipload::pip_load_cache("PRY", year = 2019, tool = "PC")
idn <- pipload::pip_load_cache("IDN", year = 2015, tool = "PC")
chn <- pipload::pip_load_cache("CHN", year = 2015, tool = "PC")


# Median for a country at the national level PRY
compute_median(pry, welfare = "welfare", weight = "weight")

# Median for a country at urban/rural data level
compute_median(idn, welfare = "welfare", weight = "weight")


# Median for a grouped data country

# Load auxiliary data for Group data calculations
# following the current structure in `pipdm` and the pipeline
gdm = pipload::pip_load_aux("gdm")
pop = pipload::pip_load_aux("pop")

# Get the corresponding means for the selected survey
chn_ids <- chn[, 
          lapply(.SD, unique), 
          .SDcols = c("survey_id", "pop_data_level")]

chn_info <- joyn::merge(x          = chn_ids, 
                        y          = gdm, 
                        by         = c("survey_id", "pop_data_level"), 
                        match_type = "1:m", 
                        keep       = "inner", 
                        yvars      = "survey_mean_lcu", 
                        reportvar = FALSE)

# In `pipdm` and the pipeline we need named mean
chn_mean        <- chn_info[, survey_mean_lcu]
names(chn_mean) <- chn_info[, pop_data_level]

compute_median(chn, 
               welfare = "welfare", 
               weight  = "weight", 
               mean    = chn_mean, 
               pop     = pop)


