## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(wbpip)

## -----------------------------------------------------------------------------
# Load datasets
data('md_ABC_2000_income')
data('md_ABC_2010_income')

## -----------------------------------------------------------------------------
# Clean datasets 
dl <- list(md_ABC_2000_income, md_ABC_2010_income)
dl <- lapply(dl, function(df) {
  wbpip:::md_clean_data(df, welfare = 'welfare', weight = 'weight')$data
})

## -----------------------------------------------------------------------------
# Calculate survey mean (in local currency units)
svy_mean_lcu <- vapply(dl, FUN.VALUE = numeric(1), function(df) {
  weighted.mean(df$welfare, df$weight)
})
print(svy_mean_lcu)

## -----------------------------------------------------------------------------
# Convert to daily values 
svy_mean_lcu <- svy_mean_lcu / 365 

## -----------------------------------------------------------------------------
svy_mean_ppp <- deflate_welfare_mean(
  welfare_mean = svy_mean_lcu, 
  ppp = 1081, 
  cpi = c(0.6, 0.9))
print(svy_mean_ppp)

## -----------------------------------------------------------------------------
# Predict welfare means for the request year
pred_mean_ppp <- predict_request_year_mean(
  survey_year = c(2000, 2010), 
  survey_mean = svy_mean_ppp,
  proxy = list(
    value0 = 1200, value1 = 1900, req_value = 1500
  )
)
print(pred_mean_ppp)

## -----------------------------------------------------------------------------
# Calculate poverty statitics for 2005 
res <- fill_gaps(
  request_year = 2005,
  data = list(df0 = dl[[1]], df1 = dl[[2]]),
  predicted_request_mean = pred_mean_ppp,
  survey_year = c(2000, 2010), 
  default_ppp = c(1, 1),  
  distribution_type = 'micro',
  poverty_line = 1.9) 

## -----------------------------------------------------------------------------
str(res)

