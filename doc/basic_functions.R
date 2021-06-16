## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(wbpip)

## -----------------------------------------------------------------------------
data("md_ABC_2000_income")

wbpip:::md_compute_gini(welfare = md_ABC_2000_income$welfare, 
                weight = md_ABC_2000_income$weight)

