## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(wbpip)

## ----user---------------------------------------------------------------------
welfare_mean    <- 51.56
ppp             <- 58.16
daily_povline   <- 1.9
monthly_povline <- daily_povline * 365 / 12

## ----micro--------------------------------------------------------------------
data("md_ABC_2000_income")

# Basic cleaning operation (remove missings, negative values, etc.)
micro <- wbpip:::md_clean_data(md_ABC_2000_income, 
                              welfare = "welfare", 
                              weight  = "weight")
micro <- micro$data
# Turn welfare vector to monthly values
# All computations assume monthly welfare values
micro$welfare <- micro$welfare / 12

## ----group--------------------------------------------------------------------
# Create grouped data (Type 1)
# http://iresearch.worldbank.org/povcalnet/PovCalculator.aspx
population <- c(0.0005,
                0.0032,
                0.014799999999999999,
                0.0443,
                0.0991,
                0.257,
                0.4385,
                0.5938,
                0.7089,
                1)

welfare <- c(5.824760527229386e-05,
             0.000604029410841011,
             0.0037949334793616948,
             0.013988878652244477,
             0.036992164583098786,
             0.12140708906131342,
             0.24531391873082081,
             0.37446670169288321,
             0.48753116241194566,
             1)

## ----microstats---------------------------------------------------------------

out <- compute_pip_stats(welfare           = micro$welfare,
                         povline           = monthly_povline,
                         population        = micro$weight,
                         requested_mean    = welfare_mean,
                         popshare          = NULL,
                         default_ppp       = ppp,
                         ppp               = NULL,
                         distribution_type = "micro")

out


## ----groupstats---------------------------------------------------------------

out <- compute_pip_stats(welfare           = welfare,
                         povline           = monthly_povline,
                         population        = population,
                         requested_mean    = welfare_mean,
                         popshare          = NULL,
                         default_ppp       = ppp,
                         ppp               = NULL,
                         distribution_type = "group")

out


