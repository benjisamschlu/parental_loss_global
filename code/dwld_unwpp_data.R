
##------------- DOWNLOAD UNWPP DATA FOR PARENTAL LOSS GLOBAL -------------------
## 
## 
##  Author: Benjamin Schlüter
##  Date: August 2023
##
##
## -----------------------------------------------------------------------------


rm(list = ls())

## Install/load packages
packages <- c("tidyverse", "here")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
source(here("code", "get_UNWPP_inputs_ben.R"))

my_startyr <- 1950
my_endyr <- 2022
variant <- "Median"

# Find the codes for countries
base_url <- 'https://population.un.org/dataportalapi/api/v1'
target <- paste0(base_url, '/locations?sort=id&format=csv')

# Get country names
countries <- read.csv(target, sep='|', skip=1) |> 
    ## Filter non country
    filter(!is.na(Latitude),
           Name != "Holy See") |> 
    pull(Name)


## Too many countries lead the dwld from API to fail
## Doing one country at a time seems to be quicker/more stable
## but takes +- 2 hours
list.df <- lapply(countries, 
                  function(c) get_UNWPP_inputs_ben(c, my_startyr, my_endyr, variant))

## Mortality and fertility data
df.vital.rates <- do.call("rbind", sapply(list.df, "[[", 1, simplify = FALSE))

## Birth data
df.birth <- do.call("rbind", sapply(list.df, "[[", 2, simplify = FALSE))

## Store data
saveRDS(
    df.vital.rates,
    here("data_raw", "data_rates_unwpp.rda")
    )

saveRDS(
    df.birth,
    here("data_raw", "data_birth_unwpp.rda")
    )

