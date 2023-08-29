
##------------- JOIN UNWPP AND GBD DATA ----------------------------------------
## 
##
##
##  Notes:
##  1. GBD data has been created by Antonino Polizzi (pclm!)
## 
##  Author: Benjamin Schlüter
##  Date: August 2023
##
##
## -----------------------------------------------------------------------------


rm(list=ls())

## Install/load packages
packages <- c("tidyverse", "here")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Load data -------------------------------------------------------------------

df_rates_un = readRDS(here("data_raw", "data_rates_unwpp.rda"))
df_birth_un = readRDS(here("data_raw", "data_birth_unwpp.rda"))
df_gbd = readRDS(here("data_raw", "gbd-harmonized.RDS"))
df_causes = readRDS(here("data_raw", "gbd-causes.RDS"))



## Join data sets---------------------------------------------------------------

## 204 and 231 countries in GBD and UN
length(unique(df_rates_un$ctry))
length(unique(df_gbd$cntry))

## Which GBD countries are not found in UN countries
unique(df_gbd$cntry)[!(unique(df_gbd$cntry) %in% unique(df_rates_un$ctry))]

## Rename UN countries to match the missing GBD
df_rates_un = df_rates_un |> 
    mutate(
        ctry = case_when(
            ctry == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
            ctry == "Dem. Rep. of the Congo" ~ "Democratic Republic of the Congo",
            ctry == "Dem. People's Rep. of Korea" ~ "Dem. People's Republic of Korea",
            ctry == "Lao People's Dem. Republic" ~ "Lao People's Democratic Republic",
            ctry == "TÃ¼rkiye" ~ "Turkiye",
            TRUE ~ ctry
        ))
df_birth_un = df_birth_un |> 
    mutate(
        ctry = case_when(
            ctry == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
            ctry == "Dem. Rep. of the Congo" ~ "Democratic Republic of the Congo",
            ctry == "Dem. People's Rep. of Korea" ~ "Dem. People's Republic of Korea",
            ctry == "Lao People's Dem. Republic" ~ "Lao People's Democratic Republic",
            ctry == "TÃ¼rkiye" ~ "Turkiye",
            TRUE ~ ctry
        ))

## Join dfs
df_joined = df_rates_un |> 
    left_join(df_gbd |> 
                  rename("ctry" = cntry),
              by = c("ctry", "year", "age", "sex")) |> 
    ## Age order matter for kmm fct
    arrange(ctry, year, sex, age)

## Save df
saveRDS(
    df_joined,
    here("data_private", "df_joined.rda")
    )

saveRDS(
    df_birth_un,
    here("data_private", "df_birth.rda")
)


## Select a sample of countries for data size (Github) & computation time ------

countries <- c("United States of America", "Senegal", "Germany", 
               "Mexico", "Afghanistan", "China")

df_ex_countries <- df_joined %>% 
    filter(ctry %in% countries)

df_birth_ex_countries <- df_birth_un %>% 
    filter(ctry %in% countries)



saveRDS(
    df_ex_countries,
    here("data", "df_rates_example_countries.rda")
)

saveRDS(
    df_birth_ex_countries,
    here("data", "df_birth_example_countries.rda")
)





