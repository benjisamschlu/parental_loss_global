
##------------- GENERATE OUTPUTS FROM DYNAMICS OF PARENT -----------------------
## 
## 
##
##  Notes:
##  1. 
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



## Functions -------------------------------------------------------------------

## Script containing the functions computing
## the main elements required to run the kin dynamics
source(here("code", "fct_kmm.R"))



## Load data -------------------------------------------------------------------

df = readRDS(here("data", "df_rates_example_countries.rda"))
df_birth = readRDS(here("data", "df_birth_example_countries.rda"))
causes_names = readRDS(here("data", "gbd-causes.rds"))
d_x_t = readRDS(here("data_private", "d_x_t.rda"))



## Key dimensions for code ---------------------------------------------------------

## Consider 21 different "causes" of death
alpha <- sum(grepl("cause", names(df)))

## Ages 
ages <- unique(df$age)
omega <- length(ages)

## Years (GBD stops in 2019)
years <- 1950:2019
n.years <- length(years)

# Sex
sex <- c("mother", "father")
n.sex <- length(sex)

## Example countries
countries <- unique(df$ctry)
n.countries <- length(countries)

## Radix in UN life table
radix <- 1e5

## Rows alive/dead in d_x_t
rows.stage <- list(
    "alive" = 1:(2*omega),
    "dead" = ((2*omega)+1):nrow(d_x_t)
)

## rows female/male alive in d_x_t
rows.alive.sex <- list(
    "female" = 1:omega,
    "male" = (omega + 1):(2 * omega)
    )

## rows female/male dead in d_x_t
rows.dead.sex <- list(
    "female" = ((2*omega)+1):((((2*omega)+1) + (alpha*omega))-1),
    "male" = ((((2*omega)+1) + (alpha*omega))):nrow(d_x_t)
)



## Parental loss at individual level -------------------------------------------

## Container
nber.dth <- array(NA, 
                  dim = c(alpha, 
                          omega,
                          n.sex,
                          n.years,
                          n.countries),
                  dimnames = list("cause" = causes_names,
                                  "age" = ages,
                                  "sex" = sex,
                                  "year" = years,
                                  "ctry" = countries))

## Sum over ages of parents death by cause (Eq. 16)
for (s in 1:n.sex) {
    
    ## Matrix allowing to sum over all parent's ages
    ## by cause
    sum.vec <- (t(rep(1, omega)) %x% diag(alpha))
    
    for (c in countries) {
        for (y in 1:n.years) {
            for (x in 1:omega) {
                
                ## Sum over all parent's age by cause
                nber.dth[ , x, s, y, c] <- sum.vec %*% d_x_t[rows.dead.sex[[s]], x, y, c]
            }
        }
    }
}

## Clean output
df.nber.dth <- as.data.frame.table(nber.dth) %>%
    rename("N" = Freq) %>%
    mutate(
        year = as.character(year) %>% as.numeric,
           age = as.character(age) %>% as.numeric
        ) |> 
    ## Period perspective: sum of death of mother/father over ages != 1
    ## --> Rescaling
    ## This decision should be discussed
    ## but if not rescaled, \sum probability != 1
    group_by(
        sex, year, ctry
        ) |> 
    mutate(
        totdeath = sum(N)
        ) |> 
    ungroup() |> 
    mutate(
        ## Rescale such that death mother/father
        ## over ages = 1
        N = N/totdeath
        )
    


## Number/share of children affected by parental loss --------------------------

## Parental death
df.results <- df.nber.dth |> 
    dplyr::select(
        ctry, year, cause, age, sex, N
        ) |> 
    mutate(
        ## Compute prob of bereavement 
        ## Weird math expression but kept for coherence with 
        ## prob of at least one parent dies
        prob_ber = 1 - (1 - (N))
        ) |> 
    ## Join with nber of surviving children as they ages
    left_join(
        ## Add proportion of surviving children (lx)
        df |> 
            filter(
                sex == "Both sexes"
                ) |> 
            mutate(
                ## Compute proportion surviving
                lx = lx/radix
                ) |> 
            ## Add total birth
            left_join(
                df_birth |> 
                    filter(
                        sex == "Both sexes"
                        ),
                by = c("ctry", "year", "sex")
            ) |> 
            mutate(
                ## Number of born children surviving over ages
                surv.children = lx * n_birth
            ) |> 
            dplyr::select(
                ctry, year, age, surv.children, n_birth
                ),
        by = c("ctry", "year", "age")
    ) |> 
    mutate(
        ## Nber of children losing a parent (clp)
        clp = prob_ber * surv.children,
        ## Share of born children losing a parent
        perc = (clp/n_birth)*100
    )

## Save outputs
saveRDS(df.results,
        here("data", "df_results.rda"))
