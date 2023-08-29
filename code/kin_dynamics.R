
##------------- RUN THE KIN DYNAMICS OF PARENT ---------------------------------
## 
## 
##
##  Notes:
##  1. This code involves big arrays (alive/dead by cause i, age, sex, year, 
##     country), it thus takes time to run (in actual state, +-30min)
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



## Key dimensions for code -----------------------------------------------------

## Consider 21 different "causes" of death
alpha <- sum(grepl("cause", names(df)))

## Ages 
ages <- unique(df$age)
omega <- length(ages)

## Years (GBD stops in 2019)
years <- 1950:2019
n.years <- length(years)

## Sex
sex <- c("mother", "father")
n.sex <- length(sex)

## Example countries
countries <- unique(df$ctry)
n.countries <- length(countries)



## Container for dynamics of parents of Focal ----------------------------------

## Age distribution of live/dead parents
d_x_t <- array(NA,
               dim = c(2*(omega + alpha*omega), 
                       omega, 
                       n.years, 
                       n.countries),
               dimnames = list("parents" = 1:(2*(omega + alpha*omega)),
                               "focal_age" = ages,
                               "year" = years,
                               "ctry" = countries))



## Run the dynamics ------------------------------------------------------------

## These lists allow to select part of Parent vector 

## alive/dead while looping
rows <- list("alive" = 1:(2*omega),
             "dead" = ((2*omega)+1):nrow(d_x_t)
)

## rows female/male dead in d_x_t
rows.dead.sex <- list(
    "female" = ((2*omega)+1):((((2*omega)+1) + (alpha*omega))-1),
    "male" = ((((2*omega)+1) + (alpha*omega))):nrow(d_x_t)
)

## Boundary conditions at t=0 (year=1950)
## -> Assume vital rate as if stable population
## because Focal is a randomly drawn individual
## in the population but he/she could be any age.
## d(0,0) consists of the dist. of the ages 
## of the parents of offspring  in the year 1950
## at Focal's birth.

## START LOOP ON COUNTRY
for (c in countries) {
    
    ## Iteration stage
    cat("Working on ", c, "\n")
    
    ## Boundary conditions
    ## Distribution of parents' age of offspring
    d_x_t[rows[["alive"]], 1, 1, c] <- get_pi(ages, df, df, min(years), c)
    ## Assumption of no dead parents at birth of Focal
    d_x_t[rows[["dead"]], 1, 1, c] <- rep(0, (2*(alpha*omega)))
    
    ## Get d(x,0) for all x assuming stable pop in 
    ## the year 1950 (Focal could be any age in the
    ## year 1950)
    U_tilde <- get_U_tilde(df, min(years), c, alpha = alpha, cum = F)
    
    for (x in 2:omega) {
        
        d_x_t[, x, 1, c] <- U_tilde %*% d_x_t[, (x-1), 1, c]
    }
    
    ## Create the dynamics by looping over the years
    
    ## START LOOP ON YEARS
    
    for (y in 2:n.years) {
        
        y.n <- years[y]
        
        ## Iteration stage
        if ((y.n %% 10) == 0) {
            cat("Starting year", y.n, "\n")
        }
        
        ## Boundary conditions
        ## Focal birth:
        ## Distribution of parents' age at offspring
        d_x_t[rows[["alive"]], 1, y, c] <- get_pi(ages, df, df, y.n, c)
        ## Assumption of no dead parents at birth of Focal
        d_x_t[rows[["dead"]], 1, y, c] <- rep(0, (2*(alpha*omega)))
        
        ## Dynamics of parents
        U_tilde <- get_U_tilde(df, y.n, c, alpha = alpha, cum = F)
        
        d_x_t[, 2:omega, y, c] <- U_tilde %*% d_x_t[, 1:(omega-1), (y-1), c]
 
    }
}



## Save dynamics of parents ----------------------------------------------------

saveRDS(d_x_t,
        here("data_private", "d_x_t.rda"))

