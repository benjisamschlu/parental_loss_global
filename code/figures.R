

## Install/load packages
packages <- c("tidyverse", "here", "ggplot2")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}



## Load data -------------------------------------------------------------------




## Fig 1 -----------------------------------------------------------------------

##  Nber of all-cause parental death at individual level

fig1.years = c(seq(1960, 2000, 20), 2019)

df.nber.dth |> 
    filter(year %in% fig1.years) |> 
    ## Sum over all causes of death
    group_by(ctry, year, age, sex) |> 
    summarise(N = sum(N)) |> 
    ## delete the cumsum but highlights
    ## that it does not reach 1
    ungroup() |> 
    group_by(ctry, year, sex) |> 
    mutate(cumN = cumsum(N)) |> 
    ggplot(aes(x = age, y = cumN, col = ctry)) +
    facet_grid(sex ~ year) +
    geom_line(linewidth = 1) +
    theme_bw()
