#############################
# INSTALL AND LOAD PACKAGES #
#############################

## change options for large github packages
options(timeout = 600)

## packages to be installed from cran
from.cran <- c("tidyverse")

## packages to be installed from github, paths listed separately
from.git <- c("DemoTools", "wpp2022")
from.git.path <- c("timriffe/DemoTools", "PPgp/wpp2022")

## check if installed, else install
for(i in c(from.cran, from.git)){
  
  ## cran packages
  if(i %in% from.cran){
    
    if(system.file(package=i)==""){install.packages(i)}
    
  }
  
  ## github packages
  if(i %in% from.git){
    
    if(system.file(package=i)==""){
      
      if(i=="DemoTools"){
        
        install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
        
      }
      
      devtools::install_github(from.git.path[which(from.git==i)])
      
    }
  }
  
}

## load frequently used or complicated to use packages
library(tidyverse)
library(wpp2022)

#################
# LOAD GBD DATA #
#################

## list .zip files in input folder
files <- list.files(here::here("input"), pattern=".zip")

## unzip, load, and append data files
for(i in files) {

if(i==files[1]){    

  aux <- 
    read_csv(
      
      unz(description=here::here("input", i), 
        filename=paste0(gsub(".zip", "", i), ".csv"))
      
    )
  
}else{
  
  aux <- 
    aux %>% 
    add_row(
      
      read_csv(
        
        unz(description=here::here("input", i), 
            filename=paste0(gsub(".zip", "", i), ".csv"))
      
      )
    )
  
  }
}
  
## edit combined data set
gbd <- 
  aux %>% ## remove unnecessary variables
  select(Year = year,
         Location = location_name,
         Age = age_name,         
         Sex = sex_name,
         Cause = cause_name,
         Deaths = val
  ) %>% ## create numerical age variable
  mutate(Age = case_when(Age == "<1 year" ~ 0,
                         Age == "1-4 years" ~ 1,
                         Age == "5-9 years" ~ 5,
                         Age == "10-14 years" ~ 10,
                         Age == "15-19 years" ~ 15,
                         Age == "20-24 years" ~ 20,
                         Age == "25-29 years" ~ 25,
                         Age == "30-34 years" ~ 30,
                         Age == "35-39 years" ~ 35,
                         Age == "40-44 years" ~ 40,
                         Age == "45-49 years" ~ 45,
                         Age == "50-54 years" ~ 50,
                         Age == "55-59 years" ~ 55,
                         Age == "60-64 years" ~ 60,
                         Age == "65-69 years" ~ 65,
                         Age == "70-74 years" ~ 70,
                         Age == "75-79 years" ~ 75,
                         Age == "80-84" ~ 80,
                         Age == "85-89" ~ 85,
                         Age == "90-94" ~ 90,
                         Age == "95+ years" ~ 95)) %>% 
  ## not all age-cause combinations have rows in the combined data set;
  ## reshaping to wide and back to long will create those missing rows
  ## alternatively: use "complete" command (?)
  pivot_wider(names_from = "Age", 
              values_from = "Deaths") %>% 
  pivot_longer(cols = -c(Year, Location, Sex, Cause), 
               names_to = "Age",
               values_to = "Deaths") %>% 
  mutate(Age = as.numeric(Age), ## reshaping caused age variable to be categorical, change back to numerical
         Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>% ## reshaping caused death counts for previously missing age-cause combinations to be NA, set to zero
  group_by(Year, Location, Sex, Cause) %>% 
  ## count number of age categories
  mutate(n=1,
         n=sum(n)) %>% 
  ungroup() %>% 
  arrange(Year, Location, Sex, Cause, Age)

## check if data are in correct format  
unique(gbd$n) ## number of age categories should always be 21

####################
# UNGROUP GBD DATA #
####################

## ungroup death counts using pclm, new open-ended age category = 100
gbd.ungrouped <-  
  gbd %>% 
  reframe(Deaths = DemoTools::graduate(Deaths,
                                       Age = Age,
                                       OAnew = 100,
                                       method = "pclm",
                                       constrain = TRUE), .by = c(Year, Location, Sex, Cause)) %>% 
  group_by(Year, Location, Sex, Cause) %>%   
  mutate(Age = 0:100) %>% ## add new age variable
  ungroup()
  
######################
# HARMONIZE GBD DATA #
######################

## load unwpp location names
data(UNlocations)

## find gbd location names that differ from unwpp location names
unique(
  
  gbd.ungrouped %>% 
    filter(
           
         !Location %in% c(UNlocations %>% filter(name %in% unique(gbd.ungrouped$Location)) %>% pull(name))
         
         ) %>% pull(Location)
  
  )

## pull list of causes of death
gbd.causes <- unique(gbd.ungrouped$Cause) 

## harmonize gbd data set with Ben's unwpp data set
gbd.harmonized <-
  gbd.ungrouped %>% 
  mutate(## give generic names to causes of death
         Cause = factor(Cause, levels = gbd.causes, labels = paste0("cause", 1:length(gbd.causes))),
         ## change location names in line with unwpp
         Location = case_when(Location == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                              Location == "Democratic People's Republic of Korea" ~ "Dem. People's Republic of Korea",
                              Location == "Micronesia (Federated States of)" ~ "Micronesia (Fed. States of)",
                              Location == "Palestine" ~ "State of Palestine",
                              Location == "Taiwan (Province of China)" ~ "China, Taiwan Province of China",
                              Location == "Turkey" ~ "Turkiye",
                              TRUE ~ Location)) %>% 
  group_by(Year, Location, Sex, Age) %>% 
  ## calculate cause of death share
  mutate(Deaths = Deaths/sum(Deaths)) %>% 
  ungroup() %>% 
  ## reshape to wide format, 1 column per cause
  pivot_wider(names_from = "Cause",
              values_from = "Deaths") %>% 
  ## rename variables following Ben's naming conventions
  rename(year = Year,
         cntry = Location,
         sex = Sex,
         age = Age)

###############
# SAVE OUTPUT #
###############

saveRDS(gbd, here::here("output", "gbd-original.rds"))
saveRDS(gbd.ungrouped, here::here("output", "gbd-ungrouped.rds"))     
saveRDS(gbd.harmonized, here::here("output", "gbd-harmonized.rds"))
saveRDS(gbd.causes, here::here("output", "gbd-causes.rds"))
