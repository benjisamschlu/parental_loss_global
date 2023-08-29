get_UNWPP_inputs_ben <- function(countries, my_startyr, my_endyr, variant = "Median"){
    
    print("Getting API ready...")
    print("Make sure you have a working internet connection!")
    
    # Get data from UN using API
    
    base_url <- 'https://population.un.org/dataportalapi/api/v1'
    
    # First, identify which indicator codes we want to use
    
    target <- paste0(base_url,'/indicators/?format=csv')
    codes <- read.csv(target, sep='|', skip=1) 
    
    # Get births, qx, lx, asfr codes
    
    qx_code <- codes$Id[codes$ShortName == "qx1"]
    lx_code <- codes$Id[codes$ShortName == "lx1"]
    pop_code <- codes$Id[codes$ShortName == "PopByAge1AndSex"]
    asfr_code <- codes$Id[codes$ShortName == "ASFR1"]
    brth_code <- codes$Id[codes$ShortName == "TBirths"]
    
    # Get location codes
    
    target <- paste0(base_url, '/locations?sort=id&format=csv')
    df_locations <- read.csv(target, sep='|', skip=1)
    
    # Find the codes for countries
    
    my_location <- 
        df_locations |> 
        dplyr::filter( Name %in% countries) |>  
        dplyr::pull(Id) |>  
        paste(collapse = ",")
    
    # LIFE TABLES
    
    print(paste0("Getting mortality data for ", paste(countries, collapse = ", ")))
    
    # Bind life table codes
    my_indicator <- paste0(c(qx_code, lx_code, pop_code), collapse = ",")
    my_location  <- my_location
    
    target <- paste0(base_url,
                     '/data/indicators/',my_indicator,
                     '/locations/',my_location,
                     '/start/',my_startyr,
                     '/end/',my_endyr,
                     '/?format=csv')
    
    lt <- 
        read.csv(target, sep='|', skip=1) |>  
        dplyr::filter(Variant %in% variant) |>  
        dplyr::mutate(lt_name = case_when(IndicatorId == lx_code ~ "lx", 
                                          IndicatorId == qx_code ~ "qx",
                                          IndicatorId == pop_code ~ "pop")) |> 
        dplyr::select(ctry = Location, year = TimeLabel, age = AgeStart, sex = Sex,
                      lt_name, Value) |> 
        pivot_wider(names_from = lt_name, values_from = Value) |> 
        # Don't filter sex (both also of interest 
        # to estimate the number of children losing parent)
        # dplyr::filter(Sex == sex) %>% 
        dplyr::mutate(px = 1- qx)
        
    
    # ASFR
    
    print(paste0("Getting fertility data for ", paste(countries, collapse = ", ")))
    
    my_indicator <- asfr_code
    
    target <- paste0(base_url,
                     '/data/indicators/',my_indicator,
                     '/locations/',my_location,
                     '/start/',my_startyr,
                     '/end/',my_endyr,
                     '/?format=csv')
    
    asfr <- 
        read.csv(target, sep='|', skip=1) %>% 
        dplyr::filter(Variant %in% variant) %>% 
        dplyr::select(ctry = Location, year = TimeLabel, age = AgeStart, fx = Value) %>% 
        dplyr::mutate(fx = fx/1000,
                      sex = "Female")
    
    # Join asfr and life table
    
    data <- 
        dplyr::left_join(lt, asfr, 
                         by = c("ctry", "year", "age", "sex")) %>% 
        dplyr::mutate(fx = replace(fx,is.na(fx) & (sex == "Female"),0)) 
    
    # BIRTHS 
    
    print(paste0("Getting birth data for ", paste(countries, collapse = ", ")))

    my_indicator <- brth_code

    target <- paste0(base_url,
                     '/data/indicators/',my_indicator,
                     '/locations/',my_location,
                     '/start/',my_startyr,
                     '/end/',my_endyr,
                     '/?format=csv')

    brth <-
        read.csv(target, sep='|', skip=1) %>%
        dplyr::filter(Variant %in% variant) %>%
        dplyr::select(ctry = Location, year = TimeLabel, sex = Sex,
                      n_birth = Value)
    
    # Store in list
    
    out <- list(vital_rates = data, 
                birth_counts = brth)
    

    return(out)
}
