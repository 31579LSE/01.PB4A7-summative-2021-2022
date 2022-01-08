
# 0.0. setup ---------------------------------------------------------------------------------------

options(scipen=999)

lib <- c('tidyr','plyr', 'ggplot2','viridis','dplyr',
         'forcats','hrbrthemes','data.table','curl',
         'readxl','foreign','ggalt','Hmisc',
         'tidyverse','fastDummies','RCurl','httr','rio')

lapply(lib, library, character.only = TRUE);rm(lib)

# set download paths for files in the github folder
gb_data <- paste0('https://raw.githubusercontent.com/LuisCarlosGuevara/',
                   '01.PB405-summative-2021-2022/main/',
                   '99.data%20and%20tables/')
gb_dats <- paste0('https://github.com/LuisCarlosGuevara/',
                  '01.PB405-summative-2021-2022/raw/main/99.data%20and%20tables/')

# 0.1. data ----------------------------------------------------------------------------------------

# loads main data
d1 <- data.table::fread(paste0(
                            gb_data,
                            '0.1.1.main_data.csv?token=GHSAT0AAAAAABQL2NUCW242GB3HZJEA4KSQYOZTR2A'
                               ))

    ## renames variables from main dataset
    names(d1) <- tolower(names(d1))
    
    d1_varlist <- rio::import(paste0(gb_dats,'0.1.1.varlist.xlsx'))
    
    for(i in 1:length(names(d1))){
    names(d1)[i] <- d1_varlist$lab[d1_varlist$var == names(d1)[i]]
    }; rm(i,d1_varlist)
 
# loads names survey results
d2 <- rio::import(paste0(gb_dats,'0.1.2.name_survey_results.xlsx'))

# loads host additional data
d3 <- data.table::fread(paste0(
                             gb_data,
                             '0.1.3.hosts.csv?token=GHSAT0AAAAAABQL2NUD35C5OLWAK2OAMB6OYOZT3NQ'
                             ))

# 0.2. merge main table with host and name calsification survey-------------------------------------
    
    ## 0.2.1. Names survey -------------------------------------------------------------------------
    
    print(d2$guest_first_name[d2$guest_first_name %in% d1$guest_first_name == FALSE])
    # Keisha and Kareen are present in the survey records but not in the main data.
    # While Keisha is completely absend in the main, Kareen may be included with a
    # typo in the form of Kareem.

    d2$guest_first_name[d2$guest_first_name == 'Kareen'] <-  'Kareem'
    
dt <- merge(d1,d2, all.x = TRUE, by = 'guest_first_name') 
      table(is.na(dt$guest_race_continuous))

    ## 0.2.2. additional host data -----------------------------------------------------------------
    
    print(d3$host_id[d3$host_id %in% d1$host_id == FALSE])
    # 46 host in the additional data table are nor present in the main data set.

    dt <- merge(dt,d3, all.x = TRUE, by = 'host_id') 
    table(is.na(dt$past_guest_merge))

rm(d1,d2,d3)    

dt <- as.data.frame(dt)
# 0.3. variables coding and labeling --------------------------------------------------------------
  
dt[,4:55] <- lapply(dt[,4:55] , as.character)

    ## 0.3.1. missing data coding-------------------------------------------------------------------

    dt[,4:55] <- lapply(dt[,4:55],function(x) gsub("NULL|\\N",NA,x))
    dt[,4:55][dt[,4:55] == -1 ] <- NA

    ## 0.3.2. guest dummy variables ----------------------------------------------------------------
    
    dt <- fastDummies::dummy_cols(dt, select_columns = c('guest_race','guest_gender'))
    
    cols <- c( grep('guest_race_' , names(dt)), grep('guest_gender_' , names(dt))) 
    
    dt[names(dt)[cols]][is.na(dt[names(dt)[cols]])] <- 0 ; rm(cols)
    
    ## 0.3.3. city name id -------------------------------------------------------------------------
    
    dt$name_by_city <- paste0(dt$guest_first_name, dt$city)
    table(is.na(dt$name_by_city))
    
    ## 0.3.4. guest score re-scaling ---------------------------------------------------------------
    
    table(dt$guest_race_continuous)
    dt$guest_race_continuous = as.numeric(dt$guest_race_continuous) - 1
    
    ## 0.3.5. host dummy variables -----------------------------------------------------------------
    
    dt <- fastDummies::dummy_cols(dt, select_columns = c('host_race','host_gender'))
    
    cols <- c( grep('host_race_' , names(dt)), grep('host_gender_' , names(dt))) 
    
    dt[names(dt)[cols]][is.na(dt[names(dt)[cols]])] <- 0
    
    dt$host_gender_same_sex <- 0
    dt$host_gender_same_sex[dt$host_gender_MM == 1 | dt$host_gender_FF == 1 ] <- 1
    
    table(dt$host_gender_same_sex,dt$host_gender_FF)
    
    table(dt$host_age) # host age categories
    d_host_age <- rio::import(paste0(gb_dats,'0.3.5.host_age_cat.xlsx'))
    d_host_age$host_age_cat[is.na(d_host_age$host_age_cat) == TRUE] <- 0
    print(unique(dt$host_age[(dt$host_age %in% d_host_age$host_age) == FALSE]))
    
    dt <- merge(dt, d_host_age, all.x =  TRUE, by =  'host_age')
    dt$host_age_cat[is.na(dt$host_age_cat) == TRUE] <- 0
    table(dt$host_age)
    table(dt$host_age,dt$host_age_cat); rm(d_host_age)

    ## 0.3.6. other dummy variables ----------------------------------------------------------------
  
    dt$ten_reviews        <- ifelse(dt$number_of_reviews >= 10, 1, 0) ; table(dt$ten_reviews)
    dt$five_star_property <- ifelse(dt$apt_rating == 5, 1, 0) ; table(dt$five_star_property)
    dt$multiple_listings  <- ifelse(dt$number_of_listings > 1, 1, 0) ; table(dt$multiple_listings)
    dt$shared_property    <- ifelse(dt$property_setup ==  "Private Room" | 
                                    dt$property_setup == "Shared Room" , 1, 0)
                              table(dt$shared_property,dt$property_setu)
    dt$shared_bathroom    <- ifelse(dt$shared_property ==  1 | 
                                    dt$bathrooms < 1.5 , 1, 0)
                              table(dt$shared_bathroom,dt$bathrooms)
    dt$has_cleaning_fee   <- ifelse(is.na(dt$cleaning_fee) == FALSE, 1, 0)
                              table(dt$cleaning_fee,dt$has_cleaning_fee)
    dt$strict_cancellatio <- ifelse(dt$cancellation_policy == 'Strict', 1, 0)
                              table(dt$strict_cancellatio,dt$cancellation_policy)
    dt$young              <- ifelse(dt$host_age_cat == 0, 1, 0)
    dt$middle             <- ifelse(dt$host_age_cat == 1 | dt$host_age_cat == 2, 1, 0)
                              table(dt$host_age_cat,dt$middle)
    dt$old                <- ifelse(dt$host_age_cat == 3 | dt$host_age_cat == 4, 1, 0)
                              table(dt$host_age_cat,dt$old)
    dt$price              <- as.numeric(dt$price)
    dt$pricey             <- ifelse(dt$price >= as.numeric(quantile( dt$price, 
                                              probs = seq(.1, .9, by = .1),
                                              na.rm = TRUE)[9]),1, 0)
                              table(dt$price,dt$pricey)
                          table(dt$price,dt$pricey)
    dt$price_median       <- ifelse(dt$price > as.numeric(quantile( dt$price, 
                                              probs = seq(.1, .9, by = .1),
                                              na.rm = TRUE)[5]),1, 0)
                              table(dt$price,dt$price_median)
    dt$log_price          <- log(dt$price)

    census_v <- c('whites','blacks','asians','hispanics','population')

    dt <- as.data.frame(dt)
    
    dt[census_v] <- sapply(dt[census_v] , as.numeric)
    
    dt$white_proportion    <- dt$whites/dt$population
    dt$black_proportion    <- dt$blacks/dt$population
    dt$asian_proportion    <- dt$asians/dt$population
    dt$hispanic_proportion <- dt$hispanics/dt$population
    
    dt$latitude <- as.numeric(dt$latitude)
    dt <- dt[which(dt$latitude > 0),] %>% 
              group_by(census_tract, latitude) %>% 
              mutate(tract_listings = n())

    dt$log_tract_listings = log(dt$tract_listings)

## 0.3.7. host response labeling -------------------------------------------------------------------
    
    d_host_res <- rio::import(paste0(gb_dats,'0.3.7.host_response_cat.xlsx'))
    dt <- merge(dt, d_host_res, all.x = TRUE, by = "host_response")
    table(dt$simplified_host_response_labels,dt$yes); rm(d_host_res)
    
## 0.3.8. drop incomplete records conditional on city ----------------------------------------------
    
    table(dt$city);table(is.na(dt$city))
    
    dt$city[dt$city == 'Los-Angeles'] <- 'Los_Angeles'
    dt$city[dt$city == 'St-Louis']    <- 'St_Louis'
    dt <- dt[which(dt$city != "Tampa"),]
    dt <- dt[which(dt$city != "Atlanta"),]
    dt <- fastDummies::dummy_cols(dt, select_columns = c('city'))
    cols <- c( grep('city_' , names(dt))) 
    dt[,cols][is.na(dt[,cols])] <- 0
   
    
    
## 0.3.9. probability weigth for likelihood of being filled in septermber --------------------------

    dt$filed_september <- 0
    dt$filed_september[dt$up_not_available_september == 1] <- 1
    
    
    
    dt$number_of_reviews <- as.numeric(dt$number_of_reviews)    
    dt$bedrooms <- as.numeric(dt$bedrooms)  
    
    sept_probit <- stats::glm(filed_september ~ host_race_black + host_race_asian + host_race_hisp +
                                                host_gender_M + log_price + bedrooms + shared_bathroom +
                                                shared_property + number_of_reviews +
                                                young + multiple_listings + white_proportion +
                                                log_tract_listings +
                                                city_Baltimore + 
                                                city_Dallas + 
                                                city_Los_Angeles +
                                                city_St_Louis,
                              family = binomial(link = "probit"), data = dt)	
    
    sept_probit <- as.data.frame(predict(sept_probit, type = "response", se.fit = TRUE))
    table(is.na(sept_probit$fit))
    