### data analysis

### prepare the data


if(!require("pacman")){
  
  install.packages("pacman")
  
}


### please also make sure to install the R devtools to enable the install_github() function
### to install the plspm library, please uncomment
# devtools :: install_github("gastonstat/plspm")### this is for the plspm package. This package was on CRAN, therefore it was validated. The author however exceeded the period to not mark the package as maintained, that's why his github repo is used as download directory.

pacman :: p_load(
  tidyverse,
  readxl,
  plspm,
  plsdepot,
  psych,
  install = TRUE
  
)

dat = list.files("./Data/", full.names = TRUE) %>%
  .[grepl("\\.xlsx", .)] %>% 
  read_excel(.) %>% 
  set_names(
    str_replace_all(names(.), "\\s", "_")
  ) %>% 
  
  set_names(
    str_replace_all(names(.), "[_]{2,}", "_")## qualtrics put sometimes underlines in multiple succession in the attribute names
  ) %>% 
  
  set_names(
    str_remove_all(names(.), "\\(|\\)")
  ) %>% 
  filter(
    !grepl("Start Date", StartDate)
  ) %>% 
  mutate_at(
    vars(StartDate, EndDate, Progress),
    as.numeric
  ) %>% 
  mutate(
    StartDate = as.POSIXct(StartDate * 60 * 60 * 24,
                           origin="1899-12-30"
                           ),
    EndDate = as.POSIXct(EndDate * 60 * 60 * 24,
                           origin="1899-12-30"
    )
    
  ) %>% 
  arrange(StartDate) %>% 
  filter(
    StartDate > "2020-11-25 00:00:00" & Progress >= 100
  )
  



### making the data more usable



# dat %>% 
#   select(
#     group, matches("_trExFunct_4")
#   ) %>% View()


### this code block tidies all the variables together grouped by treatments

temp = dat %>% 
  mutate(
    
    TrustingSt_Agent_2 = coalesce(!!! select(., matches("TrustingSt_Agent_2"))),
    TrustingSt_Agent_3 = coalesce(!!! select(., matches("TrustingSt_Agent_3"))),
    TrustingSt_Agent_4 = coalesce(!!! select(., matches("TrustingSt_Agent_4"))),
    trExReliability_2rev = coalesce(!!! select(., matches("trExReliability_2rev"))),
    trExReliability_3 = coalesce(!!! select(., matches("trExReliability_3"))),
    trExReliability_4 = coalesce(!!! select(., matches("trExReliability_4"))),
    proposition = coalesce(!!! select(., matches("proposition"))),
    trExFunct_2 = coalesce(!!! select(., matches("trExFunct_2"))),
    trExFunct_3 = coalesce(!!! select(., matches("trExFunct_3"))),
    trExFunct_4 = coalesce(!!! select(., matches("trExFunct_4"))),
    trExHelp_2 = coalesce(!!! select(., matches("trExHelp_2"))),
    trExHelp_5 = coalesce(!!! select(., matches("trExHelp_5"))),
    trExHelp_3 = coalesce(!!! select(., matches("trExHelp_3"))),
    consideration = coalesce(!!! select(., matches("consideration"))),
    reUse = coalesce(!!! select(., matches("reUse"))),
    email = coalesce(!!! select(., matches("TYGB")))
    
    
  ) %>% 
  rename(Playful1_2rev = Playful1_2)### minor correction as this item was found reversed - forgot it to flag it into the native survey data



temp %>% 
  count(., group)





save(temp, file = "./data/dataCompiled.RData")### entire data with email addresses

### extracting the data without email and location data

calcDat = temp %>% 
  select(
    !matches("loc|mail|tygb|ipaddress")
  ) %>% 
  select(-StartDate, -EndDate)

save(calcDat, file = "dataSet.RData")

temp %>% 
  select(matches("rev")) %>% 
  names()


