#### SEM 

source("constructs.R")


## introduce a variable which is coding for anthropomorphism and for empathy separately

semDat = calcDat %>% 
  mutate(
    
    anthropo = case_when(
      
      grepl("^A", group) ~ 1,
      grepl("^N", group) ~ 0
      
    ),
    
    empathy = case_when(
      
      grepl("E$", group) ~ 1,
      grepl("N$", group) ~ 0
      
    )
  )
