#### SEM 

source("constructs.R")


pacman :: p_load(
  lavaan,
  mvnormalTest,
  install = TRUE
)

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



## shapiro-wilk univariate normality test

univnorm = semDat %>% 
  select(
    technologyPlayfulness:UseContIntentionsAll
  ) %>% 
  mutate_all(., as.numeric) 




for(i in 1 : ncol(univnorm)){
  
  test = mardia(univnorm[i])
  names(univnorm[i]) %>% print()
  test$uv.shapiro %>% print()
  test$mv.test %>% print()
  
}



