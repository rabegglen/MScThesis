### Descriptive analyses

source("dataPreparation.R")

load("./dataSet.RData")



if(!require("pacman")){
  
  install.packages("pacman")
  
}



pacman :: p_load(
  lavaan,
  lavaanPlot,
  devtools,
  install = TRUE
  
)



### some descriptive statistics

desc = calcDat %>% 
  count(Age_1)


lang = calcDat %>% 
  count(language)


edu = calcDat %>% 
  count(Edu) %>% 
  mutate(
    percentage = n / sum(n) * 100
  )
