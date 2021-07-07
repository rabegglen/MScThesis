### Descriptive analyses

source("dataPreparation.R")

load("./dataSet.RData")



if(!require("pacman")){
  
  install.packages("pacman")
  
}

# devtools :: install_github("alishinski/lavaanPlot")

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


