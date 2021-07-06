### Descriptive analyses

source("dataPreparation.R")

load("./data/dataCompiled.RData")


desc = temp %>% 
  count(Age_1)
