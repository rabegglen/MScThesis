### Descriptive analyses and graphics

# source("dataPreparation.R")
source("plsPath4.R")

# load("./dataSet.RData")



### some simple descriptive statistics

desc = calcDat %>% 
  count(Age_1)


lang = calcDat %>% 
  count(language)


edu = calcDat %>% 
  count(Edu) %>% 
  mutate(
    percentage = n / sum(n) * 100
  )


groups = calcDat %>% 
  count(., group)

### get some heat maps


crossloadings = anthroModel4$crossloadings






