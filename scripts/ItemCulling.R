### showing the culled items

source("./scripts/plsPath4.R")
source("./scripts/plsPathAllVars.R")



vars4 = names(modelDat4) %>% 
  tibble(vars = .)
varsAll = names(ModelDatAll) %>% 
  tibble(vars = .)



### see what's not in the final data


varsOmitted = varsAll %>% 
  anti_join(
    ., vars4, by = "vars"
  ) %>% 
  filter(
    !grepl("inter", ignore.case = TRUE, vars)
  )
