### showing the culled items

source("./scripts/plsPath4.R")
source("./scripts/plsPathAllVars.R")



vars4 = names(modelDat4) %>% 
  tibble(vars = .)


varsAll = names(ModelDatAll) %>% 
  tibble(vars = .)

varsOmit = "robotSelfEfficacy|inter|technologyPlayfulness|generalTrustingStance|generalTrustingStance|trustingStanceAgent|treatAnthroEmpath|anthroEmpath|techhnologyInnovativeness"

vars4 = vars4 %>% 
  filter(
    !grepl(varsOmit, vars, ignore.case = TRUE)
  )


varsAll = varsAll %>% 
  filter(
    !grepl(varsOmit, vars, ignore.case = TRUE)
  )


