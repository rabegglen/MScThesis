### ceiling or floor effects oliver (2014, p. 106)

source("./scripts/plsPath4.R")


## testing correlation between PI-expectations and disconfirmation. Comparison 


# selecting vars

correlation = modelDat4 %>% 
  select(matches("pi_trex|disc")) %>% 
  mutate(
    mean_PI_TrustingExpectations = select(., matches("pi_trex")) %>% rowMeans(),
    mean_PI_TrustingDisconfirm = select(., matches("disc")) %>% rowMeans()
  ) %>% 
  select(matches("mean_")) 


result = corr.test(correlation$mean_PI_TrustingExpectations, correlation$mean_PI_TrustingDisconfirm)

result$p.adj# significantly correlated
