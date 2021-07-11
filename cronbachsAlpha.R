#### cronbach's alpha

#### this functionality comes with the plspm package anyway


source("constructs.R")



alphaDat = calcDat %>% 
  select(-matches("^AN|^NN|^NE|^AE"))

### calculating the cronbach's alpha of each construct

names(alphaDat)

# Playfulness

alphaDat %>% 
  select(matches("playful")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.83


# Innovativeness

alphaDat %>% 
  select(matches("innov")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


### 0.82

# Trusting Stance


alphaDat %>% 
  select(matches("TrustingStance")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.62



# Robot Self-efficacy


alphaDat %>% 
  select(matches("robotse")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.77


# Trusting stance towards agent


alphaDat %>% 
  select(matches("^TrustingSt_Agent")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


### 0.78



# Trusting expectations reliability


alphaDat %>% 
  select(matches("^trExReliability")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.48


# Trusting expectations functionality


alphaDat %>% 
  select(matches("^trExFunct")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.86


# Trusting expectations helpfulness



alphaDat %>% 
  select(matches("^trExHelp")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


### 0.81

# general pre-incident expectations

alphaDat %>% 
  select(matches("^trEx")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha(.)



###### trusting expectations

# Post incident trusting expectations functionality


alphaDat %>% 
  select(matches("^PI_trex_func")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.9



# Post incident trusting expectations helpfulness


alphaDat %>% 
  select(matches("^PI_trex_helpful")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


# Post incident trusting expectations functionality


alphaDat %>% 
  select(matches("^PI_trex_reliab")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


# general post incident expectation beliefs

alphaDat %>% 
  select(matches("^PI_trex")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha(., check.keys = TRUE)




###### expectations disconfirmation post-incident


# Post incident trusting expectations disconfirmation functionality


alphaDat %>% 
  select(matches("^PI_TrDisc_func")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.9



# Post incident trusting expectations disconfirmation helpfulness


alphaDat %>% 
  select(matches("^PI_TrDisc_helpful")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


# Post incident trusting expectations disconfirmation functionality


alphaDat %>% 
  select(matches("^PI_TrDisc_reliab")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


### all expectation disconfirmation beliefs


alphaDat %>% 
  select(matches("^PI_TrDisc")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


###### trusting performance




# Post incident trusting performance functionality


alphaDat %>% 
  select(matches("^PI_TrPerf_func")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()

### 0.9



# Post incident trusting performance helpfulness


alphaDat %>% 
  select(matches("^PI_TrPerf_helpful")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


# Post incident trusting performance functionality


alphaDat %>% 
  select(matches("^PI_TrPerf_reliab")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()


### overall performance beliefs

alphaDat %>% 
  select(matches("^PI_TrPerf")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha(., check.keys = TRUE)


#### tech satisfaction

alphaDat %>% 
  select(matches("^PI_Tech_Satisfact")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()




#### tech trusting intentions

alphaDat %>% 
  select(matches("^PI_TechTr_Intentio")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()



#### usage continuation intention

alphaDat %>% 
  select(matches("^Usage_Cont_Intention")) %>% 
  mutate_all(., funs(as.numeric(.))) %>% 
  alpha()






####### MC variables


# cronbach's alpha


### anthropomorphism in general

alphaDat %>% 
  select(matches("anthro")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# .73

# only anthropomorphic bots

alphaDat %>% 
  dplyr :: select(matches("anthro|group")) %>% 
  dplyr :: filter(
    grepl("^A", group)
  ) %>% 
  select(matches("anthro")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.6


# only empathic bots

alphaDat %>% 
  dplyr :: select(matches("empathy|group")) %>% 
  dplyr :: filter(
    grepl("E$", group)
  ) %>% 
  select(matches("empathy")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.81



# only non-anthropomorphic bots

alphaDat %>% 
  dplyr :: select(matches("anthro|group")) %>% 
  dplyr :: filter(
    grepl("^N", group)
  ) %>% 
  select(matches("anthro")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.59


# only non-empathic bots

alphaDat %>% 
  dplyr :: select(matches("empathy|group")) %>% 
  dplyr :: filter(
    grepl("N$", group)
  ) %>% 
  select(matches("empathy")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.75






### empathy

# empathy in general


alphaDat %>% 
  select(matches("empathy")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()




# did you lose money?

alphaDat %>% 
  select(MC_serviceFailure_2, MC_serviceFailure_3rev) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()



alphaDat %>% 
  select(MC_serviceFailure_2, MC_serviceFailure_3rev) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

alphaDat %>% 
  select(MC_serviceFailure_2) %>% 
  mutate_all(., list(as.numeric)) %>%
  .$MC_serviceFailure_2 %>% 
  mean()


lm(MC_serviceFailure_3rev ~ group, data = alphaDat) %>% 
  summary


### not acceptable - tossed out


## let's filter only for the different types of bots


alphaDat %>% 
  dplyr :: select(matches("service|group")) %>% 
  dplyr :: filter(
    grepl("^A", group)
  ) %>% 
  select(matches("service")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.6


# AE bots


alphaDat %>% 
  dplyr :: select(matches("service|group")) %>% 
  dplyr :: filter(
    grepl("^AE$", group)
  ) %>% 
  select(matches("service")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()


# only empathic bots

alphaDat %>% 
  dplyr :: select(matches("service|group")) %>% 
  dplyr :: filter(
    grepl("E$", group)
  ) %>% 
  select(matches("service")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.81



# only non-anthropomorphic bots

alphaDat %>% 
  dplyr :: select(matches("service|group")) %>% 
  dplyr :: filter(
    grepl("^N", group)
  ) %>% 
  select(matches("service")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.59


# only non-empathic bots

alphaDat %>% 
  dplyr :: select(matches("service|group")) %>% 
  dplyr :: filter(
    grepl("N$", group)
  ) %>% 
  select(matches("service")) %>% 
  mutate_all(., list(as.numeric)) %>% 
  alpha()

# 0.75



