#### cronbach's alpha


# source("dataPreparation.R")
source("constructs.R")

pacman :: p_load(
  psych,
  install = TRUE
)


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


###### expectations disconfirmation


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



