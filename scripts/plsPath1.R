#### raw PLS Path Model 

source("./scripts/constructs.R")

# devtools :: install_github("gastonstat/plspm")

pacman :: p_load(
  plspm,
  plsdepot,
  install = TRUE
)

## introduce a variable which is coding for anthropomorphism and for empathy separately

plsDat = calcDat %>% 
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




### create the path matrix with the relevant inner model constructs


anthro                 =      c(0, 0, 0, 0, 0, 0, 0, 0, 0)
empathy                =      c(0, 0, 0, 0, 0, 0, 0, 0, 0)
InitialTechTrustExpect =      c(1, 1, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustExpect =      c(0, 0, 1, 0, 0, 0, 0, 0, 0)
PostIncTechTrustPerfor =      c(0, 0, 0, 1, 0, 0, 0, 0, 0)
PostIncTechTrustDiconf =      c(0, 0, 0, 1, 1, 0, 0, 0, 0)
PostIncTechTrustSatisf =      c(0, 0, 0, 1, 1, 1, 0, 0, 0)
PostIncTechTrustIntent =      c(0, 0, 0, 0, 1, 1, 1, 0, 0)
IntentOfUsageContinuat =      c(0, 0, 0, 0, 0, 0, 1, 1, 0)


techPath = rbind(
  anthro                ,
  empathy               ,
  InitialTechTrustExpect,
  PostIncTechTrustExpect,
  PostIncTechTrustPerfor,
  PostIncTechTrustDiconf,
  PostIncTechTrustSatisf,
  PostIncTechTrustIntent,
  IntentOfUsageContinuat
                 )

colnames(techPath) = rownames(techPath)


innerplot(techPath, box.size = 0.1)


### creating the outer model for the constructs. The indicators will be reflective as stated in Lankton 2014


## creating the data set for the model

modelDat = plsDat %>%
  select(
    
    matches("^anthropo$|^empathy$"),
    matches("anthro.*_[1-9]{1}"), matches("empathy.*_[1-9]{1}"), matches("^trex.*_[1-9]{1}"), matches("pi_trex.*_[1-9]{1}"), matches("trdisc.*_[1-9]{1}"), matches("perf.*_[1-9]{1}"), matches("satis.*[1-9]{1}"), matches("techtr_intention.*_[1-9]{1}"), matches("cont_int.*_[1-9]{1}")
  ) %>% 
  mutate_all(., as.numeric) %>% 
  
  mutate(
    
    ## post incident trusting expectation measures for reliability are anti-correlated due to how the questions were asked
    PI_TrEx_Reliability_1 = -PI_TrEx_Reliability_1 + max(PI_TrEx_Reliability_1) + 1,
    PI_TrEx_Reliability_2 = -PI_TrEx_Reliability_2 + max(PI_TrEx_Reliability_2) + 1,
    PI_TrEx_Reliability_3 = -PI_TrEx_Reliability_3 + max(PI_TrEx_Reliability_3) + 1,
    
    ## post incident trusting performance measures for reliability are anti-correlated due to how the questions were asked
    PI_TrPerf_Reliab_1 = -PI_TrPerf_Reliab_1 + max(PI_TrPerf_Reliab_1) + 1,
    PI_TrPerf_Reliab_2 = -PI_TrPerf_Reliab_2 + max(PI_TrPerf_Reliab_2) + 1,
    PI_TrPerf_Reliab_3rev = -PI_TrPerf_Reliab_3rev + max(PI_TrPerf_Reliab_3rev) + 1,
    
  )
  
  
  

### getting the index numbers of the data frame - it recycles the above used search strings. Much safer than actual index numbers by human eye recognition
# For the treatment

treatIndex = modelDat %>% 
  names() %>% 
  grep("anthro|empathy", .)


# perceived anthropomorphism


anthroIndex = modelDat %>% 
  names() %>% 
  grep("anthro.*_[1-9]{1}", ., ignore.case = TRUE)


# perceived empathy

emphaIndex = modelDat %>% 
  names() %>% 
  grep("empathy.*_[1-9]{1}", ., ignore.case = TRUE)


# trusting expectations

trexIndex = modelDat %>% 
  names() %>% 
  grep("^trex.*_[1-9]{1}", ., ignore.case = TRUE)


# post-incident trusting expectations

PItrexIndex = modelDat %>% 
  names() %>% 
  grep("^pi_trex.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting expectations disconfirmation

trdiscIndex = modelDat %>% 
  names() %>% 
  grep("trdisc.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting performance

trPerfIndex = modelDat %>% 
  names() %>% 
  grep("perf.*_[1-9]{1}", ., ignore.case = TRUE)


# Satisfaction

satisIndex = modelDat %>% 
  names() %>% 
  grep("satis.*[1-9]{1}", ., ignore.case = TRUE)


# Technology trusting intention

trIntIndex = modelDat %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting intentions

techtrInt = modelDat %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# usage continuation intention

useContIndex = modelDat %>% 
  names() %>% 
  grep("cont_int.*_[1-9]{1}", ., ignore.case = TRUE)






modelBlocks = list(
  anthroIndex,
  emphaIndex,
  trexIndex,
  PItrexIndex,
  trdiscIndex,
  trPerfIndex,
  satisIndex,
  techtrInt,
  useContIndex
)


blockAmount = length(modelBlocks)

# They are reflective as per definition in Lankton 2014

modelModes = rep("A", blockAmount) 



anthroPLS1 = plspm(modelDat, techPath, modelBlocks, modes = modelModes)

# the whole model

anthroModel = plspm(Data = modelDat, path_matrix = techPath, blocks = modelBlocks, modes = modelModes)


anthroModel$unidim

plot(anthroModel, what = "loadings")

 

### showing the loadings

anthroModel$outer_model




### plotting the loadings


ggplot(data = anthroModel$outer_model,
       aes(x = name, y = loading, fill = block)) +
  
  geom_bar(
    stat = "identity", position = "dodge"
  ) +
  # threshold line (to peek acceptable loadings above 0.7)
  geom_hline(yintercept = 0.7, color = "gray50"
  ) +
  # add title
  ggtitle("Barchart of Loadings") +
  # rotate x-axis names
  theme(axis.text.x = element_text(angle = 90))










