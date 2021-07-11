#### Path model with all variables included, without any item-culling or similar
# this is just for pure reference and will not be used for the study


source("./scripts/constructs.R")

# devtools :: install_github("gastonstat/plspm")

pacman :: p_load(
  plspm,
  plsdepot,
  install = TRUE
)

## introduce a variable which is coding for anthropomorphism and for empathy separately

## 

plsDat = calcDat %>% 
  mutate(
    
    anthropo = case_when(
      
      grepl("^A", group) ~ 1,
      grepl("^N", group) ~ 0
      
    ),
    
    empathy = case_when(
      
      grepl("E$", group) ~ 1,
      grepl("N$", group) ~ 0
      
    ),
    
    
    eduNum = case_when(
      
      grepl("Mandatory School", Edu, ignore.case = TRUE) ~ 1,
      grepl("Vocational Education and Training/Professional Certificate", Edu, ignore.case = TRUE) ~ 2,
      grepl("Grammar school or equivalent", Edu, ignore.case = TRUE) ~ 3,
      grepl("Bachelor's degree or equivalent", Edu, ignore.case = TRUE) ~ 4,
      grepl("Master's degree", Edu, ignore.case = TRUE) ~ 5,
      grepl("PhD, MD or higher", Edu, ignore.case = TRUE) ~ 6
      
    ),
    
    
    # Creating meaningful numeric measures for control vars like age, gender and education
    
    genderNum = case_when(
      
      grepl("Female", Gender) ~ 1,
      grepl("Male", Gender) ~ 0
      
    )
    
    
    
  )


names(plsDat)

### model 3: adding interaction terms for anthro and empathy


## stripping the data from the vars with insufficient loadings




modelDat = plsDat %>%
  select(
    # control variables
    matches("innov|playful|trustingstance|robotse|edunum|gendernum"),
    # service failure
    matches("service"),
    # the treatment
    matches("^anthropo$|^empathy$"),
    # Contructs
    matches("anthro.*_[1-9]{1}"), matches("empathy.*_[1-9]{1}"), matches("^trex.*_[1-9]{1}"), matches("pi_trex.*_[1-9]{1}"), matches("trdisc.*_[1-9]{1}"), matches("perf.*_[1-9]{1}"), matches("satis.*[1-9]{1}"), matches("techtr_intention.*_[1-9]{1}"), matches("cont_int.*_[1-9]{1}"),
    matches("^reuse$")
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





modelDatAll = modelDat %>% 
  # select(
  #   -matches("anthro_3rev|anthro_1|^trexreliability|^trexhelp_5|pi_trex_reliability|trdisc_reliabilit|pi_trperf_reliab|servicefailure_1|robotSE1|trustingstance_2|robotse3|robotse2_2|robotse2_3|playful1_2rev|playful2_1rev|playful2_3rev")## item culling
  # ) %>% 
  mutate(
    
    ## interaction with antrhopomorphism
    anthroInter1 = MC_serviceFailure_2 * MC_Anthro_2,
    anthroInter2 = MC_serviceFailure_3rev * MC_Anthro_2,
    
    ## interaction with empathy
    
    # empathInter1 = MC_serviceFailure_2 * MC_Empathy_1,
    # empathInter2 = MC_serviceFailure_2 * MC_Empathy_2,
    # empathInter3 = MC_serviceFailure_2 * MC_Empathy_3rev,
    empathInter4 = MC_serviceFailure_3rev * MC_Empathy_1,
    empathInter5 = MC_serviceFailure_3rev * MC_Empathy_2,
    empathInter6 = MC_serviceFailure_3rev * MC_Empathy_3rev,
    
    
    ## interaction with treatment anthropomorphism
    TreatAnthroInter1 = MC_serviceFailure_2 * anthropo,
    TreatAnthroInter2 = MC_serviceFailure_3rev * anthropo,
    
    
    ## interaction with treatment empathy
    TreatEmpathInter1 = MC_serviceFailure_2 * empathy,
    TreatEmpathInter2 = MC_serviceFailure_3rev * empathy
    
    
    # anthropo = as.factor(anthropo),
    # empathy = as.factor(empathy)
    # 
    
    
    
    
    
  )


names(modelDatAll)



modelDatAll %>% names()



### create the path matrix with the relevant inner model constructs


anthroTreat            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
empathTreat            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
treatAnthroInter       =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
treatEmpathInter       =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
anthro                 =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
empathy                =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
interEmpath            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
interAnthro            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
servFailure            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
InitialTechTrustExpect =      c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustExpect =      c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustPerfor =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustDiconf =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustSatisf =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustIntent =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
playfulness            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
innovativeness         =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
robotSE                =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
trustingstance         =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
genderNum              =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
eduNum                 =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
IntentOfUsageContinuat =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
reUse                  =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0)



techPath = rbind(
  anthroTreat           ,
  empathTreat           ,
  treatAnthroInter      ,
  treatEmpathInter      ,
  anthro                ,
  empathy               ,
  interEmpath           ,
  interAnthro           ,
  servFailure           ,
  InitialTechTrustExpect,
  PostIncTechTrustExpect,
  PostIncTechTrustPerfor,
  PostIncTechTrustDiconf,
  PostIncTechTrustSatisf,
  PostIncTechTrustIntent,
  playfulness           ,
  innovativeness        ,
  robotSE               ,
  trustingstance        ,
  genderNum             ,
  eduNum                ,
  IntentOfUsageContinuat,
  reUse                 
)

colnames(techPath) = rownames(techPath)


innerplot(techPath, box.size = 0.1)





# service failure index


failIndex = modelDatAll %>% 
  names() %>% 
  grep("service", ., ignore.case = TRUE)



# interactions

interAnthroIndex = modelDatAll %>% 
  names() %>% 
  grep("^anthroInter", ., ignore.case = TRUE)



interEmpathIndex = modelDatAll %>% 
  names() %>% 
  grep("^empathInter", ., ignore.case = TRUE)





treatAnthroInterIndex = modelDatAll %>% 
  names() %>% 
  grep("^treatanthroInter", ., ignore.case = TRUE)



treatEmpathInterIndex = modelDatAll %>% 
  names() %>% 
  grep("^treatempathInter", ., ignore.case = TRUE)




# For the treatment

anthroTreatIndex = modelDatAll %>% 
  names() %>% 
  grep("^anthropo$", ., ignore.case = TRUE)


# For the treatment

empathTreatIndex = modelDatAll %>% 
  names() %>% 
  grep("^empathy", ., ignore.case = TRUE)


# perceived anthropomorphism


anthroIndex = modelDatAll %>% 
  names() %>% 
  grep("anthro.*_[1-9]{1}", ., ignore.case = TRUE)


# perceived empathy

emphaIndex = modelDatAll %>% 
  names() %>% 
  grep("empathy.*_[1-9]{1}", ., ignore.case = TRUE)


# trusting expectations

trexIndex = modelDatAll %>% 
  names() %>% 
  grep("^trex.*_[1-9]{1}", ., ignore.case = TRUE)


# post-incident trusting expectations

PItrexIndex = modelDatAll %>% 
  names() %>% 
  grep("^pi_trex.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting expectations disconfirmation

trdiscIndex = modelDatAll %>% 
  names() %>% 
  grep("trdisc.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting performance

trPerfIndex = modelDatAll %>% 
  names() %>% 
  grep("perf.*_[1-9]{1}", ., ignore.case = TRUE)


# Satisfaction

satisIndex = modelDatAll %>% 
  names() %>% 
  grep("satis.*[1-9]{1}", ., ignore.case = TRUE)


# Technology trusting intention

trIntIndex = modelDatAll %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting intentions

techtrInt = modelDatAll %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# usage continuation intention

useContIndex = modelDatAll %>% 
  names() %>% 
  grep("cont_int.*_[1-9]{1}", ., ignore.case = TRUE)


# actual re-use of the system


reUseIndex = modelDatAll %>% 
  names() %>% 
  grep("reUse", ., ignore.case = TRUE)


## control vars


playfulnessIndex           = modelDatAll %>% 
  names() %>% 
  grep("playful[1-9]{1}_", ., ignore.case = TRUE)


innovativenessIndex        = modelDatAll %>% 
  names() %>% 
  grep("innov_[1-9]{1}", ., ignore.case = TRUE)


robotSEIndex               = modelDatAll %>% 
  names() %>% 
  grep("robotSE[1-9]{1}_", ., ignore.case = TRUE)


trustingstanceIndex        = modelDatAll %>% 
  names() %>% 
  grep("trustingstance_", ., ignore.case = TRUE)


genderNumIndex             = modelDatAll %>% 
  names() %>% 
  grep("genderNum", ., ignore.case = TRUE)


eduNumIndex                = modelDatAll %>% 
  names() %>% 
  grep("eduNum", ., ignore.case = TRUE)






modelBlocks = list(
  anthroTreatIndex,
  empathTreatIndex,
  treatAnthroInterIndex,
  treatEmpathInterIndex,
  anthroIndex,
  emphaIndex,
  interEmpathIndex,
  interAnthroIndex,
  failIndex,
  trexIndex,
  PItrexIndex,
  trPerfIndex,
  trdiscIndex,
  satisIndex,
  techtrInt,
  playfulnessIndex,   
  innovativenessIndex,
  robotSEIndex,       
  trustingstanceIndex,
  genderNumIndex,     
  eduNumIndex,        
  useContIndex,
  reUseIndex
)


blockAmount = length(modelBlocks)

# They are reflective as per definition in Lankton 2014

modelModes = rep("A", blockAmount) 


# the whole model

modelDatAll = modelDatAll %>% 
  filter(!is.na(genderNum))


anthromodelAll = plspm(Data = modelDatAll, path_matrix = techPath, blocks = modelBlocks, modes = modelModes)


anthromodelAll$unidim

plot(anthromodelAll, what = "loadings")

# loadings

anthromodelAll$outer_model


# cross loadings

crossLoadings = anthromodelAll$crossloadings


### plotting the loadings


ggplot(data = anthromodelAll$outer_model,
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




plot(anthromodelAll)



### inner model regressions


innerModel = anthromodelAll$inner_model




### effects


effects = anthromodelAll$effects %>% 
  filter(direct != 0 | indirect != 0)



pathEffects = as.matrix(effects[2:3])

rownames(pathEffects) = effects$relationships





### plotting the effects

# setting margin size
op = par(mar = c(8, 3, 2, 0.5))
# barplots of total effects (direct + indirect)
barplot(t(pathEffects), border = NA, col = c("#9E9AC8", "#DADAEB"),
        las = 2, cex.names = 0.8, cex.axis = 0.8,
        legend = c("Direct", "Indirect"),
        args.legend = list(x = "top", ncol = 2, border = NA,
                           bty = "n", title = "Effects"))
# resetting default margins
par(op)




### inner model summary


anthromodelAll$inner_summary



### goodness of fit

anthromodelAll$gof
