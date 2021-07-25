#### the interaction model for the moderation term on the service failure and  also introducing the actual use term. Additionally here with control vars.

source("./scripts/constructs.R")


## introduce a variable which is coding for anthropomorphism and for empathy separately

## 

plsDat = calcDat %>% 
  mutate(
    
    anthropo = case_when(
      
      grepl("^A", group) ~ 1,
      grepl("^N", group) ~ 0
      
    ) %>% as.factor(),
    
    empathy = case_when(
      
      grepl("E$", group) ~ 1,
      grepl("N$", group) ~ 0
      
    ) %>% as.factor(),
    
    
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
      
      grepl("Female", Gender) ~ 2,
      grepl("Male", Gender) ~ 1,
      grepl("No Answer", Gender) ~ 0
    ) %>% as.factor()
    
    
    
  )


plsDat$Gender %>% unique()

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
    
    ## post incident trusting expectation measures for reliability are anti-correlated due to how the questions were asked, that's why they are reversed here
    PI_TrEx_Reliability_1 = -PI_TrEx_Reliability_1 + max(PI_TrEx_Reliability_1) + 1,
    PI_TrEx_Reliability_2 = -PI_TrEx_Reliability_2 + max(PI_TrEx_Reliability_2) + 1,
    PI_TrEx_Reliability_3 = -PI_TrEx_Reliability_3 + max(PI_TrEx_Reliability_3) + 1,
    
    ## post incident trusting performance measures for reliability are anti-correlated due to how the questions were asked, that's why they are reversed here
    PI_TrPerf_Reliab_1 = -PI_TrPerf_Reliab_1 + max(PI_TrPerf_Reliab_1) + 1,
    PI_TrPerf_Reliab_2 = -PI_TrPerf_Reliab_2 + max(PI_TrPerf_Reliab_2) + 1,
    PI_TrPerf_Reliab_3rev = -PI_TrPerf_Reliab_3rev + max(PI_TrPerf_Reliab_3rev) + 1,
    
  )





ModelDatAll = modelDat %>% 
  # select(
  #   -matches("anthro_3rev|anthro_1|^trexreliability|^trexhelp_5|pi_trex_reliability|trdisc_reliabilit|pi_trperf_reliab|servicefailure_1|robotSE1|trustingstance_2|robotse3|robotse2_2|robotse2_3|playful1_2rev|playful2_1rev|playful2_3rev|innov_3rev")
  # ) %>% 
  mutate(
    
    ## interaction with antrhopomorphism and service failure
    anthroInter1 = MC_serviceFailure_2 * MC_Anthro_2,
    anthroInter2 = MC_serviceFailure_3rev * MC_Anthro_2,
    
    ## interaction with empathy and service failure
    
    empathInter4 = MC_serviceFailure_3rev * MC_Empathy_1,
    empathInter5 = MC_serviceFailure_3rev * MC_Empathy_2,
    empathInter6 = MC_serviceFailure_3rev * MC_Empathy_3rev,
    
    
    ## interaction with treatment anthropomorphism and service failure
    TreatAnthroInter1 = MC_serviceFailure_2 * anthropo,
    TreatAnthroInter2 = MC_serviceFailure_3rev * anthropo,
    
    
    ## interaction with treatment empathy and service failure
    TreatEmpathInter1 = MC_serviceFailure_2 * empathy,
    TreatEmpathInter2 = MC_serviceFailure_3rev * empathy,
    
    
    ## interaction with treatment empathy and anthro
    
    treatAnthroEmpath = empathy * anthropo,
    
    
    ## interaction with perceived empathy and anthro
    
    anthroEmpath1 = MC_Anthro_2 * MC_Empathy_1,
    anthroEmpath2 = MC_Anthro_2 * MC_Empathy_2,
    anthroEmpath3 = MC_Anthro_2 * MC_Empathy_3rev,
    
    
  )


names(ModelDatAll)



ModelDatAll %>% names()



### create the path matrix with the relevant inner model constructs


anthroTreat            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
empathTreat            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
treatAnthroEmpath      =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
treatAnthroInter       =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
treatEmpathInter       =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
anthro                 =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
empathy                =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
AnthroEmpath           =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
interEmpath            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
interAnthro            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
servFailure            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
InitialTechTrustExpect =      c(1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustExpect =      c(0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustPerfor =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustDiconf =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustSatisf =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustIntent =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
playfulness            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
innovativeness         =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
robotSE                =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
trustingstance         =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
genderNum              =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
eduNum                 =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
IntentOfUsageContinuat =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
reUse                  =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0)



techPath = rbind(
  anthroTreat           ,
  empathTreat           ,
  treatAnthroEmpath     ,
  treatAnthroInter      ,
  treatEmpathInter      ,
  anthro                ,
  empathy               ,
  AnthroEmpath          ,
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


innerplot(techPath, box.size = 0.05)



#### creating the data indices to feed the data into the model the correct way. I relied on pattern matching instead of judgement by eye, that's why there is grep()

# service failure index


failIndex = ModelDatAll %>% 
  names() %>% 
  grep("service", ., ignore.case = TRUE)



# interactions

interAnthroIndex = ModelDatAll %>% 
  names() %>% 
  grep("^anthroInter", ., ignore.case = TRUE)



interEmpathIndex = ModelDatAll %>% 
  names() %>% 
  grep("^empathInter", ., ignore.case = TRUE)



treatAnthroInterIndex = ModelDatAll %>% 
  names() %>% 
  grep("^treatanthroInter", ., ignore.case = TRUE)



treatEmpathInterIndex = ModelDatAll %>% 
  names() %>% 
  grep("^treatempathInter", ., ignore.case = TRUE)




AnthroEmpathIndex = ModelDatAll %>% 
  names() %>% 
  grep("^AnthroEmpath", ., ignore.case = TRUE) 




treatAnthroEmpathIndex = ModelDatAll %>% 
  names() %>% 
  grep("^treatAnthroEmpath$", ., ignore.case = TRUE) 



# For the treatment

anthroTreatIndex = ModelDatAll %>% 
  names() %>% 
  grep("^anthropo$", ., ignore.case = TRUE)


# For the treatment

empathTreatIndex = ModelDatAll %>% 
  names() %>% 
  grep("^empathy", ., ignore.case = TRUE)


# perceived anthropomorphism


anthroIndex = ModelDatAll %>% 
  names() %>% 
  grep("anthro.*_[1-9]{1}", ., ignore.case = TRUE)


# perceived empathy

emphaIndex = ModelDatAll %>% 
  names() %>% 
  grep("empathy.*_[1-9]{1}", ., ignore.case = TRUE)


# trusting expectations

trexIndex = ModelDatAll %>% 
  names() %>% 
  grep("^trex.*_[1-9]{1}", ., ignore.case = TRUE)


# post-incident trusting expectations

PItrexIndex = ModelDatAll %>% 
  names() %>% 
  grep("^pi_trex.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting expectations disconfirmation

trdiscIndex = ModelDatAll %>% 
  names() %>% 
  grep("trdisc.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting performance

trPerfIndex = ModelDatAll %>% 
  names() %>% 
  grep("perf.*_[1-9]{1}", ., ignore.case = TRUE)


# Satisfaction

satisIndex = ModelDatAll %>% 
  names() %>% 
  grep("satis.*[1-9]{1}", ., ignore.case = TRUE)


# Technology trusting intention

trIntIndex = ModelDatAll %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting intentions

techtrInt = ModelDatAll %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# usage continuation intention

useContIndex = ModelDatAll %>% 
  names() %>% 
  grep("cont_int.*_[1-9]{1}", ., ignore.case = TRUE)


# actual re-use of the system


reUseIndex = ModelDatAll %>% 
  names() %>% 
  grep("reUse", ., ignore.case = TRUE)


## control vars


playfulnessIndex           = ModelDatAll %>% 
  names() %>% 
  grep("playful[1-9]{1}_", ., ignore.case = TRUE)


innovativenessIndex        = ModelDatAll %>% 
  names() %>% 
  grep("innov_[1-9]{1}", ., ignore.case = TRUE)


robotSEIndex               = ModelDatAll %>% 
  names() %>% 
  grep("robotSE[1-9]{1}_", ., ignore.case = TRUE)


trustingstanceIndex        = ModelDatAll %>% 
  names() %>% 
  grep("trustingstance_", ., ignore.case = TRUE)


genderNumIndex             = ModelDatAll %>% 
  names() %>% 
  grep("genderNum", ., ignore.case = TRUE)


eduNumIndex                = ModelDatAll %>% 
  names() %>% 
  grep("eduNum", ., ignore.case = TRUE)








#### assembling the model blocks



modelBlocks = list(
  anthroTreatIndex,
  empathTreatIndex,
  treatAnthroEmpathIndex,
  treatAnthroInterIndex,
  treatEmpathInterIndex,
  anthroIndex,
  emphaIndex,
  AnthroEmpathIndex,
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


ModelDatAll %>% 
  filter_all(
    any_vars(is.na(.))
  )


# the whole model

ModelDatAll = ModelDatAll %>%
  filter(!is.na(genderNum))


anthroModelAll = plspm(Data = ModelDatAll, path_matrix = techPath, blocks = modelBlocks, modes = modelModes)


anthroModelAll$unidim

plot(anthroModelAll, what = "loadings")

# loadings

anthroModelAll$outer_model


# cross loadings

crossLoadings = anthroModelAll$crossloadings


### plotting the loadings


ggplot(data = anthroModelAll$outer_model,
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




plot(anthroModelAll)



### inner model regressions


innerModel = anthroModelAll$inner_model
innerModel



### effects


effects = anthroModelAll$effects %>% 
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


anthroModelAll$inner_summary



### goodness of fit

anthroModelAll$gof

