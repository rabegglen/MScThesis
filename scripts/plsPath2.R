## model2

#### PLS Path Model with constructs alone

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




### model 2: Some vars had to be tossed out as there was insufficient loadings


## stripping the data from the vars with insufficient loadings




modelDat = plsDat %>%
  select(
    matches("service"),
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



modelDat2 = modelDat %>% 
  select(
    -matches("anthro_3rev|anthro_1|^trexreliability|^trexhelp_5|pi_trex_reliability|trdisc_reliabilit|pi_trperf_reliab|servicefailure_1")
  )

names(modelDat2)







### create the path matrix with the relevant inner model constructs


anthro                 =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
empathy                =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
servFailure            =      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
InitialTechTrustExpect =      c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustExpect =      c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0)
PostIncTechTrustPerfor =      c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
PostIncTechTrustDiconf =      c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
PostIncTechTrustSatisf =      c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0)
PostIncTechTrustIntent =      c(0, 0, 0, 0, 0, 1, 1, 1, 0, 0)
IntentOfUsageContinuat =      c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0)


techPath = rbind(
  anthro                ,
  empathy               ,
  servFailure           ,
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















# service failure index


failIndex = modelDat2 %>% 
  names() %>% 
  grep("service", ., ignore.case = TRUE)


# For the treatment

treatIndex = modelDat2 %>% 
  names() %>% 
  grep("anthro|empathy", .)


# perceived anthropomorphism


anthroIndex = modelDat2 %>% 
  names() %>% 
  grep("anthro.*_[1-9]{1}", ., ignore.case = TRUE)


# perceived empathy

emphaIndex = modelDat2 %>% 
  names() %>% 
  grep("empathy.*_[1-9]{1}", ., ignore.case = TRUE)


# trusting expectations

trexIndex = modelDat2 %>% 
  names() %>% 
  grep("^trex.*_[1-9]{1}", ., ignore.case = TRUE)


# post-incident trusting expectations

PItrexIndex = modelDat2 %>% 
  names() %>% 
  grep("^pi_trex.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting expectations disconfirmation

trdiscIndex = modelDat2 %>% 
  names() %>% 
  grep("trdisc.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting performance

trPerfIndex = modelDat2 %>% 
  names() %>% 
  grep("perf.*_[1-9]{1}", ., ignore.case = TRUE)


# Satisfaction

satisIndex = modelDat2 %>% 
  names() %>% 
  grep("satis.*[1-9]{1}", ., ignore.case = TRUE)


# Technology trusting intention

trIntIndex = modelDat2 %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# Trusting intentions

techtrInt = modelDat2 %>% 
  names() %>% 
  grep("techtr_intention.*_[1-9]{1}", ., ignore.case = TRUE)


# usage continuation intention

useContIndex = modelDat2 %>% 
  names() %>% 
  grep("cont_int.*_[1-9]{1}", ., ignore.case = TRUE)






modelBlocks = list(
  anthroIndex,
  emphaIndex,
  failIndex,
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


# the whole model

anthroModel2 = plspm(Data = modelDat2, path_matrix = techPath, blocks = modelBlocks, modes = modelModes)


anthroModel2$unidim

plot(anthroModel2, what = "loadings")

# loadings

anthroModel2$outer_model


# cross loadings

anthroModel2$crossloadings


### plotting the loadings


ggplot(data = anthroModel2$outer_model,
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




plot(anthroModel2)



### inner model regressions


innerModel = anthroModel2$inner_model




### effects


effects = anthroModel2$effects %>% 
  filter(direct != 0 | indirect != 0)



pathEffects = as.matrix(effects[2:3])

rownames(pathEffects) = effects$relationships





### plotting the effects

# setting margin size
op = par(mar = c(8, 3, 1, 0.5))
# barplots of total effects (direct + indirect)
barplot(t(pathEffects), border = NA, col = c("#9E9AC8", "#DADAEB"),
        las = 2, cex.names = 0.8, cex.axis = 0.8,
        legend = c("Direct", "Indirect"),
        args.legend = list(x = "top", ncol = 2, border = NA,
                           bty = "n", title = "Effects"))
# resetting default margins
par(op)




### inner model summary


anthroModel2$inner_summary



### goodness of fit

anthroModel2$gof

