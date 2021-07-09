## model2

source("plsPath1.R")



### model 2: Some vars had to be tossed out as there was insufficient loadings


## stripping the data from the vars with insufficient loadings

modelDat2 = modelDat %>% 
  select(
    -matches("anthro_3rev|anthro_1|^trexreliability|^trexhelp_5|pi_trex_reliability|trdisc_reliabilit|pi_trperf_reliab")
  )




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



