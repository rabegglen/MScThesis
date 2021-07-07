#### Constructs

source("dataPreparation.R")



### this script is responsible for the overall data management regarding the scales and constructs. There is a complete recoding, with respective averaging of the items to tidy the data up.


## general agreeance
unique(calcDat$RobotSE1_1) %>% na.omit()

## disconfirmation

### for recoding scales
scale = c(1, 2, 3, 4, 5, 6, 7)
-scale + max(scale) + 1 # test to make it reversed

## recoding

calcDat = calcDat %>% 
  
  
  ## recode all text to numbers
  
  mutate_all(.,
         funs(
           case_when(
             grepl("extremely disagree|^strongly disagree$|^much worse$|^extremely dissatisfied$|^Extremely miserable$|^extremely frustrated|extremely displeased", ., ignore.case = TRUE) ~ "1",
             grepl("^disagree$|^worse$|^dissatisfied$|^miserable$|^frustrated$|^Displeased$", ., ignore.case = TRUE) ~ "2",
             grepl("^somewhat disagree$|^somewhat worse$|^somewhat dissatisfied$|^somewhat miserable$|^somewhat frustrated$|^somewhat displeased$", ., ignore.case = TRUE) ~ "3",
             
             grepl("^neutral$", ., ignore.case = TRUE) ~ "4",
             
             grepl("^somewhat agree$|^somewhat better$|^somewhat satisfied$|^somewhat delighted$|^somewhat contented$|^somewhat Pleased$", ., ignore.case = TRUE) ~ "5",
             grepl("^agree$|^better$|^satisfied$|^delighted$|^contented$|^Pleased$", ., ignore.case = TRUE) ~ "6",
             grepl("^strongly agree$|^much better$|^extremely satisfied$|^extremely delighted$|^Extremely contented$|^Extremely Pleased$", ., ignore.case = TRUE) ~ "7", 
             
             TRUE ~ as.character(.) ### converting back afterwards to numeric
           )
           
         )    
             ) %>% 
  
  ### recoding reversed items
  mutate_at(
    vars(matches("rev$")),
    list(~
      case_when(
        . == "1" ~ "7",
        . == "2" ~ "6",
        . == "3" ~ "5",
        . == "4" ~ "4",
        . == "5" ~ "3",
        . == "6" ~ "2",
        . == "7" ~ "1",
        TRUE ~ .
      )
    )
  ) %>% 
  

  # creating unified scales with the rowmeans of the items
  
  mutate_at(### string which matches all the measured variables
    vars(matches("playful|innov|TrustingStance|robotse|^TrustingSt_Agent|^trExReliability|^trExFunct|^trExHelp|^PI_trex_func|^PI_trex_helpful|^PI_trex_reliab|^PI_TrDisc_func|^PI_TrDisc_helpful|^PI_TrDisc_reliab|^PI_TrPerf_func|^PI_TrPerf_reliab|^PI_TrPerf_helpful|^PI_TechTr_Intentio|^Usage_Cont_Intention|^PI_Tech_Satisfact")),
    list(
      ~as.numeric(.)
    )
  ) %>% 
  
  
  mutate(
    technologyPlayfulness = rowMeans(select(., matches("playful"))),
    techhnologyInnovativeness = rowMeans(select(., matches("innov"))),
    generalTrustingStance =  rowMeans(select(., matches("TrustingStance"))),
    robotSelfEfficacy =  rowMeans(select(., matches("robotse"))),
    
    trustingStanceAgent = rowMeans(select(., matches("^TrustingSt_Agent"))),
    
    
    
    
    
    # Trusting expectations
    
    # all vars
    
    trExpectAll = rowMeans(select(., matches("^trEx"))),
    
    # single vars
    
    trExpectReliability = rowMeans(select(., matches("^trExReliability"))),
    trExpectFunctionality = rowMeans(select(., matches("^trExFunct"))),
    trExpectHelpful = rowMeans(select(., matches("^trExHelp"))),
    
   
    
    # Post incident expectation
    
    
    ### first need to reverse PI_TrEx_Reliability
    
    PI_TrEx_Reliability_1 = -PI_TrEx_Reliability_1 + max(PI_TrEx_Reliability_1) + 1,
    PI_TrEx_Reliability_2 = -PI_TrEx_Reliability_2 + max(PI_TrEx_Reliability_2) + 1,
    PI_TrEx_Reliability_3 = -PI_TrEx_Reliability_3 + max(PI_TrEx_Reliability_3) + 1,
    
    # all vars
    
    PI_trExpectAll = rowMeans(select(., matches("^PI_trex"))),
    
    # revert it back
    
    PI_TrEx_Reliability_1 = -PI_TrEx_Reliability_1 + max(PI_TrEx_Reliability_1) + 1,
    PI_TrEx_Reliability_2 = -PI_TrEx_Reliability_2 + max(PI_TrEx_Reliability_2) + 1,
    PI_TrEx_Reliability_3 = -PI_TrEx_Reliability_3 + max(PI_TrEx_Reliability_3) + 1,
    
    # single vars
    
    
    PI_trExpectfunct = rowMeans(select(., matches("^PI_trex_func"))),
    PI_trExpectHelpful = rowMeans(select(., matches("^PI_trex_helpful"))),
    PI_trExpectReliab = rowMeans(select(., matches("^PI_trex_reliab"))),
    
    
    
    # expectation disconfirmation
    
    # all vars
    
    PI_trDisc = rowMeans(select(., matches("^PI_TrDisc"))),
    
    # single vars
    
    PI_trDiscfunction = rowMeans(select(., matches("^PI_TrDisc_func"))),
    PI_trDiscHelpful = rowMeans(select(., matches("^PI_TrDisc_helpful"))),
    PI_trDiscReliab = rowMeans(select(., matches("^PI_TrDisc_reliab"))),
    
    
    
    # Trusting performance
    
    # all vars
    
    ### first need to reverse PI_TrPerf_reliab
    
    PI_TrPerf_Reliab_1 = -PI_TrPerf_Reliab_1 + max(PI_TrPerf_Reliab_1) + 1,
    PI_TrPerf_Reliab_2 = -PI_TrPerf_Reliab_2 + max(PI_TrPerf_Reliab_2) + 1,
    PI_TrPerf_Reliab_3rev = -PI_TrPerf_Reliab_3rev + max(PI_TrPerf_Reliab_3rev) + 1,
    
    PI_trPerfAll = rowMeans(select(., matches("^PI_TrPerf"))),
    
    ### reverse it back
    
    PI_TrPerf_Reliab_1 = -PI_TrPerf_Reliab_1 + max(PI_TrPerf_Reliab_1) + 1,
    PI_TrPerf_Reliab_2 = -PI_TrPerf_Reliab_2 + max(PI_TrPerf_Reliab_2) + 1,
    PI_TrPerf_Reliab_3rev = -PI_TrPerf_Reliab_3rev + max(PI_TrPerf_Reliab_3rev) + 1,
    
    
    PI_trPerformFunc = rowMeans(select(., matches("^PI_TrPerf_func"))),
    PI_trPerformRelia = select(., matches("^PI_TrPerf_reliab")) %>% rowMeans(),
    PI_trPerformHelp = rowMeans(select(., matches("^PI_TrPerf_helpful"))),
    
    
    
    
    
    # all vars
    
    
    # Technology satisfaction
    
    PI_TechSatisfaction = rowMeans(select(., matches("^PI_Tech_Satisfact"))),
    
    
    PI_trPerform = rowMeans(select(., matches("^PI_TrPerf"))),
    
    # Tech trusting intentions
    TechTrIntentions = rowMeans(select(., matches("^PI_TechTr_Intentio"))),
    
    # usage continuance intentions
    UseContIntentions = rowMeans(select(., matches("^Usage_Cont_Intention"))),
    
  ) %>% 
  
  select(-matches("^AN|^NN|^NE|^AE|^Q[1-9]{1,}")) ### tossing out the cluttered variables which were unified anyway into their grouped constructs


names(calcDat)















