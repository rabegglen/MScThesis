#### Constructs

source("dataPreparation.R")


#### transforming all constructs to numeric

## getting all the different levels of answers

## general agreeance
unique(calcDat$RobotSE1_1) %>% na.omit()

## disconfirmation
unique(calcDat$PI_TrDisc_Helpful_3) %>% na.omit()

## satisfaction
unique(calcDat$PI_Tech_Satisfact1) %>% na.omit()
unique(calcDat$PI_Tech_Satisfact2) %>% na.omit()
unique(calcDat$PI_Tech_Satisfact3) %>% na.omit()
unique(calcDat$PI_Tech_Satisfact4) %>% na.omit()



## recoding

temp = calcDat %>% 
  
  select(-StartDate, -EndDate) %>% ## dates always cause trouble due to posixct standards interfering with other operations
  
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
  )


names(dat)


#### cronbach's alpha







