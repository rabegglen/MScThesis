### manipulation checks

source("./scripts/constructs.R")

pacman :: p_load(
  psych,
  car,
  PMCMRplus,
  PMCMR,
  install = TRUE
)



### extracting MC variables together with the treatment group vars



MCdat = calcDat %>% 
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
    ),
    
    
    
    ageNum = case_when(
      
      grepl("Younger than 20", Age_1, ignore.case = TRUE) ~ 1,
      grepl("20-29", Age_1, ignore.case = TRUE) ~ 2,
      grepl("30-39", Age_1, ignore.case = TRUE) ~ 3,
      grepl("40-49", Age_1, ignore.case = TRUE) ~ 4,
      grepl("50-59", Age_1, ignore.case = TRUE) ~ 5,
      grepl("60-69", Age_1, ignore.case = TRUE) ~ 6,
      grepl("70-79", Age_1, ignore.case = TRUE) ~ 7,
      grepl("Older than 80", Age_1, ignore.case = TRUE) ~ 8,
      
    ),
    
    
  ) %>% 
  mutate(
    group = as.factor(group)
  )




### anthropomorphism

anthro = MCdat %>% 
  
  select(matches("group|anthro")) %>% 
  
  mutate_at(
    vars(-matches("group|anthropo")),
    list(as.numeric)
  ) %>% 
  
  mutate(
    group = as.factor(group),
  ) %>% 
  
  mutate(
    MCAnthro = rowMeans(select(., matches("anthro_"))) ### creating the mean to simplify the variable
  ) 




# Kurskal Test
kruskal.test(MCAnthro ~ anthropo, data=anthro) # significant
anthromod = lm(MCAnthro ~ anthropo, data = anthro) %>% 
  summary()




### empathy

empath = MCdat %>% 
  
  select(matches("group|empath")) %>% 
  
  mutate_at(
    vars(-matches("group|^empathy$")),
    list(as.numeric)
  ) %>% 
  
  mutate(
    group = as.factor(group),
  ) %>% 
  
  mutate(
    MCEmpath = rowMeans(select(., matches("mc_empath")))
  )




# Kurskal Test
kruskal.test(MCEmpath ~ empathy, data=empath) # significant


# see what an LM can tell us? 
empathmod = lm(MCEmpath ~ empathy, data = empath) %>% 
  summary()

 


### make a regression table

stargazer(
  type = "html",
  title = "Manipulation Check",
  anthromod,
  empathmod,
  align = TRUE,
  out = "./tables/ManipulationCheck.html"
)










##### checking the demographics



lm(genderNum ~ group, data = MCdat) %>% 
  summary()

lm(eduNum ~ group, data = MCdat) %>% 
  summary()

lm(ageNum ~ group, data = MCdat) %>% 
  summary()



### control vars


MCdat$technologyPlayfulness
MCdat$techhnologyInnovativeness
MCdat$generalTrustingStance
MCdat$robotSelfEfficacy



# innovativeness

lm(techhnologyInnovativeness ~ group, data = MCdat) %>% 
  summary()

# Playfulness

lm(technologyPlayfulness ~ group, data = MCdat) %>% 
  summary()



# Trusting Stance

lm(generalTrustingStance ~ group, data = MCdat) %>% 
  summary()


# rotob self-efficacy

lm(robotSelfEfficacy ~ group, data = MCdat) %>% 
  summary()
