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
  select(matches("group|^MC_")) %>% 
  mutate(
    anthropo = case_when(
      
      grepl("^A", group) ~ 1,
      grepl("^N", group) ~ 0
      
    ) %>% as.factor(),
    
    empathy = case_when(
      
      grepl("E$", group) ~ 1,
      grepl("N$", group) ~ 0
      
    ) %>% as.factor(),
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
anthromod = lm(MCAnthro ~ anthropo, data = anthro) 




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
empathmod = lm(MCEmpath ~ empathy, data = empath) 

 


### make a regression table

stargazer(
  type = "html",
  title = "Manipulation Check",
  anthromod,
  empathmod,
  align = TRUE,
  out = "./tables/ManipulationCheck.html"
)



 