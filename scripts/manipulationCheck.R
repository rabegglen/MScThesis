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
  select(matches("group|^MC_"))




### anthropomorphism

anthro = MCdat %>% 
  
  select(matches("group|anthro")) %>% 
  
  mutate_at(
    vars(-matches("group")),
    list(as.numeric)
  ) %>% 
  
  mutate(
    group = as.factor(group),
  ) %>% 
  
  mutate(
    MCAnthro = rowMeans(select(., matches("anthro"))) ### creating the mean to simplify the variable
  )




# Kurskal Test
kruskal.test(MCAnthro ~ group, data=anthro) # significant




### empathy

empath = MCdat %>% 
  
  select(matches("group|empath")) %>% 
  
  mutate_at(
    vars(-matches("group")),
    list(as.numeric)
  ) %>% 
  
  mutate(
    group = as.factor(group),
  ) %>% 
  
  mutate(
    MCEmpath = rowMeans(select(., matches("empath")))
  )




# Kurskal Test
kruskal.test(MCEmpath ~ group, data=empath) # significant


# see what an LM can tell us? 
lm(MCEmpath ~ group, data = empath) %>% 
  summary()


 
 