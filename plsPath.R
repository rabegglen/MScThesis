#### SEM 

source("constructs.R")

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




### create the path matrix with the relevant constructs


anthro                 =      c(0, 0, 0, 0, 0, 0, 0, 0, 0)
empathy                =      c(0, 0, 0, 0, 0, 0, 0, 0, 0)
InitialTechTrustExpect =      c(1, 1, 0, 0, 0, 0, 0, 0, 0)
PostIncTechTrustExpect =      c(0, 0, 1, 0, 0, 0, 0, 0, 0)
PostIncTechTrustDiconf =      c(0, 0, 0, 1, 0, 1, 0, 0, 0)
PostIncTechTrustPerfor =      c(0, 0, 0, 1, 0, 0, 0, 0, 0)
PostIncTechTrustSatisf =      c(0, 0, 0, 0, 1, 1, 0, 0, 0)
PostIncTechTrustIntent =      c(0, 0, 0, 0, 1, 1, 1, 0, 0)
IntentOfUsageContinuat =      c(0, 0, 0, 0, 0, 0, 1, 1, 0)


techPath = rbind(
  anthro,
  empathy,
  InitialTechTrustExpect,
  PostIncTechTrustExpect,
  PostIncTechTrustDiconf,
  PostIncTechTrustPerfor,
  PostIncTechTrustSatisf,
  PostIncTechTrustIntent,
  IntentOfUsageContinuat
                 )

colnames(techPath) = rownames(techPath)


innerplot(techPath, box.size = 0.1)

