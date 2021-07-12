### Descriptive analyses and graphics

source("./scripts/plsPath4.R")
dir.create("./tables")
dir.create("./graphs")

### some simple descriptive statistics

## Age

desc = calcDat %>% 
  count(Age_1)

## language

lang = calcDat %>% 
  count(language)


## education

edu = calcDat %>% 
  count(Edu) %>% 
  mutate(
    percentage = n / sum(n) * 100
  )


## distribution within the different treatement levels

groups = calcDat %>% 
  count(., group)





### get some heat maps


## create matrix
crossloadings = anthroModel4$crossloadings

loadingNames = crossloadings$name


hmCrossl = crossloadings %>% 
  select(-block) %>% 
  melt(., value.name = "Cross Loading") %>% 
  rename(
    Block = variable, Item = name 
  ) %>% 
  filter(
    !grepl("innov|trustingstance|gender|edunum|playful|robotse", ignore.case = TRUE, Block) & !grepl("innov|trustingstance|gender|edunum|playful|robotse", ignore.case = TRUE, Item)
  ) 



## create raster plot

# make nicer names for the items for the graph


rasterDat = hmCrossl %>% 
  

  mutate(., 
         
         Item = str_replace_all(Item, "Usage_Cont_Intention_", "Usage Continuance Intention "),
         Item = str_replace_all(Item, "PI_", "PI-"),
         Item = str_replace_all(Item, "anthroEmpath", "Anthro (P) x Empathy (P)"),
         Item = str_replace_all(Item, "TrDisc", "Trusting Disconfirmation"),
         Item = str_replace_all(Item, "TrEx|trEx", "Trusting Expectations "),
         Item = str_replace_all(Item, "TrPerf", "Trusting Performance"),
         Item = str_replace_all(Item, "empathInter", "Empathy (P) x Failure (P)"),
         Item = str_replace_all(Item, "anthroInter", "Anthro (P) x Failure (P)"),
         Item = str_replace_all(Item, "treatAnthroEmpath", "Anthro (T) x Empathy "),
         Item = str_replace_all(Item, "TreatAnthroInter", "Anthro (T) x Failure (P) "),
         Item = str_replace_all(Item, "TreatEmpathInter", "Empathy (T) x Failure (P) "),
         Item = str_replace_all(Item, "reUse", "re-use"),
         Item = str_replace_all(Item, "empathy", "Empathy (T)"),
         Item = str_replace_all(Item, "anthropo", "Anthro (T)"),
         Item = str_replace_all(Item, "MC_serviceFailure_", "Failure (P)"),
         
         Item = str_replace_all(Item, "_", " ")
         
         
            ) %>% 
  
  mutate(
    Block = str_replace_all(Block, "anthroTreat", "Anthro (T)"),
    Block = str_replace_all(Block, "empathTreat", "Empathy (T)"),
    Block = str_replace_all(Block, "treatAnthroEmpath", "Anthro (T) x Empathy (T)"),
    Block = str_replace_all(Block, "treatAnthroInter", "Anthro (T) x Failure (P)"),
    Block = str_replace_all(Block, "treatEmpathInter", "Empathy (T) x Failure (P)"),
    Block = str_replace_all(Block, "anthro", "Anthro (P)"),
    Block = str_replace_all(Block, "empathy", "Empathy (P)"),
    Block = str_replace_all(Block, "AnthroEmpath", "Empathy (P) x Anthro (P)"),
    Block = str_replace_all(Block, "interEmpath", "Empathy (P) x Failure (P)"),
    Block = str_replace_all(Block, "interAnthro", "Anthro (P) x Failure (P)"),
    Block = str_replace_all(Block, "servFailure", "Failure (P)"),
    Block = str_replace_all(Block, "PostInc", "PI-"),
    Block = str_replace_all(Block, "interEmpath", "Empathy (P) x Failure (P)"),
    Block = str_replace_all(Block, "reUse", "re-use"),
    Block = str_replace_all(Block, "reUse", "re-use"),
  )


unique(rasterDat$Item)
unique(rasterDat$Block)



ggplot(rasterDat, aes(x = factor(Block, levels = unique(Block)), y = factor(Item, levels = unique(Item)), fill = `Cross Loading`)) + 
  geom_raster() +
  # scale_y_discrete(guide = guide_axis(angle = -45)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Cross Loadings for Item Blocks") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  paletteer::scale_fill_paletteer_c("viridis::plasma") +
  
  labs(
    x = "Block",
    y = "Items"
  ) +
  
  ggsave(
    width = 30, height = 20, units = "cm", filename = "./graphs/CrossloadingsHeatMap.svg", dpi = 300
  ) + 

  ggsave(
    width = 30, height = 20, units = "cm", filename = "./graphs/CrossloadingsHeatMap.png", dpi = 300
  )







### now the same as a Stargazer Table

crossloadings %>% 
  filter(
    !grepl("innov|trustingstance|gender|edunum|playful|robotse", ignore.case = TRUE, name)
  ) %>% 
  stargazer(., type = "html", title = "Cross Loadings", summary = FALSE, out = "tables/crossLoadings.html", rownames = FALSE)


### tables for CA, etc.



unidim = anthroModel4$unidim %>% 
  mutate(
  `Latent Variable` = rownames(.),
  Mode = "Reflective"
  ) %>% 
  select(`Latent Variable`, matches(".*"))


unidim %>% 
  stargazer(., type = "html", title = "Unidimensionality", summary = FALSE, out = "tables/unidim.html", rownames = FALSE)

