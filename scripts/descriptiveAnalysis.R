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


ggplot(hmCrossl, aes(x = Block, y = Item, fill = `Cross Loading`)) + 
  geom_raster() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Cross Loadings for Item Blocks") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  paletteer::scale_fill_paletteer_c("viridis::plasma") +
  ggsave(
    width = 30, height = 20, units = "cm", filename = "./graphs/CrossloadingsHeatMap.svg", dpi = 300
  ) 



## create a correlation heat map of all vars






### now the same as a Stargazer Table

crossloadings %>% 
  filter(
    !grepl("innov|trustingstance|gender|edunum|playful|robotse", ignore.case = TRUE, name)
  ) %>% 
  stargazer(., type = "html", title = "Cross Loadings", summary = FALSE, out = "tables/crossLoadings.html", rownames = FALSE)





