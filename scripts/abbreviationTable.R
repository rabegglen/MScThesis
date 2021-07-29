### abbreviation table

pacman :: p_load(
  tidyverse,
  stargazer,
  readxl,
  install = TRUE
)



abbr = list.files("./Data/", full.names = TRUE) %>%
  .[grepl("abbrevia.+\\.xlsx", ., ignore.case = TRUE)] %>% 
  read_excel(.) 


abbr %>% 
  stargazer(
    ., type = "html", summary = FALSE, out = "./tables/abbreviations.html", rownames = FALSE
  )
