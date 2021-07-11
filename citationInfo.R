### citation information
packages = c(
  "pacman",
  "tidyverse",
  "readxl",
  "plspm",
  "plsdepot",
  "psych"
)



for(i in packages){

citation(i) %>% print()
}
