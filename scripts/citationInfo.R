### citation information
packages = c(
  "pacman",
  "tidyverse",
  "readxl",
  "plspm",
  "reshape2",
  "plsdepot",
  "psych"
)



for(i in packages){

citation(i) %>% print()
}
