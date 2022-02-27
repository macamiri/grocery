##### 1: Load packages -----
# Main packages loaded:
# Packages used with namespace: pacman, here
pacman::p_load(dplyr, purrr, readr, stringr)

##### 2: Misc Funcs -----


##### 3: Load data -----
ocado <- read_rds(here::here("data/ocado_data_for_analysis.rds"))
