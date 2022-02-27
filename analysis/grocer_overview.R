##### 1: Load packages -----
# Main packages loaded:
# Packages used with namespace: pacman, here
pacman::p_load(dplyr, purrr, readr, stringr)

##### 2: Misc Funcs -----


##### 3: Load data -----
grocer_products <- read_csv(here::here("data/grocer_products_for_analysis.csv"))
grocer_stores <- read_csv(here::here("data/grocer_stores_for_analysis.csv"))
