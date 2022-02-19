# Clean collected sample data: location ---> store ---> (sub)category ---> item

##### 1: Load packages -----
# Main packages loaded: purrr, dplyr, readr, stringr
# Packages used with namespace:
pacman::p_load(purrr, dplyr, readr, stringr)

##### 2: Load in sample data -----
data_files <- fs::dir_ls(here::here("data"), regexp = ".csv$")

data_tibble <- 
  data_files %>% 
  map(., ~ read_csv(.x, col_types = cols(.default = col_character()))) %>% 
  set_names(data_files %>% str_extract("[^/]*$") %>% str_remove(".csv"))

nested_grocery <- tibble::enframe(data_tibble, name = "layer", value = "data")


