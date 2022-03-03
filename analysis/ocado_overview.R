##### 1: Load packages -----
# Main packages loaded:
# Packages used with namespace: pacman, here, tidyr
pacman::p_load(dplyr, purrr, readr, stringr)

##### 2: Misc Funcs -----
collect_energy <- function(df = ocado) {
  ocado_energy <- 
    df %>% 
    select(product, nutrition) 
  
  along <- 1:length(ocado_energy$product)
  
  along %>% 
    map_dfr(., function(.x) {
      tryCatch(
        expr = {
          
          product <- 
            ocado_energy %>% 
            pluck(1) %>% 
            pluck(.x)
          
          energy <- 
            ocado_energy %>% 
            pluck(2) %>% 
            pluck(.x) %>% 
            filter(str_detect(`Typical Values`, regex("energy(\\skcal)?", ignore_case = TRUE))) %>% 
            select(2) %>% .[[1, 1]] %>% as.character()
          
          cat(crayon::blue("Completed ", .x, "\n"))
          
          tibble::tibble(
            product = product, 
            energy = energy
          )
        }, 
        error = function(e) {}
      )
    })
}
extract_kcal <- function(df) {
  calories %>% 
    mutate(energy = str_replace_all(energy, " ", ""), 
           kcal = map_chr(energy, function(.x) {
             if(!is.na(.x)) {
               if(str_detect(.x, "\\d$")) {
                 if(!is.na(as.numeric(.x) / 4.184)) {
                   as.numeric(.x) / 4.184
                 } else {
                   str_extract(.x, "(?<=/)\\d+")
                 }
               } else if(str_detect(.x, "kcal")) {
                 str_extract(.x, "\\d+\\.\\d+(?=kcal)|\\d+(?=kcal)")
               } else if(str_detect(.x, "\\d(?=(k|K)J$)")) {
                 as.numeric(str_extract(.x, "\\d+|\\d+\\.\\d+(?=kJ$)")) / 4.184
               } else if(str_detect(.x, "\\)$")) {
                 str_extract(.x, "(?<=\\()\\d+(?=\\))")
               } else {
                 NA
               }
             } else {
               NA
             }
           }))
}

##### 3: Load data -----
ocado <- read_rds(here::here("data/ocado_data_for_analysis.rds"))

# Data without review/ingredient/nutrition
ocado2 <- 
  ocado %>% 
  select(1:11, 15)

# data_files <- fs::dir_ls(here::here("data"), regexp = "clean_ocado.*")
# file_names <- stringr::str_extract(data_files, "clean_ocado_.*(?=.csv|.rds)")
# 
# clean_ocado_category <- read_csv(as.character(data_files[1]))
# clean_ocado_nutrition <- read_rds(as.character(data_files[2]))
# clean_ocado_product_extra <- read_csv(as.character(data_files[3]))
# clean_ocado_product_general <- read_csv(as.character(data_files[4]))
# clean_ocado_review <- read_review(as.character(data_files[5]))

##### 4: Analysis -----

### 
# Top 5 country of origin products
ocado2 %>% 
  count(country) %>% 
  slice_max(n = 10, order_by = n)

# Badge count
ocado2 %>% 
  tidyr::separate_rows(badge, sep = ", ") %>% 
  count(badge)

### BRANDS
# Top 5 frequent brands
ocado2 %>% 
  count(brand) %>% 
  slice_max(n = 5, order_by = n)

# Top 5 brands with highest recommendation score, 
# with >= 100 reviews across all products (most popular)
ocado2 %>% 
  group_by(brand) %>% 
  filter(!is.na(recommend) | !is.na(brand), 
         num_of_reviews >= 100) %>% 
  summarise(avg_recommendation = mean(recommend, na.rm = T), 
            num_reviews = sum(num_of_reviews, na.rm = TRUE)) %>% 
  slice_max(n = 5, order_by = avg_recommendation)

# Top 5 expensive brands with >=15 products
ocado2 %>% 
  group_by(brand) %>% 
  filter(!is.na(price) | !is.na(brand)) %>% 
  summarise(avg_price = mean(price, na.rm = T), 
            num_products = n()) %>% 
  filter(num_products >= 15) %>% 
  slice_max(n = 5, order_by = avg_price)
  
# Top 6 brands with highest number of products suitable for vegetarians
ocado2 %>% 
  tidyr::separate_rows(badge, sep = ", ") %>% 
  filter(badge == "Suitable for vegetarians") %>% 
  group_by(brand) %>% 
  summarise(vegetarian = n()) %>% 
  inner_join(ocado2 %>% count(brand), by = "brand") %>% 
  rename("total_products" = n) %>% 
  mutate(perc_of_veg = vegetarian/total_products, 
         perc_of_veg = scales::percent(perc_of_veg)) %>% 
  slice_max(n = 5, order_by = vegetarian)

# Avg price for each of the 3 categories
ocado2 %>% 
  group_by(category) %>% 
  summarise(avg_price = mean(price, na.rim = TRUE))

# Top 5 most common shelf life by the top 5 brands
ocado2 %>% 
  inner_join(ocado2 %>% 
               count(brand) %>% 
               slice_max(n = 5, order_by = n), 
             by = "brand") %>% 
  count(brand, shelf_life, name = "shelf_count") %>% 
  group_by(brand) %>% 
  mutate(position = rank(-shelf_count)) %>% 
  filter(position == 1) %>% 
  ungroup() %>% 
  slice_max(n = 5, order_by = shelf_count)

### SHELF LIFE
ocado2 %>% 
  count(shelf_life) %>% 
  slice_max(n = 5, order_by = n)

### PRODUCT
# Top 5 rated products with >=100 reviews
ocado2 %>% 
  filter(num_of_reviews >= 100) %>% 
  slice_max(n = 5, order_by = rating) %>% 
  select(product, rating, num_of_reviews, image_link)


### NUTRITION
# Collect product kcals
calories <- collect_energy(ocado)
kcal <- extract_kcal(calories)

# Top 10 products with highest calories
kcal %>% 
  mutate(kcal = as.numeric(kcal)) %>% 
  slice_max(n = 20, order_by = kcal) %>% 
  left_join(ocado, by = "product") %>% 
  select(product, kcal, weight) %>% 
  distinct(product, .keep_all = TRUE) %>% 
  slice_max(n = 10, order_by = kcal) %>% 
  left_join(ocado, by = c("product", "weight")) %>% 
  select(product, kcal, weight, brand, image_link)


### INGREDIENT/REVIEWS
# continue...

