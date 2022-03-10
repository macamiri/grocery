##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(tidyverse)
library(stringr)

##### 2: Analysis -------------------------------------------------------------
# Availbale datasets in package
# data(package = "grocerycart")
# ?oc_data

data("oc_data")

### (A) COUNTRY
oc_country <- 
  oc_data %>% 
    filter(!is.na(country)) %>% 
    count(country) %>% 
    separate_rows(country, sep = ", ") %>% 
    group_by(country) %>% 
    summarise(products = sum(n)) %>% 
    arrange(desc(products))

### (B) BRAND
# Number of products per brand
oc_pro <- 
  oc_data %>% 
    select(brand, product) %>% 
    filter(!is.na(brand)) %>% 
    count(brand, name = "products") %>% 
    arrange(desc(products))

# Top 5 most expensive brands, on average (stores with >= 15 products)
oc_data %>% 
  semi_join(oc_pro %>% filter(products >= 15), by = "brand") %>% 
  group_by(brand) %>% 
  summarise(max_price = max(price, na.rm = TRUE), 
            min_price = min(price, na.rm = TRUE), 
            avg_price = mean(price, na.rm = TRUE), 
            median_price = median(price, na.rm = TRUE))
  

# Which of the top 5 brands with the most reviews 
# has the highest recommendation score
oc_top5_rev <- 
  oc_data %>% 
    filter(!is.na(recommend), !is.na(brand), !is.na(reviews)) %>% 
    group_by(brand) %>% 
    summarise(reviews = sum(num_of_reviews)) %>% 
    slice_max(n = 5, order_by = reviews)

oc_data %>% 
  select(brand, recommend) %>% 
  semi_join(oc_top5_rev, by = "brand") %>% 
  group_by(brand) %>% 
  summarise(avg_recommend = mean(recommend, na.rm = TRUE)) %>% 
  inner_join(oc_pro, by = "brand") %>% 
  arrange(desc(products))

### (C) BADGE
# Badge count & median product price by badge
oc_data %>% 
  select(badge, price) %>% 
  separate_rows(badge, sep = ", ") %>% 
  group_by(badge) %>% 
  summarise(products = n(), median_price = median(price), avg_price = mean(price)) %>% 
  arrange(desc(products))

# Which of the top 3 brands with the most products 
# has the highest % of vegetarian friendly products (veg/all)
oc_data %>% 
  select(brand, badge, product) %>% 
  filter(!is.na(brand)) %>% 
  separate_rows(badge, sep = ", ") %>% 
  filter(badge == "Suitable for vegetarians") %>% 
  count(brand, name = "veg_products") %>% 
  inner_join(oc_pro, by = "brand") %>% 
  mutate(perc_veg = veg_products / products) %>% 
  slice_max(n = 3, order_by = products)

# Which of the top 4 countries with the most products 
# has the highest % of vegetarian friendly products (veg/all)
oc_data %>% 
  select(country, badge, product) %>% 
  filter(!is.na(country)) %>% 
  separate_rows(badge, sep = ", ") %>% 
  filter(badge == "Suitable for vegetarians") %>% 
  count(country, name = "veg_products") %>% 
  inner_join(oc_country, by = "country") %>% 
  mutate(perc_veg = veg_products / products) %>% 
  slice_max(n = 4, order_by = products)

### SHELF LIFE
# Top 5 most common shelf life for products
oc_data %>% 
  filter(!is.na(shelf_life)) %>% 
  count(shelf_life, name = "products") %>% 
  slice_max(n = 5, order_by = products)

# Most common shelf life for each brand
oc_data %>% 
  filter(!is.na(shelf_life), !is.na(brand)) %>% 
  group_by(brand, shelf_life) %>% 
  summarise(products = n()) %>% 
  mutate(rank = dense_rank(desc(products))) %>% 
  filter(rank == 1) %>% 
  arrange(-products)

### PRODUCT
# Top 5 most reviewed products
oc_data %>% 
  select(product, rating, num_of_reviews, recommend, image_link) %>% 
  slice_max(n = 5, order_by = num_of_reviews)

# Top 5 highest rated products with >=100 reviews
oc_data %>% 
  filter(num_of_reviews >= 100) %>% 
  slice_max(n = 5, order_by = rating) %>% 
  select(product, rating, num_of_reviews, image_link)

### NUTRITION
# Collect product kcals
calories <- extract_energy(oc_data, item = "product", nutrition = "nutrition")
kcal <- extract_kcal(calories)

# Top 10 products with highest calories (ignoring weight of product)
kcal %>% 
  select(product, kcal) %>% 
  distinct(product, .keep_all = TRUE) %>% 
  filter(!is.na(kcal)) %>% 
  mutate(kcal = as.numeric(kcal)) %>% 
  slice_max(n = 10, order_by = kcal) %>% 
  inner_join(oc_data) %>% 
  select(product, kcal, image_link) %>% 
  distinct(product, .keep_all = TRUE)
  
### INGREDIENT
# # Collapse all ingredients into 1 vector
# oc_data %>%
#   select(ingredient) %>%
#   filter(!is.na(ingredient)) %>%
#   paste()

### REVIEWS
# # Filter out the products with no reviews written
# oc_true_reviews <- 
#   oc_data %>%
#     select(product, reviews) %>%
#     pluck(2) %>% 
#     map(., ~ !is.null(.x)) %>% 
#     unlist()
# 
# oc_reviews <- 
#   oc_data %>% 
#     select(product, reviews) %>% 
#     filter(oc_true_reviews) %>% 
#     distinct(product, .keep_all = TRUE)
# 
# oc_reviews %>% 
#   slice(5) %>% 
#   pluck(2)

         
