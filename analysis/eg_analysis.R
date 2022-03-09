##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(tidyverse)
library(stringr)

##### 2: Analysis -------------------------------------------------------------
# Availbale datasets in package
# data(package = "grocerycart")
# ?eg_data

### (A) STORE
data("eg_store")

# Distribution of minimum order amount
eg_store %>% 
  count(min_order_amount, name = "stores") %>% 
  arrange(desc(stores))

# Popular payment methods - reshape payment method column
separator_payment <- paste("Online Payment", "Credit Card on delivery", 
                           "Cash on delivery", sep = "|", collapse = "|")

eg_payment <- 
  eg_store %>% 
  select(location, city, store_name, payment_method) %>% 
  arrange(location, city, store_name) %>% 
  separate_rows(payment_method, 
                sep = stringr::str_glue("(?<={separator_payment})\\s(?={separator_payment})")) %>% 
  mutate(payment_method = stringr::str_trim(payment_method, "both"))

eg_payment %>% 
  count(payment_method, name = "stores")

eg_payment %>% 
  count(store_name, name = "num_payment_methods") %>% 
  count(num_payment_methods, name = "stores")

# rshiny custom
eg_payment %>% 
  filter(store_name == "Al Adil - Discovery Gardens") %>% 
  select(payment_method) %>% 
  pluck(1)

### (B) CATEGORY
data("eg_category")

# Category count in stores
eg_category %>% 
  count(store_name, name = "categories") %>% 
  arrange(desc(categories))

# Category buckets for graphing purposes
eg_category %>% 
  select(1:2) %>% 
  count(store_name) %>% 
  mutate(cat_bucket = case_when(
    n < 10 ~ "0-9", 
    n >= 10 & n < 20 ~ "10-19", 
    n >= 20 & n < 30 ~ "20-29", 
    n >= 30 & n < 40 ~ "30-37", 
    TRUE ~ "other")) %>% 
  count(cat_bucket)

# rshiny custom
eg_category %>% 
  select(store_name, category) %>% 
  filter(store_name == "Al Adil - Discovery Gardens") %>% 
  count(store_name, name = "categories") %>% 
  pluck(2)

### (C) SUBCATEGORY
data("eg_subcategory")

eg_sub <- 
  eg_subcategory %>% 
    inner_join(eg_category, by  = "category_link")

# Subcategory count in stores
eg_sub %>% 
  count(store_name, name = "subcategories") %>% 
  arrange(desc(subcategories))

# Subategory buckets for graphing purposes
eg_sub %>% 
  count(store_name) %>% 
  mutate(subcat_bucket = case_when(
    n < 10 ~ "0-9", 
    n >= 10 & n < 20 ~ "10-19", 
    n >= 20 & n < 30 ~ "20-29", 
    n >= 30 & n < 46 ~ "30-45", 
    TRUE ~ "other")) %>% 
  count(subcat_bucket)

# rshiny custom
eg_sub %>% 
  count(store_name, name = "subcategories") %>% 
  filter(store_name == "Al Adil - Electra") %>% 
  pluck(2)

### (C) PRODUCT
data("eg_product")

# Top 10 expensive products (in GBP)
eg_product %>% 
  slice_max(n = 10, order_by = price) %>% 
  select(item, weight, price, item_image_link)

# Median price & number of products per store
eg_data %>% 
  group_by(store_name) %>% 
  summarise(median_price = median(price), products = n())

## Read image
# item_image <- magick::image_read(path = item_image_links %>% 
#                                    unlist())
