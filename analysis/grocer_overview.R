##### 1: Load packages -----
# Main packages loaded:
# Packages used with namespace: pacman, here, tidyr
pacman::p_load(readr, dplyr, ggplot2, waffle, hrbrthemes, extrafont)
##### 2: Misc Funcs -----

##### 3: Load data -----
data_files <- fs::dir_ls(here::here("data"), regexp = "clean_grocer.*")
file_names <- stringr::str_extract(data_files, "clean_grocer_.*(?=.csv)")

clean_grocer_category <- read_csv(as.character(data_files[1]))
clean_grocer_location <- read_csv(as.character(data_files[2]))
clean_grocer_product <- read_csv(as.character(data_files[3]))
clean_grocer_store <- read_csv(as.character(data_files[4]))
clean_grocer_subcategory <- read_csv(as.character(data_files[5]))


# JOIN category, subcategory, item tables
grocer_products <- 
  clean_grocer_category %>% 
  left_join(clean_grocer_subcategory, by = "category_link") %>% 
  left_join(clean_grocer_product, by = "subcategory_link") %>% 
  select(store_name, category, subcategory, 
         item, weight, price, 
         category_image_link, item_image_link, 
         store_link) %>% 
  arrange(store_name, category, subcategory, item, price)



# Keep stores table
separator_payment <- paste("Online Payment", "Credit Card on delivery", 
                           "Cash on delivery", sep = "|", collapse = "|")

grocer_stores <- 
  clean_grocer_store %>% 
  select(-c(delivery_timezone, location_link)) %>% 
  arrange(location, city, store_name) %>% 
  tidyr::separate_rows(payment_method, 
                       sep = stringr::str_glue("(?<={separator_payment})\\s(?={separator_payment})")) %>% 
  mutate(payment_method = stringr::str_trim(payment_method, "both"))


##### 4: Analysis -----

### LOCATION
clean_grocer_location %>% 
  count(location) %>% 
  summarise(num_of_locations = sum(n))

### STORES
grocer_stores %>% 
  distinct(store_name, payment_method) %>% 
  count(store_name) %>% 
  count(n)
# Search: Payment options for a store
grocer_stores %>% 
  distinct(store_name, payment_method) %>% 
  filter(store_name == "Al Adil - Discovery Gardens")

### CATEGORIES
# Category buckets to split num of stores in each bucket
clean_grocer_category %>% 
  select(1:2) %>% 
  count(store_name) %>% 
  mutate(cat_bucket = case_when(
    n < 10 ~ "0-9", 
    n >= 10 & n < 20 ~ "10-19", 
    n >= 20 & n < 30 ~ "20-29", 
    n >= 30 & n < 40 ~ "30-37", 
    TRUE ~ "other"))
# Search: Num of categories for a store
clean_grocer_category %>%
  select(1:2) %>% 
  filter(store_name == "Al Adil - Discovery Gardens") %>% 
  count(store_name) %>% 
  summarise(num_of_cat = sum(n))

### SUBCATEGORIES
clean_grocer_subcategory %>% 
  left_join(clean_grocer_category, by = "category_link") %>% 
  count(store_name) %>% 
  mutate(subcat_bucket = case_when(
    n < 10 ~ "0-9", 
    n >= 10 & n < 20 ~ "10-19", 
    n >= 20 & n < 30 ~ "20-29", 
    n >= 30 & n < 46 ~ "30-45", 
    TRUE ~ "other"))
# Search: Num of subcategories for a store
clean_grocer_subcategory %>% 
  left_join(clean_grocer_category, by = "category_link") %>% 
  select(store_name, subcategory) %>% 
  count(store_name) %>% 
  summarise(num_of_subcat = sum(n))

### PRODUCTS
clean_grocer_product %>% 
  slice_max(n = 10, order_by = price) %>% 
  select(-1)

# Top 10 expensive stores
clean_grocer_product %>% 
  left_join(clean_grocer_subcategory, by = "subcategory_link") %>% 
  left_join(clean_grocer_category, by = "category_link") %>% 
  group_by(store_name) %>% 
  summarise(avg_price = mean(price)) %>% 
  slice_max(n = 10, order_by = avg_price)






# item_image <- magick::image_read(path = item_image_links %>% 
#                                    unlist())


##### continue...analysis -----

# category_image <- magick::image_read(path = category_image_links %>% 
#                                        unlist())

# grocer_stores %>% 
#   .[c(-1,-2)] %>% 
#   distinct(store_link, payment_method, .keep_all = T) %>% 
#   select(store_name, payment_method) %>% 
#   arrange(store_name)

