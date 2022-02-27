##### 1: Load packages -----
# Main packages loaded: dplyr, readr, stringr, tidyr, purrr
# Packages used with namespace: pacman, fs, here
pacman::p_load(dplyr, readr, stringr, tidyr, purrr)

##### 2: Misc funcs -----
# Unnest a tibble to work with
unnest_origin <- function(main_table = nested_grocery, filter_table) {
  main_table$data[main_table$origin == filter_table] %>% 
    pluck(1)
}
# unnest_origin2 <- function(main_table = nested_grocery, 
#                           filter_table, 
#                           filter_col = origin, 
#                           value = data) {
#   main_table %>% 
#     filter({{ filter_col }} == filter_table) %>% 
#     unnest({{ value }}) %>% 
#     select(-1)
# }

# Trim all the cols to remove whitespace
trim_cols <- function(data) {
  data %>% 
    mutate(across(.cols = everything(), ~ str_trim(.x, side = "both")))
}

# Bind tables in a nested tibble
bind_nested_tables <- function(data, pattern, id) {
  data %>% 
    unnest_origin(paste0(pattern, id))
}

# Extract only ingredients
clean_ingredient <- function(column) {
  column %>% 
    as.character() %>% 
    str_split("(?<=[a-z]|™)[A-Z][a-z]+", n = 2) %>% 
    .[[1]] %>% 
    .[1]
}

##### 3: Load in collected data -----
# All files that end with .csv
data_files <- fs::dir_ls(here::here("data"), regexp = ".csv$")

# Load them all together in a list
data_list <- 
  data_files %>% 
  map(., ~ read_csv(.x, col_types = cols(.default = col_character()))) %>% 
  set_names(data_files %>% str_extract("[^/]*$") %>% str_remove(".csv"))

# Convert list into a nested tibble...extract each one/join later
nested_grocery <- 
  tibble::enframe(data_list, name = "origin", value = "data") %>% 
  arrange("origin")





##### 4A: Clean grocer data -----
# ALL: Remove any extra whitespace
nested_grocery %>% 
  mutate(data = map(data, trim_cols))

# LOCATION: No change & start extracting grocer_* tables
clean_grocer_location <- 
  nested_grocery %>% 
    unnest_origin("grocer_location")

# CATEGORY: Remove offer/promotion page since they contain products already 
# available within each sucategory
clean_grocer_category <- 
  nested_grocery %>% 
    unnest_origin("grocer_category") %>% 
    filter(!str_detect(category_link, "promotion")) %>% 
    rename("category_image_link" = image_link)

# SUBCATEGORY: No change
clean_grocer_subcategory <- 
  nested_grocery %>% 
    unnest_origin("grocer_subcategory")

# nested_grocery %>% 
#   unnest_origin("grocer_subcategory") %>%
#   mutate(store_name = 
#          subcategory_link %>% 
#          str_extract("(?<=store/).*(?=/)") %>% 
#          str_replace_all("-", " ") %>% 
#          str_trim("both") %>% 
#          str_to_title())

# ITEM: Price to numeric
clean_grocer_item <-
  nested_grocery %>% 
    unnest_origin("grocer_item") %>% 
    mutate(price = parse_number(price))

# nested_grocery %>% 
#   unnest_origin("grocer_item") %>% 
#   mutate(store_name = 
#          subcategory_link %>% 
#          str_extract("(?<=store/).*(?=/)")%>% 
#          str_replace_all("-", " ") %>% 
#          str_trim("both") %>% 
#          str_to_title(), 
#        price = parse_number(price), 
#        item = str_trim(item, "both"), 
#        weight = forcats::as_factor(weight)) %>% 
#   relocate(store_name, .before = 1L)

# STORE: separate details column
separator_detail <- paste("Min order amount", "Delivery within", "Delivery hours", 
                   "Payment method", sep = "|", collapse = "|")

new_col_names <- c("store_name", "min_order_amount", "delivery_within", 
                   "delivery_hours", "payment_method")

new_delivery_names <- c("delivery_start", "delivery_end", "delivery_timezone")
separator_delivery <- paste(" - ", " ", sep = "|", collapse = "|")
separator_payment <- paste("Online Payment", "Credit Card on delivery", 
                           "Cash on delivery", sep = "|", collapse = "|")
clean_grocer_store <- 
  nested_grocery %>% 
    unnest_origin("grocer_store") %>% 
    mutate(location = str_extract(location, "(?<= Stores in ).*")) %>% 
    separate(location, into = c("location", "city"), sep = " , ") %>% 
    separate(detail, into = new_col_names, sep = separator_detail) %>% 
    separate(delivery_hours, into = new_delivery_names, sep = separator_delivery) %>% 
    mutate(store_name = str_trim(store_name, "both"), 
           min_order_amount = parse_number(min_order_amount), 
           across(.cols = c("delivery_start", "delivery_end"), 
                  ~ hms::parse_hm(.)))


# payment_method_tibble <- tibble::tribble(
#   ~ index, ~ payment_method, 
#   1, "Online Payment", 
#   2, "Credit Card on delivery", 
#   3, "Cash on delivery", 
#   4, "Online Payment Credit Card on delivery", 
#   5, "Online Payment Cash on delivery", 
#   6, "Credit Card on delivery Cash on delivery", 
#   7, "Online Payment Credit Card on delivery Cash on delivery"
# )

# JOIN category, subcategory, item tables
grocer_products <- 
  clean_grocer_category %>% 
    left_join(clean_grocer_subcategory, by = "category_link") %>% 
    left_join(clean_grocer_item, by = "subcategory_link") %>% 
    select(store_name, category, subcategory, 
           item, weight, price, 
           category_image_link, item_image_link, 
           store_link) %>% 
    arrange(store_name, category, subcategory, item, price)

# Keep stores table
grocer_stores <- 
  clean_grocer_store %>% 
  select(-c(delivery_timezone, location_link)) %>% 
  arrange(location, city, store_name) # separate the payment method %>% 
  #separate_rows(payment_method, sep = str_glue("(?<={separator_payment})\\s(?={separator_payment})")) %>% 
  #mutate(payment_method = str_trim(payment_method, "both"))


##### 4B: Clean ocado data -----
# ALL: Remove any extra whitespace
nested_grocery %>% 
  mutate(data = map(data, trim_cols))

# CATEGORY: Turn category column into a factor
clean_ocado_category <- 
  nested_grocery %>% 
    unnest_origin("ocado_category") %>% 
    mutate(category = forcats::as_factor(category)) %>% 
    rename("category_link" = link)

# PRODUCT GENERAL: convert price to # & shelf_life to factor
# Same product_link & different category_link = same product
# The reason to keep duplicate titles/product_link = different weight or price
# Remove duplicates
clean_ocado_product_general <- 
  nested_grocery %>% 
    unnest_origin("ocado_product_general") %>% 
    mutate(price = parse_number(price), 
         shelf_life = forcats::as_factor(shelf_life)) %>% 
  rename("product" = title, "image_link" = images) %>% 
  distinct(product, weight, price, product_link, .keep_all = TRUE)

# PRODUCT REVIEW: bind all the rows & remove duplicates
# (e.g., the same product might have been collected more tha once if it was
# listed in different categories on ocado ---> review will be repeated---> 
# so, find distinct product - review pair)
# Nest the reviews data
clean_ocado_review <- 
  map_dfr(1:4, ~ bind_nested_tables(nested_grocery, 
                                  "ocado_review", 
                                  id = .x)) %>% 
    .[-3] %>% 
    distinct(product_link, reviews) %>% 
    nest(reviews = reviews)

# PRODUCT EXTRA: grab only ingredients
# Same product_link = same product ---> remove duplicates
clean_ocado_product_extra <- 
  map_dfr(1:4, ~ bind_nested_tables(nested_grocery, 
                                  "ocado_product_extra", 
                                  id = .x)) %>% 
  mutate(ingredient = map_chr(ingredient, clean_ingredient)) %>% 
  rename("num_of_reviews" = count) %>% 
  distinct(product_link, .keep_all = TRUE)

# NUTRITION: turn nutrition list into nested tibble
ocado_nutrition <- read_rds(here::here("data/ocado_nutrition.rds"))
clean_ocado_nutrition <- 
  ocado_nutrition %>% 
    tibble::enframe(name = "product_link", value = "nutrition") %>% 
    distinct(product_link, .keep_all = TRUE)

# JOIN ALL ocado tables
ocado_data_for_analysis <- 
  clean_ocado_product_extra %>% 
    left_join(clean_ocado_product_general, by = "product_link") %>% 
    left_join(clean_ocado_category, by = "category_link") %>% 
    left_join(clean_ocado_review, by = "product_link") %>% 
    left_join(clean_ocado_nutrition, by = "product_link") %>% 
    select(category, brand, product, price, weight, badge, shelf_life, country,  #8
           rating, num_of_reviews, recommend, ingredient, #12
           reviews, nutrition, #14
           image_link, product_link, category_link)#17
