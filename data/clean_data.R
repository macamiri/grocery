# Clean collected sample data: location ---> store ---> (sub)category ---> item

##### 1: Load packages -----
# Main packages loaded: purrr, dplyr, readr, stringr
# Packages used with namespace:
pacman::p_load(purrr, dplyr, readr, stringr, tidyr)


##### 2: Load in sample data -----
data_files <- fs::dir_ls(here::here("data"), regexp = ".csv$")

data_tibble <- 
  data_files %>% 
  map(., ~ read_csv(.x, col_types = cols(.default = col_character()))) %>% 
  set_names(data_files %>% str_extract("[^/]*$") %>% str_remove(".csv"))

nested_grocery <- 
  tibble::enframe(data_tibble, name = "layer", value = "data") %>% 
  arrange("location")


##### 3: Clean each tibble -----

### (A) Location: already clean/no edits -----
nested_grocery$data[[which(nested_grocery$layer == "location")]]

### (B) Store: location and details columns -----
# location coulmn
nested_grocery$data[[which(nested_grocery$layer == "store")]] <- 
  nested_grocery$data[[which(nested_grocery$layer == "store")]] %>% 
    mutate(location = 
             location %>% 
             str_extract("(?<= Stores in ).*")) %>% 
    separate(location, into = c("location", "city"), sep = ",") %>% 
    mutate(location = location %>% str_trim("both"), 
           city = city %>% str_trim("both"))

# details column
separator <- paste("Min order amount", 
                   "Delivery within", 
                   "Delivery hours", 
                   "Payment method", 
                   sep = "|", collapse = "|")

new_col_names <- c("store_name", 
                   "min_order_amount", 
                   "delivery_within", 
                   "delivery_hours", 
                   "payment_method")

new_delivery_names <- c("delivery_start", "delivery_end", "delivery_timezone")
separator_delivery <- paste(" - ", " ", sep = "|", collapse = "|")

nested_grocery$data[[which(nested_grocery$layer == "store")]] <- 
  nested_grocery$data[[which(nested_grocery$layer == "store")]] %>% 
  separate(details, into = new_col_names, sep = separator) %>% 
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


### (C) Category: category column -----
# Turn category column into a factor
nested_grocery$data[[which(nested_grocery$layer == "category")]] <- 
  nested_grocery$data[[which(nested_grocery$layer == "category")]] %>% 
    mutate(category = forcats::as_factor(category))
### (D) Subcategory: store name and subcategory columns -----
# store_name & subcategory column
nested_grocery$data[[which(nested_grocery$layer == "subcategory")]] <- 
  nested_grocery$data[[which(nested_grocery$layer == "subcategory")]] %>% 
    mutate(store_name = 
             subcategory_link %>% 
             str_extract("(?<=store/).*(?=/)") %>% 
             str_replace_all("-", " ") %>% 
             str_trim("both") %>% 
             str_to_title(), 
           subcategory = forcats::as_factor(subcategory))

### (E) item: store name, price, item and weight columns -----
# all 4 columns at once
nested_grocery$data[[which(nested_grocery$layer == "item")]] <- 
  nested_grocery$data[[which(nested_grocery$layer == "item")]] %>% 
    mutate(store_name = 
             subcategory_link %>% 
             str_extract("(?<=store/).*(?=/)")%>% 
             str_replace_all("-", " ") %>% 
             str_trim("both") %>% 
             str_to_title(), 
           price = parse_number(price), 
           item = str_trim(item, "both"), 
           weight = forcats::as_factor(weight)) %>% 
  relocate(store_name, .before = 1L)
