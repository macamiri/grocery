##### 1: Load packages -----
# Main packages loaded:fabricatr, wakefield, randomNames, charlatan, magrittr, purrr
# Packages used with namespace: pacman, readr, here, tibble, dplyr
# install.packages("pacman")
# pacman::p_install("fabricatr", "wakefield", "randomNames", "charlatan", 
#                   "magrittr", "purrr", "readr", "here", "tibble", "dplyr")
pacman::p_load(fabricatr, wakefield, randomNames, charlatan, magrittr, purrr)

##### 2: Misc functions -----
unnest_origin <- function(main_table = nested_grocery, filter_table) {
  main_table$data[main_table$origin == filter_table] %>% 
    purrr::pluck(1)
}

select_products <- function(id = order_db$customer_id, num_of_products_mean = 26, sd = 4) {
  id %>% 
    map(., function(.x) {
      num_of_products <- round(rnorm(1, mean = num_of_products_mean, sd = sd))
      
      if(num_of_products >= 5) {
        sample(product_prob$product, 
               size = num_of_products, 
               prob = product_prob$probs, 
               replace = FALSE)
      } else {
        num_of_products <- 5
        sample(product_prob$product, 
               size = num_of_products, 
               prob = product_prob$probs, 
               replace = FALSE)
      }
    }) %>% 
    map(., ~ paste0(.x, collapse = "@")) %>% 
    unlist()
}

select_products_funmart <- function(id = order_funmart_db$customer_id, num_of_products_mean = 9, sd = 2) {
  id %>% 
    map(., function(.x) {
      num_of_products <- round(rnorm(1, mean = num_of_products_mean, sd = sd))
      
      if(num_of_products >= 3) {
        sample(product_prob_funmart$product, 
               size = num_of_products, 
               prob = product_prob_funmart$probs, 
               replace = FALSE)
      } else {
        num_of_products <- 3
        sample(product_prob_funmart$product, 
               size = num_of_products, 
               prob = product_prob_funmart$probs, 
               replace = FALSE)
      }
    }) %>% 
    map(., ~ paste0(.x, collapse = "@")) %>% 
    unlist()
}

##### 3: Load grocery data *_for_analysis -----
stores <- readr::read_csv(here::here("data/clean_grocer_store.csv"))
products <- readr::read_csv(here::here("data/grocer_products_for_analysis.csv"))
ocado <- readr::read_rds(here::here("data/ocado_data_for_analysis.rds"))

##### 4: NOTES: rules related to how data is generated -----
### order_db
## order_date: 
# 40% of orders from 2020 & 60% of orders from 2021
# 30% 1st half of the year, 70% second half of the year
# (.4*.3)/183 #1-183
# (.4*.7)/183 #184-366
# (.6*.3)/183 #367-549
# (.6*.7)/182 #550-731

## order_time: probability estimates
# 5% of orders from 00:00 to 8:00 am - .05/80 (1-81)
# 20% of orders from 8:00 to 10:00 am - .20/20 (81-101)
# 25% of orders from 10:00 to 12:00 pm - .25/20 (101-121)
# 25% of orders from 12:00 to 6:00 pm - .25/60 (121-181)
# 15% of orders from 6:00 to 10:00 pm - .15/40 (181-221)
# 10% of orders from 10:00 to 12:00 am - .1/20 (221-241)

## store: prob of ordering from each store according the the # of products
store_prob <- 
  stores %>% 
    dplyr::inner_join(products, by = "store_name") %>% 
    dplyr::group_by(store_name) %>% 
    dplyr::summarise(products = dplyr::n()) %>% 
    dplyr::mutate(probs = products / sum(products)) %>% 
    dplyr::arrange(desc(products))

### basket_db: prob of ordering a product is based on num of reviews + % recommend
num_of_products <- 
  products %>% 
  dplyr::filter(!is.na(item)) %>% 
  dplyr::distinct(item) %>% 
  dplyr::rename("product" = item) %>% 
  nrow()

score_column <- 
  sample(1:39, size = num_of_products, replace = TRUE) %>% 
  tibble::tibble(score = .)

# [remove same items across all stores]
grocer_products <- 
  products %>% 
    dplyr::filter(!is.na(item)) %>% 
    dplyr::distinct(item) %>% 
    dplyr::rename("product" = item) %>% 
    dplyr::bind_cols(score_column)

product_prob <- 
  ocado %>% 
    dplyr::group_by(product) %>% 
    dplyr::summarise(score = ceiling((as.numeric(num_of_reviews) + as.numeric(recommend)) * .5)) %>% 
    dplyr::filter(!is.na(score)) %>% 
    dplyr::ungroup() %>% 
    dplyr::bind_rows(grocer_products) %>% 
    dplyr::mutate(probs = score / sum(score)) %>% 
    dplyr::arrange(desc(score))

product_prob_funmart <- 
  tibble::tibble(
    product = sample(product_prob$product, size = 200, replace = FALSE)) %>% 
    dplyr::mutate(probs = probs(j = 200))

# Prices for all 12,539 products [remove same items across all stores]
prices <- 
  ocado %>% 
    dplyr::group_by(product) %>% 
    dplyr::mutate(score = as.numeric(num_of_reviews) + as.numeric(recommend)) %>% 
    dplyr::filter(!is.na(score)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(product, price) %>% 
    dplyr::bind_rows(products %>% 
                       dplyr::filter(!is.na(item)) %>% 
                       dplyr::distinct(item, .keep_all = TRUE) %>% 
                       dplyr::select(item, price) %>% 
                       dplyr::rename("product" = item))
  

##### 5A: Create fake random customer data -----
num_of_customers <- 100000
num_of_orders <- 250000

# Customer database table - keep distinct names
customer_db <- fabricate(
  N = num_of_customers, 
  customer_name = randomNames(n = N, name.sep = " ", name.order = "first.last"), 
  long = ch_position(n = N, bbox = c(51, 22.50, 56.25, 26)) %>% purrr::map_chr(., ~ .[1]), 
  lat = ch_position(n = N, bbox = c(51, 22.50, 56.25, 26)) %>% purrr::map_chr(., ~ .[2]),
) %>% 
  tibble::as_tibble() %>% 
  dplyr::distinct(customer_name, .keep_all = TRUE) %>% 
  dplyr::rename("customer_id" = ID)

# readr::write_csv(customer_db, here::here("data/customer_db.csv"))

# Orders database table
order_db <- fabricate(
  N = num_of_orders, 
  customer_id = as.character(sample(customer_db$customer_id, size = N, replace = TRUE)), 
  order_date = date_stamp(n = N, 
                          start = as.Date("2020-01-01"), 
                          k = 731, 
                          by = "1 day", 
                          prob = c(rep(0.0006557377, 183), 
                                   rep(0.001530055, 183), 
                                   rep(0.0009836066, 183), 
                                   rep(0.002307692, 182))), 
  order_time = hour(n = N, 
                    x = seq(from = 0, to = 23.9, by = .1), 
                    prob = c(rep(0.000625, 80), 
                             rep(0.01, 20), 
                             rep(0.0125, 20), 
                             rep(0.004166667, 60), 
                             rep(0.00375, 40), 
                             rep(0.005, 20))), 
  store = sample(store_prob$store_name, 
                 prob = store_prob$probs, 
                 size = N, 
                 replace = TRUE),
) %>% 
  tibble::as_tibble() %>% 
  dplyr::rename("order_id" = ID)

# readr::write_csv(order_db, here::here("data/order_db.csv"))

# Basket line item database table - join later if need more ocado product info
basket_db <- fabricate(
  N = num_of_orders, 
  order_id = as.character(sample(order_db$order_id, size = N, replace = FALSE)), 
  product = select_products(), 
) %>% 
  tibble::as_tibble() %>% 
  tidyr::separate_rows(product, sep = "@") %>% 
  dplyr::left_join(prices, by = "product") %>% 
  dplyr::select(1:3, price) %>% 
  dplyr::rename("basket_id" = ID) %>% 
  dplyr::distinct(basket_id, product, .keep_all = TRUE)

# readr::write_csv(basket_db, here::here("data/basket_db.csv"))

##### 5B: Create fake random customer data for 1 store (funmart) -----
num_of_customers <- 1000
num_of_orders <- 2500

# Customer database table - keep distinct names
customer_funmart_db <- fabricate(
  N = num_of_customers, 
  customer_name = randomNames(n = N, name.sep = " ", name.order = "first.last"), 
  long = ch_position(n = N, bbox = c(51, 22.50, 56.25, 26)) %>% purrr::map_chr(., ~ .[1]), 
  lat = ch_position(n = N, bbox = c(51, 22.50, 56.25, 26)) %>% purrr::map_chr(., ~ .[2]),
) %>% 
  tibble::as_tibble() %>% 
  dplyr::distinct(customer_name, .keep_all = TRUE) %>% 
  dplyr::rename("customer_id" = ID)

# readr::write_csv(customer_funmart_db, here::here("data/customer_funmart_db.csv"))

# Orders database table
order_funmart_db <- fabricate(
  N = num_of_orders, 
  customer_id = as.character(sample(customer_funmart_db$customer_id, size = N, replace = TRUE)), 
  order_date = date_stamp(n = N, 
                          start = as.Date("2020-01-01"), 
                          k = 731, 
                          by = "1 day", 
                          prob = c(rep(0.0006557377, 183), 
                                   rep(0.001530055, 183), 
                                   rep(0.0009836066, 183), 
                                   rep(0.002307692, 182))), 
  order_time = hour(n = N, 
                    x = seq(from = 0, to = 23.9, by = .1), 
                    prob = c(rep(0.000625, 80), 
                             rep(0.01, 20), 
                             rep(0.0125, 20), 
                             rep(0.004166667, 60), 
                             rep(0.00375, 40), 
                             rep(0.005, 20))), 
  store = rep("Funmart", N),
) %>% 
  tibble::as_tibble() %>% 
  dplyr::rename("order_id" = ID)

# readr::write_csv(order_funmart_db, here::here("data/order_funmart_db.csv"))


# Basket line item database table - join later if need more ocado product info
basket_funmart_db <- fabricate(
  N = num_of_orders, 
  order_id = as.character(sample(order_funmart_db$order_id, size = N, replace = FALSE)), 
  product = select_products_funmart(), 
) %>% 
  tibble::as_tibble() %>% 
  tidyr::separate_rows(product, sep = "@") %>% 
  dplyr::left_join(prices, by = "product") %>% 
  dplyr::select(1:3, price) %>% 
  dplyr::rename("basket_id" = ID) %>% 
  dplyr::distinct(basket_id, product, .keep_all = TRUE)

# readr::write_csv(basket_funmart_db, here::here("data/basket_funmart_db.csv"))
