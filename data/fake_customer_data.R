##### 1: Load packages -----
# Main packages loaded:fabricatr, wakefield, randomNames, charlatan, magrittr, purrr
# Packages used with namespace: pacman, tibble, dplyr
pacman::p_load(fabricatr, wakefield, randomNames, charlatan, magrittr, purrr)

##### 2: Misc functions -----
unnest_origin <- function(main_table = nested_grocery, filter_table) {
  main_table$data[main_table$origin == filter_table] %>% 
    purrr::pluck(1)
}

select_products <- function(id = order_db$customer_id, num_of_products_mean = 20, sd = 5) {
  id %>% 
    map(., function(.x) {
      num_of_products <- round(rnorm(1, mean = num_of_products_mean, sd = sd))
      
      if(num_of_products > 5) {
        sample(product_prob$product, 
               size = num_of_products, 
               prob = product_prob$probs, 
               replace = FALSE)
      } else {
        num_of_products <- 6
        sample(product_prob$product, 
               size = num_of_products, 
               prob = product_prob$probs, 
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

##### 4: NOTES -----
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
product_prob <- 
  ocado %>% 
    dplyr::group_by(product) %>% 
    dplyr::summarise(score = as.numeric(num_of_reviews) + as.numeric(recommend)) %>% 
    dplyr::filter(!is.na(score)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(probs = score / sum(score))%>% 
    dplyr::arrange(desc(score))


##### 5: Create fake random customer data -----
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
  dplyr::left_join(ocado, by = "product") %>% 
  dplyr::select(1:3, price) %>% 
  dplyr::rename("basket_id" = ID) %>% 
  dplyr::distinct(basket_id, product, .keep_all = TRUE)

# readr::write_csv(basket_db, here::here("data/basket_db.csv"))
