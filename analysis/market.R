##### 1: Load packages -----
# Main packages loaded:
# Packages used with namespace: pacman, fs, here
pacman::p_load(dplyr, readr, stringr, lubridate)

##### 2: Misc Funcs -----
popular_order_time <- function(time_group = c(month, weekday, hour), 
                               store_filter = NA) {
  output <- 
    grocery %>% 
      .[, c("store", "order_date", "order_time", "total_cost")] %>% 
      transmute(store = store, 
                month = month(order_date), 
                weekday = wday(order_date), 
                hour = hour(order_time), 
                price = total_cost) %>% 
      group_by(store, {{ time_group }}) %>% 
      summarise(n = n(), 
                avg_price = mean(price))
  
  if(is.na(store_filter)) {
    return(output %>% ungroup())
  } else {
    output %>% 
      filter(store == store_filter) %>% 
      ungroup()
  }
}

bucket_width <- function(column, bucket_size) {
  if(bucket_size == 50) {
    c(0, 50, 100, 150, 200, 250, 300, 350, 450, 500, max({{ column }}))
  } else if(bucket_size == 100) {
    c(0, 100, 200, 300, 400, 500, max({{ column }}))
  }
}
##### 3: Load data -----
data_files <- fs::dir_ls(here::here("data"), regexp = "_db")
file_names <- stringr::str_extract(data_files, "(?<=data/).*_db(?=\\..*)")

# 5,117,570 products bought
basket_db <- read_csv(as.character(data_files[1]))
# 96,614 customers
customer_db <- read_csv(as.character(data_files[2]))
# 250,000 orders
order_db <- read_csv(as.character(data_files[3]))

ocado <- read_rds(here::here("data/ocado_data_for_analysis.rds"))

# Join tables
grocery <- 
  basket_db %>% 
    group_by(basket_id, order_id) %>% 
    summarise(total_cost = sum(price)) %>% 
    ungroup() %>% 
    left_join(order_db, by = "order_id") %>% 
    left_join(customer_db, by = "customer_id")


##### 4A: Analysis OVERVIEW -----

# Frequency of orders (i.e., 26 customers ordered 10 times)
grocery %>% 
  count(customer_id, name = "orders") %>% 
  count(orders, name = "freq")

# Customer buckets
customer_buckets <- 
  grocery %>% 
    group_by(customer_name) %>% 
    summarise(total_spent = sum(total_cost), 
              num_of_orders = n()) 

customer_buckets %>% 
  mutate(spent_bucket = cut(total_spent, 
                            breaks = bucket_width(total_spent, 50), 
                            include.lowest = TRUE, right = FALSE)) %>% 
  count(spent_bucket, name = "num_of_orders")

# Most popular hour to order across both years
order_db %>% 
  count(hour = hour(order_time), name = "orders")

# Numer of orders per year/quarter
order_db %>% 
  group_by(year = year(order_date)) %>% 
  count(quarter = quarter(order_date), name = "orders")

# Top 10 stores shopped at
order_db %>% 
  count(store, name = "orders") %>% 
  slice_max(n = 10, order_by = orders)

# Top 10 bought products
basket_db %>% 
  count(product, name = "baskets") %>% 
  slice_max(n = 10, order_by = baskets) %>% 
  left_join(ocado, by = "product") %>% 
  .[, c("product", "baskets", "image_link")] %>% 
  distinct(product, .keep_all = TRUE)

# Avg basket price & total num of orders by store
grocery %>% 
  filter(store %in% c("Mamalu Kitchen")) %>% 
  group_by(store) %>% 
  summarise(avg_price = mean(total_cost), 
            num_of_orders = n())

# Which store has most repeat buys on average 
# (i.e., on average, a customer places 1.39 orders with union coop barsha)
grocery %>% 
  count(store, customer_name, name = "num_of_orders") %>% 
  group_by(store) %>% 
  summarise(orders_per_customer = mean(num_of_orders)) %>% 
  arrange(-orders_per_customer)

# Popular order time for a store
popular_order_time(time_group = month, store = "Union Coop - Barsha")
popular_order_time(time_group = weekday, "Mamalu Kitchen")
popular_order_time(time_group = hour, "Al Adil - Discovery Gardens")


  
  


  


##### 4B: Analysis MARKET BASKET -----
# continue...


##### 4C: Analysis SPATIAL -----
# lat/long continue...
