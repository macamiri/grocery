##### 1: Load packages -----
# Main packages loaded:
# Packages used with namespace: pacman, fs, here, plyr
pacman::p_load(dplyr, readr, stringr, lubridate, arules)

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
      summarise(num_of_orders = n(), 
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

unseparate_rows <- function(data = basket_db, 
                            keep_cols = c("basket_id", "product")) {
  data %>% 
    select({{ keep_cols }}) %>% 
    plyr::ddply(.variables = "basket_id", 
                .fun = function(df1) {paste(df1$product, collapse = ",")}) %>% 
    dplyr::as_tibble() %>% 
    rename("itemset" = V1)
}
##### 3: Load data -----
data_files <- fs::dir_ls(here::here("data"), regexp = "_db")
file_names <- stringr::str_extract(data_files, "(?<=data/).*_db(?=\\..*)")

# 6,501,624 products bought
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


##### 4A: Analysis OVERVIEW [confirms data generation rules] -----

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

# 50 or 100 bucket
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

# Top stores shopped at
order_db %>% 
  count(store, name = "orders") %>% 
  mutate(order_perc = orders / sum(orders)) %>% 
  arrange(desc(order_perc))

# Top products bought
basket_db %>% 
  count(product, name = "baskets") %>% 
  mutate(basket_perc = baskets / sum(baskets)) %>% 
  arrange(desc(basket_perc)) %>% 
  left_join(ocado, by = "product") %>% 
  .[, c("product", "baskets", "basket_perc", "image_link")]

# Money spent on each product
basket_db %>% 
  group_by(product) %>% 
  summarise(total_spent = sum(price)) %>% 
  mutate(spent_perc = total_spent / sum(total_spent)) %>% 
  arrange(desc(spent_perc))

# How many items purchased per basket on average?
# confirms normally distributed data generation
basket_db %>% 
  count(basket_id) %>% 
  ungroup() %>% 
  ggplot2::ggplot(ggplot2::aes(x = n)) + 
  ggplot2::geom_histogram(fill = "skyblue") + 
  ggplot2::geom_rug()

# Avg order value & total num of orders by store
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
  arrange(desc(orders_per_customer))

# Popular order time for a store
popular_order_time(time_group = month, store = "Union Coop - Barsha")
popular_order_time(time_group = weekday, "Mamalu Kitchen")
popular_order_time(time_group = hour, "Al Adil - Discovery Gardens")

##### 4B: Analysis MARKET BASKET -----
# Prepare data: remove commas in a product's name & collapse line items
basket_item <-
  basket_db %>%
  mutate(product = str_replace_all(product, ",", " ")) %>% 
  mutate(product = str_replace_all(product, "  ", " "))

itemList <- plyr::ddply(basket_item, c("basket_id","product"),
                        function(df1)paste(df1$product,
                                           collapse = ","))

write_csv(itemList, here::here("data/itemList.csv"))

txn <-  read.transactions(here::here("data/itemList.csv"), 
                          rm.duplicates = FALSE, 
                          format = "single", 
                          header = TRUE, 
                          sep= ",", 
                          cols = c(1,2))

class(txn)
txn
inspect(head(txn, 2))
size(head(txn, 5))
LIST(head(txn, 2))
itemFrequencyPlot(txn, topN = 5, "absolute")
summary(txn)

# APRIORI
rules <- apriori(txn, parameter = list(support = .00001, conf = .25, maxlen = 3))
write(x = , here::here("data/rules.csv", sep = ","))
inspect(head(rules))

subset_rules <- which(colSums(is.subset(rules, rules)) > 1)
length(subset_rules)
rules <- rules[-subset_rules]

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf, 5))

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift, 5))

unlink(here::here("data/basket_item.csv"))


# confidence: how likely to buy B, given bought A
# (purchase involving A, is accompanied by B 75% of the time)
# lift: how MUCH MORE likely to buy B, given bought A
# (1.3 = 30% more likely to buy B if A already bought | .9 = 10% less likely |
# 1 = occur together by chance)

##### 4C: Analysis SPATIAL -----
# lat/long continue...
