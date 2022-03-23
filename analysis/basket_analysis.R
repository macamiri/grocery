##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggforce)
library(gt)

blue_palette <- c("#99D8EB", "#81C3D7", "#62A7C1", "#3A7CA5", 
                  "#285F80", "#16425B", "#0C2C3E", "#051E2C")

##### 2: Load data ------------------------------------------------------------
# Availbale datasets in package
# data(package = "grocerycart")
# ?customer_db_funmart

# 4,996 customers
data("customer_db_funmart")

# 12,000 orders
data("order_db_funmart")

# 144,159 line items
data("basket_db_funmart")

# Grocery: join tables
grocery <- 
  basket_db_funmart %>% 
    group_by(basket_id, order_id) %>% 
    summarise(cost = sum(price)) %>% 
    ungroup() %>% 
    inner_join(order_db_funmart, by = "order_id") %>% 
    inner_join(customer_db_funmart, by = "customer_id")

##### 3: Analysis -------------------------------------------------------------
# How many products are purchased per basket?
# confirms normally distributed data generation
grocery_basket <- 
  basket_db_funmart %>% 
  count(basket_id, name = "baskets") %>% 
  ungroup()

grocery_mean <- 
  grocery_basket %>% summarise(mean(baskets)) %>% purrr::pluck(1) %>% round(2)

grocery_sd <- 
  grocery_basket %>% summarise(sd(baskets)) %>% purrr::pluck(1) %>% round(2)

grocery_basket %>% 
  ggplot(aes(x = baskets)) + 
  geom_histogram(aes(y = stat(count)), 
                 colour = "grey", fill = blue_palette[4], bins = 21) + 
  geom_density(stat = "count", alpha = 0.3, 
               fill = blue_palette[4], colour = blue_palette[3], size = 0.7) + 
  #geom_rug() + 
  labs(x = "Products", y = "Baskets", 
       title = "Number of Products per Basket", 
       subtitle = stringr::str_glue("On average, there are {round(grocery_mean)} products/basket.
                                    95% of baskets contain {round(grocery_mean - grocery_sd * 2)} to {round(grocery_mean + grocery_sd * 2)} products.")) + 
  scale_x_continuous(breaks = seq(from = 0, to = max(grocery_basket$baskets), by = 2)) + 
  hrbrthemes::theme_ipsum(grid = FALSE)

# Order frequency (3 customers ordered 9 times)
grocery_freq <- 
  grocery %>% 
    count(customer_id, name = "orders") %>% 
    count(orders, name = "customers")

grocery_freq %>% 
  ggplot(aes(x = orders, y = customers)) + 
  geom_col(colour = "grey", fill = blue_palette[4], alpha = .6) + 
  labs(x = "Orders", y = "Customers", 
       title = ("Order Freqeuncy"), 
       subtitle = "Example: 3 customers ordered 9 times") + 
  geom_text(aes(label = customers, vjust = -.2)) + 
  scale_x_continuous(breaks = 1:max(grocery_freq$customers)) + 
  hrbrthemes::theme_ipsum(grid = FALSE)

grocery %>% 
  group_by(payment_method) %>% 
  count(customer_id, name = "orders") %>% 
  count(orders, name = "customers") %>% 
  ungroup() %>% 
  ggplot(aes(x = orders, y = customers)) + 
  geom_col(colour = "grey", fill = blue_palette[4], alpha = .6) + 
  labs(x = "Orders", y = "Customers", 
       title = ("Order Freqeuncy by Payment Method")) + 
  geom_text(aes(label = customers, vjust = -.2)) + 
  scale_x_continuous(breaks = 1:max(grocery_freq$customers)) + 
  hrbrthemes::theme_ipsum(grid = FALSE) + 
  facet_wrap(~ payment_method)
  

# Average order value
grocery_aov <- 
  grocery %>% 
    group_by(Month = month(order_date, label = TRUE)) %>% 
    summarise(AOV = round(mean(cost), 1), 
              Orders = n() %>% scales::comma(), 
              Customers = n_distinct(customer_id) %>% scales::comma())

grocery_aov %>% 
  gt() %>% 
  tab_header(title = md("**Average Order Value**"), 
             subtitle = "Broken down by month*") %>% 
  tab_source_note(md("*\\*Combined data from 2020 & 2021*")) %>% 
  tab_footnote(footnote = md("Month with Highest Order Value"), 
               locations = cells_body(
                 columns = Month, 
                 rows = AOV == max(AOV)
               )) %>% 
  data_color(
    columns = AOV,
    colors = blue_palette) %>% 
  tab_row_group(
    label = "Q4",
    rows = 10:12
  ) %>% 
  tab_row_group(
    label = "Q3",
    rows = 7:9
  ) %>% 
  tab_row_group(
    label = "Q2",
    rows = 4:6
  ) %>% 
  tab_row_group(
    label = "Q1",
    rows = 1:3
  )

# Create grocery buckets based on width or number of buckets
bucket_width <- 100
bucket_num <- 5
grocery_buckets <- function(width = NULL, n = NULL) {
  if(is.null(width) && is.null(n) || !is.null(width) && !is.null(n)) {
    cat(crayon::red("Specify exactly one of width and n\n"))
  }
  
  grocery %>% 
    group_by(customer_name) %>% 
    summarise(total_spent = sum(cost)) %>% 
    mutate(spent_bucket = cut_interval(total_spent, 
                                       length = width, 
                                       n = n, 
                                       right = "FALSE")) %>% 
    count(spent_bucket, name = "num_orders")
}

grocery_buckets(width = 100) %>% 
  ggplot(aes(x = spent_bucket, y = num_orders)) + 
  geom_segment(aes(x = spent_bucket, xend = spent_bucket, y = 0, yend = num_orders),
               color = blue_palette[4], lwd = .25, lty = 2, alpha = .6) + 
  geom_point(size = 14, pch = 21, bg = blue_palette[3], col = blue_palette[1]) + 
  labs(x = "Price Range", y = "Baskets", 
       title = ("Basket Price Range"), 
       subtitle = "Example: 1521 orders had a value b/w 100 to 200") + 
  geom_text(aes(label = num_orders, size = 3), color = "white", fontface = "bold") + 
  hrbrthemes::theme_ipsum(grid = FALSE) + 
  scale_x_discrete(labels = function(x) stringr::str_replace(x, ",", "-") %>% 
                     stringr::str_remove_all("[\\(\\)\\[\\]]")) + 
  theme(legend.position = "none")

# Product distribution in baskets (draw average line)
basket_sq <- 
  basket_db_funmart %>% 
    group_by(product) %>% 
    summarise(revenue = sum(price), baskets = n()) %>% 
    mutate(revenue_perc = revenue / sum(revenue), 
           baskets_perc = baskets / sum(baskets), 
           clrs = if_else(revenue_perc > mean(revenue_perc) & baskets_perc > mean(baskets_perc), "I", 
                          if_else(revenue_perc > mean(revenue_perc) & baskets_perc < mean(baskets_perc), "II", 
                                  if_else(revenue_perc < mean(revenue_perc) & baskets_perc < mean(baskets_perc), "III", "IV"
                                          ))))

# Revenue generated by products (Nikai Air Fryer 3.2L generated 10% of revenue)
plotly::ggplotly(
  p = 
    basket_sq %>% 
      ggplot(aes(x = baskets_perc, y = revenue_perc)) + 
      geom_point(aes(color = clrs,
                     text = stringr::str_glue(
                       "Product: {product}
                       Revenue: ￡{scales::comma(round(revenue))} ({round(revenue_perc *100, 4)}%)
                       Basket: {scales::comma(round(baskets))} ({round(baskets_perc *100, 4)}%)"))) + 
      geom_segment(aes(x = mean(baskets_perc), xend = mean(baskets_perc), 
                       y = 0, yend = max(revenue_perc)), 
                   color = blue_palette[3], lwd = .25, lty = 2, alpha = .6) + 
      geom_segment(aes(x = 0, xend = max(baskets_perc), 
                       y = mean(revenue_perc), yend = mean(revenue_perc)), 
                   color = blue_palette[3], lwd = .25, lty = 2, alpha = .6) + 
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous(labels = scales::percent) + 
      coord_cartesian(ylim = c(0, .052)) + 
      labs(x = "Percent of Baskets", y = "Percent of Revenue Generated", 
           title = "Product Popularity", 
           subtitle = "Hover over the point to view product details") +  
      geom_text(aes(x = 0.0065, y = .04), label = "Products here generated more revenue\n& were bought more frequently on average", nudge_x = 0.002, col = blue_palette[6]) + 
      geom_text(aes(x = 0, y = .04), label = "Products here generated more revenue,\nbut were bought less frequently on average", nudge_x = 0.002, col = blue_palette[4]) + 
      geom_text(aes(x = 0, y = .006), label = "Average Revenue", nudge_x = 0.0001, col = blue_palette[3]) + 
      scale_color_manual(values = blue_palette[c(7, 5, 3, 1)]) + 
      hrbrthemes::theme_ipsum(grid = FALSE) + 
      theme(legend.position = "none"), 
  tooltip = "text", 
)

# Popular times for orders
popular_order_time <- function(data = grocery, interval = c(month, weekday, hour)) {
  data %>%
    .[, c("order_date", "order_time", "cost")] %>%
    transmute(month = month(order_date, label = TRUE),
              weekday = wday(order_date, label = TRUE),
              hour = tryCatch(hour(order_time), error = function(e) {rep(NA, length(grocery$order_time))}),
              price = cost) %>%
    group_by({{ interval }}) %>%
    summarise(orders = n(), avg_price = mean(price)) %>%
    ungroup()
}

# Orders placed across quarters
order_db_funmart %>%
  group_by(year = year(order_date)) %>%
  count(quarter = quarter(order_date), name = "orders")

# Month orders
grocery %>%
  popular_order_time(interval = month) %>% 
  ggplot(aes(x = month, y = orders)) + 
  geom_point(colour = blue_palette[5], alpha = .6) + 
  geom_line(aes(group = 1), colour = blue_palette[4], alpha = .6) + 
  labs(x = "Month", y = "Orders", 
       title = ("Monthly Orders"), 
       subtitle = "Combined data for 2020 & 2021") + 
  geom_text(aes(label = orders, vjust = -.8)) + 
  hrbrthemes::theme_ipsum(grid = FALSE)
