##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(tidyverse)
library(stringr)
library(forcats)
library(ggrepel)
library(ggimage)
library(ggbeeswarm)

eg_palette <- c("#92DD7A", "#73D055", "#55C667", "#3CBB75", 
                "#2AA260", "#1B7B46", "#0F5830", "#074422")

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

gg_eg_payment <- 
  eg_payment %>% 
    count(payment_method, name = "stores") %>% 
    ggplot(aes(x = payment_method %>% fct_reorder(stores) %>% fct_rev(), 
               y = stores)) + 
    geom_col(colour = "grey", fill = eg_palette[4], alpha = .6) + 
    labs(x = "Payment Method", y = "Stores"#, title = ("Payment Methods Offered At Stores")
         ) + 
    geom_text(aes(label = stores, vjust = -.2)) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
    hrbrthemes::theme_ipsum(grid = "none")

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
gg_eg_categories <- 
  eg_category %>% 
    select(1:2) %>% 
    count(store_name) %>% 
    mutate(cat_bucket = case_when(
      n < 10 ~ "0-9", 
      n >= 10 & n < 20 ~ "10-19", 
      n >= 20 & n < 30 ~ "20-29", 
      n >= 30 & n < 40 ~ "30-37", 
      TRUE ~ "other")) %>% 
    count(cat_bucket) %>% 
    ggplot(aes(x = cat_bucket, y = n)) + 
    geom_segment(aes(x = cat_bucket, xend = cat_bucket, y = 0, yend = n),
                 color = eg_palette[4], lwd = .25, lty = 2, alpha = .6) + 
    geom_point(size = 10, pch = 21, bg = eg_palette[3], col = eg_palette[1]) + 
    labs(x = "Categories", y = "Stores", 
         #title = ("Number of Categories in Stores"), 
         #subtitle = "Example: 66 stores have 0 to 9 categories"
         ) + 
    geom_text(aes(label = n, size = 3), color = "white", fontface = "bold") + 
    hrbrthemes::theme_ipsum(grid = FALSE) + 
    coord_flip() + 
    theme(legend.position = "none")

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
gg_eg_subcategories <- 
  eg_sub %>% 
    count(store_name) %>% 
    mutate(subcat_bucket = case_when(
      n < 10 ~ "0-9", 
      n >= 10 & n < 20 ~ "10-19", 
      n >= 20 & n < 30 ~ "20-29", 
      n >= 30 & n < 46 ~ "30-45", 
      TRUE ~ "other")) %>% 
    count(subcat_bucket) %>% 
    ggplot(aes(x = subcat_bucket, y = n)) + 
    geom_col(colour = "grey", fill = eg_palette[4], alpha = .6) + 
    labs(x = "Subcategories", y = "Stores", 
         #title = ("Number of Subcategories in Stores"), 
         #subtitle = "Example: 77 stores have 0 to 9 subcategories"
         ) + 
    geom_text(aes(label = n, hjust = -.2)) + 
    hrbrthemes::theme_ipsum(grid = "none") + 
    coord_flip()

# rshiny custom
eg_sub %>% 
  count(store_name, name = "subcategories") %>% 
  filter(store_name == "Al Adil - Electra") %>% 
  pluck(2)

### (C) PRODUCT
data("eg_product")

# Top 5 expensive products (in GBP)
eg_top5 <- 
  eg_product %>% 
    distinct(item, price, .keep_all = TRUE) %>% 
    slice_max(n = 5, order_by = price) %>% 
    select(item, weight, price, item_image_link) %>% 
    bind_cols(palette = eg_palette[8:4])

gg_eg_top5 <- 
  eg_top5 %>% 
    mutate(item = item %>% fct_reorder(price) %>% fct_rev()) %>% 
    ggplot(aes(x = item, y = price)) + 
    geom_image(aes(image = item_image_link), size = .2) + 
    labs(x = "Product", y = "Price (GBP)"#, title = ("Top 5 Most Expensive Products")
         ) + 
    geom_label_repel(aes(label = round(price), fill = item), colour = "white", 
                    segment.colour = eg_top5$palette, 
                    segment.curvature = -0.5, 
                    segment.ncp = 3,
                    segment.angle = 20, 
                    fontface = "bold", 
                    box.padding = unit(1, "cm"),
                    point.padding = unit(2, "cm")) + 
    hrbrthemes::theme_ipsum(grid = FALSE) + 
    coord_cartesian(ylim = c(0, 700)) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
    scale_fill_manual(values = setNames(eg_top5$palette, levels(eg_top5$item))) + 
    theme(legend.position = "none")

# Top 3 most expensive stores, on average
eg_top3_store <-
  eg_data %>% 
    group_by(store_name) %>% 
    summarise(median_price = median(price), products = n()) %>% 
    slice_max(n = 3, order_by = median_price)

eg_top3 <- 
  eg_data %>% 
  semi_join(eg_top3_store, by = "store_name")

eg_biorganic <- 
  eg_top3 %>% 
    filter(store_name == "Biorganic - Abu Dhabi") %>% 
    slice_max(n = 2, order_by = price)
  
eg_eataly <- 
  eg_top3 %>% 
    filter(store_name == "Eataly Dubai Mall") %>% 
    slice_max(n = 3, order_by = price) %>% 
  mutate(item = str_wrap(item, width = 15))

eg_seafood <- 
  eg_top3 %>% 
  filter(store_name == "Gulf Seafood") %>% 
  slice_max(n = 2, order_by = price) %>% 
  mutate(item = str_wrap(item, width = 15))

gg_eg_top3 <- 
  eg_top3 %>% 
    ggplot(aes(x = store_name, y = price, colour = store_name)) + 
    ggbeeswarm::geom_quasirandom(alpha = .6, size = 2) + 
    geom_boxplot(fill = NA, outlier.color = NA, alpha = .6) + 
    geom_text_repel(data = eg_biorganic, aes(label = item), nudge_y = -.1) + 
    geom_text_repel(data = eg_eataly, aes(label = item), ylim = c(-NA, NA), 
                     segment.curvature = -0.5, nudge_y = 2, nudge_x = 1, 
                     segment.ncp = 3,
                     segment.angle = 10, 
                     point.padding = unit(.2, "lines")) + 
    geom_text_repel(data = eg_seafood, aes(label = item), nudge_y = 30, xlim = c(-NA, NA)) + 
    labs(x = "Store", y = "Price (GBP)"#, title = ("Products Prices:\nTop 3 Most Expensive Stores on Average")
         ) + 
    hrbrthemes::theme_ipsum(grid = "Y") + 
    coord_cartesian(ylim = c(0, 125)) + 
    scale_colour_manual(values = c(eg_palette[3], "#7db5d1", "#664EAB")) + 
    theme(legend.position = "none")

## Read image
# item_image <- magick::image_read(path = item_image_links %>% 
#                                    unlist())
