##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(tidyverse)
library(stringr)
library(reactable)
library(gganimate)
# devtools::install_github("jimjam-slam/ggflags")
library(ggflags)
library(ggimage)
library(ggrepel)
library(htmltools)

oc_palette <- c("#D3CAEC", "#B3A2E7", "#9F8BDC", "#7D67BD",
                "#664EAB", "#513C90", "#36246C", "#281956")

blue_palette <- c("#99D8EB", "#81C3D7", "#62A7C1", "#3A7CA5", 
                  "#285F80", "#16425B", "#0C2C3E", "#051E2C")

##### 2: Analysis -------------------------------------------------------------
# Availbale datasets in package
# data(package = "grocerycart")
# ?oc_data

data("oc_data")

### (A) COUNTRY
oc_country <- 
  oc_data %>% 
    filter(!is.na(country)) %>% 
    count(country) %>% 
    separate_rows(country, sep = ", ") %>% 
    group_by(country) %>% 
    summarise(products = sum(n)) %>% 
    arrange(desc(products))

### (B) BRAND
# Number of products per brand
oc_pro <- 
  oc_data %>% 
    select(brand, product) %>% 
    filter(!is.na(brand)) %>% 
    count(brand, name = "products") %>% 
    arrange(desc(products))

# Average price for each brand
oc_top_pro <- 
  oc_data %>% 
    inner_join(oc_pro, by = "brand") %>% 
    group_by(brand) %>% 
    summarise(products = n(), 
              avg_price = round(mean(price, na.rm = TRUE), 2), 
              median_price = round(median(price, na.rm = TRUE), 2), 
              max_price = round(max(price, na.rm = TRUE), 2), 
              min_price = round(min(price, na.rm = TRUE), 2))

oc_pal <- function(x) rgb(colorRamp(c(oc_palette[1], oc_palette[6]))(x), 
                          maxColorValue = 255)

oc_top_pro %>% 
  reactable(
    defaultSortOrder = "desc", 
    defaultSorted = c("products", "avg_price"), 
    columns = list(
      avg_price = colDef(style = function(.x) {
        norm_avg_price <- 
          (.x - min(oc_top_pro$avg_price)) / (max(oc_top_pro$avg_price) - min(oc_top_pro$avg_price))
        
        color <- oc_pal(norm_avg_price)
        
        list(background = color)
      })
    ), 
    defaultColDef = colDef(
      header = function(.x) {str_replace(.x, "_", " ") %>% str_to_title()},
      cell = function(.x) format(.x, nsmall = 1),
      align = "center",
      minWidth = 70, 
      headerStyle = list(background = "light grey")
    ), 
    defaultPageSize = 20, 
    bordered = TRUE, striped = TRUE, highlight = TRUE
  )

# Which of the top 5 brands with the most reviews 
# has the highest recommendation score
oc_top5_rev <- 
  oc_data %>% 
    filter(!is.na(recommend), !is.na(brand), !is.na(reviews)) %>% 
    group_by(brand) %>% 
    summarise(reviews = sum(num_of_reviews)) %>% 
    slice_max(n = 5, order_by = reviews)

oc_data %>% 
  select(brand, recommend) %>% 
  semi_join(oc_top5_rev, by = "brand") %>% 
  group_by(brand) %>% 
  summarise(avg_recommend = mean(recommend, na.rm = TRUE)) %>% 
  inner_join(oc_pro, by = "brand") %>% 
  arrange(desc(products))

### (C) BADGE
# Badge count & median product price by badge
oc_badge <- 
  oc_data %>% 
    filter(!is.na(badge)) %>% 
    select(badge, price) %>% 
    separate_rows(badge, sep = ", ") %>% 
    group_by(badge) %>% 
    summarise(products = n(), median_price = median(price), avg_price = mean(price)) %>% 
    arrange(desc(products))

p_badge <- 
  oc_badge %>% 
    mutate(badge = badge %>% fct_reorder(products)) %>% 
    ggplot(aes(x = badge, y = products)) + 
    geom_segment(aes(x = badge, xend = badge, y = 0, yend = products),
                 color = oc_palette[4], lwd = .25, lty = 2, alpha = .6) +
    geom_point(size = 12, pch = 21, bg = oc_palette[3], col = oc_palette[1]) + 
    labs(x = "Dietary", y = "Products", 
         # title = ("Products with Dietary Notes"), 
         # subtitle = glue::glue("{oc_badge$products} products are {oc_badge$badge}")
         ) + 
    geom_text(aes(label = products, size = 3), color = "white", fontface = "bold") + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
    hrbrthemes::theme_ipsum(grid = FALSE) + 
    theme(legend.position = "none")
    
anim_badge <- 
  p_badge + 
  transition_states(badge, transition_length = 2, wrap = FALSE) + 
  ease_aes("sine-in-out") + 
  enter_fly(y_loc = -10) + 
  shadow_mark() + 
  ggtitle("Products with Dietary Notes", subtitle = ("{closest_state}"))

animate(anim_badge, renderer = gifski_renderer(loop = FALSE), nframes = 40)

# Which of the top 3 brands with the most products 
# has the highest % of vegetarian friendly products (veg/all)
oc_data %>% 
  select(brand, badge, product) %>% 
  filter(!is.na(brand)) %>% 
  separate_rows(badge, sep = ", ") %>% 
  filter(badge == "Suitable for vegetarians") %>% 
  count(brand, name = "veg_products") %>% 
  inner_join(oc_pro, by = "brand") %>% 
  mutate(perc_veg = veg_products / products) %>% 
  slice_max(n = 3, order_by = products)

# Which of the top 4 countries with the most products 
# has the highest % of vegetarian friendly products (veg/all)
oc_flags <- 
  oc_data %>% 
    select(country, badge, product) %>% 
    filter(!is.na(country)) %>% 
    separate_rows(badge, sep = ", ") %>% 
    filter(badge == "Suitable for vegetarians") %>% 
    count(country, name = "veg_products") %>% 
    inner_join(oc_country, by = "country") %>% 
    mutate(country = if_else(country %in% c("England", "United Kingdom"), 
                             "UK", 
                             country)) %>% 
    group_by(country) %>% 
    summarise(veg_products = sum(veg_products), products = sum(products)) %>% 
    ungroup() %>% 
    mutate(perc_veg = veg_products / products) %>% 
    slice_max(n = 5, order_by = products) %>% 
    mutate(country = if_else(country == "Italy", "it", 
                             if_else(country == "France", "fr", 
                                     if_else(country == "EU", "eu", 
                                             if_else(country == "Spain", "es", "gb")))))

oc_flags %>% 
  ggplot(aes(x = perc_veg, y = country %>% fct_reorder(perc_veg))) + 
  geom_col(colour = "grey", alpha = .6, fill = oc_palette[6]) + 
  geom_flag(x = 0, aes(country = country), size = 15) + 
  labs(x = "Percent of Products that are Vegetarian", y = "Location", 
       title = ("Vegetarian Products Exported by the Top 5 Locations"), 
       subtitle = "Top locations = locations with most exported products") + 
  geom_text(aes(label = products, hjust = -.2)) + 
  scale_x_continuous(labels = scales::percent) + 
  hrbrthemes::theme_ipsum(grid = FALSE)

### SHELF LIFE
# Top 5 most common shelf life for products
oc_data %>% 
  filter(!is.na(shelf_life)) %>% 
  count(shelf_life, name = "products") %>% 
  slice_max(n = 5, order_by = products)

# Most common shelf life for each brand
oc_data %>% 
  filter(!is.na(shelf_life), !is.na(brand)) %>% 
  group_by(brand, shelf_life) %>% 
  summarise(products = n()) %>% 
  mutate(rank = dense_rank(desc(products))) %>% 
  filter(rank == 1) %>% 
  arrange(-products) %>% 
  ungroup()

### PRODUCT
# Top 5 most reviewed products
oc_top5_rev <- 
  oc_data %>% 
    select(product, rating, num_of_reviews, recommend, image_link) %>% 
    slice_max(n = 5, order_by = num_of_reviews) %>% 
    mutate(product = product %>% fct_reorder(num_of_reviews) %>% fct_rev()) %>% 
    bind_cols(palette = c("#DFBF61", blue_palette[7], "#BFB394", "#D85252", "#D87B3D"))

oc_top5_rev %>% 
  ggplot(aes(x = product, y = num_of_reviews)) + 
  geom_image(aes(image = image_link), size = .2) + 
  geom_label_repel(aes(label = glue::glue("{num_of_reviews} reviews\n{recommend}% recommend"), 
                       fill = product), 
                   colour = "white", 
                   segment.colour = oc_top5_rev$palette, 
                   segment.curvature = -0.5, 
                   segment.ncp = 3,
                   segment.angle = 20, 
                   fontface = "bold", 
                   box.padding = unit(2, "cm"),
                   point.padding = unit(2, "cm")) + 
  labs(x = "Product", y = "Reviews", 
       title = ("5 Most Reviewed Products"), 
       subtitle = "Customer recommendation rate (%)") + 
  hrbrthemes::theme_ipsum(grid = FALSE) + 
  coord_cartesian(ylim = c(0, 1000)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = setNames(oc_top5_rev$palette, levels(oc_top5_rev$product))) + 
  theme(legend.position = "none")

# Top 5 highest rated products with >=100 reviews
oc_data %>% 
  filter(num_of_reviews >= 100) %>% 
  slice_max(n = 5, order_by = rating) %>% 
  select(product, rating, num_of_reviews, image_link)

### NUTRITION
# Collect product kcals
calories <- extract_energy(oc_data, item = "product", nutrition = "nutrition")
kcal <- extract_kcal(calories)

# Table of kcals (ignoring weight of product)
oc_kcal <- 
  kcal %>% 
    select(product, kcal) %>% 
    distinct(product, .keep_all = TRUE) %>% 
    filter(!is.na(kcal)) %>% 
    mutate(kcal = as.numeric(kcal)) %>% 
    #slice_max(n = 5, order_by = kcal) %>% 
    inner_join(oc_data) %>% 
    select(product, price, kcal, image_link) %>% 
    distinct(product, .keep_all = TRUE)


oc_kcal %>% 
  reactable(
    defaultSortOrder = "desc", 
    defaultSorted = "kcal", 
    resizable = TRUE, 
    columns = list(
      image_link = colDef(cell = function(value) {
        image <- img(src = value, height = "120px", alt = "")
        tagList(
          div(style = list(display = "inline-block", width = "225px"), image)
        )
      }, 
      name = "Image"
      ), 
      kcal = colDef(
        name = "Energy (kcal)", 
        cell = function(value) {round(value)}, 
        maxWidth = 80
      ), 
      price = colDef(
        name = "Price (GBP)", 
        cell = function(value) {round(value, 2)}, 
        maxWidth = 75
      ), 
      product = colDef(
        name = "Product"
      )
    ), 
    defaultPageSize = 20, 
    bordered = TRUE, striped = TRUE, highlight = TRUE
  )

  
### INGREDIENT
# # Collapse all ingredients into 1 vector
# oc_data %>%
#   select(ingredient) %>%
#   filter(!is.na(ingredient)) %>%
#   paste()

### REVIEWS
# # Filter out the products with no reviews written
# oc_true_reviews <- 
#   oc_data %>%
#     select(product, reviews) %>%
#     pluck(2) %>% 
#     map(., ~ !is.null(.x)) %>% 
#     unlist()
# 
# oc_reviews <- 
#   oc_data %>% 
#     select(product, reviews) %>% 
#     filter(oc_true_reviews) %>% 
#     distinct(product, .keep_all = TRUE)
# 
# oc_reviews %>% 
#   slice(5) %>% 
#   pluck(2)
