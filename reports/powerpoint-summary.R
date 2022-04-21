##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(officer)
library(dplyr)
library(stringr)
library(ggplot2)
library(flextable)
library(cohorts)
library(patchwork)
library(lubridate)

blue_palette <- c("#99D8EB", "#81C3D7", "#62A7C1", "#3A7CA5", 
                  "#285F80", "#16425B", "#0C2C3E", "#051E2C")

##### 2: Powerpoint & data prep -----------------------------------------------
# Load data
data("grocery_data", package = "grocerycart")

# Initialize slides
ppt <- read_pptx(here::here("reports/grocerycart-template.pptx"))

layout_summary(ppt)

# View placeholders
ppt_placeholders <- function(pptx, layout, bg_color, hjust = 0) {
  
  ppt_properties <- layout_properties(ppt, layout = layout)
  
  print(ppt_properties %>% select(name, type, id, ph_label))
  
  ppt_properties %>% 
    ggplot(aes(xmin = offx, ymin = -offy, xmax = offx + cx, ymax = -offy - cy)) + 
    geom_rect(fill = bg_color) + 
    geom_text(aes(x = offx, y = -offy - cy/2, label = ph_label), 
              color = "black", size = 3.5, hjust = hjust) + 
    theme_void()
}

##### 3: Add slides & content to powerpoint -----------------------------------
# Title Slide -----
ppt_placeholders(ppt, "Title Slide", "steelblue")
ppt <- 
  ph_with(ppt, 
               value = "Funmart Summary", 
               location = ph_location_label(ph_label = "Title 1")) %>% 
    ph_with(value = str_glue("Date: {format(Sys.Date())}"), 
            location = ph_location_label(ph_label = "Slide Number Placeholder 6"))

# Summary Slide -----
ppt_placeholders(ppt, "Title and Content", "steelblue")

ul <- unordered_list(
  str_list = c("KPIs", "Order Frequency", "Payment Methods", "Monthly Overview", "Cohort Analysis"), 
  level_list = rep(1, 5), 
  style = fp_text(color = "steelblue", italic = TRUE, underlined = TRUE)
)

ppt <- 
  add_slide(ppt, layout = "Title and Content", master = "grocerycart-template") %>% 
    ph_with(value = "Summary", location = ph_location_label(ph_label = "Title 1")) %>% 
    ph_with(value = ul, location = ph_location_label(ph_label = "Content Placeholder 2")) %>% 
    ph_with(value = "2", location = ph_location_label(ph_label = "Slide Number Placeholder 6"))

# KPIs -----
ppt_placeholders(ppt, "Title and Content", "steelblue")

kpi_table <- 
  grocery_data %>% 
  summarise("Orders" = scales::comma(n_distinct(basket_id)), 
            "Revenue" = scales::dollar(sum(cost), prefix = "AED "), 
            "Average Order Value" = scales::dollar(round(mean(cost), 2), prefix = "AED "))

kpi_text <- 
  str_glue("This data is from {min(grocery_data$order_date)} to {max(grocery_data$order_date)}. There were {kpi_table$Orders} orders during this time with a total revenue of {kpi_table$Revenue} and average order value of {kpi_table$`Average Order Value`}.")

footer_text <- paste0("Table 1.1: Key Performance Indicators. ", kpi_text)

kpi_table <-
  kpi_table %>%
    flextable() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    theme_tron() %>%
    add_footer_lines(footer_text)

ppt <- 
  add_slide(ppt, layout = "Title and Content", master = "grocerycart-template") %>% 
  ph_with(value = "KPIs", location = ph_location_label(ph_label = "Title 1")) %>% 
  ph_with(value = kpi_table, location = ph_location_label(ph_label = "Content Placeholder 2")) %>% 
  ph_with(value = "3", location = ph_location_label(ph_label = "Slide Number Placeholder 6"))

# Order Frequency -----
ppt_placeholders(ppt, "Blank", "steelblue")

grocery_freq <- 
  grocery_data %>% 
    count(customer_id, name = "orders") %>% 
    count(orders, name = "customers")

of_plot <- 
  grocery_freq %>% 
    ggplot(aes(x = orders, y = customers)) + 
    geom_segment(aes(x = orders, xend = orders, y = 0, yend = customers), 
                 color = "#3CBB75", lwd = .25, lty = 2, alpha = .6) + 
    geom_point(size = 13, pch = 21, bg = "#55C667", col = "#92DD7A") + 
    geom_text(aes(label = customers, size = 3), color = "white", fontface = "bold") + 
    labs(x = "Orders", y = "Customers", 
         title = ("Order Freqeuncy"), 
         subtitle = "Example: 3 customers ordered 9 times") + 
    scale_x_continuous(breaks = 1:max(grocery_freq$orders)) + 
    hrbrthemes::theme_ipsum(grid = FALSE) + 
    theme(legend.position = "none")

ppt <- 
  add_slide(ppt, layout = "Blank", master = "grocerycart-template") %>% 
    ph_with(value = of_plot, location = ph_location_fullsize())

# Payment Methods -----
ppt_placeholders(ppt, "Blank", "steelblue")

payment_graph <- function(pm) {
  grocery_data %>% 
    filter(payment_method == pm) %>% 
    count(customer_id, name = "orders") %>% 
    count(orders, name = "customers") %>% 
    ungroup() %>% 
    ggplot(aes(x = orders, y = customers)) + 
    geom_col(colour = "grey", fill = "#1B7B46", alpha = .6) + 
    geom_text(aes(label = customers, vjust = -.2)) + 
    scale_x_continuous(breaks = 1:max(grocery_freq$customers)) + 
    labs(x = "Orders", y = "Customers") +  
    hrbrthemes::theme_ipsum(grid = FALSE) + 
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      axis.title.y.left = element_text(margin = margin(r = 10)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()) + 
    ggtitle(as.character(pm) %>% str_to_title())
}

p1 <- payment_graph(unique(grocery_data$payment_method)[[1]])
p2 <- payment_graph(unique(grocery_data$payment_method)[[2]])
p3 <- payment_graph(unique(grocery_data$payment_method)[[3]])

plots <- p1 + p2 + p3

p_all_plot <- 
  plots +
    plot_annotation(
      title = "Order Freqeuncy by Payment Method", 
      subtitle = "Example: 1554 Customers placed 1 order using Online Payment & 113 Customers placed 2 orders using Cash on Delivery", 
      caption = "Source: Data from grocerycart
               github.com/moamiristat/grocerycart",
      theme = theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))
    )

ppt <- 
  add_slide(ppt, layout = "Blank", master = "grocerycart-template") %>% 
    ph_with(value = p_all_plot, location = ph_location_fullsize())

# Monthly Overview -----
ppt_placeholders(ppt, "Content with Caption", "steelblue")

popular_order_time <- function(data, interval = c(month, weekday, hour)) {
  data %>%
    .[, c("order_date", "order_time", "cost")] %>%
    transmute(month = month(order_date, label = TRUE),
              weekday = wday(order_date, label = TRUE),
              hour = tryCatch(hour(order_time), error = function(e) {rep(NA, length(data$order_time))}),
              price = cost) %>%
    group_by({{ interval }}) %>%
    summarise(orders = n(), avg_price = mean(price)) %>%
    ungroup()
}

month_plot <- 
  grocery_data %>%
  popular_order_time(interval = month) %>% 
  ggplot(aes(x = month, y = orders)) + 
  geom_point(colour = "#1B7B46", alpha = .6) + 
  geom_line(aes(group = 1), colour = "#2AA260", alpha = .6) + 
  labs(x = "Month", y = "Orders", 
       title = ("Monthly Orders"), 
       subtitle = "Combined data for 2020 & 2021") + 
  geom_text(aes(label = orders, vjust = -.8)) + 
  hrbrthemes::theme_ipsum(grid = FALSE)

month_table <- 
  grocery_data %>% 
    group_by(Month = month(order_date, label = TRUE)) %>% 
    summarise(AOV = round(mean(cost), 1), 
              Orders = n() %>% scales::comma(), 
              Customers = n_distinct(customer_id) %>% scales::comma()) %>% 
    flextable() %>% 
    color(j = "Month", color = blue_palette[3]) %>% 
    add_footer_lines("Table 1.2: Average Order Value by Month")

ppt <- 
  add_slide(ppt, layout = "Content with Caption", master = "grocerycart-template") %>% 
    ph_with(value = "Monthly Overview", location = ph_location_label(ph_label = "Title 1")) %>% 
    ph_with(value = month_table, location = ph_location_label(ph_label = "Text Placeholder 3")) %>% 
    ph_with(value = month_plot, location = ph_location_label(ph_label = "Content Placeholder 2")) %>% 
    ph_with(value = "6", location = ph_location_label(ph_label = "Slide Number Placeholder 7"))

# Cohort Analysis -----
ppt_placeholders(ppt, "Blank", "steelblue")

cohort_table <- 
  grocery_data %>% 
    cohort_table_month(customer_id, order_date) %>% 
    shift_left_pct() %>% 
    tidyr::pivot_longer(cols = -cohort, names_to = "time") %>% 
    mutate(time = str_remove(time, "t") %>% as.numeric())

cohort_plot <- 
  cohort_table %>% 
    filter(time > 0, value > 0) %>% 
    ggplot(aes(x = time, y = cohort %>% reorder(desc(cohort)))) + 
    geom_raster(aes(fill = log(value))) + 
    labs(title = "Customer Cohort Analysis", 
         x = "Month", y = "Cohort", 
         subtitle = "Example from the top row: 22% of the customers from 'Cohort 1' placed an order 23 months after their 1st order") + 
    geom_text(aes(label = str_glue("{round(value, 0)}%")), size  = 3, colour = "snow") + 
    hrbrthemes::theme_ipsum(grid = FALSE, base_size = 16) + 
    scale_fill_gradient(low = blue_palette[1], high = blue_palette[8], guide = "none") + 
    scale_x_continuous(breaks = 1:max(cohort_table$time))

ppt <- 
  add_slide(ppt, layout = "Blank", master = "grocerycart-template") %>% 
    ph_with(value = cohort_plot, location = ph_location_fullsize())

# Save -----
print(ppt, target = "reports/powerpoint-report.pptx")
