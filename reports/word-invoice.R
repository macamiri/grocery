##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(officer)
library(dplyr)
library(lubridate)
library(flextable)

##### 2: Powerpoint & data prep -----------------------------------------------
# Load data
data("grocery_data", package = "grocerycart")

# Initialize document
doc <- read_docx()

styles_info(doc)

# Add logo and date -----
doc <- 
  doc %>% 
    body_add_img(src = here::here("funmart-logo.png"), 
                 height = .81, width = 2, 
                 style = "Normal") %>% 
    body_add_par(value = stringr::str_glue("Invoice Period: {min(grocery_data$order_date)} to {max(grocery_data$order_date)}"), 
                 style = "Normal")

# Add Fee table (Fee = 4% of cost + 9.5 delivery fee) -----
fee_percent <- .04
fee_delivery <- 9.5

invoice_table <- 
  grocery_data %>% 
    mutate(percent_fee = (cost * fee_percent), 
           delivery_fee = fee_delivery, 
           total_fee = percent_fee + delivery_fee, 
           Month = floor_date(order_date, unit = "month")) %>% 
    group_by(Month) %>% 
    summarise(Revenue = sum(cost), 
              `Percent Fees` = sum(percent_fee), 
              `Delivery Fees` = sum(delivery_fee), 
              `Total Fees` = sum(total_fee) %>% scales::dollar(prefix = "AED ")) %>% 
    mutate(Month = Month %>% format("%h-%y") %>% stringr::str_replace("-", " '"))

invoice_table <- 
  flextable(invoice_table) %>% 
    set_table_properties(layout = "autofit")

doc <- 
  doc %>% 
    body_add_par(value = "Invoice Table", style = "heading 1") %>% 
    body_add_flextable(value = invoice_table)

# Save -----
print(doc, target = "reports/invoice-report.docx")
