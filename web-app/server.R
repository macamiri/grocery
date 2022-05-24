# Load packages
library(shiny)
library(grocerycart)
library(ggplot2)
library(dplyr)
library(gt)
library(reactable)
library(reactablefmtr)
library(stringr)
library(grocerycart)
library(cohorts)
library(shinyFeedback)
library(arules)
library(arulesViz)
library(visNetwork)
library(Matrix)
library(recommenderlab)
library(officer)
library(lubridate)
library(flextable)
library(cohorts)
library(patchwork)
library(lubridate)
pacman::p_load(fabricatr, wakefield, randomNames, charlatan, magrittr, purrr)

blue_palette <- c("#99D8EB", "#81C3D7", "#62A7C1", "#3A7CA5", 
                  "#285F80", "#16425B", "#0C2C3E", "#051E2C")

# Set server logic
shinyServer(function(input, output) {
  
  # Basket Tab -----
  output$gg_product_per_basket <- renderPlot({
    gg_product_per_basket
  })
  
  output$gg_order_freq <- renderPlot({
    gg_order_freq
  })
  
  output$gg_month_order <- renderPlot({
    gg_month_order
  })
  
  output$gt_table <- render_gt(
    expr = gt_aov
  )
  
  output$reactable_quarter_table <- renderReactable({
    table_quarter_order
  })
  
  output$gg_payment <- renderPlot({
    gg_payment
  })
  
  output$gg_buckets <- renderPlot({
    grocery_buckets(width = input$bucket_width %>% as.numeric()) %>% 
      ggplot(aes(x = spent_bucket, y = num_orders)) + 
      geom_segment(aes(x = spent_bucket, xend = spent_bucket, y = 0, yend = num_orders),
                   color = blue_palette[4], lwd = .25, lty = 2, alpha = .6) + 
      geom_point(size = 14, pch = 21, bg = blue_palette[3], col = blue_palette[1]) + 
      labs(x = "Price Range", y = "Baskets") + 
      geom_text(aes(label = num_orders, size = 3), color = "white", fontface = "bold") + 
      hrbrthemes::theme_ipsum(grid = FALSE) + 
      scale_x_discrete(labels = function(x) stringr::str_replace(x, ",", "-") %>% 
                         stringr::str_remove_all("[\\(\\)\\[\\]]")) + 
      theme(legend.position = "none")
  })
  
  output$plotly_popularity <- renderPlotly({
    plotly_pop
  })
  
  output$gg_cohort <- renderPlot({
    cohort_table <- 
      grocery_data %>% 
      cohort_table_month(customer_id, order_date) %>% 
      shift_left_pct() %>% 
      tidyr::pivot_longer(cols = -cohort, names_to = "time") %>% 
      mutate(time = str_remove(time, "t") %>% as.numeric())
    
    cohort_table %>% 
      filter(time > 0, value > 0) %>% 
      ggplot(aes(x = time, y = cohort %>% reorder(desc(cohort)))) + 
      geom_raster(aes(fill = log(value))) + 
      labs(x = "Month", y = "Cohort") + 
      geom_text(aes(label = str_glue("{round(value, 0)}%")), size  = 3, colour = "snow") + 
      hrbrthemes::theme_ipsum(grid = FALSE, base_size = 16) + 
      scale_fill_gradient(low = blue_palette[1], high = blue_palette[8], guide = "none") + 
      scale_x_continuous(breaks = 1:max(cohort_table$time))
  })
  
  # Data Download Tab -----
  # Generate the Data
  output$out <- renderText({
    if (any(!between(input$num_of_customers, 10, 10000),
            !between(input$num_of_orders, 24, 24000),
            !between(input$num_of_products, 40, 400),
            !between(input$avg_items, 3, 30))) {

      validate("Please follow these guidelines while entering your input:
               Customers: 10 to 10000
               Orders: 24 to 24000
               Products: 40 to 400
               Average Products/Basket: 3 to 30")
    }
  })
  
  validate_input <- function(num_of_customers, num_of_orders, num_of_products, avg_items) {
    
    cus_valid <- between(num_of_customers, 10, 10000)
    ord_valid <- between(num_of_orders, 24, 24000)
    pro_valid <- between(num_of_products, 40, 400)
    avg_valid <- between(avg_items, 3, 30)
    
    shinyFeedback::feedbackDanger("num_of_customers", !cus_valid, "cust")
    shinyFeedback::feedbackDanger("num_of_orders", !ord_valid, "ord")
    shinyFeedback::feedbackDanger("num_of_products", !pro_valid, "pro")
    shinyFeedback::feedbackDanger("avg_items", !avg_valid, "avg")
    
    req(cus_valid, ord_valid, pro_valid, avg_valid, cancelOutput = TRUE)
    
    product_prob_funmart <-
      tibble::tibble(
        product = sample(product_prob$product, size = num_of_products, replace = FALSE)) %>%
      dplyr::mutate(probs = probs(j = num_of_products))
    
    customer_db <- fabricate(
      N = num_of_customers,
      customer_name = randomNames(n = N, name.sep = " ", name.order = "first.last"),
      customer_age = wakefield::age(n = N, x = 18:75, prob = c(rep(.3, 13), rep(.6, 30), rep(.1, 15))),
      household_size = wakefield::children(n = N, x = 1:7, prob = c(.17, .25, .25, .2, .05, .05, .02)),
      long = ch_position(n = N, bbox = c(51, 22.50, 56.25, 26)) %>% purrr::map_chr(., ~ .[1]),
      lat = ch_position(n = N, bbox = c(51, 22.50, 56.25, 26)) %>% purrr::map_chr(., ~ .[2]),
    ) %>%
      tibble::as_tibble() %>%
      dplyr::distinct(customer_name, .keep_all = TRUE) %>%
      dplyr::rename("customer_id" = ID)
    
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
      payment_method = sample(x = eg_payment_method$method,
                              size = N,
                              replace = TRUE,
                              prob = eg_payment_method$prob),
      store = rep("funmart", N),
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename("order_id" = ID)
    
    basket_db <- fabricate(
      N = length(order_db$order_id),
      order_id = as.character(sample(order_db$order_id, size = N, replace = FALSE)),
      product = grocerycart::select_products(products = product_prob_funmart$product,
                                             customer_id = order_db$customer_id,
                                             probs = product_prob_funmart$probs,
                                             min_products = 3,
                                             mean_products = avg_items,
                                             sd_products = 3),
    ) %>%
      tibble::as_tibble() %>%
      tidyr::separate_rows(product, sep = "@") %>%
      dplyr::left_join(product_prob, by = "product") %>%
      dplyr::select(ID, order_id, product, price) %>%
      dplyr::rename("basket_id" = ID) %>%
      dplyr::distinct(basket_id, product, .keep_all = TRUE)
    
    list(customer_db, order_db, basket_db)
  }
  
  datalist <- eventReactive(input$generate, {
    validate_input(input$num_of_customers, 
                   input$num_of_orders, 
                   input$num_of_products, 
                   input$avg_items)
  })
  
  output$reactable_customer <- renderReactable({
    
    datalist()[[1]] %>% 
      reactable(., resizable = TRUE, showPageSizeOptions = FALSE, 
                onClick = "select", highlight = TRUE, sortable = FALSE, 
                theme = fivethirtyeight(centered = TRUE, header_font_size = 11), 
                
                columns = list(
                  customer_id = colDef(name = "Customer ID"), 
                  customer_name = colDef(name = "Name"), 
                  customer_age = colDef(name = "Age"), 
                  household_size = colDef(name = "Household Size"), 
                  long = colDef(name = "Longitude"), 
                  lat = colDef(name = "Latitude")
                )
                
      )
  })
  
  output$reactable_order <- renderReactable({
    
    datalist()[[2]] %>% 
      reactable(., resizable = TRUE, showPageSizeOptions = FALSE, 
                onClick = "select", highlight = TRUE, sortable = FALSE, 
                theme = fivethirtyeight(centered = TRUE, header_font_size = 11), 
                
                columns = list(
                  order_id = colDef(name = "Order ID"), 
                  customer_id = colDef(name = "Customer ID"), 
                  order_date = colDef(name = "Order Date"), 
                  payment_method = colDef(name = "Payment Method"), 
                  store = colDef(name = "Store")
                )
                
      )
  })
  
  output$reactable_basket <- renderReactable({
    
    datalist()[[3]] %>% 
      reactable(., resizable = TRUE, showPageSizeOptions = FALSE, 
                onClick = "select", highlight = TRUE, sortable = FALSE, 
                theme = fivethirtyeight(centered = TRUE, header_font_size = 11), 
                
                columns = list(
                  basket_id = colDef(name = "Basket ID"), 
                  order_id = colDef(name = "Order ID"), 
                  product = colDef(name = "Product"), 
                  price = colDef(name = "Price")
                )
                
      )
  })
  
  # Download the Data
  cus_data <- reactive(datalist()[[1]])
  ord_data <- reactive(datalist()[[2]])
  bas_data <- reactive(datalist()[[3]])
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("grocery_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      vroom::vroom_write(cus_data(), file.path(temp_directory, "customer_db.tsv"))
      vroom::vroom_write(ord_data(), file.path(temp_directory, "order_db.tsv"))
      vroom::vroom_write(bas_data(), file.path(temp_directory, "basket_db.tsv"))
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    contentType = "application/zip"
  )

  
  # Report Tab -----
  notify <- function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE, type = "message")
  }
  
  grocery_sample <- reactive({
    data("grocery_data", package = "grocerycart")
    grocery_data %>% 
      select(-order_time) %>% 
      relocate(store, .after = last_col())
    })
  
  output$download_sample <- downloadHandler(
    filename = function() {
      paste0("grocery_sample_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      readr::write_tsv(grocery_sample(), file)
    }
  )
  
  report_data <- reactive({
    req(input$upload_data)
    ext <- tools::file_ext(input$upload_data$name)
    switch(ext,
           csv = vroom::vroom(input$upload_data$datapath, delim = ","),
           tsv = vroom::vroom(input$upload_data$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$invoice <- downloadHandler(
    filename = function(){
      paste0("funmart_invoice_", Sys.Date(), ".docx")
      },
    content = function(file) {
      req(input$upload_data)
      
      id <- showNotification(
        "Reading in data...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      notify("Reading in data...", id = id)
      Sys.sleep(1)
      
      doc <- read_docx()
      
      # Add logo and date -----
      notify("Calculating fees...", id = id)
      Sys.sleep(1)
      
      doc <- 
        doc %>% 
        body_add_img(src = input$upload_image$datapath, 
                     height = 1, width = 1, 
                     style = "Normal") %>% 
        body_add_par(value = stringr::str_glue("Invoice Period: {min(grocery_data$order_date)} to {max(grocery_data$order_date)}"), 
                     style = "Normal")
      
      # Add Fee table (Fee = 4% of cost + 9.5 delivery fee) -----
      fee_percent <- input$fee_percent
      fee_delivery <- input$fee_delivery
      
      invoice_table <- 
        report_data() %>% 
        mutate(percent_fee = (cost * fee_percent), 
               delivery_fee = fee_delivery, 
               total_fee = percent_fee + delivery_fee, 
               Month = floor_date(order_date, unit = "month")) %>% 
        group_by(Month) %>% 
        summarise(Revenue = sum(cost), 
                  `Percent Fees` = sum(percent_fee), 
                  `Delivery Fees` = sum(delivery_fee), 
                  `Total Fees` = sum(total_fee) %>% scales::dollar(prefix = "GBP ")) %>% 
        mutate(Month = Month %>% format("%h-%y") %>% stringr::str_replace("-", " '"))
      
      invoice_table <- 
        flextable(invoice_table) %>% 
        set_table_properties(layout = "autofit")
      
      doc <- 
        doc %>% 
        body_add_par(value = "Invoice Table", style = "heading 1") %>% 
        body_add_flextable(value = invoice_table)
      
      # Print -----
      notify("Preparing to download invoice...", id = id)
      Sys.sleep(1)
      doc %>% 
        print(target = file)
    }
  )
  
  output$report <- downloadHandler(
    filename = function(){
      paste0("funmart_report_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      
      template_path <- tempfile(fileext = ".pptx")
      file.copy("grocerycart-template.pptx", template_path, overwrite = TRUE)
      
      ppt <- read_pptx(template_path)
      
      id <- showNotification(
        "Setting up presentation...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      notify("Setting up presentation...", id = id)
      Sys.sleep(1)
      
      # Title Slide -----
      notify("Analyzing data...", id = id)
      Sys.sleep(1)
      ppt <- 
        ph_with(ppt, 
                value = "Funmart Summary", 
                location = ph_location_label(ph_label = "Title 1")) %>% 
        ph_with(value = str_glue("Date: {format(Sys.Date())}"), 
                location = ph_location_label(ph_label = "Slide Number Placeholder 6"))
      
      # Summary Slide -----
      ul <- unordered_list(
        str_list = c("KPIs", "Order Frequency", "Cohort Analysis"), 
        level_list = rep(1, 3), 
        style = fp_text(color = "steelblue", italic = TRUE, underlined = TRUE)
      )
      
      ppt <- 
        add_slide(ppt, layout = "Title and Content", master = "grocerycart-template") %>% 
        ph_with(value = "Summary", location = ph_location_label(ph_label = "Title 1")) %>% 
        ph_with(value = ul, location = ph_location_label(ph_label = "Content Placeholder 2")) %>% 
        ph_with(value = "2", location = ph_location_label(ph_label = "Slide Number Placeholder 6"))
      
      # KPIs -----
      kpi_table <- 
        report_data() %>% 
        summarise("Orders" = scales::comma(n_distinct(basket_id)), 
                  "Revenue" = scales::dollar(sum(cost), prefix = "GBP "), 
                  "Average Order Value" = scales::dollar(round(mean(cost), 2), prefix = "GBP "))
      
      kpi_text <- 
        str_glue("There were {kpi_table$Orders} orders during this time with a total revenue of {kpi_table$Revenue} and average order value of {kpi_table$`Average Order Value`}.")
      
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
      grocery_freq <- 
        report_data() %>% 
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
      
      # Cohort Analysis -----
      cohort_table <- 
        report_data() %>% 
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
             x = "Month", y = "Cohort") + 
        geom_text(aes(label = str_glue("{round(value, 0)}%")), size  = 3, colour = "snow") + 
        hrbrthemes::theme_ipsum(grid = FALSE, base_size = 16) + 
        scale_fill_gradient(low = blue_palette[1], high = blue_palette[8], guide = "none") + 
        scale_x_continuous(breaks = 1:max(cohort_table$time))
      
      ppt <- 
        add_slide(ppt, layout = "Blank", master = "grocerycart-template") %>% 
        ph_with(value = cohort_plot, location = ph_location_fullsize())
      # Print -----
      notify("Preparing to download report...", id = id)
      Sys.sleep(1)
      ppt %>% 
        print(target = file)
    }
  )
  
  output$files <- renderReactable({
    report_data() %>% 
      reactable(., resizable = TRUE, showPageSizeOptions = FALSE, 
                onClick = "select", highlight = TRUE, sortable = FALSE, 
                theme = fivethirtyeight(centered = TRUE, header_font_size = 11), 
                
                columns = list(
                  basket_id = colDef(name = "basket_id"), 
                  order_id = colDef(name = "order_id"), 
                  cost = colDef(name = "cost"), 
                  customer_id = colDef(name = "customer_id"), 
                  order_date = colDef(name = "order_date"), 
                  payment_method = colDef(name = "payment_method"), 
                  store = colDef(name = "store"), 
                  customer_name = colDef(name = "customer_name"), 
                  customer_age = colDef(name = "customer_age"), 
                  household_size = colDef(name = "household_size"), 
                  long = colDef(name = "long"), 
                  lat = colDef(name = "lat")
                )
                
      )
  })
  
  output$images <- renderImage({
    
    expr = list(src = input$upload_image$datapath, 
                width = "auto", 
                height = "100px")
    
  }, deleteFile = FALSE)
  
  output$sample_data <- renderReactable({
    grocery_sample() %>% 
      reactable(., resizable = TRUE, showPageSizeOptions = FALSE, 
                onClick = "select", highlight = TRUE, sortable = FALSE, 
                theme = fivethirtyeight(centered = TRUE, header_font_size = 11), 
                
                columns = list(
                  basket_id = colDef(name = "basket_id"), 
                  order_id = colDef(name = "order_id"), 
                  cost = colDef(name = "cost"), 
                  customer_id = colDef(name = "customer_id"), 
                  order_date = colDef(name = "order_date"), 
                  payment_method = colDef(name = "payment_method"), 
                  store = colDef(name = "store"), 
                  customer_name = colDef(name = "customer_name"), 
                  customer_age = colDef(name = "customer_age"), 
                  household_size = colDef(name = "household_size"), 
                  long = colDef(name = "long"), 
                  lat = colDef(name = "lat")
                )
                
                )
  })
  
  
  # Recommendation Tab -----
  # Apriori Graph
  output$apriori <- renderVisNetwork({
    txn <-  read.transactions(here::here("data/apriori_items.csv"),
                              format = "basket",
                              sep= ",",
                              rm.duplicates = FALSE,
                              header = TRUE)
    
    unique_rules <- function(rules) {
      rules_subset <- which(colSums(is.subset(rules, rules)) > 1)
      length(rules_subset)
      rules[-rules_subset]
    }
    
    inspect_rules <- function(rules, measure = "support", top = 10) {
      rules %>% 
        sort(by = measure, decreasing = TRUE) %>% 
        head(top) %>% 
        arules::inspect()
    }

    apriori_rules <-
      txn %>%
      apriori(parameter = list(supp = 0.0005, conf = 0.8, minlen = 2, maxlen = 10)) %>%
      unique_rules()

    plot(apriori_rules, method = "graph", engine = "htmlwidget") %>% visIgraphLayout()
  })
  
  # Recommendations
  recom <- reactive({
    grocery_matrix <- 
      grocery %>% 
      mutate(value = 1) %>% 
      tidyr::pivot_wider(names_from = product, values_from = value, values_fill = 0) %>% 
      select(-basket_id) %>% 
      as.matrix() %>% 
      as("binaryRatingMatrix")
    
    grocery_scheme <- 
      grocery_matrix %>% 
      evaluationScheme(method = "bootstrap", k = 10, train = .8, given = -1)
    
    win_model <- Recommender(getData(grocery_scheme, "train"), method = "Popular")
  })
  
  output$table_recommendations <- renderTable({
    input$recommend
    
    sample_order <- isolate(unique(c(input$product1, input$product2, 
                                     input$product3, input$product4, input$product5)))
    
    sample_order_mat <- 
      grocery %>% 
      distinct(product) %>% 
      mutate(ordered = as.numeric(product %in% sample_order)) %>% 
      tidyr::pivot_wider(names_from = product, values_from = ordered) %>% 
      as.matrix() %>% 
      as("binaryRatingMatrix")
    
    pre <- predict(recom(), 
                   newdata = sample_order_mat, 
                   n = 10)
    
    pre %>% as(., "list") %>% .[[1]] %>% as_tibble() %>% rename("Products" = value) %>% arrange(Products)
  })
  
  # Summary Tab -----
  # elgrocer
  output$gg_eg_payment <- renderPlot({
    gg_eg_payment
  })
  
  output$gg_eg_categories <- renderPlot({
    gg_eg_categories
  })
  
  output$gg_eg_subcategories <- renderPlot({
    gg_eg_subcategories
  })
  
  output$gg_eg_top5 <- renderPlot({
    gg_eg_top5
  })
  
  output$gg_eg_top3 <- renderPlot({
    gg_eg_top3
  })
  
  # ocado
  output$reactable_oc_brand <- renderReactable({
    table_oc_brand
  })

  output$reactable_oc_kcal <- renderReactable({
    table_oc_kcal
  })

  output$gg_oc_review <- renderPlot({
    gg_oc_review
  })

  output$reactable_oc_shelf <- renderReactable({
    table_oc_shelf
  })
  
  
})

