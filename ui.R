# Load packages
library(shiny)
library(shinyFeedback)
library(grocerycart)
library(bs4Dash)
library(gt)
library(reactable)
library(fresh)
library(plotly)
library(visNetwork)
library(reactablefmtr)
options(spinner.color="#36827F")

# Define UI using bs4Dash -----
shinyUI(bs4DashPage(
  
  shinyFeedback::useShinyFeedback(), 
  
  title = "grocery_dashboard", 
  
  # Header -----
  header = bs4DashNavbar(
    
    title = dashboardBrand(
      title = "Grocery Dashboard",
      color = "olive",
      href = "https:/github.com/moamiristat/grocery", 
      image = "https://raw.githubusercontent.com/moamiristat/grocerycart/main/grocerycart-hexsticker.png"
    ), 
    
    rightUi = dropdownMenu(
      type = "notifications", 
      notificationItem(
        inputId = "notificationbell", 
        text = "New: Automated Reports Tab", 
        status = "primary", 
        icon = icon("smile")
      )
    ), 
    
    controlbarIcon = icon("filter")
    
  ), 
  
  controlbar = bs4DashControlbar(
    id = "controlbar", 
    collapsed = TRUE, 
    overlay = FALSE, 
    pinned = FALSE, 
    
    controlbarMenu(
      id = "tabcard", 
      side = "left", 
      
      controlbarItem(
        title = "Buckets", 
        radioButtons("bucket_width", "Select Bucket Width:", c(50, 100, 200, 300), inline = TRUE)
      )
      
    )
    
  ), 

  # Left Sidebar -----
  sidebar = dashboardSidebar(
    status = "olive", 
    skin = "light", 
    collapsed = FALSE, 
    minified = TRUE, 
    expandOnHover = TRUE, 
    bs4SidebarUserPanel(image = "https://cdn4.iconfinder.com/data/icons/user-people-2/48/5-512.png", 
                        name = "Hello!"), 
    
    sidebarMenu(
      id = "sidebar_tabs", 
      menuItem(
        text = "Basket Analysis", 
        tabName = "basket_tab", 
        icon = icon("shopping-basket")
      ), 
      menuItem(
        text = "Generate Grocery Data", 
        tabName = "data_tab", 
        icon = icon("database")
      ), 
      menuItem(
        text = "Automated Reports", 
        tabName = "reports_tab", 
        icon = icon("file-download"), 
        badgeLabel = "new", 
        badgeColor = "primary"
      ), 
      menuItem(
        text = "Recommendations", 
        tabName = "recommendation_tab", 
        icon = icon("exchange-alt")
      ), 
      menuItem(
        text = "Summary", 
        icon = icon("bars"), 
        startExpanded = FALSE,
        menuSubItem(text = "Elgrocer", 
                    tabName = "eg_tab", 
                    icon = icon("apple-alt")), 
        menuSubItem(text = "Ocado", 
                    tabName = "oc_tab", 
                    icon = icon("carrot"))
      ), 
      menuItem(
        text = "Coming Soon", 
        tabName = "comingsoon_tab", 
        icon = icon("spinner")
      )
    )
    
  ), 
  
  # Main Body -----
  body = dashboardBody(
    
    
    bs4TabItems(
      
      # Basket Tab -----
      bs4TabItem(
        tabName = "basket_tab", 
        bs4Quote("Analyze grocery basket data. Product data was collected/scraped from the 
                 elgrocer and Ocado websites, while customer/basket data was 
                 simulated (view the 'Generate Grocery Data' for more info). 
                 Click on each '+' below to expand the card.", color = "info"), 
        
        fluidRow(
          valueBox(
            value = "12,000", 
            subtitle = "Orders", 
            color = "olive", 
            footer = div("Between Jan 1, 2020 & Dec 31, 2021"), 
            icon = icon("hand-holding-usd"), 
            width = 4
          ), 
          valueBox(
            value = 12, 
            subtitle = "products/basket on average", 
            color = "primary", 
            footer = div("95% of baskets contain 6 to 18 products"), 
            icon = icon("shopping-cart"), 
            width = 4
          ), 
          valueBox(
            value = "75%", 
            subtitle = "of customers ordered ≤ 3 times", 
            color = "lightblue", 
            footer = div("The other 25% ordered 4 to 9 times"), 
            icon = icon("cash-register"), 
            width = 4
          )), 
        
        fluidRow(
          bs4Card(plotOutput("gg_month_order"), 
                  width = 4, status = "olive", collapsed = TRUE, 
                  title = "Combined Monthly Orders for '20-'21"), 
          bs4Card(plotOutput("gg_product_per_basket"), 
                  width = 4, status = "primary", collapsed = TRUE, 
                  title = "# of Products/Basket"), 
          bs4Card(plotOutput("gg_order_freq"), 
                  width = 4, status = "lightblue", collapsed = TRUE, 
                  title = "Order Frequency")
        ), 
        
        fluidRow(
          bs4Card(gt_output(outputId = "gt_table"),
                  width = 4, status = "olive", collapsed = TRUE, 
                  title = "Average Order Value by Month"), 
          bs4Card(reactableOutput("reactable_quarter_table") %>% shinycssloaders::withSpinner(), 
                  width = 8, status = "olive", collapsed = FALSE, 
                  title = "Orders Growth by Quarter")
        ), 
        
        fluidRow(
          bs4Card(plotOutput("gg_buckets", height = "450px") %>% shinycssloaders::withSpinner(), 
                  width = 12, status = "primary", collapsed = FALSE, 
                  title = "Basket Price Buckets: Click on the filter icon on the 
                  top right corner of the page to select bucket width", 
                  footer = "Example: 345 Orders were between 0-50", 
                  maximizable = TRUE)
        ), 
        
        fluidRow(
          bs4Card(plotOutput("gg_payment"), 
                  width = 12, status = "lightblue", collapsed = TRUE, 
                  title = "Order Frequency by Payment Method", 
                  footer = "Example: 113 Customers placed 2 orders using Cash on Delivery")
        ), 
        
        fluidRow(
          bs4Card(plotlyOutput("plotly_popularity", height = "700px"), 
                  width = 12, status = "primary", collapsed = TRUE, 
                  title = "Product Popularity: Hover over the points to view 
                  how much of the total revenue it was responsible for & 
                  # of baskets it was included in", 
                  footer = "A: High $ + # | 
                  B: High $ + Low # | 
                  C: Low $ + Low # | 
                  D: Low $ + High $", 
                  maximizable = TRUE)
        ), 
        
        fluidRow(
          bs4Card(plotOutput("gg_cohort", height = "600px"), 
                  width = 12, status = "lightblue", collapsed = TRUE, 
                  title = "Customer Cohort Analysis", 
                  footer = "Example from the end of the top row: 18% of the customers 
                  from 'Cohort 1' placed an order 23 months after their 1st order", 
                  maximizable = TRUE)
        )

      ), 
      
      # Data Download Tab -----
      bs4TabItem(
        tabName = "data_tab", 
        bs4Quote("Download grocery basket data. Set your parameters below, 
                 then click on the 'Generate Data' button. After a few seconds, 
                 you can view the data in the app and even download it as a zip folder 
                 for your use. The data is fictional.", color = "indigo"), 
        
        fluidRow(
          
          bs4Card(numericInput("num_of_customers", "Total Customers:", 
                               value = 100), 
                  numericInput("num_of_orders", "Orders Placed:", 
                               value = 240), 
                  numericInput("num_of_products", "Products Available:", 
                               value = 40), 
                  sliderInput("avg_items", "Average # of Products per Basket: ", 
                              value = 12, min = 3, max = 30, step = 1, round = TRUE), 
                  textOutput("out"), 
                  br(), 
                  actionButton("generate", "Generate Data", icon = icon("lightbulb")), 
                  br(), 
                  p("Note: The exact number of Customers might not match up exactly 
                    as your input since duplicates of customer's names are dropped. 
                    The minimum possible # of products in any basket is 3 (this value cannot be changed)."), 
                  width = 4, status = "indigo", collapsed = FALSE, collapsible = FALSE,  
                  title = "Required Inputs"), 
          
          bs4Card(title = "Generated Data: Click to expand", 
            width = 8, status = "indigo", collapsed = FALSE, collapsible = FALSE, 
            accordion(id = "accordion_data", 
                      accordionItem(
                        title = "Customer Table", 
                        reactableOutput("reactable_customer")
                        ), 
                      accordionItem(
                        title = "Order Table", 
                        reactableOutput("reactable_order")
                        ), 
                      accordionItem(
                        title = "Basket Table", 
                        reactableOutput("reactable_basket")
                        )
                      ), 
            downloadButton("download_data", "Download All 3 .tsv files", icon = icon("file-download"))
            )
        
          )
        
      ), 
      
      # Reports Tab -----
      bs4TabItem(
        tabName = "reports_tab", 
        bs4Quote("Upload data files to automatically generate reports based on template. 
                 Follow the steps below for a quick walkthrough. 
                 The editable report and invoice will each take less than 10 seconds 
                 to become available for download.", color = "maroon"), 
        
        fluidRow(
          bs4Card("STEP 1: Download sample data (see below) to upload later", 
                  br(), 
                  downloadButton("download_sample", "Download Sample Data", icon = icon("download")), 
                  br(), 
                  br(), 
                  "STEP 2: Enter inputs used for invoice calculations", 
                  numericInput("fee_delivery", "Delivery Fee ($):", 
                               value = 9), 
                  numericInput("fee_percent", "Sales Fee (%):", 
                               value = .04), 
                  "STEP 3: Upload image/logo (added to invoice header)", 
                  fileInput("upload_image", "Upload 1 Logo (5 MB limit):", 
                            accept = c("image/*")), 
                  "STEP 4: Upload the sample data from step 1", 
                  fileInput("upload_data", NULL, accept = c(".csv", ".tsv")), 
                  width = 4, status = "maroon", collapsed = FALSE, collapsible = FALSE, 
                  "STEP 5: Generate Invoice", 
                  br(), 
                  downloadButton("invoice", "Generate Invoice (.docx)", icon = icon("file-invoice-dollar")), 
                  br(), 
                  br(), 
                  "STEP 6: Generate Powerpoint Report", 
                  br(), 
                  downloadButton("report", "Generate Report (.pptx)", icon = icon("file-powerpoint"))
          ), 
          
          bs4Card(title = "Successfully Uploaded Items", 
                  width = 8, status = "maroon", collapsed = FALSE, collapsible = FALSE, 
                  h3(strong("Note")), 
                  p("This automated report generation example was built to 
                    showcase the possibilites, not to be flexible. For example, 
                    the column names and types need to be the exact same as the sample data below. 
                    That is why it is best to follow the steps on the left."), 
                  h3(strong("Image")), 
                  imageOutput("images", height = "100px", width = "auto"), 
                  h3(strong("Data")), 
                  reactableOutput("files")
          )
          
        ), 
        
        bs4Card(reactableOutput("sample_data"), 
                width = 12, status = "maroon", collapsed = TRUE, 
                title = "Sample Data to Download", 
                maximizable = TRUE)
        #CONITUE............Upload ---> Does work in Rmd ---> Download
      ), 
      
      # Recommendations Tab -----
      bs4TabItem(
        tabName = "recommendation_tab", 
        bs4Quote("Product recommendations based on products already in basket.", color = "lightblue"), 
        
        bs4Card(width = 12, status = "lightblue", collapsed = FALSE, 
                title = "Product Associations: Select a specific product", 
          fluidRow(
            bs4Card(purrr::map(1:5, function(i) {
              # Functional programming to add multiple input boxes in parallel
              selectInput(paste0("product", i), paste0("Product #", i), choices = c("", products_available))
              }), 
                    actionButton("recommend", "Recommend Products", icon = icon("plus")), 
                    width = 4, status = "lightblue", collapsed = FALSE, collapsible = FALSE,  
                    title = "Recommender System: Recommend 10 products based on 5 products added to basket"), 
            bs4Card(p("A small # of products were present in most baskets. Naturally, these common products will
                    be recommended by the system a lot since they exist in an overwhleming # of baskets
                    (i.e., Nikai Air Fryer 3.2L). Also, there are only 200 products in total, so this magnifies
                    the effect of the common products."), 
              tableOutput("table_recommendations"), 
                    width = 8, status = "lightblue", collapsed = FALSE, collapsible = FALSE,  
                    title = "Recommended based on other orders") 
          )), 
        
        bs4Card(visNetworkOutput("apriori") %>% shinycssloaders::withSpinner(), 
                width = 12, status = "lightblue", collapsed = TRUE, 
                title = "Associations Network Graph: Select a specific product via the dropdown menu, then
                hover over (or zoom into) the graph to view the products most commonly bought with it", 
                maximizable = TRUE)
        
      ), 
      
      # Summary Tab -----
      bs4TabItem(
        tabName = "eg_tab", 
        bs4Quote("Summary of the data scraped from the elgrocer website", color = "olive"), 
        
        fluidRow(
          bs4Card(plotOutput("gg_eg_top5") %>% shinycssloaders::withSpinner(), 
                  width = 6, status = "olive", collapsed = FALSE, 
                  title = "Top 5 Most Expensive Products", 
                  maximizable = TRUE), 
          bs4Card(plotOutput("gg_eg_top3") %>% shinycssloaders::withSpinner(), 
                  width = 6, status = "olive", collapsed = FALSE, 
                  title = "Top 3 Most Expensive Stores on Average", 
                  maximizable = TRUE)
        ), 
        
        fluidRow(
          bs4Card(plotOutput("gg_eg_payment"), 
                  width = 4, status = "olive", collapsed = TRUE, 
                  title = "Payment Methods Offered At Stores"), 
          bs4Card(plotOutput("gg_eg_categories"), 
                  width = 4, status = "olive", collapsed = TRUE, 
                  title = "Number of Categories in Stores", 
                  footer = "Example: 66 stores have 0 to 9 categories"), 
          bs4Card(plotOutput("gg_eg_subcategories"), 
                  width = 4, status = "olive", collapsed = TRUE, 
                  title = "Number of Subcategories in Stores", 
                  footer = "Example: 77 stores have 0 to 9 subcategories")
        )
        
      ), 

      bs4TabItem(
        tabName = "oc_tab",
        bs4Quote("Summary of the data scraped from the ocado website", color = "purple"),

        fluidRow(
          bs4Card(plotOutput("gg_oc_review") %>% shinycssloaders::withSpinner(),
                  width = 6, status = "purple", collapsed = FALSE,
                  title = "Most Reviewed Products"),
          bs4Card(reactableOutput("reactable_oc_shelf") %>% shinycssloaders::withSpinner(),
                  width = 6, status = "purple", collapsed = FALSE,
                  title = "Most common shelf life for each brand",
                  maximizable = TRUE)
        ),

        fluidRow(
          bs4Card(reactableOutput("reactable_oc_kcal") %>% shinycssloaders::withSpinner(),
                  width = 6, status = "purple", collapsed = FALSE,
                  title = "Product Price & Calories",
                  maximizable = TRUE), 
          bs4Card(reactableOutput("reactable_oc_brand"),
                  width = 6, status = "purple", collapsed = TRUE,
                  title = "Brands",
                  maximizable = TRUE)
        )
        
      ),
      
      # Coming Soon Tab -----
      bs4TabItem(
        tabName = "comingsoon_tab", 
        jumbotron(title = "Reviews Text Analysis", 
                  lead = "Analyze customer reviews for Ocado products", 
                  p("The raw text data was scraped from the Ocado website and 
                    is available as part of the 'grocerycart' R package."), 
                  btnName = "View Updates on Github", 
                  href = "https:/github.com/moamiristat/grocery", 
                  status = "info"), 
        boxLayout(
          type = "deck", 
          map(1:4, function(i) {
            box(
              width = NULL, 
              title = paste("Card", i), 
              closable = FALSE, 
              collapsible = FALSE, 
              "Text analysis algorithm 1-4"
            )
          })
        )
      )
    )
    
  ), 
  
  # Footer -----
  footer = bs4DashFooter(left = a("Project Github", href = "https:/github.com/moamiristat/grocery"), 
                         fixed = FALSE)
))
