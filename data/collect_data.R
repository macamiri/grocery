##### 1: Load packages -----
# Main packages loaded: robotstxt, Selenium, rvest, purrr
# Packages used with namespace: netstat, crayon
pacman::p_load(robotstxt, RSelenium, rvest, purrr, stringr)

##### 2: Misc. functions -----
# Sleep & print time
get_page_title <- function(remDr) {
  cat(crayon::blue(remDr$getTitle()))
}

nytnyt <- function(period = c(1, 2)){
  tictoc <- runif(1, period[1], period[2])
  cat(crayon::green(paste0(">>> Sleeping for ", round(tictoc, 2), " seconds\n")))
  Sys.sleep(tictoc)
}

# Get the html elementS using rvest & chosen selector
get_html_elements <- function(remDr, ..., type = "text", attribute_selector) {
  page <- remDr$getPageSource() %>% 
    .[[1]] %>% 
    read_html() %>% 
    html_elements(...)
  
  if(type == "text") {
    page %>% 
      html_text()
  } else if(type == "attribute") {
    page %>% 
      html_attr(attribute_selector)
  } else if(type == "attributes") {
    page %>% 
      html_attrs()
  } else {
    cat(crayon::red("Type must be 1 of these: text, attribute, attributes"))
  }
}

# Get the html elemenT using rvest & chosen selector
get_html_element <- function(remDr, ..., type = "text", attribute_selector) {
  page <- remDr$getPageSource() %>% 
    .[[1]] %>% 
    read_html() %>% 
    html_element(...)
  
  if(type == "text") {
    page %>% 
      html_text()
  } else if(type == "attribute") {
    page %>% 
      html_attr(attribute_selector)
  } else if(type == "attributes") {
    page %>% 
      html_attrs()
  } else {
    cat(crayon::red("Type must be 1 of these: text, attribute, attributes"))
  }
}

# Scroll to & highlight element
scroll_to_element <- function(remDr, webElem) {
  remDr$executeScript("arguments[0].scrollIntoView(true);",
                      args = list(webElem))
  webElem$highlightElement()
}

# Scroll to bottom of page and dynamically load more results
scroll_down_and_load <- function(remDr){
  
  last_height <- 
    remDr$executeScript("return document.body.scrollHeight") %>% 
    unlist()
  
  new_height <- 0
  
  while(TRUE){
    remDr$executeScript(
      script = "window.scrollTo(0, document.body.scrollHeight)")
    
    nytnyt(c(2, 3))
    
    new_height <- 
      remDr$executeScript("return document.body.scrollHeight") %>% 
      unlist()
    
    print(list(old = last_height, new = new_height))
    
    if(new_height == last_height){
      break
    }
    
    last_height <- new_height
  }
  nytnyt(period = c(2,5))
  return(last_height)
}

# Scroll to the top of the page
scroll_to_top <- function() {
  remDr$executeScript("window.scrollTo(0, 0);", args = list(1))
}

# Number of times to go back
go_back <- function(remDr, 
                    times = length(store_subcategories) + 1, 
                    min = 5, 
                    max = 10) {
  walk(1:times, function(x) {
    remDr$goBack()
    nytnyt(c(min, max))
  })
}

# Go to the categories page of the current store
store_categories_url <- function(remDr) {
  url <- remDr$getCurrentUrl() %>% 
    unlist()
  
  cat(crayon::blue("Saved url: ", url))
  return(url)
}

# Verify that output length from selenium & rvest match
verify_length_match <- function(sel = num_of_stores_selenium, 
                                rve = num_of_store_rvest) {
  if(sel == rve) {
    cat(crayon::green("Success! Lengths match: ",  rve, "\n"))
  } else {
    cat(crayon::red("Go Back! Lengths match DO NOT match:\n",
                    "From sel: ", sel, "\n", 
                    "From rve: ", rve,"\n"))
  }
}

# Extract the link for a specific subcategory
category_element <- function(type = "category", element) {
  if(!is.character(element)) {
    element <- as.character(element)
    cat(crayon::blue("Input was coerced into a character \n"))
  }
  
  switch(type, 
         "category" = {
           category_position <-
             store_categories %>%
             str_detect(regex(pattern = element, ignore_case = TRUE)) %>%
             which()
           
           if(is_empty(category_position)) {
             cat(crayon::blue("No match was found \n"))
           } else {
             paste0(url, category_links[category_position])
           }
         }, 
         "subcategory" = {
           subcategory_position <-
             store_subcategories %>%
             str_detect(regex(pattern = element, ignore_case = TRUE)) %>%
             which()
           
           if(is_empty(subcategory_position)) {
             cat(crayon::blue("No match was found \n"))
           } else {
             paste0(url, subcategory_links[subcategory_position])
           }
         })
}

##### 3: Check which webpages are not bot friendly -----
url <- "https://www.elgrocer.com"

rtxt <- robotstxt(domain = url)
rtxt$comments
rtxt$crawl_delay
rtxt$permissions

paths_allowed(domain = url, paths = c("/store", "/stores"))
# We can collect data from the webpages we are interested in

##### 4: Initiate Selenium server -----
initiate_server <- rsDriver(port = netstat::free_port(), 
                            browser = "firefox", 
                            verbose = FALSE)

# Set up remote client driver & open a session
remDr <- initiate_server$client
remDr$open()

##### 5: Selenium to collect data -----
### How many links to click in the '1st-layer'?
remDr$navigate(url)
get_page_title(remDr)

# Find all available locations : 131 links in the '1st-layer'
locations <- get_html_elements(remDr, 
                               css = ".text-success", 
                               type = "text")
locations


### Collect the store info for each location
# Click on location link
location_links <- get_html_elements(remDr, 
                                    css = ".text-success", 
                                    type = "attribute", 
                                    attribute_selector = "href")

remDr$navigate(paste0(url, location_links[[1]]))

# Click on 'i' icon: (1) click on each one, (2) collect data
# (1) scroll to the title & click on each 'i' icon
scroll_down_and_load(remDr)
scroll_to_top()
location_title <- get_html_element(remDr, css = "h1")

num_of_store_rvest <- 
  get_html_elements(remDr, css = "h2.text-black") %>% 
  length()

rem_store_info <- remDr$findElements(using = "class name", 
                                     value = "store-info")
rem_store_info %>% 
  map(~ .$clickElement()) %>% 
  unlist()

num_of_stores_selenium <- length(rem_store_info)

# Verify correct number of stores
verify_length_match()


# (2) collect data
store_details <- get_html_elements(remDr, css = ".store-detail")
store_links <- get_html_elements(remDr, 
                                 css = ".store-grid", 
                                 type = "attribute", 
                                 attribute_selector = "href")

### Collect category data from each store
# Click on store ---> categories
remDr$navigate(paste0(url, store_links[[1]]))

rem_category_link <- remDr$findElement(using = "class name", 
                                       value = "category-link")
rem_category_link$clickElement()

current_store_categories_url <- store_categories_url(remDr)

# Grab the categories image links
category_image_links <- get_html_elements(remDr, 
                                          css = "img.center", 
                                          type = "attribute", 
                                          attribute_selector = "src")

# category_image <- magick::image_read(path = category_image_link %>% 
#                                        unlist())

# What categories are available
store_categories <- 
  get_html_elements(remDr, css = "h3.text-black") %>% 
  str_trim(side = "both")

# Grab the category links (e.g., snacks OR store_categories[[i]])
scroll_down_and_load(remDr)
scroll_to_top()

category_links <- get_html_elements(remDr, 
                                    css = ".category-card", 
                                    type = "attribute", 
                                    attribute_selector = "href")

verify_length_match(sel = length(store_categories), 
                    rve = length(category_links))

category_url <- category_element(type = "category", element = "snacks")

# Click on chosen category
remDr$navigate(category_url)

# How many "sub-categories" /{all}
store_subcategories <- 
  get_html_elements(remDr, css = ".text-primery-1") %>% 
  str_trim(side = "both")
length(store_subcategories)

subcategory_links <- 
get_html_elements(remDr, 
                    css = "div.ng-tns-c20-0>div:nth-child(1) > a:nth-child(1)", 
                    type = "attribute", 
                    attribute_selector = "href") %>% 
  .[-1]

verify_length_match(sel = length(store_subcategories), 
                    rve = length(subcategory_links))

# Choose a specific store subcategory (e.g., sweets OR store_subcategories[[i]])
subcategory_url <- category_element(type = "subcategory", element = "biscuits")

# Click on chosen subcategory
remDr$navigate(subcategory_url)

# Scroll to the bottom to dynamically load all of the items
scroll_down_and_load(remDr)
scroll_to_top()

# Grab item title, weight, price, image
item_title <- get_html_elements(remDr, css = "h2.text-black")
item_weight <- get_html_elements(remDr, css = "div.item-label")
item_price <- get_html_elements(remDr, css = "div.item-price")
item_image_links <- get_html_elements(remDr, 
                                      css = "img.center", 
                                      type = "attribute", 
                                      attribute_selector = "src")

# item_image <- magick::image_read(path = item_image_links %>% 
#                                    unlist())

tibble::tibble(item_title, item_weight, item_price, item_image_links)
# Repeat above for each subcategory

# Go back subcategory + 1 times OR simply navigate to categories page
go_back(remDr, times = length(subcategory_links) + 1)
# OR
remDr$navigate(current_store_categories_url)

# Repeat 127 "What categories are available" TO 214 for the next category

# Go back to homepage ---> next location
go_back(remDr, times = 1)

# Repeat "Selenium to collect data" for each location 

# continue...
# IF store info already collected ---> break from existing loop
# loop 1: nest the locations
# loop 2: if new store ---> info + click, else ignore/break store
# loop 3: click each category ---> grab item data



##### Close Selenium server -----
remDr$close()
remDr$closeWindow()
system("kill /im java.exe /f")
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
gc()
##### Substitute Rselenium with Rvest where applicable -----
# lines 180-183
page <- 
  remDr$getPageSource() %>% 
  .[[1]] %>% 
  read_html() %>% 
  html_elements(css = ".text-black") %>% 
  html_text()


##### All in 1 function -----
rem_locations_links <- vector(mode = "list", length = length(locations))
# use map/walk instead of for loop
for(i in 1:4) {
  rem_locations_links[[i]] <- remDr$findElement(using = "link text", 
                                                value = locations[[i]])
  print(i)
  rem_locations_links[[i]]$clickElement()
  nytnyt(period = c(9,10))
  remDr$goBack()
  nytnyt(period = c(9,10))
}

