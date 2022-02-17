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
    
    Sys.sleep(2)
    
    new_height <- 
      remDr$executeScript("return document.body.scrollHeight") %>% 
      unlist()
    
    print(list(old = last_height, new = new_height))
    
    if(new_height == last_height){
      break
    }
    
    last_height <- new_height
  }
  return(last_height)
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

# Find all available locations
rem_locations <- remDr$findElements(using = "class name", 
                                    value = "text-success")
locations <- rem_locations %>% 
  map(~ .$getElementText()) %>% 
  unlist()

locations
# There are 131 links to click in the '1st-layer'

### Collect the store info for each location
# Click on location link
rem_locations_link <- remDr$findElement(using = "link text", 
                                        value = "Abu Hail")

rem_locations_link$clickElement()

# Click on 'i' icon: (1) click on each one, (2) collect data
# (1) scroll to the title & click on each 'i' icon
rem_title <- remDr$findElement(using = "tag name", 
                               value = "h1")

rem_title$getElementText() # SAVE the title for later use

# NEED TO SCROLL???
# scroll_to_element(remDr, rem_title)

rem_store_info <- remDr$findElements(using = "class name", 
                                     value = "store-info")

rem_store_info %>% 
  map(~ .$clickElement()) %>% 
  unlist()


# (2) collect data
rem_store_details <- remDr$findElements(using = "class name", 
                                        value = "store-detail")

store_details <- rem_store_details %>% 
  map(~ .$getElementText()) %>% 
  unlist() %>% 
  str_replace_all("\n", " ")

### Collect category data from each store
# Click on store ---> categories
rem_store_details[[1]]$clickElement()
rem_category_link <- remDr$findElement(using = "class name", 
                                       value = "category-link")
rem_category_link$clickElement()

# What categories are available
rem_category_title <- remDr$findElements(using = "tag name", 
                                         value = "h3")
store_categories <- rem_category_title %>% 
  map(~ .$getElementText()) %>% 
  unlist()

# Choose a specific store category (e.g., snacks OR store_categories[[i]])
category_element <- rem_category_title %>% 
  map(., ~ .$getElementText()) %>% 
  unlist() %>% 
  str_detect(., regex("\\s{0}snacks\\s{0}", ignore_case = TRUE)) %>% 
  which()

rem_category_selected <- rem_category_title[[category_element]]
scroll_to_element(remDr, rem_category_selected)

# Grab the categories images
rem_category_images <- remDr$findElements(using = "css selector", 
                                          value = "img.center")

category_image_link <- rem_category_images[1] %>% 
  map(., ~ .$getElementAttribute("src"))
category_image <- magick::image_read(path = category_image_link %>% 
                                       unlist())

# Click on chosen category
rem_category_selected$clickElement()

# How many "sub-categories" /{all}
rem_subcategory_title <- remDr$findElements(using = "class name", 
                                            value = "text-primery-1")

store_subcategories <- rem_subcategory_title %>% 
  map(., ~ .$getElementText()) %>% 
  unlist()
length(store_subcategories)

# Choose a specific store subcategory (e.g., sweets OR store_subcategories[[i]])
subcategory_element <- store_subcategories %>% 
  str_detect(., regex("\\s{0}sweets\\s{0}", ignore_case = TRUE)) %>% 
  which()

rem_subcategory_selected <- rem_subcategory_title[[subcategory_element]]

# Click on chosen subcategory
rem_subcategory_selected$clickElement()
scroll_to_element(remDr, rem_subcategory_selected)

# Scroll to the bottom to dynamically load all of the items
scroll_down_and_load(remDr)

# Grab item title, weight, price, image
rem_item_title <- remDr$findElements(using = "class name", 
                                     value = "text-black")
item_title <- rem_item_title %>% 
  map(., ~ .$getElementText()) %>% 
  unlist()

rem_item_title <- remDr$findElements(using = "class name", 
                                     value = "item-label")
item_label <- rem_item_title %>% 
  map(., ~ .$getElementText()) %>% 
  unlist()

rem_item_title <- remDr$findElements(using = "class name", 
                                     value = "item-price")
item_price <- rem_item_title %>% 
  map(., ~ .$getElementText()) %>% 
  unlist()

rem_subcategory_images <- remDr$findElements(using = "css selector", 
                                             value = "img.center")
subcategory_image_link <- rem_subcategory_images %>% 
  map(., ~ .$getElementAttribute("src"))
subcategory_image <- magick::image_read(path = subcategory_image_link %>% 
                                          unlist())

tibble::tibble(item_title, item_label, item_price, subcategory_image_link)
# Repeat above for each subcategory

# Go back subcategory + 1 times
go_back(remDr)

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

