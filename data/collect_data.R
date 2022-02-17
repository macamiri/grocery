##### Load packages -----
# Main packages loaded: robotstxt, Selenium, rvest, purrr
# Packages used with namespace: netstat, crayon
pacman::p_load(robotstxt, RSelenium, rvest, purrr)

##### Misc. functions -----
# nytnyt: sleep & print it time
nytnyt <- function(period = c(1, 2)){
  tictoc <- runif(1, period[1], period[2])
  cat(paste0(">>> Sleeping for ", round(tictoc, 2), "seconds\n"))
  Sys.sleep(tictoc)
}

# scroll_to_element: scrolls to & highlights element
scroll_to_element <- function(remDr, webElem) {
  remDr$executeScript("arguments[0].scrollIntoView(true);",
                      args = list(webElem))
  webElem$highlightElement()
}

##### Check which webpages are not bot friendly -----
url <- "https://www.elgrocer.com"

rtxt <- robotstxt(domain = url)
rtxt$comments
rtxt$crawl_delay
rtxt$permissions

paths_allowed(domain = url, paths = c("/store", "/stores"))
# We can collect data from the webpages we are interested in

##### Initiate Selenium server -----
initiate_server <- rsDriver(port = netstat::free_port(), 
                            browser = "firefox", 
                            verbose = FALSE)

# Set up remote client driver & open a session
remDr <- initiate_server$client
remDr$open()

##### Selenium to collect data -----
### How many links to click in the '1st-layer'?
remDr$navigate(url)
cat(crayon::blue(remDr$getTitle()))

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

# Grab the categories images
rem_category_images <- remDr$findElements(using = "css selector", 
                                          value = "img.center")

category_image <- rem_category_images[[1]]$getElementAttribute("src")
category_image <- magick::image_read(path = category_image %>% unlist())


# Click on a specific store category (e.g., snacks)
# continue...





##### All in 1 function -----
rem_locations_links <- vector(mode = "list", length = length(locations))
for(i in 1:4) {
  rem_locations_links[[i]] <- remDr$findElement(using = "link text", 
                                                value = locations[[i]])
  print(i)
  rem_locations_links[[i]]$clickElement()
  nytnyt(period = c(9,10))
  remDr$goBack()
  nytnyt(period = c(9,10))
}

