##### 1: Load packages -----
# Main packages loaded:robotstxt, Selenium, rvest, purrr
# Packages used with namespace: etstat, crayon, tibble, dplyr, magick, progressr
pacman::p_load(robotstxt, RSelenium, rvest, purrr, stringr)




##### 2: Misc. functions -----
# Sleep & print time
get_page_title <- function(remDr) {
  cat(crayon::blue(remDr$getTitle()))
}

nytnyt <- function(period = c(1, 2), crayon_col = crayon::green, ...){
  tictoc <- runif(1, period[1], period[2])
  cat(crayon_col(paste0(">>> Sleeping for ", 
                           round(tictoc, 2), 
                           " seconds\n", ...)))
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
  
  cat(crayon::blue("Saved url:", url))
  return(url)
}

# Get the current url in selenium
current_url <- function(remDr) {
  url <- remDr$getCurrentUrl() %>% 
    unlist()
  
  cat(crayon::bgCyan("Currently on:", url, "\n"))
}

# Verify that output length from selenium & rvest match
verify_length_match <- function(sel = num_of_stores_selenium, 
                                rve = num_of_stores_rvest) {
  if(sel == rve) {
    cat(crayon::green("Success! Lengths match: ",  rve, "\n"))
  } else {
    stop(crayon::red("Go Back! Lengths match DO NOT match:\n",
                    "From sel:", sel, "\n", 
                    "From rve:", rve))
  }
}

# Sound when work complete in map
sound_work_complete <- function(expr1, expr2) {
  
  if(expr1 == expr2) {
    beepr::beep(sound = "complete", 
                expr = cat(crayon::bgWhite$green$bold("Work Complete!")))
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
### (A) Collect the locations and their links -----
collect_location_links <- function() {
  # Navigate to homepage
  remDr$navigate(url)
  nytnyt(c(5, 10), crayon_col = crayon::blue, "Make sure page loads \n")
  get_page_title(remDr)
  
  # 131 total locations
  locations <- get_html_elements(remDr, 
                                 css = ".text-success", 
                                 type = "text")
  
  # Collect 131 location links
  location_links_extensions <- get_html_elements(remDr, 
                                                 css = ".text-success", 
                                                 type = "attribute", 
                                                 attribute_selector = "href")
  location_links <- paste0(url, location_links_extensions)
  
  tibble::tibble(location = locations, location_link = location_links)
}
location_tibble <- collect_location_links()

# Find the potential number of stores - to avoid recursively building tibble
# 331
# start <- Sys.time()
# num_of_stores <- 
#   location_links %>% 
#   map(., function(.x) {
#     cat(crayon::bgMagenta("Currently on:", .x, "\n"))
#     nytnyt(c(2, 5))
#     
#     .x %>% 
#       read_html() %>% 
#       html_elements(css = "h2.text-black") %>% 
#       length()
#   }) %>% 
#   reduce(.f = sum)
# end <- Sys.time()

### (B) Collect the store 'i' details, and links -----
collect_stores_details <- function(links_to_use = location_tibble$location_link) {
  
  links_to_use %>% 
    map_dfr(., function(.x) {
      # Navigate to the url
      remDr$navigate(.x)
      nytnyt(c(5, 10), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Scroll
      scroll_down_and_load(remDr)
      scroll_to_top()
      
      # Grab location title
      location_title <- get_html_element(remDr, css = "h1")
      
      # Number of stores - rvest
      num_of_stores_rvest <- 
        get_html_elements(remDr, css = "h2.text-black") %>% 
        length()
      
      # Click on the 'i' icon to reveal more data
      rem_store_info <- remDr$findElements(using = "class name", 
                                           value = "store-info")
      
      rem_store_info %>% 
        map(., ~ .$clickElement()) %>% 
        unlist()
      
      # Number of stores - selenium
      num_of_stores_selenium <- length(rem_store_info)
      
      # Verify that all stores' 'i' icon was clicked
      verify_length_match(num_of_stores_rvest, num_of_stores_selenium)
      
      # Collect the extra 'i' icon data
      store_details <- get_html_elements(remDr, css = ".store-detail")
      nytnyt(c(0, 1), 
             crayon_col = crayon::magenta, 
             "Got details. Grab links in location:", 
             which(.x == links_to_use), 
             " out of ", 
             length(links_to_use),
             "\n")
      
      store_links <- get_html_elements(remDr, 
                                       css = ".store-grid", 
                                       type = "attribute", 
                                       attribute_selector = "href")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == links_to_use), length(links_to_use))
      
      # Store data in a tibble
      tibble::tibble(location = rep(location_title, num_of_stores_rvest), 
                     details = store_details, 
                     store_link = paste0(url, store_links), 
                     location_link = rep(.x, num_of_stores_rvest))
    }
    )
}
store_tibble <- collect_stores_details(location_tibble$location_link)

### (C) Collect categories data (delete duplicates here) ----- 
collect_categories <- function(links_to_use = store_tibble$store_link) {
  # Category links
  links <- paste0(links_to_use, "/categories")
  unique_links <- unique(links)
  
  unique_links %>% 
    map_dfr(., function(.x) {
      remDr$navigate(.x)
      nytnyt(c(5, 10), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Grab store name
      store_name <- 
        get_html_element(remDr, css = "h2.text-black") %>% 
        str_trim(side = "both") %>% 
        str_remove(" Product Categories")
      
      # Scroll
      scroll_down_and_load(remDr)
      scroll_to_top()
      
      # Grab category image links
      category_image_links <- get_html_elements(remDr, 
                                                css = "img.center", 
                                                type = "attribute", 
                                                attribute_selector = "src")
      
      # Grab the category titles
      store_categories <- 
        get_html_elements(remDr, css = "h3.text-black") %>% 
        str_trim(side = "both")
      
      num_of_categories <- length(store_categories)

      # Grab the category links
      category_link_ext <- get_html_elements(remDr, 
                                             css = ".category-card", 
                                             type = "attribute", 
                                             attribute_selector = "href")
      
      category_links <- paste0(url, category_link_ext)
      
      # Verify that every category's link was collected
      verify_length_match(sel = num_of_categories, 
                          rve = length(category_links))
      
      # Sleep
      nytnyt(c(0, 1),
             crayon_col = crayon::magenta,
             "Got category images, titles & links. Completed ",
             which(.x == unique_links),
             " out of ",
             length(unique_links),
             " links \n")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == unique_links), length(unique_links))
      
      # Store data in a tibble
      tibble::tibble(store_name = rep(store_name, num_of_categories), 
                     category = store_categories, 
                     category_link = category_links, 
                     image_link = category_image_links, 
                     num_of_categories = num_of_categories, 
                     store_link = rep(.x, num_of_categories))
    }
    )
}
category_tibble <- collect_categories(store_tibble$store_link)

# Get number of categories in each store, then remove column
num_of_categories_tibble <- 
  category_tibble %>% 
    dplyr::group_by(store_name) %>% 
    dplyr::summarise(num_of_categories = max(num_of_categories))

category_tibble <- 
  category_tibble %>% 
  dplyr::mutate(num_of_categories = NULL)

# Remove offer/promotion page
category_tibble <- 
  category_tibble %>% 
    dplyr::filter(!str_detect(category_link, "promotion"))

# category_image <- magick::image_read(path = category_image_links %>% 
#                                        unlist())


### (D) Collect subcategories data -----
collect_subcategories <- function(links_to_use = category_tibble$category_link) {
  
  links_to_use %>% 
    map_dfr(., function(.x) {
      # Navigate to subcategory
      remDr$navigate(.x)
      nytnyt(c(5, 10), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Grab store name
      store_title <- 
        get_html_element(remDr, css = "h1.store-name") %>% 
        str_trim(side = "both")
      
      # Scroll
      scroll_down_and_load(remDr)
      scroll_to_top()
      
      # Grab subcategory links /{all}
      num_of_categories <- 
        num_of_categories_tibble %>% 
        dplyr::filter(str_to_lower(store_name) == str_to_lower(store_title)) %>% 
        .[[2]]
      
      num_of_categories <- num_of_categories + 1
      
      subcategory_link_extensions <- 
        get_html_elements(remDr, 
                          css = "div.slider-item > a:nth-child(1)", 
                          type = "attribute", 
                          attribute_selector = "href") %>% 
        .[-c(1:num_of_categories)]
      
      subcategory_links <- paste0(url, subcategory_link_extensions)

      # Count subcategories
      store_subcategories <- 
        get_html_elements(remDr, css = ".text-primery-1") %>% 
        str_trim(side = "both")
      
      num_of_subcategories <- length(store_subcategories)
      
      # Verify that every subcategory's link was collected
      verify_length_match(sel = num_of_subcategories, 
                          rve = length(subcategory_links))
      
      # Sleep
      nytnyt(c(0, 1),
             crayon_col = crayon::magenta,
             "Got subcategories. Completed ",
             which(.x == links_to_use),
             " out of ",
             length(links_to_use),
             " categories \n")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == links_to_use), length(links_to_use))
      
      # Store data in a tibble
      tibble::tibble(subcategory = store_subcategories, 
                     subcategory_link = subcategory_links, 
                     category_link = rep(.x, num_of_subcategories))
    }
    )
}
subcategory_tibble <- collect_subcategories(category_tibble$category_link)

### (E) Collect item data -----
collect_items <- function(links_to_use = subcategory_tibble$subcategory_link) {
  
  links_to_use %>% 
    map_dfr(., function(.x) {
      # Navigate to subcategory page
      remDr$navigate(.x)
      nytnyt(c(5, 10), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Scroll
      scroll_down_and_load(remDr)
      scroll_to_top()
      
      # Grab title
      item_title <- get_html_elements(remDr, css = "h2.text-black")
      
      # Grab weight
      item_weight <- get_html_elements(remDr, css = "div.item-label")
      
      # Grab price
      item_price <- get_html_elements(remDr, css = "div.item-price")
      
      # Grab image link
      item_image_links <- get_html_elements(remDr, 
                                            css = "img.center", 
                                            type = "attribute", 
                                            attribute_selector = "src")
      
      # Sleep
      subcategory_title <- get_html_element(remDr, css = "h2.ng-star-inserted")
      nytnyt(c(0, 1),
             crayon_col = crayon::magenta,
             "Got items. Completed ",
             which(.x == links_to_use),
             " out of ",
             length(links_to_use),
             " sub-subcategories \n", 
             "Current subcategory:", subcategory_title, "\n")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == links_to_use), length(links_to_use))
      
      # Store data in a tibble
      tibble::tibble(subcategory_link = rep(.x, length(item_title)), 
                     item = item_title, 
                     weight = item_weight, 
                     price = item_price, 
                     item_image_link = item_image_links)
    }
    )
}
item_tibble <- collect_items(subcategory_tibble$subcategory_link)

# item_image <- magick::image_read(path = item_image_links %>% 
#                                    unlist())



##### 6: Close Selenium server -----
remDr$close()
remDr$closeWindow()
system("kill /im java.exe /f")
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
gc()



##### After collecting data: clean in new file & put in package with misc. funcs

# Save sample data tibbles to create clean up workflow
data_tibbles <- list(location = location_tibble, 
                     store = store_tibble, 
                     category = category_tibble, 
                     subcategory = subcategory_tibble, 
                     item = item_tibble)

data_tibbles %>% 
  map2(., names(data_tibbles), function(.x, .y) {
    readr::write_csv(.x, path = here::here(paste0("data/", .y, ".csv")))
  })
