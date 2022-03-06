##### 1: Load packages -----
# Main packages loaded: robotstxt, RSelenium, rvest, purrr, stringr, readr
# Packages used with namespace: pacman, here, netstat, crayon, tibble, dplyr, beepr
# install.packages("pacman")
# pacman::p_install("robotstxt", "RSelenium", "rvest", "purrr", "stringr", "readr", 
#                   "here", "netstat", "crayon", "tibble", "dplyr", "beepr")
pacman::p_load(robotstxt, RSelenium, rvest, purrr, stringr, readr)



##### 2: Misc. functions -----
# Sleep & print time
get_page_title <- function(remDr) {
  cat(crayon::blue(remDr$getTitle(), "\n"))
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
  } else if(type == "table") {
    page %>% 
      html_table()
  } else {
    cat(crayon::red("Type must be: text, attribute, attributes or table"))
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
  } else if(type == "table") {
    page %>% 
      html_table()
  } else {
    cat(crayon::red("Type must be: text, attribute, attributes or table"))
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
    
    nytnyt(c(2, 3), 
           crayon_col = crayon::green, 
           "Scrolling to the bottom of the page\n")
    
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

# Scroll from top to bottom to ensure all products load
scroll_down_page_perc <- function(remDr, perc = seq(.01, 1, .005)) {
  
  scroll_to_top()
  
  last_height <- 
    remDr$executeScript("return document.body.scrollHeight") %>% 
    unlist()
  
  map(perc, function(.x) {
    last_height <- last_height * .x
    remDr$executeScript(
      script = str_glue("window.scrollTo({{
                        left: 0, 
                        top: {last_height}, 
                        behavior: 'smooth'
                        }});"))
    
    nytnyt(c(4, 6), crayon_col = crayon::yellow, "Scrolled down ", 
           .x * 100, "%\n")
  }
  )
  cat(crayon::green("Reached bottom of page \n"))
}

# Scroll from bottom to top to double ensure all products load
scroll_up_page_perc <- function(remDr, perc = seq(.01, 1, .0025)) {
  
  scroll_down_and_load(remDr)
  
  last_height <- 
    remDr$executeScript("return document.body.scrollHeight") %>% 
    unlist()
  
  map(rev(perc), function(.x) {
    last_height <- last_height * .x
    remDr$executeScript(
      script = str_glue("window.scrollTo({{
                        left: 0, 
                        top: {last_height}, 
                        behavior: 'smooth'
                        }});"))
    
    nytnyt(c(2, 3), crayon_col = crayon::yellow, "Scrolled up ", 
           100 - (.x * 100), "%\n")
  }
  )
  cat(crayon::green("Reached top of page \n"))
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

# Verify that output length from selenium & rvest match - 2 objects
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

# Verify that output length - 5 objects
verify_ocado_length_match <- function(obj1 = length(product_title), 
                                      obj2 = length(product_weight), 
                                      obj3 = length(product_price), 
                                      obj4 = length(product_images), 
                                      obj5 = length(product_links)) {
  if(obj1 == obj2 && obj2 == obj3 && obj3 == obj4 && obj4 == obj5) {
    cat(crayon::green("Success! All 5 lengths match: ",  
                      obj1, 
                      "items found", "\n"))
  } else {
    stop(crayon::red("Go Back! Lengths match DO NOT match:\n",
                     "Title:", obj1, "\n", 
                     "Weight:", obj2, "\n", 
                     "Price:", obj3, "\n", 
                     "Images:", obj4, "\n", 
                     "Links:", obj5, "\n"))
  }
}

# Sound when work complete in map
sound_work_complete <- function(expr1, expr2) {
  
  if(expr1 == expr2) {
    beepr::beep(sound = "complete", 
                expr = cat(crayon::bgWhite$green$bold("Work Complete!\n")))
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

# Click on the 'show more' button on ocado.com
click_show_more <- function() {
  i <- 0
  repeat({
    output_click <- tryCatch(
      expr = {
        remDr$findElement(using = "css", 
                          value = "button.show-more")$clickElement()
        i <- i + 1
        cat(crayon::bgCyan("Clicked ", i, " times\n"))
        nytnyt(c(2, 5))
        paste("clicked")
      }, 
      error = function(e) {
        cat(crayon::bgYellow("No show more button. Time to scroll down and up the page to load all products.\n"))
      }
    )
    if(is_null(output_click)) {break}
  })
}

# If collection object is empty ---> NA ---> else its value
empty_to_na <- function(x) {
  if(x == "" || is.na(x) || is.null(x) || is_empty(x)) {
    print(NA)
  } else {
    x
  }
}

# Country names & flags
country_names <- 
  read_html("https://www.worldometers.info/geography/flags-of-the-world/") %>% 
  html_elements(".col-md-4 > div[align='center'] > div") %>% 
  html_text() %>% 
  str_replace_all(c("U.S." = "USA", "U.K." = "UK", "U.A.E." = "UAE")) %>% 
  c(., "United Kingdom", "United States", "United Arab Emirates", "England", "EU")
country_flags <- 
  read_html("https://www.worldometers.info/geography/flags-of-the-world/") %>% 
  html_elements(".col-md-4 > div[align='center'] > a") %>% 
  html_attr("href") %>% 
  paste0("https://www.worldometers.info", .)

# Sample links
# sample_links <- tibble::tibble(links = c(
#   "https://www.ocado.com/products/sanex-biome-protect-kids-head-to-toe-wash-55476011", 
#   "https://www.ocado.com/products/brabantia-12-litre-white-pedal-bin-362751011", 
#   "https://www.ocado.com/products/nurofen-day-night-cold-flu-relief-200mg-5mg-tablets-ibuprofen-203773011", 
#   "https://www.ocado.com/products/nestle-shreddies-the-original-cereal-10054011"))





##### 3: Initiate Selenium server -----
initiate_server <- rsDriver(port = netstat::free_port(), 
                            browser = "firefox", 
                            verbose = FALSE)

# Set up remote client driver & open a session
remDr <- initiate_server$client
remDr$open()


##### 4: Selenium to collect data from elgrocer.com -----
# Check which webpages are not bot friendly
url <- "https://www.elgrocer.com"

rtxt <- robotstxt(domain = url)
rtxt$comments
rtxt$crawl_delay
rtxt$permissions

# We can collect data from the webpages we are interested in
paths_allowed(domain = url, paths = c("/store", "/stores"))

# Browse shop
remDr$navigate(url)

### (A) Collect the locations and their links: ALL -----
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
grocer_location <- collect_location_links()
# write_csv(grocer_location, here::here("data/grocer_location.csv"))

### (B) Collect the store 'i' details, and links: ALL -----
collect_stores_details <- function(links_to_use = grocer_location$location_link, 
                                   sleep_min = 0, sleep_max = 1) {
  
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
      
      nytnyt(c(sleep_min, sleep_max), 
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
                     detail = store_details, 
                     store_link = paste0(url, store_links), 
                     location_link = rep(.x, num_of_stores_rvest))
    }
    )
}
grocer_store <- collect_stores_details(grocer_location$location_link)
# write_csv(grocer_store, here::here("data/grocer_store.csv"))

### (C) Collect categories data: ALL ----- 
collect_categories <- function(links_to_use = grocer_store$store_link, 
                               sleep_min = 0, sleep_max = 1) {
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
                          rve = if(category_links == url) {
                            0
                          } else {
                            length(category_links)
                          })
      
      # Sleep
      nytnyt(c(sleep_min, sleep_max),
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
                     store_link = rep(.x, num_of_categories))
    }
    )
}
grocer_category <- collect_categories(grocer_store$store_link)
# write_csv(grocer_category, here::here("data/grocer_category.csv"))

### (D) Collect subcategories data from 100 links: 300 CATEGORIES -----
collect_subcategories <- function(links_to_use = grocer_category$category_link, 
                                  sleep_min = 0, sleep_max = 1) {
  
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
      
      # Grab subcategory links /{all}
      num_of_categories_tibble <- 
        grocer_category %>% 
        dplyr::count(store_name)
      
      
      tryCatch(
        expr = {
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
                              rve = if(subcategory_links == url) {
                                0
                              } else {
                                length(subcategory_links)
                              })
          
          # Sleep
          nytnyt(c(sleep_min, sleep_max),
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
        }, 
        error = function(e) {}
      )
    }
    )
}
random_category_links <- sample(1:length(grocer_category$category_link),
                                300, replace = FALSE)

grocer_subcategory <- 
  collect_subcategories(grocer_category$category_link[random_category_links])
# write_csv(grocer_subcategory, here::here("data/grocer_subcategory.csv"))

### (E) Collect item data: 1000 SUBCATEGORIES -----
collect_items <- function(links_to_use = grocer_subcategory$subcategory_link, 
                          sleep_min = 0, sleep_max = 1) {
  
  links_to_use %>% 
    map_dfr(., function(.x) {
      # Navigate to subcategory page
      remDr$navigate(.x)
      nytnyt(c(5, 10), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Scroll
      scroll_down_and_load(remDr)
      scroll_to_top()
      
      tryCatch(
        expr = {
          
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
          nytnyt(c(sleep_min, sleep_max),
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
          
        }, 
        error = function(e) {}
      )
    }
    )
}
random_subcategory_links <- sample(1:length(grocer_subcategory$subcategory_link), 
                                   1000, replace = FALSE)

grocer_item <- 
  collect_items(grocer_subcategory$subcategory_link[random_subcategory_links])

# write_csv(grocer_item, here::here("data/grocer_item.csv"))


##### 5: Selenium to collect data from Ocado.com -----
# Check which webpages are not bot friendly
url2 <- "https://www.ocado.com"

rtxt <- robotstxt(domain = url2)
rtxt$comments
rtxt$crawl_delay
rtxt$permissions

# We can collect data from the webpages we are interested in
paths_allowed(domain = url2, paths = c("/browse"))

# Browse shop
remDr$navigate(url2)

# Accept all cookies
nytnyt(c(5, 10))
remDr$findElement(using = "xpath", 
                  value = "//*[@id='onetrust-accept-btn-handler']")$clickElement()

# Click on 'Browse Shop' menu
nytnyt(c(5,10))
remDr$findElement(using = "link text", 
                  value = "Browse Shop")$clickElement()

# Get categories info
nytnyt(c(5,10))
ocado_category_name <- get_html_elements(remDr, 
                                         css = ".level-item-link")
nytnyt(c(5,10))
ocado_category_ext <- get_html_elements(remDr, 
                                        css = ".level-item-link", 
                                        type = "attribute", 
                                        attribute_selector = "href")

ocado_category_link <- paste0(url2, ocado_category_ext)

ocado_category <- tibble::tibble(category = ocado_category_name, 
                                 link = ocado_category_link)

# write_csv(ocado_category, here::here("data/ocado_category.csv"))

##### (A) Grab general product data: FROM 3 CATEGORIES (~10000 products) -----
collect_category_data <- function(links_to_use = ocado_category_link, 
                                  sleep_min = 0, sleep_max = 1) {
  
  links_to_use %>% 
    map_dfr(., function(.x) {
      
      # Navigate to page
      remDr$navigate(.x)
      nytnyt(c(3, 5), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Count number of items written on top of page
      ocado_category_count <- 
        get_html_element(remDr, 
                         css = ".total-product-number") %>% 
        parse_number()
      
      # Page title
      get_page_title(remDr)
      cat(crayon::blue("Collecting data from:", 
                       ocado_category_name[which(.x == links_to_use)], 
                       "\n", 
                       "Total products:", ocado_category_count, "\n"))
      
      # Click on 'show more' until there's no more 'show more'
      click_show_more()
      
      # Slowly scroll down then up to load all products
      scroll_down_page_perc(remDr)
      scroll_up_page_perc(remDr)
      
      # Grab title
      product_title <- 
        get_html_elements(remDr, 
                          css = "h4.fop-title", 
                          type = "attribute", 
                          attribute_selector = "title") %>% 
        .[-c(1:3)]
      
      nytnyt(c(6, 11), crayon_col = crayon::yellow, "Got titles\n")
      
      # Grab weight
      product_weight <- 
        get_html_elements(remDr, 
                          css = ".fop-catch-weight") %>% 
        .[-c(1:3)]
      
      nytnyt(c(6, 11), crayon_col = crayon::cyan, "Got weights\n")
      
      # Grab price
      product_price <- 
        get_html_elements(remDr, 
                          css = ".fop-price") %>% 
        .[-c(1:3)]
      
      nytnyt(c(6, 11), crayon_col = crayon::silver, "Got prices\n")
      
      # Grab shelf life
      product_shelf_life <- 
        get_html_elements(remDr, 
                          css = ".fop-pack-info") %>% 
        dplyr::na_if("") %>% 
        .[-c(1:3)]
      
      # Grab images
      product_images <- 
        get_html_elements(remDr, 
                          css = "img.fop-img", 
                          type = "attribute", 
                          attribute_selector = "src") %>% 
        paste0(url2, .) %>% 
        .[-c(1:3)]
      
      nytnyt(c(6, 11), crayon_col = crayon::blue, "Got images\n")
      
      # Grab product links
      product_links <- 
        get_html_elements(remDr, 
                          css = "li.fops-item > div:nth-child(2) > div:nth-child(1) > a:nth-child(1)", 
                          type = "attribute", 
                          attribute_selector = "href") %>% 
        paste0(url2, .)
      
      nytnyt(c(6, 11), crayon_col = crayon::green, "Got product links\n")
      
      # Verify lenghts match
      verify_ocado_length_match(
        length(product_title), 
        length(product_weight), 
        length(product_price), 
        length(product_images), 
        length(product_links)
      )
      
      # Sleep
      nytnyt(c(sleep_min, sleep_max),
             crayon_col = crayon::magenta,
             "Got items from ", 
             ocado_category_name[which(.x == links_to_use)], 
             "\n", 
             "Completed ",
             which(.x == links_to_use),
             " out of ",
             length(links_to_use),
             "\n")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == links_to_use), length(links_to_use))
      
      # Store data in a tibble
      tibble::tibble(title = product_title, 
                     weight = product_weight, 
                     price = product_price, 
                     shelf_life = product_shelf_life, 
                     images = product_images, 
                     product_link = product_links, 
                     category_link = rep(.x, length(product_title)))
    }
    )
}

# Select 3 categories to collect data from (total ~ 9000-10000 products)
chosen_category_links <- c(1, 3, 6)
ocado_product_general <- 
  collect_category_data(ocado_category_link[chosen_category_links])
# write_csv(ocado_product_general, here::here("data/ocado_product_general.csv"))

##### (B) Grab extra product details: 1000 products -----
collect_product_data <- function(links_to_use = ocado_product_general$product_link, 
                                   sleep_min = 0, sleep_max = 1) {
  
  links_to_use %>% 
    map_dfr(., function(.x) {
      # Navigate to page
      remDr$navigate(.x)
      nytnyt(c(3, 5), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)

      # Page title
      get_page_title(remDr)
      
      # Grab bop badges: vegetarian, etc...
      product_badges <- 
        get_html_elements(remDr, 
                          css = ".bop-badges > img", 
                          type = "attribute", 
                          attribute_selector = "title") %>% 
        paste(., sep = ", ", collapse = ", ") %>% 
        empty_to_na()
      cat(crayon::yellow("Got badges\n"))
      
      # Grab ingredients
      product_ingredients <- 
        get_html_elements(remDr, 
                          css = "#productInformation") %>% 
        str_extract("(?<=IngredientsIngredients).*$") %>% 
        empty_to_na()
      cat(crayon::yellow("Got ingredients\n"))
      
      
      # Grab brand
      product_brand <- 
        get_html_elements(remDr, 
                          css = ".bop-tags") %>% 
        str_subset("(?<=Brands).*$") %>% 
        str_extract("(?<=Brands).*$") %>% 
        empty_to_na()
      cat(crayon::yellow("Got brand\n"))
      
      # Grab country of origin
      product_country <- 
        get_html_elements(remDr, 
                          css = "#productInformation") %>% 
        str_extract("(?<=Country of Origin).{100}") %>% 
        str_extract(country_names) %>% 
        keep(.p = ~ !is.na(.)) %>% 
        paste(., sep = ", ", collapse = ", ") %>% 
        empty_to_na()
      cat(crayon::yellow("Got country of origin\n"))
      
      # Grab rating
      prodcut_rating <- 
        get_html_elements(remDr, 
                          css = ".bop-reviewSummary__kpiBall[itemprop='ratingValue']") %>% 
        empty_to_na()
      cat(crayon::yellow("Got rating\n"))
      
      # Grab count
      product_count <- 
        get_html_elements(remDr, 
                          css = ".bop-reviewSummary__kpiBall[itemprop='ratingCount']") %>% 
        empty_to_na()
      cat(crayon::yellow("Got count\n"))
      
      # Grab recommend %
      product_recommend <- 
        get_html_elements(remDr, 
                          css = ".bop-reviewSummary__recommendationsNumber") %>% 
        parse_number() %>% 
        empty_to_na()
      cat(crayon::yellow("Got recommend %\n"))
      
      # Sleep
      
      nytnyt(c(sleep_min, sleep_max),
             crayon_col = crayon::magenta,
             "Adding new data to tibble\n", 
             "Completed ", 
             tryCatch(
               expr = {
                 which(.x == links_to_use)
               }, 
               warning = function(w) {
                 cat(crayon::blue("Duplicate of product found\n"))
               }), 
             " out of ",
             length(links_to_use),
             "\n")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == links_to_use), length(links_to_use))
      
      # Store data in a list
      tibble::tibble(
        product_link = .x, 
        badge = product_badges, 
        ingredient = product_ingredients, 
        brand = product_brand, 
        country = product_country, 
        rating = prodcut_rating, 
        count = product_count, 
        recommend = product_recommend)
    })
}

# Randomly select 1000 products
set.seed(511)
random_product_links <- sample(1:length(ocado_product_general$product_link), 
                                   1000, replace = FALSE)

# Collect the details in 4 stages in case if error occurs at any point 
# (e.g., server disconnects) ---> do not need to start all over again
ocado_product_extra1 <- collect_product_data(ocado_product_general$product_link[random_product_links[1:250]])
# write_csv(ocado_product_extra1, here::here("data/ocado_product_extra1.csv"))
ocado_product_extra2 <- collect_product_data(ocado_product_general$product_link[random_product_links[251:500]])
# write_csv(ocado_product_extra2, here::here("data/ocado_product_extra2.csv"))
ocado_product_extra3 <- collect_product_data(ocado_product_general$product_link[random_product_links[501:750]])
# write_csv(ocado_product_extra3, here::here("data/ocado_product_extra3.csv"))
ocado_product_extra4 <- collect_product_data(ocado_product_general$product_link[random_product_links[751:1000]])
# write_csv(ocado_product_extra4, here::here("data/ocado_product_extra4.csv"))

##### (C) Grab product reviews: same 1000 products as above in (B) -----
collect_product_reviews <- function(links_to_use = ocado_product_general$product_link, 
                                    sleep_min = 0, sleep_max = 1) {
  
  links_to_use %>% 
    map_dfr(., function(.x) {
      
      # Navigate to page
      remDr$navigate(.x)
      nytnyt(c(3, 5), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Grab reviews based on: (A) any reviews?, (B) # of review pages
      product_review_count <- 
        get_html_elements(remDr, css = ".bop-reviewSummary__kpiBall[itemprop='ratingCount']")
      
      if(is_empty(product_review_count)) {
        cat(crayon::silver("No reviews for this product\n"))
        product_reviews <- NA
      } else {
        if(is_empty(get_html_elements(remDr, css = ".bop-reviews__paginationText"))) {
          cat(crayon::silver("No click next for this product \n"))
          
          product_reviews <- 
            get_html_elements(remDr, 
                              css = ".bop-reviews__review > p") %>% 
            tibble::tibble(reviews = .)
        } else {
          num_page_reviews <- 
            get_html_elements(remDr, css = ".bop-reviews__paginationText") %>% 
            .[[1]] %>% 
            parse_number()
          
          cat(crayon::silver(num_page_reviews, " pages of reviews for this product \n"))
          
          product_review_p1 <- 
            get_html_elements(remDr, 
                              css = ".bop-reviews__review > p") %>% 
            tibble::tibble(reviews = .)
          
          if(num_page_reviews > 1) {
            first_right_button <- remDr$findElement(using = "css", 
                                                    value = ".bop-reviews__paginationButton")
            
            nytnyt(c(1, 2))
            
            first_right_button$clickElement()
            
            product_review_p2 <- 
              get_html_elements(remDr, 
                                css = ".bop-reviews__review > p") %>% 
              tibble::tibble(reviews = .)
            
            if(num_page_reviews > 2) {
              product_reviews <- 
                map_dfr(1:(num_page_reviews - 2), 
                        function(.y) {

                          right_button <- 
                            remDr$findElement(using = "css", 
                                              value = "div.bop-reviews__paginationWrapper:nth-child(2) > button:nth-child(4)")
                          
                          nytnyt(c(1, 2))
                          
                          right_button$clickElement()
                          
                          product_review_ps <- 
                            get_html_elements(remDr, 
                                              css = ".bop-reviews__review > p")
                          
                          cat(crayon::blue("Collected review page ", .y + 2, 
                                           " out of ", num_page_reviews, "\n"))
                          
                          tibble::tibble(reviews = product_review_ps)
                        }) %>% 
                dplyr::bind_rows(product_review_p1, product_review_p2, .)
            } else {
              product_reviews <- dplyr::bind_rows(product_review_p1, product_review_p2)
            }
          }
        }
      }
      
      # Sleep
      nytnyt(c(sleep_min, sleep_max),
             crayon_col = crayon::magenta,
             "Adding new data to tibble\n", 
             "Completed ", 
             which(.x == links_to_use),
             " out of ",
             length(links_to_use),
             "\n")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == links_to_use), length(links_to_use))

      # Store data in a tibble
      if(is_empty(product_review_count)) {
        dplyr::bind_cols(product = .x, 
                         reviews = NA) 
      } else {
        dplyr::bind_cols(product_link = rep(.x, length(product_reviews$reviews)), 
                         reviews = product_reviews) 
      }
    })
}

ocado_review1 <- collect_product_reviews(ocado_product_general$product_link[random_product_links[1:250]])
# write_csv(ocado_review1, here::here("data/ocado_review1.csv"))
ocado_review2 <- collect_product_reviews(ocado_product_general$product_link[random_product_links[251:500]])
# write_csv(ocado_review2, here::here("data/ocado_review2.csv"))
ocado_review3 <- collect_product_reviews(ocado_product_general$product_link[random_product_links[501:750]])
# write_csv(ocado_review3, here::here("data/ocado_review3.csv"))
ocado_review4 <- collect_product_reviews(ocado_product_general$product_link[random_product_links[751:1000]])
# write_csv(ocado_review4, here::here("data/ocado_review4.csv"))

##### (D) Grab nutrition table: same 1000 products as above in (B) -----
collect_nutrition_table <- function(links_to_use = ocado_product_general$product_link, 
                                    sleep_min = 0, sleep_max = 1) {
  
  links_to_use %>% 
    map(., function(.x) {
      # Navigate to page
      remDr$navigate(.x)
      nytnyt(c(3, 5), crayon_col = crayon::blue, "Make sure page loads \n")
      current_url(remDr)
      
      # Page title
      get_page_title(remDr)
      
      # Grab nutrition table
      nutrition_table <-
        get_html_elements(remDr,
                          css = ".bop-nutritionData__origin",
                          type = "table") %>%
        empty_to_na() %>% 
        pluck(1)
      cat(crayon::yellow("Got nutrition table\n"))
      
      # Sleep
      nytnyt(c(sleep_min, sleep_max),
             crayon_col = crayon::magenta,
             "Completed ", 
             which(.x == links_to_use),
             " out of ",
             length(links_to_use),
             "\n")
      
      # Play sound only at end - when work complete
      sound_work_complete(which(.x == links_to_use), length(links_to_use))
      
      tibble::tibble(nutrition_table)
      }) %>% 
    set_names(links_to_use)
}

ocado_nutrition_table <- collect_nutrition_table(ocado_product_general$product_link[random_product_links])
# write_rds(ocado_nutrition_table, here::here("data/ocado_nutrition.rds"))


##### 6: Close Selenium server -----
remDr$close()
remDr$closeWindow()
system("kill /im java.exe /f")
gc()
