##### Load packages -----
# Main packages loaded: robotstxt, Selenium, rvest, purrr
# Packages used with namespace: netstat, crayon
pacman::p_load(robotstxt, RSelenium, rvest, purrr)

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



