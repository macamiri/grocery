### Load packages -----
pacman::p_load(robotstxt, RSelenium, rvest)

### Confirm which webpages are not bot friendly -----
url <- "https://www.elgrocer.com"

rtxt <- robotstxt(domain = url)
rtxt$comments
rtxt$crawl_delay
rtxt$permissions

paths_allowed(domain = url, paths = c("/store", "/stores"))

# We can collect data from the webpages we are interested in

