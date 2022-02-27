
# Collect, Generate, Analyze & Report on Online Grocery Data

## Roadmap

Each of the first 5 stages focuses on a broad task for the project,
while 7 & 8 include extra project-wide information:  
1. [Collect data from 2 online grocery services](#collect-online-data)  
2. [Clean collected data](#clean-collected-data)  
3. [Generate fake data](#generate-fake-data)  
4. [Analyze grocery data](#analyze-grocery-data)  
5. [Grocery Dashboard](#grocery-dashboard)  
6. [Build Automation Tools](#build-automation-tools)  
7. [Extra: R Packages used](#r-packages-used)  
8. [Extra: Notes & FAQs](#notes-and-faqs)

### Collect Online Data

------------------------------------------------------------------------

Our first task is to find grocery data. In our case, we collected the
data by scraping 2 online gorcery services:
[elGrocer](https://www.elgrocer.com) (based in UAE) and
[Ocado](https://www.ocado.com) (based in UK).

Once confirming that the robotxt files do not disallow us from scraping
the websites, we started building functions to organize the data
collection workflow for each online grocery delivery service.

Consistent features for all collector functions:  
- Functional Programming: via the map function in the purrr package  
- Output: returns a tibble/table of the data collected  
- Verbose: cat and crayon packages print to the console the progress
being made  
- Beep: beepr package sounds a ‘Work Complete’ audio once the required
data is collected

elGrocer data collected:  
- Location (UAE) of grocery stores that elGrocer delivers from  
- Details of each store (i.e., delivery times, minimum order amount)  
- Categories & subcategories of products available in each store  
- Product details (i.e., name, price, weight, image link)

Ocado data collected:  
- All categories available  
- Product details for 1,000 products (i.e., name, price, weight,
nutrition table, ingredients, country of origin, rating, text reviews)  
+ The 1,000 products were randomly selected from 3 (of the 13)
categories due to the large number of products available. All products
would have taken \> 11 hours to collect (regardless of hardware) because
the system/bot was instructured to sleep within each collector function
to prevent overloading the website. The time would be less with parallel
processing (i.e., opening multiple RSelenium servers at once and using
parallel functional programming vua future package in R).

Finally, we also collected country names and flags from
[worldometers](https://www.worldometers.info/geography/flags-of-the-world/).
The purpose of this was to make it possible to extract the country of
origin for the products on the Ocado website.

Code found in: WIP  
Collected data found in: WIP

### Clean Collected Data

------------------------------------------------------------------------

The data cleaning process led to 3 new data files that will be used to  
generate fake customer data and in analysis.

Code found in: WIP  
Collected data found in: WIP

### Generate Fake Data

------------------------------------------------------------------------

Fake orders were synthesized using the collected data:  
- customer_db: customer details (i.e., id, name, longitude, latitude).
Location of long and lat is constrained within UAE.  
- order_db: orders placed from 2020-01-01 to 2021-12-31 (i.e., id,
customer id, date, time, store)  
+ 97 available stores  
+ 40% of orders from 2020 and 60% from 2021 + The probability of
shopping at each store was calculated according to the # of products
(i.e., more products available in a store —> higher probability of
ordering from that store).  
+ 5% of orders from 00:00 to 8:00 am  
+ 20% of orders from 8:00 to 10:00 am  
+ 25% of orders from 10:00 to 12:00 pm  
+ 25% of orders from 12:00 to 6:00 pm  
+ 15% of orders from 6:00 to 10:00 pm  
+ 10% of orders from 10:00 to 12:00 am  
- basket_db: products bought in each order (i.e., id, order id, product,
price)  
+ 1,000 products to select from (see ‘Ocado data collected’ above)  
+ The probability of ordering a product was based on a ‘score’ metric =
nummber of reviews for that product + % of customers that recommend it
(i.e., higher score for a product —> higher probability of ordering that
product).  
+ The number of products in each basket is normally distributed with a
mean of 20 and standard deviation of 5 (minimum is 5 products/basket)

To generate a new grocery dataset, visit the [dashboard for this
project](#grocery-dashboard).

### Analyze Grocery Data

------------------------------------------------------------------------

-   [ ] Summary Data  
-   [ ] Market Basket Analysis  
-   [ ] Customer Cohort  
-   [ ] Reviews’ Text Analysis  
    Example of findings: 1-2 summary pics

### Grocery Dashboard

------------------------------------------------------------------------

-   [ ] R shiny
-   [ ] Grocery data generator
-   [ ] Automation tools

### Build Automation Tools

------------------------------------------------------------------------

-   [ ] Report powerpoint generation  
-   [ ] Invoice pdf

### R Packages Used

------------------------------------------------------------------------

| **Stage** | **Packages Loaded**                                           | **Packages used with Namespace(::)**          |
|-----------|---------------------------------------------------------------|-----------------------------------------------|
| 1         | robotstxt, RSelenium, rvest, purrr, stringr, readr            | pacman, netstat, crayon, tibble, dplyr, beepr |
| 2         | dplyr, readr, stringr, tidyr, purrr                           | pacman, fs, here                              |
| 3         | fabricatr, wakefield, randomNames, charlatan, magrittr, purrr | pacman, tibble, dplyr                         |
| 4         | WIP                                                           | WIP                                           |
| 5         | WIP                                                           | WIP                                           |

### Notes and FAQs

------------------------------------------------------------------------

-   [ ] Bibliography  
-   [ ] Session Info
