# Grocery Business R Dashboard

An R Shiny App to visualize and analyze grocery data as well as to act as a hub for automated report and invoice generation. See below for a short description of the tabs included in this dashboard.

## Demo
A demo version of the app is available here: [R Shiny Dashboard Demo](http://moamiristat.shinyapps.io/grocery)

## Data Source

I collected product data by scraping 2 online gorcery services - [elGrocer](https://www.elgrocer.com) (based in UAE) and [Ocado](https://www.ocado.com) (based in UK). After cleaning the data, I made them available in the [`grocerycart`](https://github.com/moamiristat/grocerycart) package.  

For more information on how the data was collected and cleaned, view the [`grocerycart`](https://github.com/moamiristat/grocerycart) R package. The package includes 16 grocery related datasets and a tutorial on how you can collect and clean more data.  

Also, I simulated a fake grocery store's customers, orders and baskets dataset based on the scraped data. You too can generate and download such a dataset via the 'Generate Grocery Data' tab in the dashboard.

## Dashboard Tabs

Here is a brief description of the 6 tabs in the r Shiny App.

+ **Basket Analysis**: *Analyzes* the simulated grocery data.  
+ **Generate Grocery Data**: Automatically *generates a set of 3 downloadable, related random datasets* from a fictional grocery store, funmart.  
+ **Automated Reports**: Automatically *creates downloadable and editable powerpoint report and word invoice* based on the data uploaded and input paramters set by the user.  
+ **Recommendations**: *Recommends 10 products* based on 5 products already in the basket. The 'Popular items (POPULAR)' recommendation algorithm is used.  
+ **Summary Elgrocer**: *Descriptive Summary* of the data scraped from the elgrocer website (more analysis can be conducted).  
+ **Summary Ocado**: *Descriptive Summary* of the data scraped from the ocado website (more analysis can be conducted).

## Future Additional Features

Here are some features that can be included for a more complex version of the App: 

+ A live version of the App can be built by connecting the R scripts to a live database, causing the dashboard to automatically update periodically (i.e., hourly or daily), or even whenever a user clicks on a 'Refresh' button.  
+ Statistical models can be built to predict future orders (i.e., predict customer lifetime revenue).  
+ Include a filtering option (i.e., only view January data).  
+ Allow users to sign up to receive a daily summary of the dashboard's KPIs.  
+ Text analysis of reviews or customer chat support, if available (i.e., what are the common questions that customers are asking -> build an automated bot support that answers these FAQs).

## R Packages Used

[R App](https://github.com/moamiristat/grocery/tree/main/web-app): `shiny`, `shinyFeedback`, `grocerycart`, `bs4Dash`, `gt`, `plotly`, `visNetwork`, `reactable`, `reactablefmtr`, `ggplot2`, `dplyr`, `stringr`, `cohorts`, `arules`, `arulesViz`, `visNetwork`, `Matrix`, `recommenderlab`, `officer`, `lubridate`, `flextable`, `patchwork`, `fabricatr`, `wakefield`, `randomNames`, `charlatan`, `magrittr`, `purrr`.  
[basket_analysis.R](https://github.com/moamiristat/grocery/blob/main/analysis/basket_analysis.R): `grocerycart`, `dplyr`, `lubridate`, `ggplot2`, `ggforce`, `gt`, `reactable`, `reactablefmtr`, `purrr`, `stringr`, `hrbrthemes`, `lubridate`, `scales`.  
[oc_analysis.R](https://github.com/moamiristat/grocery/blob/main/analysis/oc_analysis.R): `grocerycart`, `tidyverse`, `stringr`, `reactable`, `gganimate`, `ggflags`, `ggimage`, `ggrepel`, `htmltools`, `hrbrthemes`, `scales`, `glue`.  
