# Grocery Business R Dashboard

An R Shiny App to visualize and analyze grocery data as well as to act as a hub for automated report and invoice generation. See below for a short description of the tabs included in this dashboard.

## Demo
A demo version of the app is available here: [R Shiny Dashboard Demo](http://moamiristat.shinyapps.io/grocery). It takes about 30-45 seconds for the dashboard to load initially, due to known hosting limitations. View the code at the end of this page on how to download and run the app on your local machine.

## Data Source

I collected product data by scraping 2 online gorcery services - [elGrocer](https://www.elgrocer.com) (based in UAE) and [Ocado](https://www.ocado.com) (based in UK). After cleaning the data, I made them available in the [`grocerycart`](https://github.com/moamiristat/grocerycart) package.  

For more information on how the data was collected, cleaned and analyzed, view the [grocerycart R package](https://github.com/moamiristat/grocerycart). The package includes 16 grocery related datasets and a tutorial on how you can collect and clean more data.  

Also, I simulated a fake grocery store's customers, orders and baskets dataset based on the scraped data. You too can generate and download such a dataset via the 'Generate Grocery Data' tab in the dashboard.

## Dashboard Walkthrough

Here are 4 quick videos to give a preview of the app:

https://user-images.githubusercontent.com/98935576/172756076-944c53a6-c56e-4493-9f30-37ef33e0c7c4.mov

https://user-images.githubusercontent.com/98935576/172756099-9cd0ed68-5d4b-42f0-84bd-94c5dba25111.mov

https://user-images.githubusercontent.com/98935576/172756113-c97e7604-1757-4d05-bd46-e28a835ead0b.mov

https://user-images.githubusercontent.com/98935576/172756122-8909f8dd-f3ac-4043-8904-9ac0415dc924.mov

## Dashboard Tabs

Here is a brief description of the 6 tabs in the r Shiny App:

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
+ Deploy App on Amazon Web Services, Digital Ocean or Heroku for better performance and security.

## R Packages Used

*ui & server*: `shiny`, `shinyFeedback`, `grocerycart`, `bs4Dash`, `gt`, `plotly`, `visNetwork`, `reactable`, `reactablefmtr`, `ggplot2`, `dplyr`, `stringr`, `cohorts`, `arules`, `arulesViz`, `Matrix`, `recommenderlab`, `officer`, `lubridate`, `flextable`, `patchwork`, `fabricatr`, `wakefield`, `randomNames`, `charlatan`, `magrittr`, `purrr`.  
[*basket_analysis.R*](https://github.com/moamiristat/grocery/blob/main/analysis/basket_analysis.R): `grocerycart`, `dplyr`, `lubridate`, `ggplot2`, `ggforce`, `gt`, `reactable`, `reactablefmtr`, `purrr`, `stringr`, `hrbrthemes`, `scales`.  
[*eg_analysis.R*](https://github.com/moamiristat/grocery/blob/main/analysis/eg_analysis.R): `grocerycart`, `tidyverse`, `stringr`, `forcats`, `ggrepel`, `ggimage`, `ggbeeswarm`.  
[*oc_analysis.R*](https://github.com/moamiristat/grocery/blob/main/analysis/oc_analysis.R): `grocerycart`, `tidyverse`, `stringr`, `reactable`, `gganimate`, `ggflags`, `ggimage`, `ggrepel`, `htmltools`, `hrbrthemes`, `scales`, `glue`.

## Run App Locally

To download and run the app directly on your machine (i.e.,local R session), run the following code:
```
# install the packages used in this project
cran_packages <- c("shiny", "shinyFeedback", "bs4Dash", "gt", "plotly", "visNetwork", 
                   "reactable", "reactablefmtr", "ggplot2", "dplyr", "stringr", "cohorts", 
                   "arules", "arulesViz", "Matrix", "recommenderlab", "officer", "lubridate", 
                   "flextable", "patchwork", "randomNames", "charlatan", "magrittr", "purrr", 
                   "ggforce", "hrbrthemes", "scales", "tidyverse", "forcats", "ggrepel", 
                   "ggimage", "ggbeeswarm", "gganimate", "htmltools", "glue")

cran_to_install <- cran_packages[!cran_packages %in% installed.packages()[, "Package"]]
install.packages(to_install, repos = "https://cran.rstudio.com/")

devtools::install_github("https://github.com/moamiristat/grocery.git")
devtools::install_github("https://github.com/trinker/wakefield.git")
devtools::install_github("https://github.com/jimjam-slam/ggflags.git")
install.packages("fabricatr", dependencies = TRUE, repos = c("http://r.declaredesign.org", 
                 "https://cloud.r-project.org"))


# download and run the application - the app will open automatically
library("shiny")
shiny::runGitHub('grocery', 'moamiristat')
```

