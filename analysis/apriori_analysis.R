##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(arules)
library(dplyr)

##### 2: Load data ------------------------------------------------------------
# Availbale datasets in package
# data(package = "grocerycart")
# ?basket_db_funmart

# 144,159 line items
data("basket_db_funmart")

apriori_items <- 
  basket_db_funmart %>% 
    mutate(product = stringr::str_replace_all(product, ",", " & ")) %>% 
    mutate(product = stringr::str_replace_all(product, "  ", " ")) %>% 
    plyr::ddply(.variables = c("basket_id", "order_id"), 
                .fun = function(x)paste(x$product, collapse = ",")) %>% 
    select(V1) %>% 
    rename("items" = V1)

write.csv(apriori_items, here::here("data/apriori_items.csv"), quote = FALSE, row.names = FALSE)

txn <-  read.transactions(here::here("data/apriori_items.csv"), 
                          format = "basket", 
                          sep= ",", 
                          rm.duplicates = FALSE, 
                          header = TRUE)

summary(txn)

##### 3: Apriori --------------------------------------------------------------
# Misc funcs
unique_rules <- function(rules) {
  rules_subset <- which(colSums(is.subset(rules, rules)) > 1)
  length(rules_subset)
  rules[-rules_subset]
}
inspect_rules <- function(rules, measure = "support", top = 10) {
  rules %>% 
    sort(by = measure, decreasing = TRUE) %>% 
    head(top) %>% 
    arules::inspect()
}

# Generate rules
apriori_rules <- 
  txn %>% 
    apriori(parameter = list(supp = 0.0005, conf = 0.7, minlen = 2, maxlen = 10)) %>% 
    unique_rules()

summary(apriori_rules)

inspect_rules(apriori_rules, measure = "lift", top = 5)


# Specific products (What is likely to be bought BEFORE 'Earl Grey')
custom_rules1 <- apriori(txn, 
                        parameter = list(support = 0.0005, conf = .25, minlen = 2, maxlen = 3), 
                        appearance = list(default = "lhs", rhs = "Earl Grey"), 
                        control = list(verbose = FALSE))

inspect_rules(custom_rules1, measure = "confidence", top = 5)

# Specific products (Customer who bought 'Nikai Air Fryer 3.2L', also bought...)
custom_rules2 <- apriori(txn, 
                        parameter = list(support = 0.01, conf = .05, minlen = 2, maxlen = 3), 
                        appearance = list(default = "rhs", lhs = "Nikai Air Fryer 3.2L"), 
                        control = list(verbose = FALSE))

inspect_rules(custom_rules2, measure = "support", top = 5)

# Visualize

