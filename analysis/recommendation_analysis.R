##### 1: Load packages --------------------------------------------------------
# devtools::install_github("moamiristat/grocerycart")
library(grocerycart)
library(recommenderlab)
library(dplyr)
library(ggplot2)

col_palette <- c("#001219", "#005F73", "#0A9396", "#94D2BD",
                 "#E9D8A6", "#ECBA53", "#EE9B00", "#DC8101", 
                 "#CA6702", "#BB3E03", "#AE2012", "#9B2226")

##### 2: Load data ------------------------------------------------------------
# Availbale datasets in package
# data(package = "grocerycart")
# ?basket_db_funmart

# 144,159 line items
data("basket_db_funmart")

grocery <- 
  basket_db_funmart %>% 
  distinct(basket_id, product)

##### 3: Analysis -------------------------------------------------------------
# Set up Binary Rating Matrix
grocery_matrix <- 
  grocery %>% 
    mutate(value = 1) %>% 
    tidyr::pivot_wider(names_from = product, values_from = value, values_fill = 0) %>% 
    select(-basket_id) %>% 
    as.matrix() %>% 
    as("binaryRatingMatrix")

# Set up Evaluation Scheme
grocery_scheme <- 
  grocery_matrix %>% 
    evaluationScheme(method = "bootstrap", k = 10, train = .8, given = -1)

# Set up algorithms
recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")
recommenderRegistry$get_entry_names()

grocery_algorithms <- list(
  "Random" = list(name = "RANDOM", param = NULL), 
  "Popular" = list(name = "POPULAR", param = NULL), 
  "Association Rules" = list(name = "AR", param = list(support = .0005, conf = .5, maxlen = 4)), 
  "IBCF Jaccard" = list(name = "IBCF", param = list(method = "jaccard", k = 20)), 
  "IBCF Cosine" = list(name = "IBCF", param = list(method = "cosine", k = 20)), 
  "IBCF Pearson" = list(name = "IBCF", param = list(method = "pearson", k = 20)), 
  "UBCF Cosine" = list(name = "UBCF", param = list(method = "cosine", nn = 450)), 
  "UBCF Pearson" = list(name = "UBCF", param = list(method = "pearson", nn = 450)), 
  "UBCF Jaccard" = list(name = "UBCF", param = list(method = "jaccard", nn = 450)), 
  
  "HYBRID Cosine" = list(name = "HYBRID", param = list(recommenders = list(
    "IBCF Cosine" = list(name = "IBCF", param = list(method = "cosine", k = 20)), 
    "UBCF Cosine" = list(name = "UBCF", param = list(method = "cosine", nn = 450))))), 
  
  "HYBRID Pearson" = list(name = "HYBRID", param = list(recommenders = list(
    "IBCF Pearson" = list(name = "IBCF", param = list(method = "pearson", k = 20)), 
    "UBCF Pearson" = list(name = "UBCF", param = list(method = "pearson", nn = 450))))), 
  
  "HYBRID Jaccard" = list(name = "HYBRID", param = list(recommenders = list(
    "IBCF Pearson" = list(name = "IBCF", param = list(method = "jaccard", k = 20)), 
    "UBCF Pearson" = list(name = "UBCF", param = list(method = "jaccard", nn = 450)))))
)

grocery_results <- evaluate(grocery_scheme, 
                            grocery_algorithms, 
                            type = "topNList", 
                            n = c(1, 3, 5, 10, 15))

# saveRDS(grocery_results, here::here("analysis/recommender_models.RDS"))
# grocery_results <- readRDS(here::here("analysis/recommender_models.RDS"))

evaluate_grocery <- function(evaluation) {
  # Evaluation results of algorithms stored as lists
  grc <- 
    evaluation %>% 
      getConfusionMatrix() %>% 
      as.list()
  
  # Tibble of average evaluation results (for the k = 10 bootstrap samples)
  as_tibble(Reduce("+", grc) / length(grc)) %>% 
    mutate(F1_score = (2 * TP) / (2 * TP + FP + FN), 
           accuracy = (TP + TN) / (TP + TN + FP + FN)) %>% 
    select(n, TPR, FPR, precision, recall, F1_score, accuracy)
}

grocery_eval <- 
  grocery_results %>% 
    purrr::map(., evaluate_grocery) %>% 
    tibble::enframe() %>% 
    tidyr::unnest(cols = value)

roc_cruve <- 
  grocery_eval %>% 
    ggplot(aes(x = FPR, y = TPR, color = name %>% forcats::fct_reorder(-TPR))) + 
    geom_line() + 
    geom_label(aes(label = n)) + 
    labs(x = "False Positive Rate", y = "True Positive Rate", 
         title = "ROC Curve") + 
    scale_color_manual(values = col_palette, name = "Algorithm") + 
    hrbrthemes::theme_ipsum()

# Precision: % of correct +ve predictions
# Recall/Sensitivity: P(result + | truth +) = classifier correctly predicted +ve
pr_curve <- 
  grocery_eval %>% 
    ggplot(aes(x = recall, y = precision, color = name %>% forcats::fct_reorder(-precision))) + 
    geom_line() + 
    geom_label(aes(label = n)) + 
    labs(x = "Recall", y = "Precision", 
         title = "Precision Recall Curve") + 
    scale_color_manual(values = col_palette, name = "Algorithm") + 
    hrbrthemes::theme_ipsum()

ggpubr::ggarrange(roc_cruve, pr_curve, ncol = 2, nrow = 1, 
                  common.legend = TRUE, legend = "bottom")

# Winning Algorithm
grocery_results$Popular

win_model <- Recommender(getData(grocery_scheme, "train"), method = "Popular")

# Test Predictions on the unseen data
# Remeber mean=26 with sd=4 and minimum=10
pre <- predict(win_model, 
               getData(grocery_scheme, "unknown"), 
               type = "topNList", byUser = TRUE)

pre %>% as(., "list")

# ?`calcPredictionAccuracy,topNList,binaryRatingMatrix-method`


# Prediction
sample_order <- c("Panzani Pasta Assorted", 
                  "Glad Zipper Freezer Bag", 
                  "Fantastic Rice Crakers Pizza", 
                  "Black Honey", 
                  "Aldouri Molokhia", 
                  "Selecta Ice Cream Cookies & Cream", 
                  "Bayara Sesame Seeds", 
                  "Ama Cashew Salted", 
                  "Nikai Air Fryer 3.2L")

sample_order_mat <- 
  grocery %>% 
    distinct(product) %>% 
    mutate(ordered = as.numeric(product %in% sample_order)) %>% 
    tidyr::pivot_wider(names_from = product, values_from = ordered) %>% 
    as.matrix() %>% 
    as("binaryRatingMatrix")

pre <- predict(win_model, 
               newdata = sample_order_mat, 
               n = 10)

pre %>% as(., "list")

### Recommendation system not accurate since the data generated included 
### (1) a low number of baskets and (2) a handful of products were present 
### in many baskets ---> naturally, those common products will be recommended
### by the system since they exist in an overwhleming # of baskets.
