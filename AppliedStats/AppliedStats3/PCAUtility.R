#
# Principle Component Analysis Utility functions
#
# These are a few helpful functions that can do PCA and output some formatting function

library(broom)
library(tidyverse)
library(kableExtra)
library(knitr)

# pca_analysis
# Parameters: 
# dataset - data.frame containing all items that will be run against pca. Note: filter factors beforehand!
# type - enumeration for "original" or "standard". orginal applies pca to the orginal values while standard does # standardized values
# returns a tibble containing the original data, a PCA object, and an augmented set of the data with fitted values from the PCA object
pca_analysis <- function(dataset, type) {
  if(type == "original") {
    dataset %>% 
      # calculate mean eigenvalue from covariance matrix to determine floor for principle component selection
      mutate(avg_eigen = eigen(var(.))$values %>% mean) %>%
      nest() %>%
      mutate(
        pca = map(data, ~ prcomp(.x %>% select(-avg_eigen))),
        # add values from pca output onto original data
        pca_aug = map2(pca, data, ~augment(.x, data = .y))
      )
  } else if(type == "standard") {
    dataset %>% 
      # calculate mean eigenvalue from correlation matrix to determine floor for principle component selection
      mutate(avg_eigen = eigen(cor(.))$values %>% mean) %>%
      nest() %>%
      mutate(
        pca = map(data, ~ prcomp(.x %>% select(-avg_eigen), scale. = TRUE, center = TRUE)),
        # add values from pca output onto original data
        pca_aug = map2(pca, data, ~augment(.x, data = .y))
      )
  }
}

# pca_tidy
# Parameters:
# pca_analysis - a tibble returned from the pca_analysis function
# Returns: a tibble containing summarized pca information
pca_summary <- function(pca_analysis) {
  pca_analysis %>%
    unnest(pca_aug) %>% 
    # calculate variance for all PC variables
    summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>% 
    # pivot columns to rows
    gather(
      key = pc, 
      value = variance
    ) %>% 
    mutate(
      # variance explained
      var_exp = variance / sum(variance),
      # cumulative sum of variance explained so far
      cum_var_exp = cumsum(var_exp),
      # name of principle component
      pc = str_replace(pc, ".fitted", "")
    )
}

# pca_kable
# Taking all the above parameters, produce a kable which highlights Principle Components where
# the variance exceeds the average eigenvalue
pca_kable <- function(pca_analysis, pca_summary, type) {
  caption_txt <- ifelse(type == "standard", "Standardized", "Original")
  pca_summary %>%
    kable(
      digits = 4,
      col.names = c("PC", "Variance", "Var. Explained", "Cumulative  Var. Explained"),
      caption = paste(caption_txt, "Principle Components. Bolded rows indicate where variance is greater than the average eigenvalue. These are the chosen PCs")
    ) %>%
    kable_styling("striped", full_width = FALSE) %>%
    # bold rows that are greater than the average eigenvalue.
    row_spec(which(pca_summary$variance >= (pca_analysis$pca_aug[[1]]$avg_eigen) %>% mean), bold = T)
  
}

# kable_pc_list
# Parameters:
# pca_analysis - output from pca_analysis function
# type - "original"/"standard" for type of analysis
# pc_list - vector of Principle Components to retrieve. i.e. c("PC1", "PC2")
# This function is a utility function to write out the PC coefficients in a formatted way
kable_pc_list <- function(pca_analysis, type, pc_list){
  caption_txt <- ifelse(type == "standard", "Standardized", "Original")
  
  pca_analysis$pca[[1]]$rotation[, pc_list] %>% 
    kable(
      caption = paste("Coefficients for selected ", caption_txt, " Principle Components"),
      col.names = pc_list
    ) %>%
    kable_styling("striped", full_width = FALSE)
}