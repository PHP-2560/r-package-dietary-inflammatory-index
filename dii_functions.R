########################
# DII Helper Functions #
########################
library(readr)
library(tidyr)
library(plyr)
library(stringr)
library(tidyverse)
library(tableone)
library(readr)
data <- readRDS("diet_data_sample.rds")
# NOTE: THIS PACKAGE REQUIRES A MATRIX OF AVERAGE INTAKE OF THE FOLLOWING ITEMS: 
# Not all items are present in most FFQs or ASA24s
glob_data <- readRDS("global_dii_data.rds")
glob_data$item

#Function to convert grams to micrograms and micrograms to grams

grams_to_micrograms <- function(item_intake_in_grams)
{
  micrograms <- item_intake_in_grams * 1/1000
  return(micrograms)
}

micrograms_to_grams <- function(item_intake_in_micrograms) 
{
  grams <- item_intake_in_micrograms * 1000
  return(grams)
}

data$caffeine <- grams_to_micrograms(data$caffeine)
micrograms_to_grams(data$caffeine)
data$b6 <- grams_to_micrograms(data$b6)

#Function which grabs intake index and matches to index for global values

#Make an empty vector
index_g <- c()
index_d <- c()
### THIS WILL BE THE HARDEST PART

sort_vars <- function(data){
 data <- data %>% 
    select("alcohol",        "b12",            "b6",
           "beta.carotene",  "caffeine",
           "chole",          "kcal",
           "fiber",          "folic.acid",     
           "iron",           "magnesium",     
           "MUFA",           "niacin",       
           "PUFA",           "riboflavin",    
           "sat.fat",        "selenium",       "thiamin",
           "vit.A",          "vit.C",         
           "vit.D",          "vit.E",          
           "zinc") 
}

sort_vars(data)
names(data)
glob.data$item
index_intake <- c(12,13,14,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52) 
index_global <- c(6,22,10,1,8,2,3,4,5,7,11,12,15,16,17,18,23,24,28,31,32,33,34,35,26,27)
### HARDEST PART

#Function which creates effect scores for intakes
#145s need to be changed to length(data) and 26 to length of vars to be used

get_dietary_inflammatory_index <- function(intake_data, index_global, index_intake_data)
{
  effect_scores <-  matrix(data = rep(glob_data[index_global,2],145), ncol = 26, nrow = 145, byrow = TRUE)
  glob_intake <- matrix(data = rep(glob_data[index_global,3],145), ncol = 26, nrow = 145, byrow = TRUE)
  glob_std_dev <- matrix(data = rep(glob_data[index_global,4],145), ncol = 26, nrow = 145, byrow = TRUE)
  actual_intake <- as.matrix(intake_data[,index_intake_data])
  z_score_LIFE <- (actual_intake - glob_intake) / glob_std_dev
  pctiles_LIFE <- pnorm(z_score_LIFE)
  DII <- pctiles_LIFE * effect_scores
  return(rowSums(DII))
}

data$total_DII <- get_dietary_inflammatory_index(data, index_global = index_global, index_intake_data = index_LIFE_intake)


# vars <- names(data[,c(2:11,53)])
# cat_vars <- names(data[,c(2,4:11)])
# DII_table <- CreateTableOne(vars, strata = "Assignment", data, cat_vars)
# print(DII_table, exact = TRUE, nonnormal = "total_DII")