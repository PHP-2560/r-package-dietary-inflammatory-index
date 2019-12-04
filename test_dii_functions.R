##################################
###### TESTING DII FUNCTIONS
##################################

source("./dii_functions.R")


## Load sample data 

test_data <- readRDS("diet_data_sample.rds")

################ Test grams to micrograms function ##########

grams_to_micrograms(test_data$caffeine)

## Check if mcg is < grams 

grams_to_micrograms(test_data$caffeine) <= test_data$caffeine

# ALL TRUE so use to convert

test_data$caffeine <- grams_to_micrograms(test_data$caffeine)

################ Test mcg to grams ##########################

micrograms_to_grams(test_data$caffeine)

## Check if grams > mcg

micrograms_to_grams(test_data$caffeine) >= test_data$caffeine

# ALL TRUE

test_data$b6 <- grams_to_micrograms(test_data$b6)

################ Test sort vars function #####################

# This function is used to build global index for dii function. Preprocessing is required to use the sorting function. 
# This processing has been done for the sample data, so the function can be used as is.

new_data <- sort_vars(test_data)[[1]] # Getting new, sorted data

global_index <- sort_vars(data)$global_index # getting global index

names(new_data) # we should see only nutrition vars... we do, so we know the sorting function worked

global_index # we have a global index... this function works

################ Test Dietary Inflammatory Index function ####

# We should obtain a vector of values indicating the inflammatory index of each observation

dii <- get_dietary_inflammatory_index(new_data); dii # It works... Is it within the range expected by DII? (i.e., (-10,10)) (Shivappa et al., 2014)

cat(min(dii), max(dii)) # It appears to be.

hist(dii)

# Diet appears to be anti-inflammatory in this sample.