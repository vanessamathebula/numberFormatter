# *******************************
# SetUp
# *******************************

# Clean environment

rm(list = ls())

pacman::p_load(dplyr, tibble, stringr, testthat, roxygen2, rmarkdown, janitor, readr)



# *******************************
# Load Data - Only need to specify data path - all the input data to be saved in the data folder with "Test Inputs"  as the column with the contents to convert.
# *******************************

source("R/number_to_words.R")

number_to_words(file_path = "data/validation_set.txt")
