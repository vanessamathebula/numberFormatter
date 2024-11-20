

# *******************************
# Setup
# *******************************


pacman::p_load(dplyr, tibble, stringr, testthat, roxygen2, rmarkdown, janitor, readr, here)




# *******************************
# Test if the function works accordingly
# *******************************


test_that("the numbers are correctly converted", {

  source(here("R", "number_to_words.R"))

  #file_path = "../data/unit_test_data.txt"
  file_path <- here("data", "unit_test_data.txt")

  dynamically_converted_tibble <-
    number_to_words(file_path = file_path) %>%
    arrange(input)

  imported_manually_converted_tibble <-
    read_delim(file = file_path, delim = "\t") %>% # This places the loaded data in columns
    as_tibble() %>%
    clean_names() %>%
    select(input = "test_input", output = "expected_output") %>%
    arrange(input)

  # Check if tibble1 and tibble2 are identical

  expect_true(all.equal(dynamically_converted_tibble, imported_manually_converted_tibble) == TRUE) # This should pass



})
