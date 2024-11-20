


# *******************************
# Create Function
# *******************************

#' number_to_words
#'
#' This function takes a sentence, checks if there is valid numerical input. If there is, converts the numerical data (up to 11 digits) to words, otherwise returns "number invalid". In the case where the number is valid, but has digits that are greater than 11, it returns "number greater than 11 digits".
#'
#' @param file_path A path to a text file with contents to convert. Required column is the "Test Input" column.
#' @return Prints a tibble with the original text in one column and conversion results in the next column.
#' @examples
#' number_to_words("data/test_input_data.txt")
#' @export


# *******************************
# Create Function
# *******************************

number_to_words <- function(file_path = "data/test_input_data.txt") {


  # *******************************
  # Load/Install and Load Packages
  # *******************************


  pacman::p_load(dplyr, tibble, stringr, testthat, roxygen2, rmarkdown, janitor, readr)


# *******************************
# Load Data
# *******************************

data <- file_path

test_data <-
  read_delim(file = data, delim = "\t") %>% # This places the loaded data in columns
  as_tibble() %>%
  clean_names()


# *******************************
# Create a digit mapping tibble
# *******************************

# This will help when converting the digits to text

normal_number_mapping <-
  tibble("digit" = rep(0:100),
         "number" = case_when(digit == "0" ~ "", digit == "1" ~ "one", digit == "2" ~ "two", digit == "3" ~ "three", digit == "4" ~ "four",
                              digit == "5" ~ "five", digit == "6" ~ "six", digit == "7" ~ "seven", digit == "8" ~ "eight",
                              digit == "9" ~ "nine", digit == "10" ~ "ten", digit == "11" ~ "eleven", digit == "12" ~ "twelve",
                              digit == "13" ~ "thirteen", digit == "14" ~ "fourteen", digit == "15" ~ "fifteen", digit == "16" ~ "sixteen",
                              digit == "17" ~ "seventeen", digit == "18" ~ "eighteen", digit == "19" ~ "nineteen", digit == "20" ~ "twenty",
                              digit == "21" ~ "twenty-one", digit == "22" ~ "twenty-two", digit == "23" ~ "twenty-three",
                              digit == "24" ~ "twenty-four", digit == "25" ~ "twenty-five", digit == "26" ~ "twenty-six",
                              digit == "27" ~ "twenty-seven", digit == "28" ~ "twenty-eight", digit == "29" ~ "twenty-nine",
                              digit == "30" ~ "thirty", digit == "31" ~ "thirty-one", digit == "32" ~ "thirty-two", digit == "33" ~ "thirty-three",
                              digit == "34" ~ "thirty-four",  digit == "35" ~ "thirty-five",  digit == "36" ~ "thirty-six",  digit == "37" ~ "thirty-seven",
                              digit == "38" ~ "thirty-eight",  digit == "39" ~ "thirty-nine", digit == "40" ~ "fourty", digit == "41" ~ "fourty-one",
                              digit == "42" ~ "fourty-two", digit == "43" ~ "fourty-three", digit == "44" ~ "fourty-four", digit == "45" ~ "fourty-five",
                              digit == "46" ~ "fourty-six", digit == "47" ~ "fourty-seven", digit == "48" ~ "fourty-eight", digit == "49" ~ "fourty-nine",
                              digit == "50" ~ "fifty", digit == "51" ~ "fifty-one", digit == "52" ~ "fifty-two", digit == "53" ~ "fifty-three",
                              digit == "54" ~ "fifty-four", digit == "55" ~ "fifty-five", digit == "56" ~ "fifty-six", digit == "57" ~ "fifty-seven",
                              digit == "58" ~ "fifty-eight", digit == "59" ~ "fifty-nine", digit == "60" ~ "sixty", digit == "61" ~ "sixty-one",
                              digit == "62" ~ "sixty-two", digit == "63" ~ "sixty-three", digit == "64" ~ "sixty-four", digit == "65" ~ "sixty-five",
                              digit == "66" ~ "sixty-six", digit == "67" ~ "sixty-seven", digit == "68" ~ "sixty-eight", digit == "69" ~ "sixty-nine",
                              digit == "70" ~ "seventy", digit == "71" ~ "seventy-one", digit == "72" ~ "seventy-two", digit == "73" ~ "seventy-three",
                              digit == "74" ~ "seventy-four", digit == "75" ~ "seventy-five", digit == "76" ~ "seventy-six", digit == "77" ~ "seventy-seven",
                              digit == "78" ~ "seventy-eight", digit == "79" ~ "seventy-nine", digit == "80" ~ "eighty", digit == "81" ~ "eighty-one",
                              digit == "82" ~ "eighty-two", digit == "83" ~ "eighty-three", digit == "84" ~ "eighty-four", digit == "85" ~ "eighty-five",
                              digit == "86" ~ "eighty-six", digit == "87" ~ "eighty-seven", digit == "88" ~ "eighty-eight", digit == "89" ~ "eighty-nine",
                              digit == "90" ~ "ninety", digit == "91" ~ "ninety-one", digit == "92" ~ "ninety-two", digit == "93" ~ "ninety-three",
                              digit == "94" ~ "ninety-four", digit == "95" ~ "ninety-five", digit == "96" ~ "ninety-six", digit == "97" ~ "ninety-seven",
                              digit == "98" ~ "ninety-eight", digit == "99" ~ "ninety-nine",
                              TRUE ~ "hundred"))

normal_number_mapping_zeros <-
  tibble("digit" = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
         'number' = c("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))


normal_number_mapping <-
  normal_number_mapping %>%
  rbind(., normal_number_mapping_zeros)

# *******************************
# Number Denominations Breakdowm
# *******************************

# This will help when converting the digits to text

number_denominations <-
  tibble(number = c("1", "10", "100", "1000", "10000",  "100000", "1000000", "10000000", "100000000", "1000000000", "10000000000", "100000000000",
                    "1000000000000", "10000000000000", "100000000000000", "1000000000000000", "10000000000000000", "100000000000000000", "1000000000000000000", "10000000000000000000"),
         denomination = c("one", "ten", "hundred", "thousand", "ten thousand", "hundred thousand", "million", "ten million",  "hundred million", "billion", "ten billion", "hundred billion",
                          "trillion", "ten trillion", "hundred trillion", "quadrillion", "ten quadrillion", "hundred quadillion", "quantillion", "ten quantillion")) %>%
  mutate(number_of_digits = str_length(number),
         zeros = str_count(string = number, pattern = "0"))

# *******************************
# Extract Numerical Contents
# *******************************

conditioning_tibble <-
  test_data %>%
  select(input = "test_input") %>%
  mutate(extracted_digits = str_extract(input, "\\S*(\\d[\\d\\s,]*\\d|#\\d+|\\d+)\\S*"), #regex matches and extracts number patterns where special characters and blank spaces between the numbers are also returned.
         extracted_digitis_formatting = str_trim(extracted_digits), # removes empty spaces before and after the string
         extracted_digits_formatting_conditioning = str_detect(string = extracted_digitis_formatting, pattern = "[^A-Za-z0-9]|\\s"), # checks for whether the numerical string has any special characters, including underscores and empty spaces.
         expected_output = case_when(extracted_digits_formatting_conditioning == "TRUE" ~ "number invalid",
                                     TRUE ~ "convert"),
         number_of_digits = str_length(extracted_digitis_formatting))

# Extract numerical data to convert

numerical_tibble <-
  conditioning_tibble %>%
  filter(expected_output == "convert") %>%
  left_join(., number_denominations, by = "number_of_digits") %>%
  select(input, extracted_digits, number_of_digits, denomination)

# Convert numbers to words

numerical_tibble_output <-
  numerical_tibble %>%
  mutate(actual_number = case_when(# Condition when there is only 1 and 2 digits
    number_of_digits %in% c(1,2) & extracted_digits %in% normal_number_mapping$digit ~
      normal_number_mapping$number[match(extracted_digits, normal_number_mapping$digit)],

    # Condition when there are 3 digits

    number_of_digits == 3 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 2, end = 2) != "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        denomination, "and", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 2, end = 3), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 2, end = 3) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        denomination),

                TRUE ~ paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                             denomination, "and", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 3), normal_number_mapping$digit)])
      ),

    # Condition when there are 4 digits

    number_of_digits == 4 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 2, end = 4) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        denomination),

                str_sub(string = extracted_digits, start = 2, end = 3) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        denomination, "and", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 2, end = 2) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        denomination, "and", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 4), normal_number_mapping$digit)]),

                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ",
                        denomination, ", ", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 2, end = 2), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 4), normal_number_mapping$digit)], sep = "")
      ),

    # Condition when there are 5 digits

    number_of_digits == 5 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 3, end = 5) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], "thousand"),

                str_sub(string = extracted_digits, start = 3, end = 4) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)],
                        "thousand", "and",  normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 3, end = 3) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)],
                        "thousand", "and", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 5), normal_number_mapping$digit)]),

                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)],
                        " thousand,", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 3), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 5), normal_number_mapping$digit)], sep = "")
      ),

    # Condition when there are 6 digits

    number_of_digits == 6 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 2, end = 6) == "00000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " hundred thousand ", sep = ''),

                str_sub(string = extracted_digits, start = 2, end = 5) == "0000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        " hundred thousand and ",  normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], sep = ""),

                str_sub(string = extracted_digits, start = 2, end = 4) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        " hundred thousand and ", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], sep = ""),

                str_sub(string = extracted_digits, start = 2, end = 3) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        " hundred thousand ", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], sep = ""),

                str_sub(string = extracted_digits, start = 2, end = 2) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        " hundred and ", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 3), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], sep = ""),

                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)],
                        " hundred and ", normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 2, end = 3), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], sep = "")),

    # Condition when there are 7 digits
    number_of_digits == 7 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 2, end = 7) == "000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], denomination),

                str_sub(string = extracted_digits, start = 2, end = 6) == "00000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], denomination, "and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 2, end = 5) == "0000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], denomination, "and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 2, end = 4) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ",",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], sep = ""),

                str_sub(string = extracted_digits, start = 2, end = 3) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ",",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 2, end = 2) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, " and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 4), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], sep = ""),


                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ",",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 2, end = 2), normal_number_mapping$digit)], " hundred, and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 4), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], sep = '')),


    # Condition when there are 8 digits
    number_of_digits == 8 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 3, end = 8) == "000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], "million"),

                str_sub(string = extracted_digits, start = 3, end = 7) == "00000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], 'million', "and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 3, end = 6) == "0000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], "million", "and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)]),


                str_sub(string = extracted_digits, start = 3, end = 5) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " million ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], sep = ""),




                str_sub(string = extracted_digits, start = 3, end = 4) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], sep = ""),



                str_sub(string = extracted_digits, start = 3, end = 3) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 5), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], sep = ""),



                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 3), normal_number_mapping$digit)], " hundred, and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 5), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], sep = '')),


    # Condition when there are 9 digits
    number_of_digits == 9 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 2, end = 9) == "00000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred million"),


                str_sub(string = extracted_digits, start = 2, end = 8) == "0000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred million and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 2, end = 7) == "000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred million and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 9), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 2, end = 6) == "00000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred million",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 9), normal_number_mapping$digit)]),


                str_sub(string = extracted_digits, start = 2, end = 5) == "0000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred million,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], "thousand,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 9), normal_number_mapping$digit)]),


                str_sub(string = extracted_digits, start = 2, end = 4) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred million,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], "thousand,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 9), normal_number_mapping$digit)]),


                str_sub(string = extracted_digits, start = 2, end = 3) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred million,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], "hundred and,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], "thousand,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 9), normal_number_mapping$digit)]),


                str_sub(string = extracted_digits, start = 2, end = 2) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 3), normal_number_mapping$digit)], "million,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], "hundred and,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], "thousand,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 9), normal_number_mapping$digit)]),

                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 2, end = 3), normal_number_mapping$digit)], "million,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], "hundred and,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 6), normal_number_mapping$digit)], "thousand,",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)], "hundred and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 9), normal_number_mapping$digit)])),


    # Condition when there are 10 digits

    number_of_digits == 10 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 2, end = 10) == "000000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], denomination),


                str_sub(string = extracted_digits, start = 2, end = 9) == "00000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], denomination, "and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 10), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 2, end = 8) == "0000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], denomination, "and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)]),


                str_sub(string = extracted_digits, start = 2, end = 7) == "000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ", ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)], sep = ""),



                str_sub(string = extracted_digits, start = 2, end = 6) == "00000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ", ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 7), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 2, end = 5) == "0000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ", ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 2, end = 4) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ", ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 2, end = 3) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ", ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 4), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 2, end = 2) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, " and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 4), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)], sep = ""),


                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 1), normal_number_mapping$digit)], " ", denomination, ", ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 2, end = 2), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 4), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 7), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 10), normal_number_mapping$digit)], sep = "")),

    # Condition when there are 11 digits

    number_of_digits == 11 & str_sub(string = extracted_digits, start = 1, end = 1) %in% normal_number_mapping$digit ~
      case_when(str_sub(string = extracted_digits, start = 3, end = 11) == "000000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], "billion"),

                str_sub(string = extracted_digits, start = 3, end = 10) == "00000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], "billion and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 11, end = 11), normal_number_mapping$digit)]),

                str_sub(string = extracted_digits, start = 3, end = 9) == "0000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], "billion and",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)]),


                str_sub(string = extracted_digits, start = 3, end = 8) == "000000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " billion, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 3, end = 7) == "00000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " billion, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 8, end = 8), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 3, end = 6) == "0000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " billion, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 3, end = 5) == "000" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " billion, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 3, end = 4) == "00" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " billion, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 5, end = 5), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)], sep = ""),


                str_sub(string = extracted_digits, start = 3, end = 3) == "0" ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " billion, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 5), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)], sep = ""),

                TRUE ~
                  paste(normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 1, end = 2), normal_number_mapping$digit)], " billion ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 3, end = 3), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 4, end = 5), normal_number_mapping$digit)], " million, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 6, end = 6), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 7, end = 8), normal_number_mapping$digit)], " thousand, ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 9, end = 9), normal_number_mapping$digit)], " hundred and ",
                        normal_number_mapping$number[match(str_sub(string = extracted_digits, start = 10, end = 11), normal_number_mapping$digit)], sep = "")),

    TRUE ~ "number greater than 11 digits"
  )
  ) %>%
  # The zeros within digit strings causes formmatting issues and this section cleans those
  mutate(actual_number = gsub("\\s+", " ", actual_number), # removes additional spaces
         actual_number_cleaned = str_replace(actual_number, "\\s+$", ""), # Removes spaces and charcters at the end of the sentence
         actual_number_cleaned = str_replace(string = actual_number_cleaned, pattern = "million, hundred and", replacement = "million and"),
         actual_number_cleaned = str_replace(string = actual_number_cleaned, pattern = "million, thousand", replacement = "million"),
         actual_number_cleaned = str_replace(string = actual_number_cleaned, pattern = "million and, thousand", replacement = "million"),
         actual_number_cleaned = str_replace(string = actual_number_cleaned, pattern = "hundred and million", replacement = "hundred and"),
         actual_number_cleaned = str_replace(string = actual_number_cleaned, pattern = "million and thousand", replacement = "million"),
         actual_number_cleaned = str_replace(string = actual_number_cleaned, pattern = "thousand, hundred and", replacement = "thousand and"),
         actual_number_cleaned = str_replace(actual_number_cleaned, " and\\s*$", "")) %>% #Replaces the noted pattern only at the the end of the sentence
  select(input, output = 'actual_number_cleaned')


# Extract the invalid numbers and append to the final converted numbers tibble

non_numerical_tibble <-
  conditioning_tibble %>%
  filter(expected_output != "convert") %>%
  select(input, output = "expected_output")

converted_tibble <-
  rbind(numerical_tibble_output, non_numerical_tibble)

print(converted_tibble)

}
