### 1.1 Load Libraries

library(tidyverse)
library(janitor)
library(here)
library(readxl)

### 1.2 Read In Data

candy_2015 <- read_excel(here("data/raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_excel(here("data/raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_excel(here("data/raw_data/boing-boing-candy-2017.xlsx"))

### Tidy Variable Names

candy_2015_clean <-  clean_names(candy_2015)
candy_2016_clean <-  clean_names(candy_2016)
candy_2017_clean <-  clean_names(candy_2017)

pattern_to_be_removed <- "^[q]+[0-9]+[_]+"

candy_2017_clean <- rename_with(candy_2017_clean, 
                                ~str_remove(.x, pattern_to_be_removed))

# Hard coding method for renaming variables.
#candy_2017_clean <- rename_with(candy_2017_clean, ~str_remove(.x, "q1_"))

# Code for Comparing Variables Across Different Data Frames
column_compare <- compare_df_cols(candy_2015_clean, candy_2016_clean, 
                                  candy_2017_clean)