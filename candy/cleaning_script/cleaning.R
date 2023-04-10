### 1.1 Load Libraries

library(tidyverse)
library(janitor)
library(here)
library(readxl)

### 1.2 Read In Data

candy_2015 <- read_excel(here("data/raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_excel(here("data/raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_excel(here("data/raw_data/boing-boing-candy-2017.xlsx"))


# Select Data and Clean Variable Names ------------------------------------

## At this stage, priority is given to cleaning variable names required for 
## binding the rows after the data sets have been pivoted.

candy_2015_clean <- candy_2015 %>%
  # Select Columns 1:3 and those beginning with `[`
  select(2:3 | starts_with("[")) %>% 
  clean_names() %>% 
  remove_empty("rows") %>% 
  rename('age' = 'how_old_are_you',
         'going_out' = 'are_you_going_actually_going_trick_or_treating_yourself')

candy_2016_clean <- candy_2016 %>%
  # Select Columns 1:6 and those beginning with `[`
  select(2:5 | starts_with("[")) %>% 
  clean_names() %>% 
  remove_empty("rows") %>% 
  rename('age' = 'how_old_are_you',
         'country' = 'which_country_do_you_live_in',
         'going_out' = 'are_you_going_actually_going_trick_or_treating_yourself',
         'gender' = 'your_gender')

pattern_to_be_removed_from_2017_data <- "^[q]+[0-9]+[_]+"

candy_2017_clean <- candy_2017 %>%
  # Select Columns 1:6 and those beginning with `Q6`
  select(2:5 | starts_with("Q6")) %>% 
  clean_names() %>%
  remove_empty("rows") %>% 
  # Remove `q.1_` or `q.10_` from start of variable name.
  rename_with(~str_remove(.x, pattern_to_be_removed_from_2017_data))

# Add Response ID Columns -------------------------------------------------

## A unique `response_id` column is added for every survey response.

## 2015 (Also Adds Gender and Country Columns Populated With NA)

candy_2015_clean <- candy_2015_clean %>%
  mutate(response_id = str_c("2015", row_number()), 
         .before = "age") %>% 
  mutate(gender = NA, 
         .before = "going_out") %>% 
  mutate(country = NA, 
         .before = age)
  # relocate(going_out, .after = response_id) %>% 
  # relocate(gender, .after = going_out) %>% 
  # relocate(age, .after = gender)

## 2016
candy_2016_clean <- candy_2016_clean %>%
  mutate(response_id = str_c("2016", row_number()), 
         .before = "going_out")

## 2017
candy_2017_clean <- candy_2017_clean %>%
  mutate(response_id = str_c("2017", row_number()), 
         .before = "going_out")

# Pivot Long --------------------------------------------------------------

## The data is pivoted to long format to support future analysis.

## 2015
candy_2015_long <- candy_2015_clean %>%
  pivot_longer(cols = 6:100, 
               names_to = "candy_type", 
               values_to = "response") 

## 2016
candy_2016_long <- candy_2016_clean %>%
  pivot_longer(cols = 6:106, 
               names_to = "candy_type", 
               values_to = "response") 

## 2017
candy_2017_long <- candy_2017_clean %>%
  pivot_longer(cols = 6:108, 
               names_to = "candy_type", 
               values_to = "response") 


# Join Data ---------------------------------------------------------------

candy_joined <- bind_rows(candy_2015_long, candy_2016_long, candy_2017_long)
  
# Remove Non-Candy Items --------------------------------------------------

## Could these rows be dropped when they are still columns, by indexing?

candy_joined <- candy_joined %>%
  filter(!str_detect(candy_type, "glow_stick|board_game|lapel_pins|pencils|abstained"),
         !str_detect(candy_type, "cash|dental_paraphenalia|hugs_actual_physical_hugs"),
         !str_detect(candy_type, "peterson_brand_sidewalk_chalk|chardonnay"),
         !str_detect(candy_type, "creepy_religious_comics_chick_tracts|acetaminophen|ignore"),
         !str_detect(candy_type, "swedish_fish|vicodin|white_bread|x114"),
         !str_detect(candy_type, "person_of_interest_season|real_housewives"))

# candy_types_view <- candy_joined %>% distinct(candy_type)

# Tidy `candy_type` -------------------------------------------------

## Standardise Names

candy_joined <- candy_joined %>%
  mutate(candy_type = str_replace_all(candy_type, "a_friend_to_diabetes", ""),
         candy_type = str_replace_all(candy_type, "the_candy", ""),
         candy_type = str_replace_all(candy_type, "x100", "100"),
         candy_type = str_replace_all(candy_type, "boxo_raisins", "box_o_raisins"),
         candy_type = str_replace_all(candy_type, "_something_or_other", ""),
         candy_type = str_replace_all(candy_type, "_we_don_t_know_what_that_is", ""))

## Replace all `_` with ` `.
candy_joined <- candy_joined %>%
  mutate(candy_type = str_replace_all(candy_type, "_", " "))


# Tidy Environment --------------------------------------------------------

rm(candy_2015)
rm(candy_2015_clean)
rm(candy_2015_long)
rm(candy_2016)
rm(candy_2016_clean)
rm(candy_2016_long)
rm(candy_2017)
rm(candy_2017_clean)
rm(candy_2017_long)


# Update Variable Types ---------------------------------------------------

# Could be updated to try and add additional values, rather than cooercing to NA.

# age_view <- candy_joined %>% distinct(age)
# 
# candy_joined <- candy_joined %>%
#   mutate(response_id = as.integer(response_id),
#          age = as.integer(age))

# Tidy `country` ----------------------------------------------------------

# country_view <- candy_joined %>% distinct(country)
# 
# 
# # Tidy `going_out` --------------------------------------------------------
# 
# 
# ## Convert to logical?
# 
# # Tidy`gender` ------------------------------------------------------------
# 
# gender_view <- candy_joined %>% distinct(gender)

# 6. Write to .csv



# 7. Remove redundant objects from environment

