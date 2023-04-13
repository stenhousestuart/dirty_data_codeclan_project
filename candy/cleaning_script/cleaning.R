
# 1. Setup ----------------------------------------------------------------

### 1.1 Load Libraries

library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(testthat)

### 1.2 Read In Data

candy_2015 <- read_excel(here("data/raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_excel(here("data/raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_excel(here("data/raw_data/boing-boing-candy-2017.xlsx"))

# 2. Select Data ------------------------------------

## Various steps are completed at this stage for each data set. For clarity,
## in-line comments have been added below.

candy_2015_clean <- candy_2015 %>%
  # Select columns 1:3 and those beginning with `[`
  select(2:3 | starts_with("[")) %>% 
  # Clean variable names.
  clean_names() %>% 
  # Drops any fully empty rows, none dropped.
  remove_empty("rows") %>% 
  # Rename columns where required.
  rename("age" = "how_old_are_you",
         "going_out_trick_or_treating" = "are_you_going_actually_going_trick_or_treating_yourself")

candy_2016_clean <- candy_2016 %>%
  # Select Columns 1:6 and those beginning with `[`
  select(2:5 | starts_with("[")) %>% 
  # Clean variable names.
  clean_names() %>% 
  # Drops any fully empty rows, none dropped.
  remove_empty("rows") %>% 
  # Rename columns where required.
  rename("age" = "how_old_are_you",
         "country" = "which_country_do_you_live_in",
         "going_out_trick_or_treating" = "are_you_going_actually_going_trick_or_treating_yourself",
         "gender" = "your_gender")

pattern_to_be_removed_from_2017_data <- "^[q]+[0-9]+[_]+"

candy_2017_clean <- candy_2017 %>%
  # Select Columns 1:6 and those beginning with `Q6`
  select(2:5 | starts_with("Q6")) %>% 
  # Clean variable names.
  clean_names() %>%
  # Drops any fully empty rows, 21.
  remove_empty("rows") %>% 
  # Remove `q.1_` or `q.10_` from start of variable name.
  rename_with(~str_remove(.x, pattern_to_be_removed_from_2017_data)) %>% 
  rename("going_out_trick_or_treating" = "going_out")

# 3. Add Missing Columns -------------------------------------------------

## 3.1 / 2015 - Add response_id, age, country, gender, year

candy_2015_clean <- candy_2015_clean %>%
  mutate(response_id = str_c("2015", row_number()),
         .before = "age") %>%
  mutate(gender = NA,
         .before = "going_out_trick_or_treating") %>%
  mutate(country = NA,
         .before = age) %>% 
  mutate(year = as.integer(2015))

## 3.2 / 2016 - Add response_id, age, country, gender, year

candy_2016_clean <- candy_2016_clean %>%
  mutate(response_id = str_c("2016", row_number()),
         .before = "going_out_trick_or_treating") %>%
  mutate(year = as.integer(2016))

## 3.3 / 2017 - Add response_id, age, country, gender, year

candy_2017_clean <- candy_2017_clean %>%
  mutate(response_id = str_c("2017", row_number()),
         .before = "going_out_trick_or_treating") %>%
  mutate(year = as.integer(2017))

# 4. Pivot Data To Long Format & Join --------------------------------------------------------------

## 4.1 / Pivot Long 2015: No. Observations after pivot should be 534850.

candy_2015_long <- candy_2015_clean %>%
  pivot_longer(cols = 6:100, 
               names_to = "candy_type", 
               values_to = "response") 

## 4.2 / Pivot Long 2016: No. Observations after pivot should be 127159

candy_2016_long <- candy_2016_clean %>%
  pivot_longer(cols = 6:106, 
               names_to = "candy_type", 
               values_to = "response") 

## 4.3 / Pivot Long 2017: No. Observations after pivot should be 251217.

candy_2017_long <- candy_2017_clean %>%
  pivot_longer(cols = 6:108, 
               names_to = "candy_type", 
               values_to = "response") 

## 4.4 / Join 2015, 2016 & 2017 Data

## No. Observations after bind should be 913226, no observations lost.

candy_joined <- bind_rows(candy_2015_long, candy_2016_long, 
                          candy_2017_long)
  
# 5. Tidy `candy_type` Data --------------------------------------------------

## 5.1 / Replace all `_` with ` `.
candy_joined <- candy_joined %>%
  mutate(candy_type = str_replace_all(candy_type, "_", " "))

## 5.2 / Standardise Candy Names

### Initial number of distinct candy_type values: 126

### Details:
### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

candy_joined <- candy_joined %>%
  mutate(candy_type = str_replace_all(candy_type, "a friend to diabetes", ""),
         candy_type = str_replace_all(candy_type, "x100", "100"))

candy_joined <- candy_joined %>% 
  mutate(candy_type = case_when(
    str_detect(candy_type, "anonymous brown globs") ~ "mary janes",
    TRUE ~ candy_type ))

### Number of distinct candy_type values after standardising spelling: 123

## 5.3 / Remove Non-Candy Items

### Details:
### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

not_candy <- c("vials of pure high fructose corn syrup for main lining into your vein", 
               "cash or other forms of legal tender", "dental paraphenalia",
               "generic brand acetaminophen", "glow sticks", "broken glow stick",
               "creepy religious comics chick tracts", "healthy fruit",
               "hugs actual physical hugs", "kale smoothie", "lapel pins",
               "box o raisins","senior mints","mint juleps", "mint leaves",
               "chick o sticks we don t know what that is", "swedish fish",
               "peterson brand sidewalk chalk" ,"trail mix", "vicodin",
               "white bread", "whole wheat anything", "bonkers the board game",
               "boxo raisins", "chardonnay",
               "person of interest season 3 dvd box set not including disc 4 with hilarious outtakes" ,
               "york peppermint patties ignore",
               "real housewives of orange county season 9 blue ray",
               "blue m ms", "red m ms", "green party m ms", "third party m ms",
               "abstained from m ming", "independent m ms")

candy_joined <- candy_joined %>%
  filter(!candy_type %in% not_candy)

### Number of distinct candy_type values after dropping non-candy: 89

### No. Observations after tidying candy_type data: 690779 (-222447).

# 6. Tidy `age` Data --------------------------------------------------------------

## 6.1 / Set any age values that contain characters that aren't 0-9 or . to NA

### Number of responses with age value: 9032

### Details:
### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

candy_joined <- candy_joined %>%
  mutate(age = str_trim(age))

non_digit_pattern <- "[^0-9 .]"

candy_joined <- candy_joined %>%
  mutate(age = str_replace_all(age, non_digit_pattern, NA_character_))

### Number of responses with age value after update: 8888

## 6.2 / Set class to numeric

candy_joined <- candy_joined %>%
  mutate(age = as.numeric(age))

## 6.3 / Update any values with invalid decimal places (eg. not .0) to NA.

candy_joined <- candy_joined %>%
  mutate(age = if_else(age %in% c(0.62, 18.17, 18.75, 23.2, 39.4, 44.4444, 
                                  70.5), NA, age))

### Number of responses with age value after update: 8880

## 6.4 / Set any value greater than 116 to NA.

### Details:
### 116 has been selected as according to online research this is the current 
### age of the oldest known living human.

candy_joined <- candy_joined %>%
  mutate(age = if_else(age > 116, NA, age))

### Number of responses with age value after update: 8867

# 7. Tidy `country` Data ----------------------------------------------------------

### Initial number of distinct country values: 169

### Details:
### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

## 7.1 / Remove Non-Alpha-Numeric Values

non_alpha_numeric_pattern <- "[\\W]"

candy_joined <- candy_joined %>%
  mutate(country = str_replace_all(country, non_alpha_numeric_pattern, " "))

## 7.2 / Set all To lower case

candy_joined <- candy_joined %>% 
  mutate(country = str_to_lower(country))

## 7.3 / Trim leading and trailing white space.

candy_joined <- candy_joined %>%
  mutate(country = str_trim(country))

### Number of distinct country values after 7.1, 7.2 and 7.3: 132

## 7.4 / Standardise Version of USA Used.

### Details:
### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "america" = "united states of america",
                          "u s" = "united states of america",
                          "u s a" = "united states of america",
                          "united states" = "united states of america",
                          "us" = "united states of america",
                          "usa" = "united states of america"))

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "not the usa or canada" = "",
                          "i pretend to be from canada  but i am really from the united states" = ""))

### Number of distinct country values after 7.4: 125

## 7.5 / States In the USA Entered As Country

### Details:
### This code has been intentionally commented out. Please see the associated 
### made at this stage. README.md for details.

### List of US States

# usa_states <- c("alaska", "alabama", "arkansas", "american samoa", "arizona",
#                 "california", "colorado", "connecticut", "district of columbia",
#                 "delaware", "florida", "georgia", "guam", "hawaii", "iowa",
#                 "idaho", "illinois", "indiana", "kansas", "kentucky",
#                 "louisiana", "massachusetts", "maryland", "maine", "michigan",
#                 "minnesota", "missouri", "mississippi", "montana", "north carolina",
#                 "north dakota", "nebraska", "new hampshire", "new jersey",
#                 "new mexico", "nevada", "new york", "ohio", "oklahoma", "oregon",
#                 "pennsylvania", "puerto rico", "rhode island", "south carolina",
#                 "south dakota", "tennessee", "texas", "utah", "virginia",
#                 "virgin islands", "vermont", "washington", "wisconsin",
#                 "west virginia", "wyoming")

### Check if any US states provided as country
# usa_state_check <- candy_joined %>%
#   distinct(response_id, .keep_all = TRUE) %>% 
#   filter(country %in% usa_states)

## 7.6 / Update UK Countries

### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "england" = "united kingdom",
                          "scotland" = "united kingdom",
                          "uk" = "united kingdom"))

### Number of distinct country values after 7.7: 122

## 7.7 / Country Language Variations

### Details:
### This code has been intentionally commented out. Please see the associated 
### made at this stage. README.md for details.

# language_country_check <- candy_joined %>%
#   distinct(response_id, .keep_all = TRUE) %>% 
#   filter(country %in% c("españa", "spain", "brasil")) %>%
#   group_by(country) %>% 
#   summarise(total_usages_by_name = n())
# 
# candy_joined <- candy_joined %>%
#   mutate(country = recode(country,
#                          "españa" = "spain",
#                          "brasil" = "brazil"))

### Number of distinct country values after 7.8: 123

## 7.8 / Prefix Variations

### Details:
### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "the netherlands" = "netherlands",
                          "the united states of america" = "united states of america"))

### Number of distinct country values after 7.9: 120

## 7.9 / Remove Non Country Values

### Details:
### Please see the associated README.md for details of assumptions and judgements
### made at this stage.

### It's acknowledged that the below section of code does not follow expected 
### style guidelines due to it's length. Unfortunately I encountered some bugs 
### when spreading this over multiple lines and was unable to correct this in time
### prior to submission.

candy_joined <- candy_joined %>%
  mutate(country = case_when(
    str_detect(country, "afghanistan|albania|algeria|andorra|angola|antigua and barbuda|argentina|armenia|australia|austria|azerbaijan|bahamas|bahrain|bangladesh|barbados|belarus|belgium|belize|benin|bhutan|bolivia (plurinational state of)|bosnia and herzegovina|botswana|brazil|brunei darussalam|bulgaria|burkina faso|burundi|cabo verde|cambodia|cameroon|canada|central african republic|chad|chile|china|colombia|comoros|congo|congo, democratic republic of the|costa rica|c√¥te d'ivoire|croatia|cuba|cyprus|czechia|denmark|djibouti|dominica|dominican republic|ecuador|egypt|el salvador|equatorial guinea|eritrea|estonia|eswatini|ethiopia|fiji|finland|france|gabon|gambia|georgia|germany|ghana|greece|grenada|guatemala|guinea|guinea-bissau|guyana|haiti|honduras|hungary|iceland|india|indonesia|iran (islamic republic of)|iraq|ireland|israel|italy|jamaica|japan|jordan|kazakhstan|kenya|kiribati|korea (democratic people's republic of)|korea, republic of|kuwait|kyrgyzstan|lao people's democratic republic|latvia|lebanon|lesotho|liberia|libya|liechtenstein|lithuania|luxembourg|madagascar|malawi|malaysia|maldives|mali|malta|marshall islands|mauritania|mauritius|mexico|micronesia (federated states of)|moldova, republic of|monaco|mongolia|montenegro|morocco|mozambique|myanmar|namibia|nauru|nepal|netherlands|new zealand|nicaragua|niger|nigeria|north macedonia|norway|oman|pakistan|palau|panama|papua new guinea|paraguay|peru|philippines|poland|portugal|qatar|romania|russian federation|rwanda|saint kitts and nevis|saint lucia|saint vincent and the grenadines|samoa|san marino|sao tome and principe|saudi arabia|senegal|serbia|seychelles|sierra leone|singapore|slovakia|slovenia|solomon islands|somalia|south africa|south sudan|spain|sri lanka|sudan|suriname|sweden|switzerland|syrian arab republic|tajikistan|tanzania, united republic of|thailand|timor-leste|togo|tonga|trinidad and tobago|tunisia|t√ºrkiye|turkmenistan|tuvalu|uganda|ukraine|united arab emirates|united kingdom|united states of america|uruguay|uzbekistan|vanuatu|venezuela (bolivarian republic of)|viet nam|yemen|zambia|zimbabwe",
               negate = TRUE) ~ NA,
    TRUE ~ country))

### Number of distinct country values after match checks: 32

## 7.10 / Set All To Upper case

candy_joined <- candy_joined %>%
  mutate(country = str_to_title(country))

# 8. Tidy `gender` Data ---------------------------------------------------

### Details:
### The original data contained 4 gender values 'Male', 'Gender', 'Other',
### 'I'd rather not say' and blank values which have been stored as NA.
### As each of these values represents a unique way of answering (or not
### answering) and I could not think of any benefits that would result from
### changes, no changes to the gender data have been made.

# 9. Tidy `going_out_trick_or_treating` Data ----------------------------------

### Details:
### The original data contained a character string of the values "Yes", "No" or
### blank if unanswered which has been stored as NA. As their are only 2 valid 
### completed responses ("Yes" and "No") this data has been updated to the 
### logical type, with TRUE indicating "Yes" and FALSE indicating "No".

candy_joined <- candy_joined %>%
  mutate(going_out_trick_or_treating = case_when(
    going_out_trick_or_treating == "Yes" ~ "TRUE",
    going_out_trick_or_treating == "No" ~ "FALSE",
    TRUE ~ going_out_trick_or_treating),
    going_out_trick_or_treating = as.logical(going_out_trick_or_treating))

# 10. Write To .csv -----------------------------------------------------------

write_csv(candy_joined, here("data/clean_data/candy_clean.csv"))

# 11. Clear Environment ----------------------------------------------------

rm(candy_2015)
rm(candy_2015_clean)
rm(candy_2015_long)
rm(candy_2016)
rm(candy_2016_clean)
rm(candy_2016_long)
rm(candy_2017)
rm(candy_2017_clean)
rm(candy_2017_long)
rm(candy_joined)
rm(non_alpha_numeric_pattern)
rm(non_digit_pattern)
rm(not_candy)
rm(pattern_to_be_removed_from_2017_data)
