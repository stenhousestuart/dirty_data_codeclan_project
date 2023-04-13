
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
### Some candy_type data looked to have been entered with slight spelling
### discrepancies. Personal judgement and online research were used to 
### standardise spelling where possible for more accurate analysis. 
### "a friend to diabetes" text was removed from "sweetums a friend to diabetes" 
### to create one distinct "sweetums" candy_type & "x" removed from 
### "x100 grand bar" to create one distinct "100 grand bar" candy_type. Data
### also suggested that entries mentioning "anonymous brown globs that come 
### in black and orange wrappers" were "mary janes" and so any candy_type
### referencing this text was updated to "mary janes". Furthermore, it was
### decided to leave all three "licorice" types ("licorice yes black", 
### "licorice not black" and "licorice") to maintain specific preferences.

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
### Based on online research using the websites of popular American Candy stores 
### a list of candy_type data that did not appear to be candy was created. In
### addition details from the data source were taken into consideration, 
### resulting in 6x M&M entries being removed as these appear to have been added
### as a 'hidden proxy question' relating to voting intentions as opposed to
### being candy types. For the avoidance of doubt, personal judgment and 
### discretion was also used at this stage and a full list of what was 
### considered not to be candy can be found below. Where this was the case, 
### these observations were filtered from the data.

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

## 6.1 / Set any age values that contain non-numeric characters to NA.

### Initial number of distinct age values: 274

### Details:
### A wide range of text was entered as age data by respondents. Considering
### project time constraints and my current proficiency level, for the purposes 
### of this project any age data which contains non-numeric text will be updated 
### to NA. It is recognised that this may lead to the loss of some usable data
### and therefor, for transparency, full working can be seen below.

non_digit_pattern <- "[\\D]+"

candy_joined <- candy_joined %>%
  mutate(age = str_replace_all(age, non_digit_pattern, NA_character_))

### Number of distinct age values after updating any with non-numeric to NA: 83

## 6.2 / Set class to numeric
candy_joined <- candy_joined %>%
  mutate(age = as.numeric(age))

## 6.3 / Set any value greater than 116 to NA.

### Details:
### 116 has been selected as according to online research this is the current 
### age of the oldest known living human.

candy_joined <- candy_joined %>%
  mutate(age = if_else(age > 116, NA, age))

### Number of distinct age values after updating any > 116 to NA: 80

# 7. Tidy `country` Data ----------------------------------------------------------

### Initial number of distinct country values: 169

### Details:
### Some country data looked to have been entered with slight spelling
### discrepancies. Personal judgement and online research were used to 
### standardise spelling where possible for more accurate analysis. When cleaning 
### the data a certain amount of personal judgement has been used to standardise 
### the formatting and spelling of country data provided. Working is provided below.

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

### When reviewing the country data 42 variations of what it's believed were
### intended to mean "United States of America" were found, totaling 3077 
### responses. 7 of these variations were used in over 10 responses and in total
### these made up 3020 responses or 98% of responses where it's believed the
### respondents country was "United States of America". With this in mind, these
### 7 variations were chosen to be re-coded (see below) and the others left to
### be updated to NA in step 7.9 when they fail to match against the country
### list.

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "america" = "united states of america",
                          "u s" = "united states of america",
                          "u s a" = "united states of america",
                          "united states" = "united states of america",
                          "us" = "united states of america",
                          "usa" = "united states of america"))

### This process also flagged to country values which contained 2 country names
### "not the usa or canada" and "i pretend to be from canada but i am really 
### from the united states". Because of problems encountered in later cleaning
### stages which I believe these may have been contributing to and because these 
### were only associated to 1 response each, I decided to update these to blank
### to be updated to NA when not matched on the country list.

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "not the usa or canada" = "",
                          "i pretend to be from canada  but i am really from the united states" = ""))

### Number of distinct country values after 7.4: 125

## 7.5 / States In the USA Entered As Country

### When reviewing the country data at this stage I noticed that some states in
### the USA looked to have provided as the country. To decide an appropriate
### approach to these, I looked at how many times this had occured. I found
### That 5 US states had been entered as the country ('alaska', 'new jersey',
### 'new york', 'north carolina' and 'california') and that each had been used
### only once. With this in mind, I decided that due to only being related to 5
### responses, leaving these to update to NA when they failed to match against
### the country list would not adversely impact the analysis. The code used for
### checking for US State data has been left for reference below, but has been
### commented out as it is not required for cleaning.

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

### As one of the analysis questions asks about UK values, the decision was made
### to group England, Scotland, Wales and Northern Ireland entries under UK.
### When reviewing the data, excluding those already specified as
### "United Kingdom", 4 entries that would fall in the category of the
### United Kingdom were identified ("united kindom", "scotland", "endland",
### "england" and "uk") which were linked to 1, 5, 1, 5 and 30 responses in turn.
### I therefor decided to recode values used 5 or more times, which would result 
### in only 2 responses having their country value updated to NA when they 
### failed to match the country list believing would not adversely impact the 
### analysis.

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "england" = "united kingdom",
                          "scotland" = "united kingdom",
                          "uk" = "united kingdom"))

### Number of distinct country values after 7.7: 122

## 7.7 / Country Language Variations

### When reviewing the data I noticed that some countries data was not in English
### and I intended to match against an English language country list. These were
### "españa" and "brasil". Both were linked to only one variation and in-line
### with other steps when cleaning the country data, I decided that leaving this
### to update to NA when they did not match to the country list would not adversely
### impact the analysis. However, to demonstrate how these could be recoded,
### the code for this has been included below and commented out. The code for
### checking the usages of each variation has also been included but commented
### out as it is not required for cleaning.

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

## In two cases I encountered problems with the prefix "the" creating seperate
## country values for both "the netherlands" and "the united states of america".
## Because of problems encountered in later cleaning stages which I believe 
## these may have been contributing to, these were both recoded to remove the
## prefix.

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "the netherlands" = "netherlands",
                          "the united states of america" = "united states of america"))

### Number of distinct country values after 7.9: 120

## 7.9 / Remove Non Country Values

### For the next stage, I sourced a full country list from 
### "https://github.com/stefangabos/world_countries/" to compare my country
### data against. Considering project  time constraints and my own current 
### proficiency level, for the purposes of this project any country data which 
### does not match will be updated to NA. It is recognised that requiring an
### exact match may lead to the loss of some data, in particular where the list
### requires country values to be formatted in a particular way, for example
### in cases where some of the country name is in parenthesis. There are 5
### countries where this is the case and only 2 responses may have been impacted
### both of which had a country values of "korea". Due to being uncertain if this
### related to "korea (democratic people's republic of)" or "korea, republic of"
### and as only 2 responses were effected, this was left to update to NA. The
### list was however updated to reflect the preferred formatting of 
### "united kingdom" where set earlier in the cleaning process.

## It's acknowledged that the below section of code does not follow expected 
## style guidelines due to it's length. Unfortunately I encountered some bugs 
## when spreading this over multiple lines and was unable to correct this in time
## prior to submission.

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

### The original data contained 4 gender values 'Male', 'Gender', 'Other',
### 'I'd rather not say' and blank values which have been stored as NA.
### As each of these values represents a unique way of answering (or not
### answering) and I could not think of any benefits that would result from
### changes, no changes to the gender data have been made.

# 9. Tidy `going_out_trick_or_treating` Data ----------------------------------

candy_joined <- candy_joined %>%
  mutate(going_out_trick_or_treating = case_when(
    going_out_trick_or_treating == "Yes" ~ "TRUE",
    going_out_trick_or_treating == "No" ~ "FALSE",
    TRUE ~ going_out_trick_or_treating),
    going_out_trick_or_treating = as.logical(going_out_trick_or_treating))

### The original data contained a character string of the values "Yes", "No" or
### blank if unanswered which has been stored as NA. As their are only 2 valid 
### completed responses ("Yes" and "No") this data has been updated to the 
### logical type, with TRUE indicating "Yes" and FALSE indicating "No".

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
