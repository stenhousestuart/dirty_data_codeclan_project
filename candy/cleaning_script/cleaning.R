
# 1. Setup ----------------------------------------------------------------

### 1.1 Load Libraries

library(tidyverse)
library(janitor)
library(here)
library(readxl)

### 1.2 Read In Data

candy_2015 <- read_excel(here("data/raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_excel(here("data/raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_excel(here("data/raw_data/boing-boing-candy-2017.xlsx"))

# 2. Select Data and Clean Variable Names ------------------------------------

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
  rename('age' = 'how_old_are_you',
         'going_out' = 'are_you_going_actually_going_trick_or_treating_yourself')

candy_2016_clean <- candy_2016 %>%
  # Select Columns 1:6 and those beginning with `[`
  select(2:5 | starts_with("[")) %>% 
  # Clean variable names.
  clean_names() %>% 
  # Drops any fully empty rows, none dropped.
  remove_empty("rows") %>% 
  # Rename columns where required.
  rename('age' = 'how_old_are_you',
         'country' = 'which_country_do_you_live_in',
         'going_out' = 'are_you_going_actually_going_trick_or_treating_yourself',
         'gender' = 'your_gender')

pattern_to_be_removed_from_2017_data <- "^[q]+[0-9]+[_]+"

candy_2017_clean <- candy_2017 %>%
  # Select Columns 1:6 and those beginning with `Q6`
  select(2:5 | starts_with("Q6")) %>% 
  # Clean variable names.
  clean_names() %>%
  # Drops any fully empty rows, 21.
  remove_empty("rows") %>% 
  # Remove `q.1_` or `q.10_` from start of variable name.
  rename_with(~str_remove(.x, pattern_to_be_removed_from_2017_data))

# 3. Add Extra Columns -------------------------------------------------

## 3.1 / 2015 - Add response_id, age, country, gender, year

candy_2015_clean <- candy_2015_clean %>%
  mutate(response_id = str_c("2015", row_number()),
         .before = "age") %>%
  mutate(gender = NA,
         .before = "going_out") %>%
  mutate(country = NA,
         .before = age) %>%
  mutate(year = as.integer(2015))

## 3.2 / 2016 - Add response_id, age, country, gender, year

candy_2016_clean <- candy_2016_clean %>%
  mutate(response_id = str_c("2016", row_number()), 
         .before = "going_out") %>%
  mutate(year = as.integer(2016))

## 3.3 / 2017 - Add response_id, age, country, gender, year

candy_2017_clean <- candy_2017_clean %>%
  mutate(response_id = str_c("2017", row_number()), 
         .before = "going_out") %>% 
  mutate(year = as.integer(2017))

# 4. Pivot Data To Long Format & Join --------------------------------------------------------------

## 4.1 / Pivot Long 2015: No. Observations after pivot should be 534850.

candy_2015_long <- candy_2015_clean %>%
  pivot_longer(cols = 6:100, 
               names_to = "candy_type", 
               values_to = "response") 

## 4.3 / Pivot Long 2016: No. Observations after pivot should be 127159

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
### Number of distinct candy_type values after standardising spelling: 123

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
### "licorice not black" and "licorice") to maintain specific preferences

candy_joined <- candy_joined %>%
  mutate(candy_type = str_replace_all(candy_type, "a friend to diabetes", ""),
         candy_type = str_replace_all(candy_type, "x100", "100"))

candy_joined <- candy_joined %>% 
  mutate(candy_type = case_when(
    str_detect(candy_type, "anonymous brown globs") ~ "mary janes",
    TRUE ~ candy_type ))

## 5.3 / Remove Non-Candy Items

### Initial number of distinct candy_type values: 126
### Number of distinct candy_type values after dropping non-candy: 89

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

## No. Observations after tidying candy_type data: 690779 (-222447).

# 6. Tidy `age` Data --------------------------------------------------------------

## 6.1 / Set any values that contain non-numeric characters to NA.

### Initial number of distinct age values: 274
### Number of distinct age values after updating any with non-numeric to NA: 83

### Details:
### A wide range of text was entered as age data by respondents. Considering
### my current time constraints and proficiency level, for the purposes of this 
### project any age data which contains non-numeric text will be updated to NA. 
### It is recognised that this may lead to the loss of some usable data being 
### lost.

age_view <- candy_joined %>% 
  distinct(age)

non_digit_pattern <- "[\\D]+"

candy_joined <- candy_joined %>%
  mutate(age = str_replace_all(age, non_digit_pattern, NA_character_))

## 6.2 / Set class to numeric
candy_joined <- candy_joined %>%
  mutate(age = as.numeric(age))

## 6.3 / Set any value greater than 116 to NA.

### Initial number of distinct age values: 83
### Number of distinct age values after updating any > 116 to NA: 80

### Details:
### 116 has been selected as according to online research this is the current 
### age of the oldest known living human.

candy_joined <- candy_joined %>%
  mutate(age = if_else(age > 116, NA, age))

# 7. Tidy `country` Data ----------------------------------------------------------

### When clearing the data a certain amount of personal judgement has been used
### to standardise the formatting and spelling of country data provided. Working
### is provided below.

non_alpha_numeric_pattern <- "[\\W]"

## 7.1 / Remove Non-Alpha-Numeric Values
candy_joined <- candy_joined %>%
  mutate(country = str_replace_all(country, non_alpha_numeric_pattern, " "))

## 7.2 / Set all To lower case
candy_joined <- candy_joined %>% 
  mutate(country = str_to_lower(country))

## 7.3 / Set all To lower case
candy_joined <- candy_joined %>%
  mutate(country = str_trim(country))

## 7.3 / Update USA Based On Partial String Matches

candy_joined <- candy_joined %>%
  mutate(country = case_when(
    str_detect(country, "state") ~ "united states of america",
    str_detect(country, "usa")  ~ "united states of america",
    str_detect(country, "us")  ~ "united states of america",
    str_detect(country, "u s a")  ~ "united states of america",
    str_detect(country, "america")  ~ "united states of america",
    str_detect(country, "united sates")  ~ "united states of america",
    str_detect(country, "murica")  ~ "united states of america",
    str_detect(country, "merica")  ~ "united states of america",
    str_detect(country, "united stetes")  ~ "united states of america",
    str_detect(country, "united staes")  ~ "united states of america",
    str_detect(country, "u s")  ~ "united states of america",
    str_detect(country, " merica")  ~ "united states of america",
    str_detect(country, "amerca")  ~ "united states of america",
    str_detect(country, "united ststes")  ~ "united states of america",
    str_detect(country, "united statss")  ~ "united states of america",
    str_detect(country, "murrika")  ~ "united states of america",
    str_detect(country, "the yoo ess of aaayyyyyy")  ~ "united states of america",
    TRUE ~ country
  ))

## 7.4 / If A US State Is Provided, Update To "united states of america"

candy_joined <- candy_joined %>%
  mutate(country = recode(country,
                          "alaska" = "united states of america",
                          "alabama" = "united states of america",
                          "arkansas" = "united states of america",
                          "american samoa" = "united states of america",
                          "california" = "united states of america",
                          "colorado" = "united states of america",
                          "connecticut" = "united states of america",
                          "district of columbia" = "united states of america",
                          "delaware" = "united states of america",
                          "florida" = "united states of america",
                          "georgia" = "united states of america",
                          "guam" = "united states of america",
                          "hawaii" = "united states of america",
                          "iowa" = "united states of america",
                          "idaho" = "united states of america",
                          "illinois" = "united states of america",
                          "indiana" = "united states of america",
                          "kansas" = "united states of america",
                          "kentucky" = "united states of america",
                          "louisiana" = "united states of america",
                          "massachusetts" = "united states of america",
                          "maryland" = "united states of america",
                          "maine" = "united states of america",
                          "michigan" = "united states of america",
                          "minnesota" = "united states of america",
                          "missouri" = "united states of america",
                          "mississippi" = "united kingdom",
                          "montana" = "united states of america",
                          "north carolina" = "united states of america",
                          "north dakota" = "united states of america",
                          "nebraska" = "united states of america",
                          "new hampshire" = "united states of america",
                          "new jersey" = "united states of america",
                          "new mexico" = "united states of america",
                          "new york" = "united states of america",
                          "ohio" = "united states of america",
                          "oklahoma" = "united states of america",
                          "oregon" = "united states of america",
                          "pennsylvania" = "united states of america",
                          "puerto rico" = "united states of america",
                          "rhode island" = "united states of america",
                          "south carolina" = "united states of america",
                          "tennessee" = "united states of america",
                          "texas" = "united states of america",
                          "utah" = "united states of america",
                          "virginia" = "united states of america",
                          "virgin islands" = "united states of america",
                          "vermont" = "united states of america",
                          "washington" = "united states of america",
                          "wisconsin" = "united states of america",
                          "west virginia" = "united states of america",
                          "wyoming" = "united states of america"))

## 7.5 / Update UK Countries Based On Partial String Matches

candy_joined <- candy_joined %>% 
  mutate(country = case_when(
    str_detect(country, "england") ~ "united kingdom",
    str_detect(country, "united kingdom")  ~ "united kingdom",
    str_detect(country, "united kindom")  ~ "united kingdom",
    str_detect(country, "u k")  ~ "united kingdom",
    str_detect(country, "endland")  ~ "united kingdom",
    str_detect(country, "scotland")  ~ "united kingdom",
    str_detect(country, "uk")  ~ "united kingdom",
    TRUE ~ country
  ))

## 7.6 / Update To English Version of Country Names / Misc.

candy_joined <- candy_joined %>% 
  mutate(country = case_when(
    str_detect(country, "españa") ~ "spain",
    str_detect(country, "brasil") ~ "brazil",
    str_detect(country, "netherlands") ~ "netherlands",
    TRUE ~ country
  ))

## 7.7 / Remove Non Country Values

### Country list sourced from: https://github.com/stefangabos/world_countries/
### With updates made to allow for varied dataset. For example, 
### "Bolivia (Plurinational State of)" updated to "

# candy_joined <- candy_joined %>%
#   mutate(country = case_when(
#     str_detect(country, "afghanistan|albania|algeria|andorra|angola|antigua and barbuda
#                |argentina|armenia|australia|austria|azerbaijan|bahamas|bahrain
#                |bangladesh|barbados|belarus|belgium|belize|benin|bhutan
#                |bolivia|bosnia and herzegovina
#                |botswana|brazil|brunei darussalam|bulgaria|burkina faso
#                |burundi|cabo verde|cambodia|cameroon|canada|central african republic
#                |chad|chile|china|colombia|comoros|congo|congo
#                |costa rica|cote d'ivoire|croatia|cuba|cyprus|czechia|denmark|djibouti
#                |dominica|dominican republic|ecuador|egypt|el salvador|equatorial guinea
#                |eritrea|estonia|eswatini|ethiopia|fiji|finland|france|gabon|gambia
#                |georgia|germany|ghana|greece|grenada|guatemala|guinea|guinea-bissau
#                |guyana|haiti|honduras|hungary|iceland|india|indonesia
#                |iran|iraq|ireland|israel|italy|jamaica
#                |japan|jordan|kazakhstan|kenya|kiribati
#                |democratic people's republic of korea|republic of korea
#                |kuwait|kyrgyzstan|lao people's democratic republic|latvia|lebanon
#                |lesotho|liberia|libya|liechtenstein|lithuania|luxembourg
#                |madagascar|malawi|malaysia|maldives|mali|malta|marshall islands
#                |mauritania|mauritius|mexico|micronesia
#                |moldova|monaco|mongolia|montenegro|morocco|mozambique
#                |myanmar|namibia|nauru|nepal|netherlands|new zealand|nicaragua
#                |niger|nigeria|north macedonia|norway|oman|pakistan|palau|panama
#                |papua new guinea|paraguay|peru|philippines|poland|portugal|qatar
#                |romania|russian federation|rwanda|saint kitts and nevis
#                |saint lucia|saint vincent and the grenadines|samoa|san marino
#                |sao tome and principe|saudi arabia|senegal|serbia|seychelles
#                |sierra leone|singapore|slovakia|slovenia|solomon islands|somalia
#                |south africa|south sudan|spain|sri lanka|sudan|suriname|sweden
#                |switzerland|syrian arab republic|tajikistan|tanzania
#                |thailand|timor-leste|togo|tonga|trinidad and tobago|tunisia|turkiye
#                |turkmenistan|tuvalu|uganda|ukraine|united arab emirates
#                |united kingdom|united states of america|uruguay|uzbekistan|vanuatu
#                |venezuela|vietnam|yemen|zambia|zimbabwe",
#                negate = TRUE) ~ NA,
#     TRUE ~ country))

## 7.7 / Set All To Upper case
# 
# candy_joined <- candy_joined %>% 
#   mutate(country = str_to_title(country))

# 8. Write To .csv -----------------------------------------------------------

write_csv(candy_joined, here("data/clean_data/candy_clean.csv"))

# 9. Clear Environment ----------------------------------------------------

rm(candy_2015)
rm(candy_2015_clean)
rm(candy_2015_long)
rm(candy_2016)
rm(candy_2016_clean)
rm(candy_2016_long)
rm(candy_2017)
rm(candy_2017_clean)
rm(candy_2017_long)
# rm(candy_joined)







