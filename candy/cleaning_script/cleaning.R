
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

## 4.1 / Pivot Long 2015

candy_2015_long <- candy_2015_clean %>%
  pivot_longer(cols = 6:100, 
               names_to = "candy_type", 
               values_to = "response") 

## 4.3 / Pivot Long 2016

candy_2016_long <- candy_2016_clean %>%
  pivot_longer(cols = 6:106, 
               names_to = "candy_type", 
               values_to = "response") 

## 4.3 / Pivot Long 2017

candy_2017_long <- candy_2017_clean %>%
  pivot_longer(cols = 6:108, 
               names_to = "candy_type", 
               values_to = "response") 

## 4.4 / Join 2015, 2016 & 2017 Data

candy_joined <- bind_rows(candy_2015_long, candy_2016_long, candy_2017_long)
  
# 5. Tidy `candy_type` Data --------------------------------------------------

## 5.1 / Replace all `_` with ` `.
candy_joined <- candy_joined %>%
  mutate(candy_type = str_replace_all(candy_type, "_", " "))

## 5.2 / Remove Non-Candy Items

### For the purpose of clarity, based on research and personal judgement
### candy_type values which contain the below strings have been removed due to
### not being considered as candy.

not_candy <- c("cash or other forms of legal tender", "dental paraphenalia",
               "generic brand acetaminophen", "glow sticks", "broken glow stick",
               "creepy religious comics chick tracts", "healthy fruit",
               "hugs actual physical hugs", "kale smoothie", "lapel pins",
               "pencils", "swedish fish", "peterson brand sidewalk chalk",
               "vicodin", "white bread", "whole wheat anything",
               "bonkers the board game", "chardonnay",
               "person of interest season 3 dvd box set not including disc 4 with hilarious outtakes",
               "abstained from m ming", 
               "real housewives of orange county season 9 blue ray", "green party m ms",
               "independent m ms", "third party m ms", "red m ms", "blue m ms")

candy_joined <- candy_joined %>%
  filter(!candy_type %in% not_candy)

## 5.2 / Standardise Candy Names

### In some instances candy_type data had been supplied with slight spelling
### discreprencies. Personal judgement and research have been used to standardise
### spelling where possible.

candy_joined <- candy_joined %>%
  mutate(candy_type = str_replace_all(candy_type, "a friend to diabetes", ""),
         candy_type = str_replace_all(candy_type, "the candy", ""),
         candy_type = str_replace_all(candy_type, "x100", "100"),
         candy_type = str_replace_all(candy_type, "boxo raisins", "box o raisins"))

candy_joined <- candy_joined %>% 
  mutate(candy_type = case_when(
    str_detect(candy_type, "anonymous brown globs") ~ "mary janes",
    TRUE ~ candy_type ))

# 6. Tidy `age` Data --------------------------------------------------------------

## 6.1 / Set any values that contain non-digit characters to NA.

non_digit_pattern <- "[\\D]+"

candy_joined <- candy_joined %>%
  mutate(age = str_replace_all(age, non_digit_pattern, NA_character_))

## 6.2 / Set class to numeric
candy_joined <- candy_joined %>%
  mutate(age = as.numeric(age))

## 6.13 /Set any value greater than 116 to NA.

### 116 has been selected as according to personal research this is the current 
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
  mutate(country = case_when(
    str_detect(country, "alabama|alaska|arizona|arkansas|california|colorado|
               connecticut|delaware|district of columbia|florida|georgia|hawaii|
               idaho|illinois|indiana|iowa|kansas|kentucky|louisiana|maine|maryland|
               massachusetts|michigan|minnesota|mississippi|missouri|montana|
               nebraska|nevada|new hampshire|new jersey|new mexico|new york|
               north carolina|north dakota|ohio|oklahoma|oregon|pennsylvania|
               rhode island|south carolina|south dakota|tennessee|texas|utah|
               vermont|virginia|washington|west virginia|wisconsin|wyoming") 
    ~ "united states of america",
    TRUE ~ country
  ))

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

candy_joined <- candy_joined %>%
  mutate(country = case_when(
    str_detect(country, "afghanistan|albania|algeria|andorra|angola|antigua and barbuda
               |argentina|armenia|australia|austria|azerbaijan|bahamas|bahrain
               |bangladesh|barbados|belarus|belgium|belize|benin|bhutan
               |bolivia|bosnia and herzegovina
               |botswana|brazil|brunei darussalam|bulgaria|burkina faso
               |burundi|cabo verde|cambodia|cameroon|canada|central african republic
               |chad|chile|china|colombia|comoros|congo|congo
               |costa rica|coe d'ivoire|croatia|cuba|cyprus|czechia|denmark|djibouti
               |dominica|dominican republic|ecuador|egypt|el salvador|equatorial guinea
               |eritrea|estonia|eswatini|ethiopia|fiji|finland|france|gabon|gambia
               |georgia|germany|ghana|greece|grenada|guatemala|guinea|guinea-bissau
               |guyana|haiti|honduras|hungary|iceland|india|indonesia
               |iran|iraq|ireland|israel|italy|jamaica
               |japan|jordan|kazakhstan|kenya|kiribati
               |democratic people's republic of korea|republic of korea
               |kuwait|kyrgyzstan|lao people's democratic republic|latvia|lebanon
               |lesotho|liberia|libya|liechtenstein|lithuania|luxembourg
               |madagascar|malawi|malaysia|maldives|mali|malta|marshall islands
               |mauritania|mauritius|mexico|micronesia
               |moldova|monaco|mongolia|montenegro|morocco|mozambique
               |myanmar|namibia|nauru|nepal|netherlands|new zealand|nicaragua
               |niger|nigeria|north macedonia|norway|oman|pakistan|palau|panama
               |papua new guinea|paraguay|peru|philippines|poland|portugal|qatar
               |romania|russian federation|rwanda|saint kitts and nevis
               |saint lucia|saint vincent and the grenadines|samoa|san marino
               |sao tome and principe|saudi arabia|senegal|serbia|seychelles
               |sierra leone|singapore|slovakia|slovenia|solomon islands|somalia
               |south africa|south sudan|spain|sri lanka|sudan|suriname|sweden
               |switzerland|syrian arab republic|tajikistan|tanzania
               |thailand|timor-leste|togo|tonga|trinidad and tobago|tunisia|turkiye
               |turkmenistan|tuvalu|uganda|ukraine|united arab emirates
               |united kingdom|united states of america|uruguay|uzbekistan|vanuatu
               |venezuela|vietnam|yemen|zambia|zimbabwe",
               negate = TRUE) ~ NA,
    TRUE ~ country))

## 7.7 / Set All To Upper case

candy_joined <- candy_joined %>% 
  mutate(country = str_to_title(country))

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
rm(candy_joined)



