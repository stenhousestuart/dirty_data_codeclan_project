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

## A unique `response_id` column is added for every survey response along with
## a year value.

## 2015 (Also Adds Gender and Country Columns Populated With NA)

candy_2015_clean <- candy_2015_clean %>%
  mutate(response_id = str_c("2015", row_number()),
         .before = "age") %>%
  mutate(gender = NA,
         .before = "going_out") %>%
  mutate(country = NA,
         .before = age) %>%
  mutate(year = as.integer(2015))

## 2016
candy_2016_clean <- candy_2016_clean %>%
  mutate(response_id = str_c("2016", row_number()), 
         .before = "going_out") %>%
  mutate(year = as.integer(2016))

## 2017
candy_2017_clean <- candy_2017_clean %>%
  mutate(response_id = str_c("2017", row_number()), 
         .before = "going_out") %>% 
  mutate(year = as.integer(2017))

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
         candy_type = str_replace_all(candy_type, "boxo_raisins", "box_o_raisins"))

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

# Tidy `age` --------------------------------------------------------------

non_digit_pattern <- "[\\D]+"

## Set any values that contain non-digit characters to NA.
candy_joined <- candy_joined %>%
  mutate(age = str_replace_all(age, non_digit_pattern, NA_character_))

## Set class to numeric
candy_joined <- candy_joined %>%
  mutate(age = as.numeric(age))

# Set any value greater than 116 (currently worlds oldest living human) to NA.
candy_joined <- candy_joined %>%
  mutate(age = if_else(age > 116, NA, age))

# Tidy `country` ----------------------------------------------------------

non_alpha_numeric_pattern <- "[\\W]"

# Remove Non-Alpha-Numeric Values
candy_joined <- candy_joined %>%
  mutate(country = str_replace_all(country, non_alpha_numeric_pattern, " "))

# Set all To lower case
candy_joined <- candy_joined %>% 
  mutate(country = str_to_lower(country))

# Update USA Based On Partial Sring Matches
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

# Update USA State Entries

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

# Update UK Countries Based On Partial Sring Matches
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

# Update To English Version of Country Names / Misc.
candy_joined <- candy_joined %>% 
  mutate(country = case_when(
    str_detect(country, "españa") ~ "spain",
    str_detect(country, "brasil") ~ "brazil",
    str_detect(country, "netherlands") ~ "netherlands",
    TRUE ~ country
  ))

# Remove Non Country Values

candy_joined <- candy_joined %>%
  mutate(country = case_when(
    str_detect(country, "afghanistan|albania|algeria|andorra|angola|antigua & deps
               |argentina|armenia|australia|austria|azerbaijan|bahamas|bahrain
               |bangladesh|barbados|belarus|belgium|belize|benin|bhutan|bolivia
               |bosnia herzegovina|botswana|brazil|brunei|bulgaria|burkina|burundi
               |cambodia|cameroon|canada|cape verde|central african rep|chad|chile
               |china|colombia|comoros|congo|congo|costa rica|croatia|cuba|cyprus
               |czech republic|denmark|djibouti|dominica|dominican republic
               |east timor|ecuador|egypt|el salvador|equatorial guinea|eritrea
               |estonia|ethiopia|fiji|finland|france|gabon|gambia|georgia|germany
               |ghana|greece|grenada|guatemala|guinea|guinea-bissau|guyana|haiti
               |honduras|hungary|iceland|india|indonesia|iran|iraq|ireland|israel
               |italy|ivory coast|jamaica|japan|jordan|kazakhstan|kenya|kiribati
               |korea north|korea south|kosovo|kuwait|kyrgyzstan|laos|latvia
               |lebanon|lesotho|liberia|libya|liechtenstein|lithuania|luxembourg
               |macedonia|madagascar|malawi|malaysia|maldives|mali|malta
               |marshall islands|mauritania|mauritius|mexico|micronesia|moldova
               |monaco|mongolia|montenegro|morocco|mozambique|myanmar|namibia
               |nauru|nepal|netherlands|new zealand|nicaragua|niger|nigeria
               |norway|oman|pakistan|palau|panama|papua new guinea|paraguay|peru
               |philippines|poland|portugal|qatar|romania|russian federation
               |rwanda|st kitts & nevis|st lucia|saint vincent & the grenadines
               |samoa|san marino|sao tome & principe|saudi arabia|senegal|serbia
               |seychelles|sierra leone|singapore|slovakia|slovenia
               |solomon islands|somalia|south africa|south sudan|spain|sri lanka
               |sudan|suriname|swaziland|sweden|switzerland|syria|taiwan
               |tajikistan|tanzania|thailand|togo|tonga|trinidad & tobago|tunisia
               |turkey|turkmenistan|tuvalu|uganda|ukraine|united arab emirates
               |united kingdom|united states of america|uruguay|uzbekistan
               |vanuatu|vatican city|venezuela|vietnam|yemen|zambia|zimbabwe",
               negate = TRUE) ~ NA,
    TRUE ~ country))

# Set all To Upper case
candy_joined <- candy_joined %>% 
  mutate(country = str_to_title(country))

country_view <- candy_joined %>% distinct(country)

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

