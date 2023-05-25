# 1. Read In Data / Pivot / Join / Clean Variable Names / Drop Columns

### 1.1 Load Libraries

library(tidyverse)
library(janitor)
library(here)

### 1.2 Read In Data

cake_ingredients <- read_csv(here("data/raw_data/cake-ingredients-1961.csv"))
ingredient_codes <- read_csv(here("data/raw_data/cake_ingredient_code.csv"))

### 1.3 Pivot To Long Format

cake_ingredients_clean <- cake_ingredients %>% 
  pivot_longer(cols = AE:ZH, 
             names_to = "ingredient_code", 
             values_to = "quantity") 

### 1.4 Join Data

cake_ingredients_clean <- left_join(cake_ingredients_clean, ingredient_codes, 
                                    by = c("ingredient_code" = "code"))

### 1.5 Clean variable names 

cake_ingredients_clean <- clean_names(cake_ingredients_clean)

### 1.6 Drop `ingredient_code` Variable / Re-Order Variables

cake_ingredients_clean <- cake_ingredients_clean %>% 
  select(cake, ingredient, measure, quantity)

# 2. Clean `ingredient` Data

### 2.1 Separate 'ingredient' value `sour cream cup`

cake_ingredients_clean <- cake_ingredients_clean %>% 
  mutate(ingredient = recode(ingredient, "Sour cream cup" = "Sour Cream"))

### 2.2 Update NA `measure values` to `cup` where the ingredient is `Sour Cream`

### Only `Sour Cream` had a measure of NA, so the below was sufficient - however,
### improvements would be required to manage additional data sets accurately.

cake_ingredients_clean <- cake_ingredients_clean %>%
  mutate(measure = coalesce(measure, "Cup"))
                        
# 3. Tidy `quantity` data.

### 3.1 Replace NAs with 0

cake_ingredients_clean <- cake_ingredients_clean %>%
  mutate(quantity = coalesce(quantity, 0))

# 4. Tidy `measurement` data.

### Replace `one` with `one whole` to avoid ambiguity.

cake_ingredients_clean <- cake_ingredients_clean %>%
  mutate(measure = recode(measure,
                         "one" = "One Whole"))

# 5. Update casing to title across all variables.

cake_ingredients_clean <- cake_ingredients_clean %>%
  mutate(cake = str_to_title(cake),
         ingredient = str_to_title(ingredient),
         measure = str_to_title(measure))

# 6. Write to .csv

write_csv(cake_ingredients_clean, here("data/clean_data/cake_ingredients_clean.csv"))

# 7. Remove redundant objects from environment

rm(ingredient_codes)
rm(cake_ingredients)
rm(cake_ingredients_clean)


