# 1. Read In Data / Pivot / Join / Clean Variable Names / Drop Columns

### 1.1 Load Libraries

library(tidyverse)
library(janitor)

### 1.2 Read In Data

cake_ingredients <- read_csv("data/raw_data/cake-ingredients-1961.csv")
ingredient_codes <- read_csv("data/raw_data/cake_ingredient_code.csv")

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

### Only `Sour Cream` had a measure of NA, so the below was sufficient. This
### could be improved howver to only update when ingredient is Sour Cream.

cake_ingredients_clean <- cake_ingredients_clean %>%
  mutate(measure = coalesce(measure, "Cup"))
                        







