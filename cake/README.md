# Cake Analysis

------------------------------------------------------------------------

## Introduction

The data used describes the ingredients used in the making of a variety of cakes, the necessary quantity of each ingredient and the measure used for the ingredient.

------------------------------------------------------------------------

## Aim

The aim of this project is to wrangle and clean the data to enable analysis to answer set questions.

------------------------------------------------------------------------

## Code Summary

### Cleaning Script: `cleaning.R`

The cleaning script is launched from the analysis notebook and performs
a range of functions, including:

-   Loads required R packages and reads in the raw data.
-   Pivot data to long format.
-   Joins the datasets based on `ingredient_code` and `code` in order
    to be able to use the full ingredient names as opposed to the
    abbreviations.
-   Cleaning the data contained within each variable - `cake`,
    `ingredient`, `measure` and `quantity`. This required a range of
    processes including, but not limited to, cleaning variable names,
    managing NA values and separating `ingredient` and
    `measurement` data that had been combined into one column.
-   Finally, writing the cleaned data to a .csv titled
    `cake_ingredients_clean.csv`.

### Analysis Notebook: `analysis.R`

The analysis notebook contains a combination of plain text and R code
chunks and provides a basic analysis on the data by answering set
questions. This is achieved using a range of `tidyverse` functions
including, but not limited to, `filter`, `group_by`, `summarise` and
`slice_max`.

------------------------------------------------------------------------

### Assumptions / Judgements:

Throughout the cleaning process a range of assumptions and personal
judgements were made. These were made within the context of my own
proficiency and knowledge at the time and also took into consideration
the timeframe for completing the project.

Details of the assumptions and personal judgments made have been
included below next to the section of the cleaning script that they
apply to.

-   **2.1/2.2 Separate `ingredient` Value `sour cream cup`:**\
    Here it was assumed that `cup` was intended to be in the `measure`
    value as opposed to the `ingredient` value. This was therefor
    seperate to a `ingredient` value of `sour cream` and a `measure`
    value of `cup`.

-   **3.1 Replace `quantity` NAs with 0:**\
    Where the `quantity` value was NA it has been assumed that this
    meant none of this ingredient was required for the cake as opposed
    to `quantity` data being missing. NA `quantity` values were therefor
    updated to 0.

-   **4 Replace `one` values in `measure` with `one whole`**\
    Where the `measure` value was `one` this was replaced with
    `one whole` to help avoid any confusion between the `measure` and
    `quantity` values.

------------------------------------------------------------------------

## Areas for Future Development and Improvement

As noted, the project was completed within the context of my own
proficiency and knowledge at the time and also took into consideration
the timeframe for completing the project. As a result, there are areas
for future development that I hope to revisit. Aside from general
efficiency improvements, I've include a list of some of the areas where
I believe improvements could be made in future revisions.

-   **Management of `quantity` NAs:**/ The current cleaning script
    manages `quantity` NAs on the basis that only `sour cream` had NA
    `quantity` values as `cup` appeared to have been concatenated within
    the `ingredient` value. This could be improved to handle NA
    `quantity` values across a range of `ingredients`.

-   **Standardisation of `measure` values:**/ The current measure values
    appear as though they may be a mix of measures used more frequently
    in the UK and in the US. For example, a number of ingredients are
    measured in `pounds` and `ounces` whereas `strawberries` are
    measured in `quart`s. As `strawberries` are the only ingredient to
    use this measure, it's not believed this impacts the current
    analysis; however, future analysis may benefit from these being
    standardised. This would require additional timeframe to research
    whether or not this was appropriate and how to calculate the
    equivalents however.

-   **Understandability vs. DRY:**/ The project code and accompanying
    comments have been written in a way which seeks to be understandable
    whilst avoiding unnecessary repetition. It's acknowledged that by
    combining some of the steps taken, a better balance could be
    reached - however, in this case this would have required additonal
    timeframe. Where unsure, priority has been given to ensuring that
    the code can be revisted and understood, even if some steps could
    have been combined, with the aim of revisiting the project to review
    the potential improvements detailed.

------------------------------------------------------------------------

## Results

| Q#  | Question                                                         | Answer                                                                                   |
|-----------------|-----------------------|---------------------------------|
| 1   | Which cake has the most cocoa in it?                             | One Bowl Chocolate, which uses 10x Tablespoons.                                          |
| 2   | For sponge cake, how many cups of ingredients are used in total? | 3.5                                                                                      |
| 3   | How many ingredients are measured in teaspoons?                  | 8                                                                                        |
| 4   | Which cake has the most unique ingredients?                      | Babas Au Rhum & One Bowl Chocolate both use 11 ingredients.                              |
| 5   | Which ingredients are used only once?                            | Bananas, Cream Of Tartar, Crushed Ice, Dried Currants, Egg White, Nutmeg, Nuts, Zwiebach |

------------------------------------------------------------------------

## Conclusions

-   Based on the completed analysis it's difficult to draw to many
    conclusions that would supplement the information already provided
    in the results above. However, I have included some ideas for future
    analysis below.

Ideas for Future Analysis:

-   The most frequently used ingredients across all cakes.
-   Addition of either current or avg. `price` information for
    ingredients, to enable analysis around most expensive or least
    expensive cakes based on ingredient costs.

------------------------------------------------------------------------

## Languages/Tools Used

#### Languages

-   R
-   R Markdown

#### R Packages

-   tidyverse
-   janitor
-   here

#### Software/Technologies

-   RStudio
-   Git
-   Github

------------------------------------------------------------------------
