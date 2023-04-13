# Candy Analysis

## April 2023

------------------------------------------------------------------------

## Introduction

The `Candy Analysis` is a task completed as part of the CodeClan Professional Data Analysis Course and as part the `Dirty Data Project`. The task is part of the `dirty_data_codeclan_project_stuartstenhouse` repository which also contains the `Cake Analysis` task.

The data used is published by "The Science Creative Quarterly" and contains the results of a questionnaire where people were asked to indicate their feelings relating to various types of candy. The questionnaire also contained questions not relating to candy, however these are excluded during data cleaning. Data was collected over three years - 2015, 2016 and 2017.

------------------------------------------------------------------------

## Aim

The aim of this project is to clean and answer set questions about the data.

------------------------------------------------------------------------

## Code Summary

### Cleaning Script: `cleaning.R`

The cleaning script is launched from the analysis notebook and performs a range of functions, including:

-   Loading the necessary R packages.
-   Reading in and selecting the required data.
-   Adding additional columns where needed - for example, the 2015 questionnaire 
had not collected `age` data. A `response_id` was also added to each response, 
made up of the year and row number, to act as a unique identifier.
-   Pivoting the data to long format before binding the rows.
-   Cleaning the data contained within each variable (incl. `country`, `age`, `candy_type`) through a range of processes including, but not limited to, filtering non-candy `candy_type` values, allowing only numeric `age` values up-to a maximum age and wrangling the `country` data to retain as much valid `country` data as possible.
-   Finally, writing the cleaned data to a .csv titled `candy_clean.csv`.

### Analysis Notebook: `analysis.R`

The analysis notebook contains a combination of plain text and R code chunks and provides an analysis on the data by answering set questions. This is achieved using a range of `tidyverse` functions along with use of two unit tested functions written for the analysis, `most_popular_candy_by_year()` and `most_popular_candy_by_year_minus_any_full_size()`

------------------------------------------------------------------------

### Assumptions / Personal Judgements:

The process of cleaning the data required that a number of assumptions and personal judgements be made. These were made within the context of my own proficiency and knowledge at the time and also took into consideration the timeframe for completing the project.

Details of the assumptions and personal judgments made have been included below next to the section of the cleaning script that they apply to.

-   **3. / Add Missing Columns:**\
    Where `age`, `country`, `gender` and/or `year` data was not collected as part of the questionnaire, this has been added as `NA`.

-   **5.2. / Standardise Candy Names:** Some `candy_type` data looked to have been entered with slight spelling discrepancies. Personal judgement and online research were used to standardise spelling where possible for more accurate analysis. "a friend to diabetes" text was removed from "sweetums a friend to diabetes" to create one distinct "sweetums" `candy_type` & "x" removed from "x100 grand bar" to create one distinct "100 grand bar" `candy_type`. Data also suggested that entries mentioning "anonymous brown globs that come in black and orange wrappers" were "mary janes" and so any `candy_type` referencing this text was updated to "mary janes". Furthermore, it was decided to leave all three "licorice" types ("licorice yes black", "licorice not black" and "licorice") to maintain specific preferences.

-   **5.3 / Remove Non-Candy Items:**\
    Based on online research using the websites of popular American Candy stores a list of `candy_type` data that did not appear to be candy was created. In addition, details from the data source were taken into consideration, resulting in 6x M&M entries being removed as these appear to have been added as a *'hidden proxy question'* relating to voting intentions as opposed to being candy types. For the avoidance of doubt, personal judgment was also used at this stage and a full list of what was considered not to be candy can be within
the cleaning step. Where this was the case, these observations were filtered from the data.

-   **6.1, 6.2, 6.3, 6.4 / Clean Age Values**\
    A wide range of text was entered as `age` data by respondents as this had been free text entry. All values which contained a character that was not numeric or a decimal point was set to NA. The variable type was then updated from `character` to `numeric` and any values with a value other than 0 after the decimal point deemed to be an invalid age and updated to NA. Finally, any value greater than 116 was set to NA with 116 being selected as the maximum as according to online research this is the current age of the oldest known living human. These steps resulted in the total number of responses with age values reducing from 9032 to 8867.
    
-   **7.4 / Standardise Version of USA Used:**\
    When reviewing the country data 42 variations of what it's believed were intended to mean "United States of America" were found, totaling 3077 responses. 7 of these variations were used in over 10 responses and in total these made up 3020 responses or 98% of responses where it's believed the respondents country was "United States of America". With this in mind, these 7 variations were chosen to be re-coded and the others left to be updated to NA in step 7.9 when they fail to match against the country list.

    This process also flagged to country values which contained 2 country names "not the usa or canada" and "i pretend to be from canada but i am really from the united states". Because of problems encountered in later cleaning stages which I believe these may have been contributing to and because these were only associated to 1 response each, I decided to update these to blank to be updated to NA when not matched on the country list.

-   **7.5 / States In the USA Entered As Country:**\
    When reviewing the `country` data at this stage I noticed that some states in the USA looked to have provided as the country. To decide an appropriate approach to these, I looked at how many times this had occurred. I found that 5 US states had been entered as the `country` ('alaska', 'new jersey', 'new york', 'north carolina' and 'california') and that each had been used only once. With this in mind, I decided that due to only being related to 5 responses, leaving these to update to NA when they failed to match against the country list would not adversely impact the analysis. The code used for checking for US State data has been left for reference in the cleaning script, but has been commented out as it is not required for cleaning.

-   **7.6 / Update UK Countries:**\
    As one of the analysis questions asks about UK values, the decision was made to group England, Scotland, Wales and Northern Ireland entries under UK. A number of respondents had also entered a version of the UK for their `country` and this also informed my decision. When reviewing the data, excluding those already specified as "United Kingdom", 4 entries that would fall in the category of the United Kingdom were identified ("united kindom", "scotland", "endland", "england" and "uk") which were linked to 1, 5, 1, 5 and 30 responses in turn. I therefor decided to recode values used 5 or more times, which would result in only 2 responses having their `country` value updated to NA when they failed to match the country list.

-   **7.7 / Country Language Variations:**\
    When reviewing the data I noticed that some `country` names was not in English and I intended to match against an English language country list. These were "españa" and "brasil". Both were linked to only one response and in-line with other steps when cleaning the country data, I decided that leaving this to update to NA when they did not match to the country list would not adversely impact the analysis. However, to demonstrate how these could be recoded, the code for this has been included in the cleaning script and commented out. The code used for checking the usages of each variation has also been included but commented out as it is not required for cleaning.

-   **7.8 / Prefix Variations:**\
    In two cases I encountered problems with the prefix "the" creating separate `country` values for both "the netherlands" and "the united states of america". Because of problems encountered in later cleaning stages which I believe these may have been contributing to, these were both recoded to remove the prefix.

-   **7.9 / Remove Non Country Values:**\
    For this stage, I sourced a full country list from "<https://github.com/stefangabos/world_countries/>" to compare my country data against. Considering project time constraints and my own current proficiency level, for the purposes of this project any `country` data which does not match will be updated to NA. It is recognised that requiring an exact match may lead to the loss of some data, in particular where the list requires `country` values to be formatted in a particular way. For example in cases where some of the country name is in parenthesis. There are 5 countries in the list which include parethesis in the name and in our data 2 values of "korea" were the only ones to feature. Due to being uncertain if this related to "korea (democratic people's republic of)" or "korea, republic of", and as only 2 responses were effected, this was left to update to NA. The list was however updated to reflect the preferred formatting of "united kingdom" where set earlier in the cleaning process.

    *NB: It's recognised that the code used in step 7.9 does not follow expected style guidelines due to it's length. Unfortunately I encountered some bugs when spreading this over multiple lines and was unable to correct this in time prior to submission.*

-   **8 / Clean `gender` Data:**\
    The original data contained 4 gender values 'Male', 'Gender', 'Other', 'I'd rather not say' and blank values which have been stored as NA. As each of these values represents a unique way of answering (or not answering) and I could not think of any benefits that would result from changes, no changes to the gender data have been made.

-   **9 / Clean `going_out_trick_or_treating` Data:**\
    The original data contained a character string of the values "Yes", "No" or blank if unanswered which has been stored as NA. As their are only 2 valid completed responses ("Yes" and "No") this data has been updated to the logical type, with TRUE indicating "Yes" and FALSE indicating "No". For the avoidance of doubt, NA values were left as NA.

------------------------------------------------------------------------

## Areas for Future Development and Improvement

As noted, the project was completed within the context of my own proficiency and knowledge at the time and also took into consideration the timeframe for completing the project. As a result, there are areas for future development that I hope to revisit. Aside from general efficiency improvements, I've include a list of some of the areas where I believe improvements could be made in future revisions.

-   **Wrangling and Cleaning of `country` and `candy_type` Data:**\ Although it's hoped that the current wrangling and cleaning is sufficient for the current data and analysis questions, if additional data sets were added with additional `country` and `candy_type` data then updates may be needed to support accurate analysis. Included within this would be bug solving to enable all code to conform to style guidelines.
-   **Year & Timestamp:**\ Currently year values are hard coded; however, where a timestamp is available I'd like to extract these from it. I did have code in place for this but there were unexpected knock on effects that I ran out of time to review for the initial version.
-   **Wrangling and Cleaning of `age` Data:**\ The cleaning process used for `age` values did not allow for any value that contained character data. This means that some usable data may have been lost (eg. sixty-nine). Improvements could be made to allow for such values.
-   **Understandability vs. DRY:**\ The project code and accompanying comments have been written in a way which seeks to be understandable whilst avoiding unnecessary repetition. It's acknowledged that by combining and potentially re-ordering some of the steps taken, a better balance could be reached. However, this would require increased proficiency and additional timeframe. Where unsure, priority has been given to ensuring that the code can be revisited and understood to support the implementation of detailed improvements.

------------------------------------------------------------------------

## Results

| Q#  | Question                                                                                                                                               | Answer                   |
|-----------------|--------------------------------------|-----------------|
| 1   | What is the total number of candy ratings given across the three years. Number of candy ratings, not the number of raters. Don't count missing values) | 586392                   |
| 2   | What was the average age of people who are going out trick or treating?                                                                                | 35                       |
| 3   | What was the average age of people who are not going trick or treating?                                                                                | 39                       |
| 4   | For each of joy, despair and meh, which candy bar received the most of these ratings?                                                                  |                          |
|     | JOY                                                                                                                                                    | any full sized candy bar |
|     | MEH                                                                                                                                                    | lollipops                |
|     | DESPAIR                                                                                                                                                | mary janes               |
| 5   | How many people rated Starburst as despair?                                                                                                            | 1990                     |

For the next three questions, the following scoring system was used:

JOY = +1 MEH = 0 DESPAIR = -1

| Q#  | Question                                                                                                  | Answer                       |
|-----------------|---------------------------------------|-----------------|
| 6   | What was the most popular candy bar by this rating system for each gender in the dataset?                 |                              |
|     | Male                                                                                                      | any full sized candy bar     |
|     | Female                                                                                                    | any full sized candy bar     |
|     | Other                                                                                                     | any full sized candy bar     |
|     | I'd rather not say                                                                                        | any full sized candy bar     |
|     | NA                                                                                                        | any full sized candy bar     |
| 7   | What was the most popular candy bar in each year?                                                         |                              |
|     | 2015                                                                                                      | any full sized candy bar     |
|     | 2016                                                                                                      | any full sized candy bar     |
|     | 2017                                                                                                      | any full sized candy bar     |
| 8   | What was the most popular candy bar by this rating for people in US, Canada, UK, and all other countries? |                              |
|     | US                                                                                                        | any full sized candy bar     |
|     | Canada                                                                                                    | any full sized candy bar     |
|     | UK                                                                                                        | tolberone something or other |
|     | All other countries (excluding US, Canada, and UK)                                                        | any full sized candy bar     |

------------------------------------------------------------------------

## Conclusions

In the majority of questions related to most popular candy, in relation to either the year or country, the `candy_type` of `any full sized candy bar` was returned as the result. This appears to be in-line with the 'main figure' on the data publishers page where `any full sized candy bar` is listed as the most popular in 2017. For additional analysis, I created a function which returned the most popular `candy_type` per year when `any full sized candy bar` was excluded. Again, the results appear to be in-line with the 'main figure' on the data publishers page where `reese s peanut butter cups` and `kit kat` both score near the top, with `reese s peanut butter cups` indeed listed second most popular for 2017.

Please find my results these additional questions below

| Question                                                                                  | Answer                     |
|-----------------------------------------------------|-------------------|
| What was the most popular candy bar in each year? (excluding 'any full sized candy bar'). |                            |
| 2015                                                                                      | reese s peanut butter cups |
| 2016                                                                                      | kit kat                    |
| 2017                                                                                      | reese s peanut butter cups |

Other insights that can be taken from the analysis include:

-   The average age of individuals going trick or treating is younger than the average age of those not going trick or treating.

-   All gender groups identified `any full sized candy bar` as the most popular candy.

-   Respondents from the UK differed from those in the USA, Canada and combined respondents (excluding USA, Canada and USA) by returning `tolberone something or other` as the most popular candy and not `any full sized candy bar`.

Ideas for Future Analysis:

-   The most popular candy in the USA, Canada and UK when excluding `any full sized candy bar`.

-   The difference in score between the top 3 most popular `candy_type`s by year and country. Is it close or is `any full sized candy bar` far out in front?

-   The average ages of different genders who are going trick or treating, or not, and how this compares to the averages calculated across all genders.

------------------------------------------------------------------------

## Languages/Tools Used

#### Languages

-   R
-   R Markdown

#### R Packages

-   tidyverse
-   janitor
-   here
-   readxl
-   testthat

#### Software/Technologies

-   RStudio
-   Git
-   Github

------------------------------------------------------------------------
