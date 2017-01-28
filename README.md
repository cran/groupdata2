
<!-- README.md is generated from README.Rmd. Please edit that file -->
groupdata2
==========

R package: Subsetting Methods for Balanced Cross-Validation, Time Series Windowing, and General Grouping and Splitting of Data.

By Ludvig R. Olsen
Cognitive Science, Aarhus University
Started in Oct. 2016

Contact at: <r-pkgs@ludvigolsen.dk>

Main functions:

-   group\_factor
-   group
-   splt
-   fold

Other tools:

-   %staircase%

Installation
------------

Development version:

install.packages("devtools")

devtools::install\_github("LudvigOlsen/groupdata2")

To do
-----

-   fold() - implement force\_equal (n.b. should be special for greedy and staircasing)
-   datatables
-   Change version number
-   Send to CRAN

Functions
---------

### group\_factor()

Returns a factor with group numbers, e.g. (1,1,1,2,2,2,3,3,3).

This can be used to subset, aggregate, group\_by, etc.

Create equally sized groups by setting force\_equal = TRUE

Randomize grouping factor by setting randomize = TRUE

### group()

Returns the given data as a dataframe with added grouping factor made with group\_factor(). The dataframe is grouped by the grouping factor for easy use with dplyr pipelines.

### splt()

Creates the specified groups with group\_factor() and splits the given data by the grouping factor with base::split. Returns the splits in a list.

### fold()

Creates (optionally) balanced folds for use in cross-validation. Balance folds on one categorical variable and/or make sure that all datapoints sharing an ID is in the same fold.

Methods
-------

There are currently 6 methods available. They can be divided into 3 categories.

Examples of group sizes are based on a vector with 57 elements.

### Specify group size

##### Method: greedy

Divides up the data greedily given a specified group size.

E.g. group sizes: 10, 10, 10, 10, 10, 7

### Specify number of groups

##### Method: n\_dist (Default)

Divides the data into a specified number of groups and distributes excess data points across groups.

E.g. group sizes: 11, 11, 12, 11, 12

##### Method: n\_fill

Divides the data into a specified number of groups and fills up groups with excess data points from the beginning.

E.g. group sizes: 12, 12, 11, 11, 11

##### Method: n\_last

Divides the data into a specified number of groups. The algorithm finds the most equal group sizes possible, using all data points. Only the last group is able to differ in size.

E.g. group sizes: 11, 11, 11, 11, 13

##### Method: n\_rand

Divides the data into a specified number of groups. Excess data points are placed randomly in groups (only 1 per group).

E.g. group sizes: 12, 11, 11, 11, 12

### Specify step size

##### Method: staircase

Uses step\_size to divide up the data. Group size increases with 1 step for every group, until there is no more data.

E.g. group sizes: 5, 10, 15, 20, 7

Examples
--------

``` r
# Attach packages
library(groupdata2)
library(dplyr)
library(knitr)
```

``` r
# Create dataframe
df <- data.frame("x"=c(1:12),
  "species" = rep(c('cat','pig', 'human'), 4),
  "age" = sample(c(1:100), 12))
```

### group()

``` r
# Using group()
group(df, 5, method = 'n_dist') %>%
  kable()
```

|    x| species |  age| .groups |
|----:|:--------|----:|:--------|
|    1| cat     |   44| 1       |
|    2| pig     |   55| 1       |
|    3| human   |   13| 2       |
|    4| cat     |    1| 2       |
|    5| pig     |   28| 3       |
|    6| human   |   20| 3       |
|    7| cat     |   56| 3       |
|    8| pig     |   99| 4       |
|    9| human   |   14| 4       |
|   10| cat     |   47| 5       |
|   11| pig     |   86| 5       |
|   12| human   |   82| 5       |

``` r

# Using group() with dplyr pipeline to get mean age
df %>%
  group(5, method = 'n_dist') %>%
  dplyr::summarise(mean_age = mean(age)) %>%
  kable()
```

| .groups |  mean\_age|
|:--------|----------:|
| 1       |   49.50000|
| 2       |    7.00000|
| 3       |   34.66667|
| 4       |   56.50000|
| 5       |   71.66667|

### fold()

``` r
# Create dataframe
df <- data.frame(
  "participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
  "age" = rep(c(20,23,27,21,32,31), 3),
  "diagnosis" = rep(c('a', 'b', 'a', 'b', 'b', 'a'), 3),
  "score" = c(10,24,15,35,24,14,24,40,30,50,54,25,45,67,40,78,62,30))
df <- df[order(df$participant),]
df$session <- rep(c('1','2', '3'), 6)
```

``` r
# Using fold() 

# First set seed to ensure reproducibility
set.seed(1)

# Use fold() with cat_col and id_col
df_folded <- fold(df, 3, cat_col = 'diagnosis',
                  id_col = 'participant', method = 'n_dist')

# Show df_folded ordered by folds
df_folded[order(df_folded$.folds),] %>%
  kable()
```

| participant |  age| diagnosis |  score| session | .folds |
|:------------|----:|:----------|------:|:--------|:-------|
| 1           |   20| a         |     10| 1       | 1      |
| 1           |   20| a         |     24| 2       | 1      |
| 1           |   20| a         |     45| 3       | 1      |
| 4           |   21| b         |     35| 1       | 1      |
| 4           |   21| b         |     50| 2       | 1      |
| 4           |   21| b         |     78| 3       | 1      |
| 6           |   31| a         |     14| 1       | 2      |
| 6           |   31| a         |     25| 2       | 2      |
| 6           |   31| a         |     30| 3       | 2      |
| 5           |   32| b         |     24| 1       | 2      |
| 5           |   32| b         |     54| 2       | 2      |
| 5           |   32| b         |     62| 3       | 2      |
| 3           |   27| a         |     15| 1       | 3      |
| 3           |   27| a         |     30| 2       | 3      |
| 3           |   27| a         |     40| 3       | 3      |
| 2           |   23| b         |     24| 1       | 3      |
| 2           |   23| b         |     40| 2       | 3      |
| 2           |   23| b         |     67| 3       | 3      |

``` r

# Show distribution of diagnoses and participants
df_folded %>% 
  group_by(.folds) %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| .folds | diagnosis | participant |    n|
|:-------|:----------|:------------|----:|
| 1      | a         | 1           |    3|
| 1      | b         | 4           |    3|
| 2      | a         | 6           |    3|
| 2      | b         | 5           |    3|
| 3      | a         | 3           |    3|
| 3      | b         | 2           |    3|

**Notice** that the we now have the opportunity to include the *session* variable and/or use *participant* as a random effect in our model when doing cross-validation, as any participant will only appear in one fold.

We also have a balance in the representation of each diagnosis, which could give us better, more consistent results.
