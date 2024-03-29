Demonstration of Strata Mismatch Accuracy Functions
================
MS Patterson, <matthewpatterson@usda.gov>; Paolo Arevalo
April 11, 2019

This script can be knit in R into a github formatted markdown file for easy reading and review of outputs. This script contains an example of using the functions required for the calculation of the unbiased areas of map classes when the samples were produced using the strata of a different map:
1. Function to calculate strata for any given pair of years
1. Calculate unbiased area proportions and variance of reference classes
1. Calculate standard error of unbiased area proportion
1. Calculate unbiased areas, their confidence interval and margin of error
The functions in this script are based on Stehman S. V. 2014. *Estimating area and map accuracy for stratified random sampling when the strata are different from the map classes.* Int. J. Remote Sens. 35: 4923-4939.

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## -- Attaching packages -------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.5
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
source("strata_mismatch_accuracy.R")


# Input data from Stehman, 2014
stehman <- read_csv(file = "stehman_test_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   stratum = col_integer(),
    ##   map_class = col_character(),
    ##   ref_class = col_character()
    ## )

A little test with data from Stehman, 2014.

``` r
orig_strata <- stehman$stratum
ref_label <- stehman$ref_class
map_label <- stehman$map_class
strata_totals <- data.frame(c(1, 2, 3, 4), c(40000, 30000, 20000, 10000))
sample_totals <- data.frame(c(1, 2, 3, 4), c(10, 10, 10, 10))
rfcodes <- c('A', 'B', 'C', 'D')
propsAndVars <- calcPropsAndVars(orig_strata, ref_label, map_label, 
                                        strata_totals, sample_totals, rfcodes)
```

A little test with data from Stehman, 2014.

``` r
totarea_pix <- sum(strata_totals)
ref_var <- propsAndVars$ref_var

propSE <- calcPropSE(strata_totals, sample_totals, ref_var, 
                                             rfcodes, totarea_pix)
```

A little test with data from Stehman, 2014.

``` r
class_prop <- propsAndVars$class_prop
pixel <- 30

unArea <- calcUnbiasedArea(totarea_pix, class_prop, propSE, pixel)
```

A little test with data from Stehman, 2014.

``` r
accurates <- calcAccuracies(strata_totals, sample_totals, rfcodes, totarea_pix,
                                    propsAndVars)
accurates
```

    ## $overall_acc
    ## [1] 0.629937
    ## 
    ## $overall_acc_min
    ## [1] 0.5254417
    ## 
    ## $overall_acc_max
    ## [1] 0.7344323
    ## 
    ## $users_acc
    ##         A         B         C         D 
    ## 0.7419355 0.5744681 0.5000000 0.7000000 
    ## 
    ## $users_acc_min
    ##         A         B         C         D 
    ## 0.5026595 0.4339686 0.2891903 0.4905284 
    ## 
    ## $users_acc_max
    ##         A         B         C         D 
    ## 0.9812114 0.7149676 0.7108097 0.9094716 
    ## 
    ## $producers_acc
    ##         A         B         C         D 
    ## 0.6571429 0.7941176 0.3000000 0.6363636 
    ## 
    ## $producers_acc_min
    ##         A         B         C         D 
    ## 0.4668923 0.6127142 0.2115584 0.4339566 
    ## 
    ## $producers_acc_max
    ##         A         B         C         D 
    ## 0.8473935 0.9755210 0.3884416 0.8387706

### Summary of implementing this approach

To run this on other data, here are the data one needs:
1. **orig\_strata**, a vector with numeric codes representing the original stratification of each sample.
2. **ref\_label**, a vector with numeric codes representing the reference label for that year/map, for each sample.
3. **map\_label**, a vector with numeric codes representing the map labels, for each sample.
4. **strata\_totals**, a dataframe with two columns and number of rows equal to the total number of classes in the original strata. The first column must have the same codes found in the original stratification and the second must have the total number of PIXELS of each class in that original strata map.
5. **sample\_totals**, a dataframe with two columns and number of rows equal to the total number of classes in the original strata. The first column must have the same codes found in the original stratification, and the second must have the total number of SAMPLE UNITS of each class collected from that original strata map.
6. **rfcodes**, a vector with numeric values representing the reference codes present in ALL of the periods.
7. **pixel**, the pixel size of the maps being analyzed.
8. **totarea\_pix**, an integer with the total number of pixels present in the original stratification map (this can be calculated from other values).
