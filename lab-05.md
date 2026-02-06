Lab 05 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Thomas Huang
2026-02-06

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
data(dennys, package = "dsbox")
data(laquinta, package = "dsbox")
```

### Exercise 1

There are three Denny’s and two La Quinta in Alaska.

``` r
dn_ak <- dennys %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

``` r
lq_ak <- laquinta %>%
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

### Exercise 2

There are three Denny’s and two La Quinta in Alaska. Each Denny’s
corresponds to two La Quinta. Therefore, there should be $$3\times2=6$$
pairs.

### Exercise 3

…

### Exercise 4

…

### Exercise 5

…

### Exercise 6

…

Add exercise headings as needed.
