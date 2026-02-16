Lab 05 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Saima Arina
02/15/2026

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

``` r
dn_ak <- dennys %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

There are 3 Denny’s locations in Alaska.

``` r
lq_ak <- laquinta %>%
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

There are 3 La Quinta locations in Alaska.

### Exercise 2

``` r
nrow(dn_ak) * nrow(lq_ak)
```

    ## [1] 6

There are 6 pairings we need to calculate.

### Exercise 3

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak,
  by = "state"
)
```

    ## Warning in full_join(dn_ak, lq_ak, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… "\nAn… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… "\nAn… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… "\nAn… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… "\nFa… 99709
    ## # ℹ 2 more variables: longitude.y <dbl>, latitude.y <dbl>

### Exercise 4

``` r
nrow(dn_lq_ak)
```

    ## [1] 6

``` r
ncol(dn_lq_ak)
```

    ## [1] 11

There are 6 observations in the joined data frame. The names of the
variables in this data frame is: address, city, state, zip, longitude,
and latitude.

### Exercise 5

We use the function “mutate” from the tidyverse to add a new variable to
a data frame while keeping the existing variables.

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 <- long1 * pi / 180
  lat1 <- lat1 * pi / 180
  long2 <- long2 * pi / 180
  lat2 <- lat2 * pi / 180

  R <- 6371 # Earth mean radius in km

  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2)^2
  d <- R * 2 * asin(sqrt(a))

  return(round(d, round)) # distance in km
}
```

### Exercise 6

``` r
dn_lq_ak <- dn_lq_ak %>% 
  mutate(distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
```

### Exercise 7

``` r
dn_lq_ak <- dn_lq_ak %>% 
  group_by(address.x) %>% 
  mutate(min_distance = min(distance)) 
```

### Exercise 8

``` r
dn_min_ak <- dn_lq_ak %>%
  group_by(address.x) %>%       
  summarize(min_distance = min(distance))
```

``` r
ggplot(dn_min_ak, aes (x = min_distance)) +
  geom_histogram(binwidth = 2) +
  labs(
    x = "Distance(minimum) to nearest La Quinta Location (km)",
    y = "Number of Denny's Locations",
    title = "Distance to Nearest La Quinta Location for Denny's in Alaska"
  )
```

![](lab-05_files/figure-gfm/-%20distribution%20of%20distances%20in%20AK-1.png)<!-- -->

``` r
dn_min_ak %>%
  summarize(
    n      = n(),
    mean   = mean(min_distance),
    sd     = sd(min_distance),
    median = median(min_distance),
    min    = min(min_distance),
    max    = max(min_distance)
  )
```

    ## # A tibble: 1 × 6
    ##       n  mean    sd median   min   max
    ##   <int> <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     3  4.41  2.10   5.20  2.04  6.00

The distribution of distances from Denny’s to the nearest La Quinta in
Alaska is skewed to the left, with one Denny’s about 2 km away and the
others about 6 km away.

### Exercise 9

``` r
dn_nc <- dennys %>%      
  filter(state == "NC")
 lq_nc <- laquinta %>%
   filter(state == "NC")
 
dn_lq_nc <- full_join(dn_nc, lq_nc,
                      by = "state"
)
```

    ## Warning in full_join(dn_nc, lq_nc, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_nc <- dn_lq_nc %>% 
  mutate(distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y, round = 3))


dn_lq_nc <- dn_lq_nc %>% 
  group_by(address.x) %>% 
  mutate(min_distance = min(distance)) 

dn_min_nc <- dn_lq_nc %>%
  group_by(address.x) %>%       
  summarize(min_distance = min(distance))

ggplot(dn_min_nc, aes (x = min_distance)) +
  geom_histogram(binwidth = 10) +
  labs(
    x = "Distance(minimum) to nearest La Quinta Location (km)",
    y = "Number of Denny's Locations",
    title = "Distance to Nearest La Quinta Location for Denny's in North Carolina"
  )
```

![](lab-05_files/figure-gfm/-%20analyzing%20NC-1.png)<!-- -->

``` r
dn_min_nc %>%
  summarize(
    n      = n(),
    mean   = mean(min_distance),
    sd     = sd(min_distance),
    median = median(min_distance),
    min    = min(min_distance),
    max    = max(min_distance)
  )
```

    ## # A tibble: 1 × 6
    ##       n  mean    sd median   min   max
    ##   <int> <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    28  65.4  53.4   53.5  1.78  188.

The distribution of distances from Denny’s to the nearest La Quinta in
North Carolina is skewed to the right, with most Denny’s within 80 km
away and a few others about much farther away, over 150 km.

### Exercise 10
