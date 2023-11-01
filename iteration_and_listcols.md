Iteration and listcols
================
Lu Qiu
2023-10-31

``` r
library(tidyverse)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

### Lists

``` r
vec_number = 1:4
vec_char = c('my', 'name', 'is', 'Lu')

tibble(
  num = vec_number,
  chr = vec_char
)
```

    ## # A tibble: 4 × 2
    ##     num chr  
    ##   <int> <chr>
    ## 1     1 my   
    ## 2     2 name 
    ## 3     3 is   
    ## 4     4 Lu

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.176833 -0.615792 -0.050031 -0.001499  0.672421  3.751145

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[2]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
l[['summary']]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.176833 -0.615792 -0.050031 -0.001499  0.672421  3.751145

### Loops

``` r
list_norms_samples = 
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(20, 0, 5),
    c = rnorm(20, 10, .2),
    d = rnorm(20, -3, 1)
  )

is.list(list_norms_samples)
```

    ## [1] TRUE

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("You need at least 2 numbers to get z scores")
  }

  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

``` r
mean_and_sd(list_norms_samples$a)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.79  1.16

``` r
mean_and_sd(list_norms_samples$b)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.74  4.04

``` r
mean_and_sd(list_norms_samples$c)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.95 0.190

``` r
mean_and_sd(list_norms_samples$d)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.15 0.814

``` r
output = vector('list', length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms_samples[[i]])
}
```

### use `map`

``` r
output_mean_sd = map(list_norms_samples, mean_and_sd)
output_median = map(list_norms_samples, median)
output_summary = map(list_norms_samples, summary)
```

### create DF

``` r
listcol_df =
  tibble(
    name = c('a', 'b', 'c', 'd'),
    samp = list_norms_samples
  )
```

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.79  1.16

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.74  4.04

``` r
mean_and_sd(listcol_df$samp[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.95 0.190

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.79  1.16
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.74  4.04
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.95 0.190
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.15 0.814

``` r
listcol_df |>
  mutate(
    mean_sd = map(samp, mean_and_sd),
    median = map(samp, median)) |>
  select(name, mean_sd) |>
  unnest(mean_sd)
```

    ## # A tibble: 4 × 3
    ##   name   mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a      2.79 1.16 
    ## 2 b      2.74 4.04 
    ## 3 c      9.95 0.190
    ## 4 d     -3.15 0.814

### NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

import function

``` r
nsduh_import <- function(html, table_num, outcome_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      outcome = outcome_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
}

nsduh_import(nsduh_html, 1, 'marj')
```

import data using a for loop

``` r
table_input = list(1, 4, 5)
name_input = list('marj', 'cocaine', 'heroine')

output = vector('list', length = 3)

for (i in 1:3) {
  output[[i]] = nsduh_import(nsduh_html, table_input[[i]], name_input[[i]])
  
}

nsduh_df = bind_rows(output)
```

Try again, using maps!

``` r
nsduh_import <- function(html, table_num) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
}

nsduh_df = 
  tibble(
    name = c("marj", "cocaine", "heroin"),
    number = c(1, 4, 5)
  ) |>
  mutate(
    table = map(number, nsduh_import, html = nsduh_html)) |>
  unnest(table)
```

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/luqiu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-10-31 11:07:27.571927 (8.538)

    ## file min/max dates: 1869-01-01 / 2023-10-31

    ## using cached file: /Users/luqiu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2023-10-31 11:07:33.378944 (3.843)

    ## file min/max dates: 1949-10-01 / 2023-10-31

    ## using cached file: /Users/luqiu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-10-31 11:07:35.328321 (0.997)

    ## file min/max dates: 1999-09-01 / 2023-10-31

``` r
weather_nest_df = 
  weather_df|>
  nest(df = date:tmin)
```

Can i regress ‘tmax’ on `tmin` for each of these…?

``` r
central_park_df = 
  weather_nest_df |>
  pull(df) |>
  nth(1)
```

fit a linear regression for central park

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}

weather_lm(central_park_df)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

let’s try a for loop

``` r
input_list = weather_nest_df |> pull(df) 
output = vector("list", length = 3)

for (i in 1:3) {
  output[[i]] = weather_lm(input_list[[i]])
}


weather_nest_df |> 
  mutate(models = map(df, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          df                 models
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>  
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
