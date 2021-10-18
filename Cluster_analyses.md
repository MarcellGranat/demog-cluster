initial
================

# TODO

-   térképen klaszterszám
-   alap mutatók klaszterenként
-   nuts vs time table -> klaszterszín

``` r
library(tidyverse)
library(eurostat)
library(sf)

set.seed(2021)
```

## Data setup

``` r
df_tfr <- get_eurostat("demo_r_find2", time_format = "num") %>% 
  filter(indic_de == "TOTFERRT") %>% 
  select(time, geo, tfr = values)

df_lifexp <- get_eurostat("demo_r_mlifexp", time_format = "num") %>% 
  filter(sex == "T" & age == "Y_LT1") %>% 
  select(time, geo, lifexp = values)
```

``` r
df_ageing <- get_eurostat("demo_r_d2jan", time_format = "num") %>% 
  filter(age != "TOTAL" & age != "UNK" & sex == "T") %>% 
  mutate(
    age = ifelse(age == "Y_LT1", 0, age),
    age = ifelse(age == "Y_OPEN", 100, age),
    age = str_remove_all(age, "Y"),
    age = as.numeric(age),
    young = age <= 14
    ) %>% 
  filter(age <= 14 | age >= 65) %>% 
  group_by(time, geo, young) %>% 
  summarise(values = sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = young, values_from = values, names_prefix = "young") %>% 
  transmute(time, geo, ageing = youngFALSE / youngTRUE)
```

``` r
df_motherrate <- get_eurostat("demo_r_d2jan", time_format = "num") %>% 
  filter(sex == "F" & age %in% c("TOTAL", str_c("Y", 15:49))) %>% 
  mutate(t = age == "TOTAL") %>% 
  group_by(time, geo, t) %>% 
  summarise(values = sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = t, values_from = values, names_prefix = "t") %>% 
  transmute(time, geo, motherrate = tFALSE / tTRUE)
```

``` r
df_emp <- get_eurostat("lfst_r_lfe2emprtn", time_format = "num") %>% 
  filter(isced11 == "TOTAL" & citizen == "TOTAL" & age == "Y15-64" & sex == "F") %>% 
  select(time, geo, emp = values)
```

``` r
dat <- list(df_tfr, df_lifexp, df_ageing, df_motherrate, df_emp) %>% 
  reduce(full_join) %>% 
  filter(str_length(geo) == 4 & geo != "HUXX") %>% 
  mutate(country = str_sub(str_sub(geo, end = 2))) %>% 
  filter(country %in% eu_countries$code) %>% 
  select(time, country, geo, everything())

dat
```

    ## # A tibble: 8,148 x 8
    ##     time country geo     tfr lifexp ageing motherrate   emp
    ##    <dbl> <chr>   <chr> <dbl>  <dbl>  <dbl>      <dbl> <dbl>
    ##  1  2019 AT      AT11   1.39   81.9  1.66       0.398  67.1
    ##  2  2019 AT      AT12   1.5    81.7  1.39       0.415  70.1
    ##  3  2019 AT      AT13   1.35   81.1  1.13       0.484  63.2
    ##  4  2019 AT      AT21   1.4    82.2  1.62       0.396  67  
    ##  5  2019 AT      AT22   1.41   82.2  1.51       0.422  70.1
    ##  6  2019 AT      AT31   1.6    82.3  1.22       0.428  72.8
    ##  7  2019 AT      AT32   1.58   82.8  1.28       0.435  73.4
    ##  8  2019 AT      AT33   1.46   83    1.23       0.448  72.1
    ##  9  2019 AT      AT34   1.68   83.3  1.08       0.446  71.6
    ## 10  2019 BE      BE10   1.71   81.6  0.662      0.500  51.8
    ## # ... with 8,138 more rows

``` r
dat <- dat %>% 
  select(-country) %>% 
  group_by(time) %>% 
  nest() %>% 
  mutate(
    data = map(data, na.omit),
    geo = map(data, "geo"),
    data = map(data, select, -geo),
    scaled_data = map(data, scale)
  )

dat
```

    ## # A tibble: 31 x 4
    ## # Groups:   time [31]
    ##     time data               geo         scaled_data    
    ##    <dbl> <list>             <list>      <list>         
    ##  1  2019 <tibble [239 x 5]> <chr [239]> <dbl [239 x 5]>
    ##  2  2018 <tibble [279 x 5]> <chr [279]> <dbl [279 x 5]>
    ##  3  2017 <tibble [280 x 5]> <chr [280]> <dbl [280 x 5]>
    ##  4  2016 <tibble [277 x 5]> <chr [277]> <dbl [277 x 5]>
    ##  5  2015 <tibble [276 x 5]> <chr [276]> <dbl [276 x 5]>
    ##  6  2014 <tibble [277 x 5]> <chr [277]> <dbl [277 x 5]>
    ##  7  2013 <tibble [266 x 5]> <chr [266]> <dbl [266 x 5]>
    ##  8  2012 <tibble [257 x 5]> <chr [257]> <dbl [257 x 5]>
    ##  9  2011 <tibble [256 x 5]> <chr [256]> <dbl [256 x 5]>
    ## 10  2010 <tibble [254 x 5]> <chr [254]> <dbl [254 x 5]>
    ## # ... with 21 more rows

## Cluster analyses

``` r
cluster_df <- crossing(time = 1999:2019, n_cluster = 2:10)

cluster_df
```

    ## # A tibble: 189 x 2
    ##     time n_cluster
    ##    <int>     <int>
    ##  1  1999         2
    ##  2  1999         3
    ##  3  1999         4
    ##  4  1999         5
    ##  5  1999         6
    ##  6  1999         7
    ##  7  1999         8
    ##  8  1999         9
    ##  9  1999        10
    ## 10  2000         2
    ## # ... with 179 more rows

``` r
cluster_df <- cluster_df %>% 
  mutate(
    fit = map2(time, n_cluster, 
               ~ kmeans(na.omit(filter(dat, time == .x)$scaled_data[[1]]), 
                        centers = .y, nstart=100, iter.max=100))
  )

cluster_df
```

    ## # A tibble: 189 x 3
    ##     time n_cluster fit     
    ##    <int>     <int> <list>  
    ##  1  1999         2 <kmeans>
    ##  2  1999         3 <kmeans>
    ##  3  1999         4 <kmeans>
    ##  4  1999         5 <kmeans>
    ##  5  1999         6 <kmeans>
    ##  6  1999         7 <kmeans>
    ##  7  1999         8 <kmeans>
    ##  8  1999         9 <kmeans>
    ##  9  1999        10 <kmeans>
    ## 10  2000         2 <kmeans>
    ## # ... with 179 more rows

``` r
save(list = c("cluster_df", "dat"), file = "Cluster_result.RData")
```
