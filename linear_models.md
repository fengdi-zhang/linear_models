linear models
================

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(borough != "Staten Island") %>% 
  select(price, stars, borough, neighborhood, room_type)
```

## Fit the first model

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb) #linear model, outcome = price, predictors = stars, borough
#reference = Bronx

fit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

``` r
summary(fit)$coef
```

    ##                   Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept)      -70.41446  14.020697 -5.022180 5.137589e-07
    ## stars             31.98989   2.527500 12.656733 1.269392e-36
    ## boroughBrooklyn   40.50030   8.558724  4.732049 2.232595e-06
    ## boroughManhattan  90.25393   8.567490 10.534465 6.638618e-26
    ## boroughQueens     13.20617   9.064879  1.456850 1.451682e-01

``` r
fit %>%
  broom::tidy() %>%
  mutate(
    term = str_replace(term, "borough", "Borough: ")
  ) %>%
  select(term, estimate, p.value) %>%
  knitr::kable(digits = 2)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |   -70.41 |    0.00 |
| stars              |    31.99 |    0.00 |
| Borough: Brooklyn  |    40.50 |    0.00 |
| Borough: Manhattan |    90.25 |    0.00 |
| Borough: Queens    |    13.21 |    0.15 |

Let’s change reference category

``` r
fit = 
  nyc_airbnb %>%
  mutate(
    borough = fct_infreq(borough) #make reference the most frequent borough
  ) %>%
  lm(price ~ stars + borough, data = .)
```

``` r
fit %>%
  broom::tidy() %>%
  mutate(
    term = str_replace(term, "borough", "Borough: ")
  ) %>%
  select(term, estimate, p.value) %>%
  knitr::kable(digits = 2)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |    19.84 |     0.1 |
| stars             |    31.99 |     0.0 |
| Borough: Brooklyn |   -49.75 |     0.0 |
| Borough: Queens   |   -77.05 |     0.0 |
| Borough: Bronx    |   -90.25 |     0.0 |

``` r
fit %>%
  broom::glance() %>%
  select(AIC)
```

    ## # A tibble: 1 × 1
    ##       AIC
    ##     <dbl>
    ## 1 404237.

## Diagnositics

``` r
modelr::add_residuals(nyc_airbnb, fit) %>%
  ggplot(aes(x = stars, y = resid)) +
  geom_point()
```

<img src="linear_models_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

``` r
#high var for stars 3-5, against constant variance assumption
```

``` r
nyc_airbnb %>%
  modelr::add_residuals(fit) %>%
  ggplot(aes(x = borough, y = resid)) +
  geom_violin() +
  ylim(-250, 250)
```

<img src="linear_models_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

## Hypothesis testing

one coef.

``` r
fit %>%
  broom::tidy()
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

``` r
fit_null = lm(price ~ stars, data = nyc_airbnb)
fit_alt = lm(price ~ stars + borough, data = nyc_airbnb)

anova(fit_null, fit_alt) %>%
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                    df.residual       rss    df   sumsq stati…¹    p.value
    ##   <chr>                         <dbl>     <dbl> <dbl>   <dbl>   <dbl>      <dbl>
    ## 1 price ~ stars                 30528    1.03e9    NA NA          NA  NA        
    ## 2 price ~ stars + borough       30525    1.01e9     3  2.53e7    256.  7.84e-164
    ## # … with abbreviated variable name ¹​statistic

``` r
#small p values -> should put borough in model
```

## Room type by borough

Interactions …?

``` r
fit = 
  nyc_airbnb %>%
  lm(price ~ stars + borough * room_type, data = .)
#get all combinations of borough and room type

fit %>%
  broom::tidy()
```

    ## # A tibble: 13 × 5
    ##    term                                   estimate std.error statistic  p.value
    ##    <chr>                                     <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                                13.1     18.3      0.718 4.73e- 1
    ##  2 stars                                      21.8      2.42     8.97  3.06e-19
    ##  3 boroughBrooklyn                            52.8     14.9      3.54  4.07e- 4
    ##  4 boroughManhattan                          108.      14.9      7.27  3.78e-13
    ##  5 boroughQueens                              21.7     15.7      1.38  1.67e- 1
    ##  6 room_typePrivate room                     -53.0     17.8     -2.99  2.82e- 3
    ##  7 room_typeShared room                      -68.5     41.5     -1.65  9.91e- 2
    ##  8 boroughBrooklyn:room_typePrivate room     -39.2     18.0     -2.17  2.98e- 2
    ##  9 boroughManhattan:room_typePrivate room    -71.6     18.0     -3.98  7.03e- 5
    ## 10 boroughQueens:room_typePrivate room       -15.6     19.0     -0.820 4.12e- 1
    ## 11 boroughBrooklyn:room_typeShared room      -37.3     42.9     -0.869 3.85e- 1
    ## 12 boroughManhattan:room_typeShared room     -85.4     42.4     -2.01  4.41e- 2
    ## 13 boroughQueens:room_typeShared room        -24.6     44.3     -0.555 5.79e- 1

So can we fit models by borough?

``` r
nyc_airbnb %>%
  nest(df = -borough) %>%
  mutate(
    models = map(.x = df, ~lm(price ~ stars + room_type, data = .x)), #map lm over df column(nested tibbles)
    results = map(models, broom::tidy)
  ) %>%
  select(borough, results) %>%
  unnest(results)
```

    ## # A tibble: 16 × 6
    ##    borough   term                  estimate std.error statistic   p.value
    ##    <chr>     <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 Bronx     (Intercept)              90.1      15.2       5.94 5.73e-  9
    ##  2 Bronx     stars                     4.45      3.35      1.33 1.85e-  1
    ##  3 Bronx     room_typePrivate room   -52.9       3.57    -14.8  6.21e- 41
    ##  4 Bronx     room_typeShared room    -70.5       8.36     -8.44 4.16e- 16
    ##  5 Queens    (Intercept)              91.6      25.8       3.54 4.00e-  4
    ##  6 Queens    stars                     9.65      5.45      1.77 7.65e-  2
    ##  7 Queens    room_typePrivate room   -69.3       4.92    -14.1  1.48e- 43
    ##  8 Queens    room_typeShared room    -95.0      11.3      -8.43 5.52e- 17
    ##  9 Brooklyn  (Intercept)              69.6      14.0       4.96 7.27e-  7
    ## 10 Brooklyn  stars                    21.0       2.98      7.05 1.90e- 12
    ## 11 Brooklyn  room_typePrivate room   -92.2       2.72    -34.0  6.40e-242
    ## 12 Brooklyn  room_typeShared room   -106.        9.43    -11.2  4.15e- 29
    ## 13 Manhattan (Intercept)              95.7      22.2       4.31 1.62e-  5
    ## 14 Manhattan stars                    27.1       4.59      5.91 3.45e-  9
    ## 15 Manhattan room_typePrivate room  -124.        3.46    -35.8  9.40e-270
    ## 16 Manhattan room_typeShared room   -154.       10.1     -15.3  2.47e- 52

Quick double check

``` r
nyc_airbnb %>%
  filter(borough == "Bronx") %>%
  lm(price ~ stars + room_type, data = .) %>%
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              90.1      15.2       5.94 5.73e- 9
    ## 2 stars                     4.45      3.35      1.33 1.85e- 1
    ## 3 room_typePrivate room   -52.9       3.57    -14.8  6.21e-41
    ## 4 room_typeShared room    -70.5       8.36     -8.44 4.16e-16
