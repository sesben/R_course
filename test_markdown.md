Eksamen 2020
================
Esben Lykke Skovgaard
6 november 2020

Indlæs pakker.

``` r
packages <- c("tidyverse", "rstatix", "here", "readxl", "knitr", "apa")
lapply(packages, library, character.only = TRUE)
```

Indlæs data.

``` r
Eksamen2020 <-
  read_excel(here("Eksamen2020.xlsx"))
head(Eksamen2020)
```

    ## # A tibble: 6 x 10
    ##      ID Height Weight Baseline Followup   Age Level TeknikBaseline
    ##   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl> <dbl>          <dbl>
    ## 1  1001   176    71.2      110      143    26     4              3
    ## 2  1002   182.   76.3      124      153    24     4              4
    ## 3  1003   175.   76.7       66       85    24     2              2
    ## 4  1004   173.   78         64       79    25     2              2
    ## 5  1005   192.   84.9      133      156    26     4              5
    ## 6  1006   176.   80.7       73       91    21     2              1
    ## # ... with 2 more variables: TeknikFollowup <dbl>, TraningsPas <dbl>

Trasformer data en anelse.

``` r
Eksamen2020 <-
  Eksamen2020 %>%
  mutate(group = factor(if_else(ID > 10000, "kontrol", "intervention"))) %>%
  mutate(gender = factor(if_else(
    ID %in% 1000:1999 |
      ID %in% 10000:11999, "dreng", "pige"
  )))
Eksamen2020 <- Eksamen2020 %>%
  mutate(Level = factor(
    Level,
    c(1, 2, 3, 4, 5),
    c("beginner", "novice", "intermediate", "advanced", "expert")
  ))
Eksamen2020[6:12]
```

    ## # A tibble: 80 x 7
    ##      Age Level       TeknikBaseline TeknikFollowup TraningsPas group      gender
    ##    <dbl> <fct>                <dbl>          <dbl>       <dbl> <fct>      <fct> 
    ##  1    26 advanced                 3              5          24 intervent~ dreng 
    ##  2    24 advanced                 4              5          22 intervent~ dreng 
    ##  3    24 novice                   2              4          22 intervent~ dreng 
    ##  4    25 novice                   2              3          22 intervent~ dreng 
    ##  5    26 advanced                 5              5          21 intervent~ dreng 
    ##  6    21 novice                   1              2          23 intervent~ dreng 
    ##  7    24 advanced                 5              5          19 intervent~ dreng 
    ##  8    28 intermedia~              4              4          21 intervent~ dreng 
    ##  9    25 advanced                 4              4          22 intervent~ dreng 
    ## 10    22 intermedia~              2              2          23 intervent~ dreng 
    ## # ... with 70 more rows

Hvor mange bla bla.

``` r
summarytools::ctable(Eksamen2020$Level, Eksamen2020$group)
```

    ## Registered S3 method overwritten by 'pryr':
    ##   method      from
    ##   print.bytes Rcpp

    ## Cross-Tabulation, Row Proportions  
    ## Level * group  
    ## Data Frame: Eksamen2020  
    ## 
    ## -------------- ------- -------------- ------------ -------------
    ##                  group   intervention      kontrol         Total
    ##          Level                                                  
    ##       beginner              5 (55.6%)    4 (44.4%)    9 (100.0%)
    ##         novice              8 (40.0%)   12 (60.0%)   20 (100.0%)
    ##   intermediate             11 (42.3%)   15 (57.7%)   26 (100.0%)
    ##       advanced             14 (56.0%)   11 (44.0%)   25 (100.0%)
    ##         expert              0 ( 0.0%)    0 ( 0.0%)    0 (  0.0%)
    ##          Total             38 (47.5%)   42 (52.5%)   80 (100.0%)
    ## -------------- ------- -------------- ------------ -------------
