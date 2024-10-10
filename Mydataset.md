My dataset
================
Leo Lu
2024-10-10

``` r
library(haven)
library(psych)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     %+%, alpha

``` r
library(tidyr)
library(bruceR)
```

    ## 
    ## bruceR (v2024.6)
    ## Broadly Useful Convenient and Efficient R functions
    ## 
    ## Packages also loaded:
    ## ✔ data.table ✔ emmeans
    ## ✔ dplyr      ✔ lmerTest
    ## ✔ tidyr      ✔ effectsize
    ## ✔ stringr    ✔ performance
    ## ✔ ggplot2    ✔ interactions
    ## 
    ## Main functions of `bruceR`:
    ## cc()             Describe()  TTEST()
    ## add()            Freq()      MANOVA()
    ## .mean()          Corr()      EMMEANS()
    ## set.wd()         Alpha()     PROCESS()
    ## import()         EFA()       model_summary()
    ## print_table()    CFA()       lavaan_summary()
    ## 
    ## For full functionality, please install all dependencies:
    ## install.packages("bruceR", dep=TRUE)
    ## 
    ## Online documentation:
    ## https://psychbruce.github.io/bruceR
    ## 
    ## To use this package in publications, please cite:
    ## Bao, H.-W.-S. (2024). bruceR: Broadly useful convenient and efficient R functions (Version 2024.6) [Computer software]. https://CRAN.R-project.org/package=bruceR

    ## 
    ## These packages are dependencies of `bruceR` but not installed:
    ## - pacman, openxlsx, ggtext, lmtest, vars, phia, MuMIn, GGally
    ## 
    ## ***** Install all dependencies *****
    ## install.packages("bruceR", dep=TRUE)

``` r
library(ggsci)
library(see)
```

    ## 
    ## Attaching package: 'see'

    ## The following objects are masked from 'package:ggsci':
    ## 
    ##     scale_color_material, scale_colour_material, scale_fill_material

``` r
load("/Users/leolu/Documents/38964-0001-Data.rda")
```

``` r
mydataset <- da38964.0001 %>%
  select(SAT1, SAT2, SAT3, SAT4, SAT5, EMP_1, EMP_2, EMP_OTHER_SELFEMP, SOCIAL_2, AGE, WORK_HRS, SLEEPHRS)

mydataset$SAT1 <- as.numeric(mydataset$SAT1)

describe(mydataset$SAT1)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 7644 3.03 1.19      3    3.06 1.48   1   7     6 -0.22    -0.91 0.01

``` r
mydataset <- mydataset %>%
  filter(SAT1 < 6)

summary(mydataset$SAT1)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.00    2.00    3.00    3.03    4.00    5.00

``` r
mydataset$SAT2 <- as.numeric(mydataset$SAT2)

describe(mydataset$SAT2)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 7640 3.04 1.19      3    3.05 1.48   1   7     6 -0.16    -0.95 0.01

``` r
mydataset <- mydataset %>%
  filter(SAT2 < 6)

summary(mydataset$SAT2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   3.037   4.000   5.000

``` r
mydataset$SAT3 <- as.numeric(mydataset$SAT3)

describe(mydataset$SAT3)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 7639 3.27 1.24      4    3.33 1.48   1   7     6 -0.35    -0.95 0.01

``` r
mydataset <- mydataset %>%
  filter(SAT3 < 6)

summary(mydataset$SAT3)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   4.000   3.266   4.000   5.000

``` r
mydataset$SAT4 <- as.numeric(mydataset$SAT4)

describe(mydataset$SAT4)
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 7638 3.36 1.21      4    3.44 1.48   1   5     4 -0.44    -0.81 0.01

``` r
mydataset <- mydataset %>%
  filter(SAT4 < 6)

summary(mydataset$SAT4)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   4.000   3.364   4.000   5.000

``` r
mydataset$SAT5 <- as.numeric(mydataset$SAT5)

describe(mydataset$SAT5)
```

    ##    vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## X1    1 7638 2.72 1.33      3    2.65 1.48   1   7     6 0.25    -1.13 0.02

``` r
mydataset <- mydataset %>%
  filter(SAT5 < 6)

summary(mydataset$SAT5)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   2.721   4.000   5.000

``` r
mydataset <- mydataset %>%
  filter(EMP_1 != "(99) Refusal")
mydataset <- mydataset %>%
  mutate_at(c('EMP_1'),funs(str_replace(., "(00) Item not selected", "Not in One Job")))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
summary(mydataset$EMP_1)
```

    ##    Length     Class      Mode 
    ##      7636 character character

``` r
mydataset <- mydataset %>%
  filter(EMP_2 != "(99) Refusal")

mydataset<- mydataset%>%
  mutate_at(c('EMP_2'),funs(str_replace(., "(00) Item not selected", "One Job")))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
summary(mydataset$EMP_2)
```

    ##    Length     Class      Mode 
    ##      7636 character character

``` r
mydataset <- mydataset %>%
  filter(EMP_OTHER_SELFEMP != "(99) Refusal")

summary(mydataset$EMP_OTHER_SELFEMP)
```

    ## (00) Item not selected     (01) Item selected           (99) Refusal 
    ##                   7500                    136                      0
