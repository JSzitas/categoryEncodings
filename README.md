
<!-- README.md is generated from README.Rmd. Please edit that file -->

# categoryEncodings

<!-- badges: start -->

[![R-CMD-check](https://github.com/JSzitas/categoryEncodings/workflows/R-CMD-check/badge.svg)](https://github.com/JSzitas/categoryEncodings/actions)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JSzitas/categoryEncodings?branch=master&svg=true)](https://ci.appveyor.com/project/JSzitas/categoryEncodings)
[![Codecov test
coverage](https://codecov.io/gh/JSzitas/categoryEncodings/branch/master/graph/badge.svg)](https://codecov.io/gh/JSzitas/categoryEncodings?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN
status](https://www.r-pkg.org/badges/version/categoryEncodings)](https://CRAN.R-project.org/package=categoryEncodings)
<!-- badges: end -->

**categoryEncodings** intends to provide a fast way to encode ‘factor’
or qualitative variables through various methods. The packages uses
**data.table** as the backend for speed, with as few other dependencies
as possible. Most of the methods are based on the paper of Johannemann
et al.(2019) - Sufficient Representations for Categorical Variables
(arXiv:1908.09874).

The current version features automatic inference of factors and uses a
very simple heuristic for encoding, as well as allowing manual controls.

## Installation

You can install the latest version of **categoryEncodings** from
[github](https://github.com/JSzitas/categoryEncodings) using the
*devtools* package

``` r
devtools::install_github("JSzitas/categoryEncodings")
```

Soon the package will be submitted to **CRAN**, and hopefully will be
accepted.

## Example

Here we want to encode all of the factors in a given data.frame.

``` r
library(categoryEncodings)

# create some example data
data_fm <- cbind( data.frame( 
   matrix( rnorm(5*100),ncol = 5)),
           sample(sample(letters, 10), 100, replace = TRUE),
           sample(sample(letters, 20), 100, replace = TRUE),
           sample(sample(1:10, 5), 100, replace = TRUE),
           sample(sample(1:50, 35), 100, replace = TRUE ),
           sample(1:2, 100, replace = TRUE ))
colnames(data_fm)[6:10] <- c( "few_letters",  "many_letters",
                              "some_numbers", "many_numbers",
                              "binary" ) 
# it does not matter how many factor variables there are, whether they are encoded as factors
# and whether you supply a method to encode them by - some simple inference of factors is done
# based on the number of distinct values in every variable - over a certain threshold 
# a variable is deemed as essentially a factor, and treated as such for conversion 
# you will be notified of which variables are being converted via a warning
result <- encoder(data_fm)
#> Warning in handle_factors(X, fact): 
#>  Inferring factors: 
#>  few_letters 
#> many_letters 
#> some_numbers 
#> many_numbers 
#> binary
# note that due to the data.table back-end, the result has to be saved to an object to be 
# visible: otherwise printing is suppressed.   
head(result$encoded)
#>            X1          X2         X3          X4         X5 few_letters_X1_mean
#> 1:  0.5556800  0.11724080 -1.4750853  0.34746987 -1.3698115          0.22361542
#> 2: -1.8382350 -0.95226303  0.5080332 -2.00939952  0.6589505          0.44676465
#> 3: -0.1631853 -0.32921264 -0.2259688  2.00210334 -1.3170355          0.03815582
#> 4:  0.1305323 -1.02394434  1.6451227  0.05381788  1.3642628         -0.15376498
#> 5: -0.6050547  0.27880899 -2.6039062  0.99012712  1.1394042         -0.47791120
#> 6:  1.5032360  0.08987233 -0.7234419 -0.04632389 -1.7575009         -0.47791120
#>    few_letters_X2_mean few_letters_X3_mean few_letters_X4_mean
#> 1:          0.11471668          -0.3843607          0.18691720
#> 2:         -0.38104242           0.1436524          0.12428624
#> 3:         -0.29781930           0.3873214          0.22297792
#> 4:         -0.25883057           0.4894195         -0.01218231
#> 5:         -0.04780475          -0.1483487         -0.30892091
#> 6:         -0.04780475          -0.1483487         -0.30892091
#>    few_letters_X5_mean many_letters_X1_mean many_letters_X2_mean
#> 1:          -0.3608007          -0.55341247           0.72452944
#> 2:          -0.1468131          -0.31448909          -0.21311914
#> 3:           0.2734796          -0.01555623           0.29572120
#> 4:           0.1250960           0.02048387           0.03904472
#> 5:           0.2020812           0.84183968          -0.22785869
#> 6:           0.2020812           0.50443472           0.45527279
#>    many_letters_X3_mean many_letters_X4_mean many_letters_X5_mean
#> 1:          -0.01512388            0.7259654           -1.0569328
#> 2:          -0.43536748           -0.2400197            0.8355270
#> 3:          -0.01501997           -0.2392253           -0.4513870
#> 4:           0.29139641           -0.2973109           -0.3112707
#> 5:          -0.24615549            0.1134242           -0.6831329
#> 6:          -0.79719502            0.1274214           -1.1575439
#>    some_numbers_1_SPCA some_numbers_2_SPCA some_numbers_3_SPCA
#> 1:          -0.4907672          -0.2650653          -0.1520587
#> 2:           0.4532239           0.1084895          -0.2113119
#> 3:          -0.5208011           0.2150207           0.1116208
#> 4:           0.2235570           0.3097695           0.1028326
#> 5:          -0.5208011           0.2150207           0.1116208
#> 6:           0.3347874          -0.3682145           0.1489172
#>    some_numbers_4_SPCA some_numbers_5_SPCA many_numbers_X1_mean
#> 1:          0.08926029        -0.008370945          -0.10209028
#> 2:         -0.07600132         0.023816041          -0.10209028
#> 3:         -0.16767924        -0.047594599          -0.02201343
#> 4:          0.18811788        -0.026271845           0.26354873
#> 5:         -0.16767924        -0.047594599           0.52636308
#> 6:         -0.03369761         0.058421349           0.52636308
#>    many_numbers_X2_mean many_numbers_X3_mean many_numbers_X4_mean
#> 1:          -0.46833432          -0.30996867           -0.5173893
#> 2:          -0.46833432          -0.30996867           -0.5173893
#> 3:          -0.71387104           0.01119392            1.1938549
#> 4:          -0.34380143           0.05412737            0.1297705
#> 5:           0.00229702          -0.85296447            0.1539010
#> 6:           0.00229702          -0.85296447            0.1539010
#>    many_numbers_X5_mean binary_1_SPCA binary_2_SPCA
#> 1:          -0.44452803    -0.1314059   0.005814203
#> 2:          -0.44452803    -0.1314059   0.005814203
#> 3:          -1.51841940    -0.1314059   0.005814203
#> 4:           0.09668415    -0.1314059   0.005814203
#> 5:          -0.09484418    -0.1314059   0.005814203
#> 6:          -0.09484418    -0.1314059   0.005814203
```

We also recover a **function closure** which we can reuse to fit new
data, as long as it conforms to the same format:

``` r
# to fit to any dataset you can either call it directly - it is a single argument function
data_fm_encoded <- result$fitted_encoder(data_fm)

# or rename it, and stash it away for later use
encoding_function <- result$fitted_encoder
```

You also get a **“de-encoding”** function -

``` r
deencoder <- result$fitted_deencoder
new_data_fm <- deencoder( result$encoded ) 
```

This undoes the “encoding”, effectively returning the original data.
This can be quite useful for interpretability methods, where the
interpretation becomes easier for un-encoded data. Note that this sadly
does not maintain the order of the data from the original - and some
attributes may be lost. Nonetheless, the recovered data is almost the
same, and equivalent for all practical purposes:

``` r
original <- data.table::data.table(data_fm)
unencoded <- new_data_fm 

all.equal( data.table::setorder(original), 
           data.table::setorder(unencoded), 
           check.attributes = FALSE )
#> [1] TRUE
```

## Contributing

Please do contribute to the projects, all contributions are welcome, as
long as people keep things civil - there is no need for negativity,
hatred, and rudeness. Also, please do refrain from adding unnecessary
dependencies (*Ex:* pipe) to the package (such pull requests as would
add an unnecessary dependency will be denied/ suspended until the code
can be made dependency free). This package wants to be as lightweight as
possible - even if this means the code is a bit harder to write and
maintain.
