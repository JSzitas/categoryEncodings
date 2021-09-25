
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

**NOTE:** The latest stable version available from **CRAN** contains
features deprecated in the current development version - I hope to
resolve this soon, and publish the development version.

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
#>            X1         X2          X3          X4          X5
#> 1:  0.9991667 -0.8563340 -0.02101734 -1.18764199 -0.09672291
#> 2: -0.0408273  0.6100193  1.05773261  0.07433868 -1.51293569
#> 3: -3.0223377  0.2533907 -0.29310384 -0.67700946 -1.17672457
#> 4:  0.2860257  0.2831005 -1.04582237  0.12541452  1.10729575
#> 5:  1.0268237 -0.1994257  0.13540853 -2.24425666 -0.48221181
#> 6:  2.8180022 -0.1213061 -1.07908356  0.72342565  1.35827814
#>    few_letters_X1_mean few_letters_X2_mean few_letters_X3_mean
#> 1:           0.1410635         -0.16953195          -0.4281999
#> 2:          -0.0593829          0.30228766          -0.2516727
#> 3:          -0.3437670          0.12934365          -0.2391281
#> 4:          -0.2300727          0.03448788          -0.5145622
#> 5:          -0.3437670          0.12934365          -0.2391281
#> 6:           0.4468059          0.21750095          -0.1616724
#>    few_letters_X4_mean few_letters_X5_mean many_letters_X1_mean
#> 1:        -0.007318328         -0.03573149         -0.009508356
#> 2:        -0.444374549         -0.33955771          0.752536873
#> 3:        -0.376740256          0.11779062         -0.009508356
#> 4:         0.275041665         -0.10717482         -0.127222281
#> 5:        -0.376740256          0.11779062          1.060956129
#> 6:         0.207355045         -0.12851764          0.425306025
#>    many_letters_X2_mean many_letters_X3_mean many_letters_X4_mean
#> 1:           0.41121640          -0.30025118         0.0508638206
#> 2:          -0.10888477          -0.03649697        -0.0004609365
#> 3:           0.41121640          -0.30025118         0.0508638206
#> 4:          -0.46090763          -0.86687255        -0.4073961849
#> 5:           0.39560841          -0.25271095        -0.2260046598
#> 6:           0.05403458          -0.01756019         0.0760504513
#>    many_letters_X5_mean some_numbers_1_SPCA some_numbers_2_SPCA
#> 1:          -0.58924951          0.22504803           0.1600847
#> 2:          -0.77765543         -0.16802721           0.2750081
#> 3:          -0.58924951          0.06959357           0.1655578
#> 4:           0.46077097          0.06959357           0.1655578
#> 5:           0.01249808         -0.51984950          -0.2600594
#> 6:           0.41030929         -0.51984950          -0.2600594
#>    some_numbers_3_SPCA some_numbers_4_SPCA some_numbers_5_SPCA
#> 1:          0.15298987        -0.094904684         0.005508219
#> 2:         -0.15234943        -0.009941846         0.003210824
#> 3:          0.05426824         0.126029279        -0.003644279
#> 4:          0.05426824         0.126029279        -0.003644279
#> 5:          0.02925956         0.017447113         0.029312642
#> 6:          0.02925956         0.017447113         0.029312642
#>    many_numbers_X1_mean many_numbers_X2_mean many_numbers_X3_mean
#> 1:           -0.1255713           -0.3799080           0.02312113
#> 2:           -0.9257131            0.3821702          -0.09373120
#> 3:           -0.9257131            0.3821702          -0.09373120
#> 4:           -0.9257131            0.3821702          -0.09373120
#> 5:            1.4593446           -0.1602822          -0.04870327
#> 6:            1.4593446           -0.1602822          -0.04870327
#>    many_numbers_X4_mean many_numbers_X5_mean binary_1_SPCA binary_2_SPCA
#> 1:           -0.1330829            0.4755544    -0.1327683    0.01185374
#> 2:           -0.1590854           -0.5274548    -0.1327683    0.01185374
#> 3:           -0.1590854           -0.5274548    -0.1327683    0.01185374
#> 4:           -0.1590854           -0.5274548    -0.1327683    0.01185374
#> 5:           -0.6368855            0.1839555    -0.1327683    0.01185374
#> 6:           -0.6368855            0.1839555    -0.1327683    0.01185374
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
```

This undoes the “encoding”, effectively returning the original data.
This can be quite useful for interpretability methods, where the
interpretation becomes easier for un-encoded data. Note that this sadly
does not maintain the order of the data from the original - and some
attributes may be lost. Nonetheless, the recovered data is almost the
same, and equivalent for all practical purposes:

``` r
original <- data.table::data.table(data_fm)
deencoded <- deencoder( result$encoded ) 


all.equal( data.table::setorder(original), 
           data.table::setorder(deencoded), 
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
