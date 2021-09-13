
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

**OLD EXAMPLE USAGE, THE DEFAULT ENCODING FUNCTION HAS CHANGED (TO
PROVIDE EASIER RE-ENCODING OF NEW DATA) AS OF VERSION &gt;= 1.5, NEW
EXAMPLES COMING SOON**

``` r
library(categoryEncodings)
# currently 
data_fm <- cbind( data.frame(matrix(rnorm(5*100),ncol = 5)),
                  sample(sample(letters, 10), 100, replace = TRUE))
colnames(data_fm)[6] <- "few_letters"

# encoding is done automatically, as is the inference of factors
    result <- encode_categories(X = data_fm)
# note that due to the data.table backend, the result has to be saved to an object to be 
# visible: otherwise printing is surpressed.     
    print(result)
    
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
    
result <- encode_categories(data_fm)
print(result)

 
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
