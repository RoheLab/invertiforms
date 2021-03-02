
<!-- README.md is generated from README.Rmd. Please edit that file -->

# invertiforms

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RoheLab/invertiforms/branch/master/graph/badge.svg)](https://codecov.io/gh/RoheLab/invertiforms?branch=master)
[![R-CMD-check](https://github.com/RoheLab/invertiforms/workflows/R-CMD-check/badge.svg)](https://github.com/RoheLab/invertiforms/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/invertiforms)](https://CRAN.R-project.org/package=invertiforms)
<!-- badges: end -->

The goal of invertiforms is to …

## Installation

You can install the released version of invertiforms from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("invertiforms")
```

``` r
library(invertiforms)
#> Loading required package: Matrix
#> 
#> Attaching package: 'invertiforms'
#> The following object is masked from 'package:base':
#> 
#>     transform

library(igraph)
#> 
#> Attaching package: 'igraph'
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
library(igraphdata)

data("karate", package = "igraphdata")

A <- get.adjacency(karate)

iform <- NormalizedLaplacian(A)

L <- transform(iform, A)
L
#> 34 x 34 sparse Matrix of class "dgCMatrix"
#>    [[ suppressing 34 column names 'Mr Hi', 'Actor 2', 'Actor 3' ... ]]
#>                                                                        
#> Mr Hi    .          0.08333333 0.07905694 0.1020621 0.1443376 0.1250000
#> Actor 2  0.08333333 .          0.10540926 0.1360828 .         .        
#> Actor 3  0.07905694 0.10540926 .          0.1290994 .         .        
#> Actor 4  0.10206207 0.13608276 0.12909944 .         .         .        
#> Actor 5  0.14433757 .          .          .         .         .        
#> Actor 6  0.12500000 .          .          .         .         .        
#> Actor 7  0.12500000 .          .          .         0.2886751 0.2500000
#> Actor 8  0.12500000 0.16666667 0.15811388 0.2041241 .         .        
#> Actor 9  0.11180340 .          0.14142136 .         .         .        
#> Actor 10 .          .          0.22360680 .         .         .        
#> Actor 11 0.14433757 .          .          .         0.3333333 0.2886751
#> Actor 12 0.25000000 .          .          .         .         .        
#> Actor 13 0.17677670 .          .          0.2886751 .         .        
#> Actor 14 0.11180340 0.14907120 0.14142136 0.1825742 .         .        
#> Actor 15 .          .          .          .         .         .        
#> Actor 16 .          .          .          .         .         .        
#> Actor 17 .          .          .          .         .         0.3535534
#> Actor 18 0.17677670 0.23570226 .          .         .         .        
#> Actor 19 .          .          .          .         .         .        
#> Actor 20 0.14433757 0.19245009 .          .         .         .        
#> Actor 21 .          .          .          .         .         .        
#> Actor 22 0.17677670 0.23570226 .          .         .         .        
#> Actor 23 .          .          .          .         .         .        
#> Actor 24 .          .          .          .         .         .        
#> Actor 25 .          .          .          .         .         .        
#> Actor 26 .          .          .          .         .         .        
#> Actor 27 .          .          .          .         .         .        
#> Actor 28 .          .          0.15811388 .         .         .        
#> Actor 29 .          .          0.18257419 .         .         .        
#> Actor 30 .          .          .          .         .         .        
#> Actor 31 .          0.16666667 .          .         .         .        
#> Actor 32 0.10206207 .          .          .         .         .        
#> Actor 33 .          .          0.09128709 .         .         .        
#> John A   .          .          .          .         .         .        
#>                                                                          
#> Mr Hi    0.1250000 0.1250000 0.1118034 .         0.1443376 0.25 0.1767767
#> Actor 2  .         0.1666667 .         .         .         .    .        
#> Actor 3  .         0.1581139 0.1414214 0.2236068 .         .    .        
#> Actor 4  .         0.2041241 .         .         .         .    0.2886751
#> Actor 5  0.2886751 .         .         .         0.3333333 .    .        
#> Actor 6  0.2500000 .         .         .         0.2886751 .    .        
#> Actor 7  .         .         .         .         .         .    .        
#> Actor 8  .         .         .         .         .         .    .        
#> Actor 9  .         .         .         .         .         .    .        
#> Actor 10 .         .         .         .         .         .    .        
#> Actor 11 .         .         .         .         .         .    .        
#> Actor 12 .         .         .         .         .         .    .        
#> Actor 13 .         .         .         .         .         .    .        
#> Actor 14 .         .         .         .         .         .    .        
#> Actor 15 .         .         .         .         .         .    .        
#> Actor 16 .         .         .         .         .         .    .        
#> Actor 17 0.3535534 .         .         .         .         .    .        
#> Actor 18 .         .         .         .         .         .    .        
#> Actor 19 .         .         .         .         .         .    .        
#> Actor 20 .         .         .         .         .         .    .        
#> Actor 21 .         .         .         .         .         .    .        
#> Actor 22 .         .         .         .         .         .    .        
#> Actor 23 .         .         .         .         .         .    .        
#> Actor 24 .         .         .         .         .         .    .        
#> Actor 25 .         .         .         .         .         .    .        
#> Actor 26 .         .         .         .         .         .    .        
#> Actor 27 .         .         .         .         .         .    .        
#> Actor 28 .         .         .         .         .         .    .        
#> Actor 29 .         .         .         .         .         .    .        
#> Actor 30 .         .         .         .         .         .    .        
#> Actor 31 .         .         0.2236068 .         .         .    .        
#> Actor 32 .         .         .         .         .         .    .        
#> Actor 33 .         .         0.1290994 .         .         .    .        
#> John A   .         .         0.1084652 0.1714986 .         .    .        
#>                                                                               
#> Mr Hi    0.1118034 .         .         .         0.1767767 .         0.1443376
#> Actor 2  0.1490712 .         .         .         0.2357023 .         0.1924501
#> Actor 3  0.1414214 .         .         .         .         .         .        
#> Actor 4  0.1825742 .         .         .         .         .         .        
#> Actor 5  .         .         .         .         .         .         .        
#> Actor 6  .         .         .         0.3535534 .         .         .        
#> Actor 7  .         .         .         0.3535534 .         .         .        
#> Actor 8  .         .         .         .         .         .         .        
#> Actor 9  .         .         .         .         .         .         .        
#> Actor 10 .         .         .         .         .         .         .        
#> Actor 11 .         .         .         .         .         .         .        
#> Actor 12 .         .         .         .         .         .         .        
#> Actor 13 .         .         .         .         .         .         .        
#> Actor 14 .         .         .         .         .         .         .        
#> Actor 15 .         .         .         .         .         .         .        
#> Actor 16 .         .         .         .         .         .         .        
#> Actor 17 .         .         .         .         .         .         .        
#> Actor 18 .         .         .         .         .         .         .        
#> Actor 19 .         .         .         .         .         .         .        
#> Actor 20 .         .         .         .         .         .         .        
#> Actor 21 .         .         .         .         .         .         .        
#> Actor 22 .         .         .         .         .         .         .        
#> Actor 23 .         .         .         .         .         .         .        
#> Actor 24 .         .         .         .         .         .         .        
#> Actor 25 .         .         .         .         .         .         .        
#> Actor 26 .         .         .         .         .         .         .        
#> Actor 27 .         .         .         .         .         .         .        
#> Actor 28 .         .         .         .         .         .         .        
#> Actor 29 .         .         .         .         .         .         .        
#> Actor 30 .         .         .         .         .         .         .        
#> Actor 31 .         .         .         .         .         .         .        
#> Actor 32 .         .         .         .         .         .         .        
#> Actor 33 .         0.2041241 0.2041241 .         .         0.2041241 .        
#> John A   0.1084652 0.1714986 0.1714986 .         .         0.1714986 0.1400280
#>                                                                               
#> Mr Hi    .         0.1767767 .         .         .         .         .        
#> Actor 2  .         0.2357023 .         .         .         .         .        
#> Actor 3  .         .         .         .         .         .         .        
#> Actor 4  .         .         .         .         .         .         .        
#> Actor 5  .         .         .         .         .         .         .        
#> Actor 6  .         .         .         .         .         .         .        
#> Actor 7  .         .         .         .         .         .         .        
#> Actor 8  .         .         .         .         .         .         .        
#> Actor 9  .         .         .         .         .         .         .        
#> Actor 10 .         .         .         .         .         .         .        
#> Actor 11 .         .         .         .         .         .         .        
#> Actor 12 .         .         .         .         .         .         .        
#> Actor 13 .         .         .         .         .         .         .        
#> Actor 14 .         .         .         .         .         .         .        
#> Actor 15 .         .         .         .         .         .         .        
#> Actor 16 .         .         .         .         .         .         .        
#> Actor 17 .         .         .         .         .         .         .        
#> Actor 18 .         .         .         .         .         .         .        
#> Actor 19 .         .         .         .         .         .         .        
#> Actor 20 .         .         .         .         .         .         .        
#> Actor 21 .         .         .         .         .         .         .        
#> Actor 22 .         .         .         .         .         .         .        
#> Actor 23 .         .         .         .         .         .         .        
#> Actor 24 .         .         .         .         .         0.2581989 .        
#> Actor 25 .         .         .         .         .         0.3333333 .        
#> Actor 26 .         .         .         0.2581989 0.3333333 .         .        
#> Actor 27 .         .         .         .         .         .         .        
#> Actor 28 .         .         .         0.2236068 0.2886751 .         .        
#> Actor 29 .         .         .         .         .         .         .        
#> Actor 30 .         .         .         0.2236068 .         .         0.3535534
#> Actor 31 .         .         .         .         .         .         .        
#> Actor 32 .         .         .         .         0.2357023 0.2357023 .        
#> Actor 33 0.2041241 .         0.2041241 0.1290994 .         .         .        
#> John A   0.1714986 .         0.1714986 0.1084652 .         .         0.1714986
#>                                                                       
#> Mr Hi    .         .         .         .         0.10206207 .         
#> Actor 2  .         .         .         0.1666667 .          .         
#> Actor 3  0.1581139 0.1825742 .         .         .          0.09128709
#> Actor 4  .         .         .         .         .          .         
#> Actor 5  .         .         .         .         .          .         
#> Actor 6  .         .         .         .         .          .         
#> Actor 7  .         .         .         .         .          .         
#> Actor 8  .         .         .         .         .          .         
#> Actor 9  .         .         .         0.2236068 .          0.12909944
#> Actor 10 .         .         .         .         .          .         
#> Actor 11 .         .         .         .         .          .         
#> Actor 12 .         .         .         .         .          .         
#> Actor 13 .         .         .         .         .          .         
#> Actor 14 .         .         .         .         .          .         
#> Actor 15 .         .         .         .         .          0.20412415
#> Actor 16 .         .         .         .         .          0.20412415
#> Actor 17 .         .         .         .         .          .         
#> Actor 18 .         .         .         .         .          .         
#> Actor 19 .         .         .         .         .          0.20412415
#> Actor 20 .         .         .         .         .          .         
#> Actor 21 .         .         .         .         .          0.20412415
#> Actor 22 .         .         .         .         .          .         
#> Actor 23 .         .         .         .         .          0.20412415
#> Actor 24 0.2236068 .         0.2236068 .         .          0.12909944
#> Actor 25 0.2886751 .         .         .         0.23570226 .         
#> Actor 26 .         .         .         .         0.23570226 .         
#> Actor 27 .         .         0.3535534 .         .          .         
#> Actor 28 .         .         .         .         .          .         
#> Actor 29 .         .         .         .         0.23570226 .         
#> Actor 30 .         .         .         .         .          0.14433757
#> Actor 31 .         .         .         .         .          0.14433757
#> Actor 32 .         0.2357023 .         .         .          0.11785113
#> Actor 33 .         .         0.1443376 0.1443376 0.11785113 .         
#> John A   0.1212678 0.1400280 0.1212678 0.1212678 0.09901475 0.07001400
#>                    
#> Mr Hi    .         
#> Actor 2  .         
#> Actor 3  .         
#> Actor 4  .         
#> Actor 5  .         
#> Actor 6  .         
#> Actor 7  .         
#> Actor 8  .         
#> Actor 9  0.10846523
#> Actor 10 0.17149859
#> Actor 11 .         
#> Actor 12 .         
#> Actor 13 .         
#> Actor 14 0.10846523
#> Actor 15 0.17149859
#> Actor 16 0.17149859
#> Actor 17 .         
#> Actor 18 .         
#> Actor 19 0.17149859
#> Actor 20 0.14002801
#> Actor 21 0.17149859
#> Actor 22 .         
#> Actor 23 0.17149859
#> Actor 24 0.10846523
#> Actor 25 .         
#> Actor 26 .         
#> Actor 27 0.17149859
#> Actor 28 0.12126781
#> Actor 29 0.14002801
#> Actor 30 0.12126781
#> Actor 31 0.12126781
#> Actor 32 0.09901475
#> Actor 33 0.07001400
#> John A   .

A_recovered <- inverse_transform(iform, L)

all.equal(A, A_recovered)
#> [1] TRUE
```

## composable

## Current transforms

-   row center
-   column center
-   double center
-   normalized graph laplacian
-   regularized graph laplacian
-   perturbed graph laplacian

## related work

-   separate transformer based objects like recipes
-   Incomplete S4 class in softImpute
