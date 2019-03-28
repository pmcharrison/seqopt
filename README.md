
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seqopt

[![Travis build
status](https://travis-ci.com/pmcharrison/seqopt.svg?branch=master)](https://travis-ci.org/pmcharrison/seqopt)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/seqopt?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/seqopt)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/seqopt/badge.svg)](https://coveralls.io/r/pmcharrison/seqopt?branch=master)

`seqopt` is an R package for finding optimal sequences with dynamic
programming. Given a list of timepoints and corresponding lists of
possible states, `seq_opt()` efficiently finds an optimal state sequence
that minimises an arbitrary transition cost function.

## Installation

``` r
if (!require(devtools)) install.packages("devtools")
devtools:install_github("pmcharrison/seqopt")
```

## Examples

``` r
library(seqopt)

# 3 time points, each of which can take values from 1 to 5
x <- lapply(1:3, function(x) 1:5)
print(x)
#> [[1]]
#> [1] 1 2 3 4 5
#> 
#> [[2]]
#> [1] 1 2 3 4 5
#> 
#> [[3]]
#> [1] 1 2 3 4 5

# Define two cost functions.
# The first enforces always-increasing state values.
# The second prefers small differences between successive states.
cost_funs <- list(
  cost_fun(context_sensitive = TRUE, 
           function(a, b) if (b <= a) Inf else 0),
  cost_fun(context_sensitive = TRUE, 
           function(a, b) abs(b - a))
)

# seq_opt finds the optimal sequence satisfying these constraints.
res <- seq_opt(x, cost_funs)
print(unlist(res))
#> [1] 1 2 3

# Now add a cost function preferring large state values.
cost_funs[[3]] <- cost_fun(context_sensitive = FALSE,
                           function(a) - a)
res <- seq_opt(x, cost_funs)
print(unlist(res))
#> [1] 3 4 5
```
