# Implementación de indicadores <img src="logo/logo.png" align="right" />

[![Life
cycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#stable)
![Codecov test
coverage](https://codecov.io/gh/tidyverse/magrittr/branch/master/graph/badge.svg)

<!---
[![Buil Status](https://travis-ci.org/tidyverse/purrr.svg?branch=master)](https://travis-ci.org/tidyverse/purrr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tidyverse/purrr?branch=master&svg=true)](https://ci.appveyor.com/project/tidyverse/purrr)
[![Coverage Status](https://img.shields.io/codecov/c/github/tidyverse/purrr/master.svg)](https://codecov.io/github/tidyverse/purrr?branch=master)
-->

## Sinopsis

Es una breve descripción del flujo de las funciones para realizar
backtesting apartir de una estrategia. Y el uso de estos.

### Estrategia

Se considera una estrategía como una combinación de indicadores y reglas
para generar señales para entrar y salir de transacciones. La idea
detras de esto se presenta en la siguiente grafica:

<div style="text-align: center">

<img src="estrategia.png" align="center" />

</div>

Esto permite realizar distintas modificaciones a la estrategía:

  - Si se desea realizar una estrategía con mas de un indicador-señal,
    entonces se puede generar varias señales y luego con algúna función
    combinarlas para convertirla en una señal.

  - Cambiar la función que genera las ordenes,

Se busca describir el flujo de las funciones para generar backtesting de
diferentes estrategias, apartir de indicadores. Y el uso de este.

## Archivos

Hay tres codigos los cuales contienen las distintas funciones que
configuran el flujo.

## `read_sheet()`

`read_sheet()` is the main “read” function and should evoke
`readr::read_csv()` and `readxl::read_excel()`. It’s an alias for
`sheets_read()`. Most functions in googlesheets4 actually start with
`sheets_`. googlesheets4 is pipe-friendly (and reexports `%>%`), but
works just fine without the pipe.

purrr enhances R’s functional programming (FP) toolkit by providing a
complete and consistent set of tools for working with functions and
vectors. If you’ve never heard of FP before, the best place to start is
the family of `map()` functions which allow you to replace many for
loops with code that is both more succinct and easier to read. The best
place to learn about the `map()` functions is the [iteration
chapter](http://r4ds.had.co.nz/iteration.html) in R for data science.

## Installation

``` r
# The easiest way to get purrr is to install the whole tidyverse:
install.packages("tidyverse")
# Alternatively, install just purrr:
install.packages("purrr")
# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/purrr")
```

## Cheatsheet

<!---
<a href="https://github.com/rstudio/cheatsheets/blob/master/purrr.pdf"><img src="https://raw.githubusercontent.com/rstudio/cheatsheets/master/pngs/thumbnails/purrr-cheatsheet-thumbs.png" width="630" height="252"/></a>  
-->

## Usage

The following example uses purrr to solve a fairly realistic problem:
split a data frame into pieces, fit a model to each piece, compute the
summary, then extract the R<sup>2</sup>.

``` r
library(purrr)
mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")
```

    ##         4         6         8 
    ## 0.5086326 0.4645102 0.4229655

This example illustrates some of the advantages of purrr functions over
the equivalents in base R:

  - The first argument is always the data, so purrr works naturally with
    the pipe.

  - All purrr functions are type-stable. They always return the
    advertised output type (`map()` returns lists; `map_dbl()` returns
    double vectors), or they throw an error.

  - All `map()` functions either accept function, formulas (used for
    succinctly generating anonymous functions), a character vector (used
    to extract components by name), or a numeric vector (used to extract
    by position).

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.