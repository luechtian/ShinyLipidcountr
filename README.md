
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ShinyLipidcountr <img src='man/figures/logo.ico' align="right" height="109" /></a>

<!-- badges: start -->

[![codecov](https://codecov.io/gh/luechtian/ShinyLipidcountr/branch/main/graph/badge.svg?token=4VrPT1kWrI)](https://codecov.io/gh/luechtian/ShinyLipidcountr)
<!-- badges: end -->

The goal of ShinyLipidcountr is to build an excel workbook that contains
quantified lipidomics data and graphs. The shiny app is able to read
LipidView and LipidXplorer report files to calculate concentrations of
various lipid species.

## Installation

You can install the development version of ShinyLipidcountr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("luechtian/ShinyLipidcountr")
```

## Getting started

You need LipidView or LipidXplorer for peak processing and annotation of
your mass spectrometric data. This app accepts text-files and csv-files
with a specific format.

For LipidView-output, use target lists and only the “MarkerView” export
option in LipidView <img src='man/figures/lv_export.png' /></a>

## 

A LipidView-textfile in a spreadsheet looks like this:
<img src='man/figures/lv_table.png' /></a>

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
