2022-10-11

- [onepunch](#onepunch)
  - [Installation](#installation)
  - [Data Wrangling Functions](#data-wrangling-functions)
  - [Exhibit Making Functions](#exhibit-making-functions)
  - [Helper Functions](#helper-functions)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# onepunch

<!-- badges: start -->
<!-- badges: end -->

The goal of `onepunch` is to facilitate the general analytic work as a
reinsurance actuary. It is a collection of different functionality,
which I will brief later. Also note that `onepunch` accepts `data.table`
as the main data-wrangling package, rather than using
`tidyverse`-family.

Below introduction will be outlined into 3 categories: 1. Data Wrangling
2. Exhibit Making 3. Helper Functions

Also note that all reinsurance specific functionality (for actuarial
science, modeling, terms calculation and etc.) is moved to a new package
called `opre`

## Installation

You can install the development version of onepunch from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("addnox/onepunch")
```

## Data Wrangling Functions

- `readxl`-family functions can help to read Excel contents as
  `data.table`, `vector` or `list`.
- `tidy`-family functions are for cleaning untidy data into long form.

## Exhibit Making Functions

- `ggplot2`extensions: e.g. `transform_dual_axis` for easier dual-axis
  plots, `palette_op` for a quick palette
- `gt` extentions: `gt_op` and `gt_save` to create a formated table and
  save it as `png`-file
- `base`-plot extensions: e.g. `doughnut` for doughnut plot and
  `plot_save_base` to quickly save baseplot into `png`-file

## Helper Functions

- `dt`-functions are trying to make some routine work easier for
  `data.table` (e.g. `separate`).
- `vec`-functions are for vector-operations, which are usually working
  hand-in-hand with `tidy`-functions.
- `stri`-functions are extra string operations that are missing in
  `stringi` package.
- `date`-functions to work with dates and periods.
- `DataTester` is a R6 class to test data and output the results into an
  Excel file.
