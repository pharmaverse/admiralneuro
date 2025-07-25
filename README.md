# admiralneuro <img src="man/figures/logo.png" align="right" width="200" style="margin-left:50px;"/>

<!-- badges: start -->
<!-- badges: end -->

Neuroscience extension package for ADaM in R Asset Library `{admiral}`

## Purpose

To provide a complementary (to `{admiral}`) toolbox that enables users
to develop neuroscience disease area specifics.

## Installation

The package is available from CRAN and can be installed with:

```r
install.packages("admiralneuro")
```

To install the latest development version of the package directly from
GitHub use the following code:

```r
pak::pkg_install("admiralneuro", dependencies = TRUE)
```

### Dependencies

The latest version of the package works with the latest versions of the
packages stated in `DESCRIPTION`.

If a previous version of the package should be used, it is recommended
to use latest version of the dependencies at the point in time when the
previous version of `{admiralneuro}` was released.

## Scope

-   Build a toolbox of re-usable functions and utilities to create
    neuroscience-specific ADaM datasets in R in a modular manner.
-   All functions are created based upon the ADaM Implementation Guide
    and aim to facilitate the programming of ADaM dataset standards.
-   Initially the package focuses on the most common efficacy
    endpoint needs for Amyloid PET and Tau PET, but over time we will look to add extra areas such as:
    cognitive scores, lab biomarkers and questionnaires.

## Expectations

`{admiralneuro}` is expected to complement `{admiral}` and provide
functions to help with the creation of the efficacy endpoints required
for neuroscience ADaMs.

## References and Documentation

Please refer to the [{admiral} References and
    Documentation](https://pharmaverse.github.io/admiral/).

## R Versions

Here's a summary of our strategy for this package related to R versions:

-   R versions for developers and users will follow the same as
    `{admiral}` core package.
-   For development the `main` branch of `{admiral}` core is used as a
    dependency. For releasing a new `{admiralneuro}` version it must run
    using the latest released `{admiral}` core version.

## Contact

We use the following for support and communications between user and
developer community:

-   [Slack](https://pharmaverse.slack.com/) - for
    informal discussions, Q&A and building our user community. If you
    don't have access, use this
    [link](https://join.slack.com/t/pharmaverse/shared_invite/zt-yv5atkr4-Np2ytJ6W_QKz_4Olo7Jo9A)
    to join the pharmaverse Slack workspace
-   [GitHub Issues](https://github.com/pharmaverse/admiralneuro/issues) -
    for direct feedback, enhancement requests or raising bugs
