# Get Started

## Introduction

As this is a package extension, if you are new to
[admiral](https://pharmaverse.github.io/admiral/) then the best place to
first start reading would be the [Get
Started](https://pharmaverse.github.io/admiral/articles/admiral.html)
page. This extension package follows the same main idea and conventions,
and re-uses many functions from
[admiral](https://pharmaverse.github.io/admiral/), so it is important to
thoroughly understand these to be able to use
[admiralneuro](https://pharmaverse.github.io/admiralneuro/).

## Creating Neuroscience ADaM Datasets

For the neuroscience ADaM data structures, an overview of the flow and
example function calls for the most common steps are provided by the
following vignette:

- [Creating
  ADAPET/ADTPET](https://pharmaverse.github.io/admiralneuro/dev/articles/adpet.md)

[admiralneuro](https://pharmaverse.github.io/admiralneuro/) also
provides template R scripts as a starting point. They can be created by
calling
[`use_ad_template()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/use_ad_template.html)
from [admiral](https://pharmaverse.github.io/admiral/), e.g.,

``` r
library(admiral)

use_ad_template(
  adam_name = "adapet",
  save_path = "./ad_adapet.R",
  package = "admiralneuro"
)
```

A list of all available templates can be obtained by
[`list_all_templates()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/list_all_templates.html)
from [admiral](https://pharmaverse.github.io/admiral/):

``` r
admiral::list_all_templates(package = "admiralneuro")
#> Existing ADaM templates in package 'admiralneuro':
#> • ADAPET
#> • ADNV
#> • ADTPET
```

Neuroscience test SDTM datasets are provided through the
[pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/)
package. The following datasets are available: `DM`, `NV`, `SUPPNV` and
`AG`.

## Support

Support is provided via the [admiral Slack
channel](https://pharmaverse.slack.com/). Additionally, please feel free
to raise issues in our [GitHub
repository](https://github.com/pharmaverse/admiralneuro/issues).
