
# secretscrape

<!-- badges: start -->
<!-- badges: end -->

The goal of secretscrape is to help users obtain data from opensecrets.org. It allows users to obtain data on industry specific Congressional political donations directly from Open Secrets. 

## Installation

You can install the released version of secretscrape from [github](https://github.com) with:

``` r
if(!require(devtools)){
 install.packages("devtools")
}
devtools::install_github("hkarp1/secretscrape")```

## Example

This is a basic example of a call to download data with secretscrape:

``` r
library(secretscrape)
donations_scrape("E01", 1998, "H", "N")
## this call results in 1998 donations from the Oil and Gas industry to all House candidates, not just eventual House members
```

