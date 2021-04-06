
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/sdaume/twittrcademic/workflows/R-CMD-check/badge.svg)](https://github.com/sdaume/twittrcademic/actions)
<!-- badges: end -->

# twittrcademic

The `twittrcademic` package provides a collection of utility functions
supporting the retrieval of Tweet data via Twitter v2 API endpoints in
the [Twitter Academic Research product
track](https://developer.twitter.com/en/solutions/academic-research).

This package has been set up as a personal library to collect Tweet data
for academic research. **The package does not provide any functions to
analyze the retrieved data.**

## Background and prerequisites

The API endpoints in the [Twitter Academic Research product
track](https://developer.twitter.com/en/solutions/academic-research)
offer access to the full Tweet archive. These endpoints rely on the
[Twitter API
v2](https://developer.twitter.com/en/docs/twitter-api/early-access) with
a significantly different Tweet object model compared to the v1.1 API.
In addition to structural differences in the JSON responses, the v2
endpoints require that most objects and attributes — in for example a
Tweet object — have to be explicitly specified in the API request in
order to be included in the response. (By default the v2 search endpoint
JSON contains only Tweet ID and text.)

In order to use the functions in this package, API keys specifically for
the [Academic Research product
track](https://developer.twitter.com/en/solutions/academic-research) are
required, standard API access keys will not work.

## Installation

Install the development version of `twittrcademic` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sdaume/twittrcademic")
```

## Usage

The package functions can be used to execute a single Tweet search API
call against the
[/2/tweets/search/all](https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all)
endpoint or execute long-running searches for large result sets and
store the results in multiple suitably sized batches.

### A single search API call

This will return a JSON response of at most 500 Tweets, which could be
processed directly with tools like `jsonlite`. The example below would
return the 100 most recent Tweets containing the keyword
***openscience*** and posted on the ***12. June 2020*** or earlier.

``` r
library(twittrcademic)

bearer <- oauth_twitter_token(consumerKey = "YOUR_ACADEMIC_PRODUCT_API_KEY",
                              consumerSecret = "YOUR_ACADEMIC_PRODUCT_API_SECRET")

json_response <- search_tweets(queryString = "openscience", 
                               maxResult = 100,  
                               toDate = "2020-06-12",
                               twitterBearerToken = bearer)
```

### Retrieve and store all available results

The following example would collect all Tweets posted in the year
***2020*** that contain the term ***‘planetary boundaries’***. This will
run until all results are retrieved. The results will be summarised into
batches of files that contain approximately ***20000*** Tweets; files
are stored in the working directory and all start with
***‘query\_label’*** (for example
`query_label_20200101_20201231_1_20453.json`); in addition to the base
label the file name indicates the date range (implicit or explicit) of
the query, a numeric index for the batch and the number of Tweet results
in the given batch.

``` r
library(twittrcademic)

bearer <- oauth_twitter_token(consumerKey = "YOUR_ACADEMIC_PRODUCT_API_KEY",
                              consumerSecret = "YOUR_ACADEMIC_PRODUCT_API_SECRET")

search_and_store_tweets(queryString = "planetary boundaries", 
                        fromDate = "2020-01-01", 
                        toDate = "2020-12-31",
                        maxBatchSize = 20000,
                        batchBaseLabel = "query_label",
                        twitterBearerToken = bearer)
```

## License, credits and acknowledgements

The package is shared under an [MIT License](LICENSE.md).

This package has been developed to support research at the [Stockholm
Resilience Centre](https://www.stockholmresilience.org); this research
has benefited from funding by the [Swedish Research Council for
Sustainable Development (Formas)](https://formas.se/).

## Disclaimer

This package has been developed as a reusable tool for the author(s) own
research and comes with no guarantee for the correctness or completeness
of the retrieved data.
