#' jq filter to extract response metadata for pagination
#'
#' @keywords internal
#'
.jq_metadata_v2_filter <- function() {
  jq_filter <- paste('if has("meta") then .meta ',
                     'else empty end | ',
                     '{"newest_id": .newest_id ,',
                     ' "oldest_id": .oldest_id, ',
                     ' "next_token": (.next_token // ""), ',
                     ' "result_count": .result_count}')

  return (jq_filter)
}



#' Search metadata as a dataframe
#'
#' @param jsonSource a JSON string as returned by the
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#'   endpoint
#'
#' @keywords internal
#'
.search_metadata <- function(jsonSource) {

  search_metadata <- jsonSource %>%
    jqr::jq(.jq_metadata_v2_filter()) %>%
    jqr::combine() %>%
    jsonlite::fromJSON() %>%
    tibble::as_tibble() %>%
    dplyr::na_if("")
}


#' Search pagination token a given result set
#'
#' The \code{next_token} parameter is required to traverse a result set with
#' consecutive API calls.
#'
#' @param searchMetadata the search metadata dataframe returned by
#'   \code{.search_metadata()}
#'
#' @keywords internal
#'
.next_token <- function(searchMetadata) {
  if (nrow(searchMetadata) > 0) {
    return(searchMetadata$next_token[1])
  } else {
    return(NA)
  }
}


#' Number of Tweets in a given result set
#'
#' @param searchMetadata the search metadata dataframe returned by
#'   \code{.search_metadata()}
#'
#' @keywords internal
#'
.result_count <- function(searchMetadata) {
  if (nrow(searchMetadata) > 0) {
    n_results <- as.numeric(searchMetadata$result_count[1])
    return(n_results)
  } else {
    return(0)
  }
}


#' ID of the oldest Tweet in a given result set
#'
#' @param searchMetadata the search metadata dataframe returned by
#'   \code{.search_metadata()}
#'
#' @keywords internal
#'
.oldest_id <- function(searchMetadata) {
  if (nrow(searchMetadata) > 0) {
    id <- searchMetadata$oldest_id[1]
    return(id)
  } else {
    return(NA)
  }
}


#' ID of the most recent Tweet in a given result set
#'
#' @param searchMetadata the search metadata dataframe returned by
#'   \code{.search_metadata()}
#'
#' @keywords internal
#'
.newest_id <- function(searchMetadata) {
  if (nrow(searchMetadata) > 0) {
    id <- searchMetadata$newest_id[1]
    return(id)
  } else {
    return(NA)
  }
}



#' jq filter to extract create date of oldest Tweet in v2 search endpoint JSON
#'
#' @param tweetId a character string specifying the ID of the oldest Tweet
#'   contained in a v2 search endpoint JSON reponse; typcially obtained via
#'   \code{.oldest_id} and \code{.search_metadata}
#'
#' @keywords internal
#'
.jq_date_oldest_id <- function(tweetId) {
  jq_filter <- paste('if has("data") then .data[] ',
                     'else empty end | ',
                     'select(.id=="', tweetId, '").created_at', sep = ""
  )

  return (jq_filter)
}



#' Create date of the oldest Tweet in a v2 search as POSIXct date-time object
#'
#' @param jsonSource a JSON string as returned by the
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#'    endpoint
#'
#' @param tweetId a character string specifying the ID of the oldest Tweet
#'   contained in a v2 search endpoint JSON reponse; typcially obtained via
#'   \code{.oldest_id} and \code{.search_metadata}
#'
#' @keywords internal
#'
.oldest_tweet_date <- function(jsonSource, tweetId) {

  oldest_date <- jsonSource %>%
    jqr::jq(.jq_date_oldest_id(tweetId)) %>%
    jqr::combine() %>%
    jsonlite::fromJSON()

  if(!is.null(oldest_date) && !is.na(oldest_date) && (nchar(oldest_date) > 0)) {
    tweet_date <- lubridate::parse_date_time(x = oldest_date,
                                             orders = "%Y-%m-%d %H:%M:%OS%z")
    return(tweet_date)
  } else {
    return(NULL)
  }
}
