#' Get JSON response for a single full archive Tweet search query
#'
#' \code{v2_tweets_search_all} allows to execute a single Tweet search API call
#' against the
#' \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#' endpoint in the Twitter Academic Research product track.
#'
#' @param queryString a character string specifying a value for the Tweet search
#'   query parameter (e.g. \emph{"sustainability (climate change)"},
#'   \emph{"from:stefandaume"} etc). See
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{here}
#'    for details. Maximum of 1024 characters.
#'
#' @param fromDate an optional character string of format ("YYYY-MM-DD")
#'   specifying the date for the oldest Tweets to be included in the search
#'   result (interpreted as inclusive); corresponds to the \code{start_time}
#'   parameter of the
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#'    endpoint. Must NOT be a date before "2006-03-21". If no value is supplied
#'   (default), it is interpreted as the 30th day before \code{toDate}.
#'
#' @param toDate an optional character string of format ("YYYY-MM-DD")
#'   specifying the date for the most recent Tweets to be included in the search
#'   result (interpreted as inclusive); corresponds to the \code{end_time}
#'   parameter of the
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#'    endpoint. If no value is supplied (default), it is interpreted as the
#'   current date.
#'
#' @param nextToken a character string specifying a token used to iterate over
#'   search results larger than the maximum of 500. If a Tweet search has more
#'   results, the metadata section in the JSON response will provide a
#'   \strong{next_token} value, which can be used to paginate over the next set
#'   of results (see
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/integrate/paginate}{here}
#'    for details.) \code{search_and_store_tweets()} uses this mechanism to
#'   retrieve complete result sets.
#'
#' @param maxResult an integer specifying the maximum number of Tweets returned
#'   in a single search API call (minimum is 10, maximum is 500, but only 100
#'   when retrieving \code{context_annotations} with the \code{tweet.fields}
#'   query parameter)
#'
#' @param twitterBearerToken a character string specifying a valid bearer token
#'   for the
#'   \href{https://developer.twitter.com/en/solutions/academic-research}{Twitter
#'   Academic Research product track} (see \code{oauth_twitter_token()}).
#'
#' @return the Twitter API JSON response as a JSON character string
#'
#' @export
#'
search_tweets <- function(queryString, fromDate = NULL, toDate = NULL,
                          nextToken = NA, maxResult = 100,
                          twitterBearerToken) {


  api_response <- get_v2_tweets_search_all(queryString = queryString,
                                           fromDate = fromDate,
                                           toDate = toDate,
                                           nextToken = nextToken,
                                           maxResult = maxResult,
                                           twitterBearerToken = twitterBearerToken)

  json <- httr::content(api_response, as = "text")

  return(json)
}



#' Get API response for a single full archive Tweet search query
#'
#' Execute a Tweet search against the the
#' \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#' endpoint and return the HTTP response object.
#'
#' Same parameters as \code{search_tweets()}.
#'
#' @return the complete Twitter API response
#'
#' @keywords internal
#'
get_v2_tweets_search_all <- function(queryString, fromDate = NULL,
                                     toDate = NULL,nextToken = NA,
                                     maxResult = 100, twitterBearerToken) {

  # create url
  search_url <- url_v2_search_tweets_all(queryString = queryString,
                                         fromDate = fromDate,
                                         toDate = toDate,
                                         nextToken = nextToken,
                                         maxResult = maxResult)

  #print(search_url)

  # run api call
  bearer <- paste("Bearer", twitterBearerToken)
  api_response <- httr::GET(search_url, httr::add_headers(Authorization = bearer))

  return(api_response)
}





#' Retrieve and store all Tweets for a given full archive search query
#'
#' \code{search_and_store_tweets} allows to execute long-running Tweet searches
#' against the
#' \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#' endpoint in the Twitter Academic Research product track
#' \href{https://developer.twitter.com/en/solutions/academic-research}{Twitter
#' Academic Research product track} and store the results as batches of files
#' containing the original JSON responses.
#'
#' Based on a given Tweet search query and (optional) date range this function
#' iteratively retrieves all matching Tweets and stores the results in batches
#' as appropriately labelled JSON files; the complete set of results is split in
#' batches containing the (approximate) number of Tweets specified by
#' \code{maxBatchSize} Tweets.
#'
#' The calls to the search API endpoint are timed such that the API call limit
#' of at most one call per second and 300 calls per 15 minute window is
#' observed; this corresponds to a maximum of 30.000 Tweets that can be
#' retrieved every 15 minutes (each individual API call returns at most 100
#' Tweets).
#'
#' When running a query a progress bar in the console indicates how quickly data
#' collection is advancing; progress is shown in relation to the (explicitly or
#' implicitly) specified search time range and dates of the Tweets in the
#' retrieved Tweet batches.
#'
#'
#' @param queryString a character string specifying a value for the Tweet search
#'   query parameter (e.g. \emph{"sustainability (climate change)"},
#'   \emph{"from:stefandaume"} etc). See
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{here}
#'    for details. Maximum of 1024 characters.
#'
#' @param fromDate a character string of format ("YYYY-MM-DD") specifying the
#'   date for the oldest Tweets to be included in the search results
#'   (interpreted as inclusive); corresponds to the \code{start_time} parameter
#'   of the
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#'    endpoint. Must NOT be a date before "2006-03-21". If no value is supplied
#'   (default), it is interpreted as the 30th day before \code{toDate}.
#'
#' @param toDate a character string of format ("YYYY-MM-DD") specifying the date
#'   for the most recent Tweets to be included in the search results
#'   (interpreted as inclusive); corresponds to the \code{end_time} parameter of
#'   the
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#'    endpoint. If no value is supplied (default), it is interpreted as the
#'   current date.
#'
#' @param batchBaseLabel a character string used to name the stored Tweet search
#'   batch files.
#'
#' @param maxBatchSize an integer specifying the approximate number of Tweets in
#'   each stored batch of Tweet search responses. This number serves as a
#'   threshold, once more Tweets than \code{maxBatchSize} are accumulated, they
#'   are stored as a batch.
#'
#' @param twitterBearerToken a character string specifying a valid bearer token
#'   for the
#'   \href{https://developer.twitter.com/en/solutions/academic-research}{Twitter
#'   Academic Research product track} (see \code{oauth_twitter_token()}).
#'
#' @param verbose a boolean indicating whether more detailed intermediate
#'   progress messages should be printed to the console; if \emph{FALSE} only a
#'   progress bar based on the specified or implied date range and the date of
#'   the latest retrieved Tweet will be printed
#'
#' @export
#'
search_and_store_tweets <- function(queryString, fromDate = NULL, toDate = NULL,
                                    batchBaseLabel = NULL, maxBatchSize = 10000,
                                    twitterBearerToken = NULL, verbose = TRUE) {

  if (is.null(twitterBearerToken)) {
    stop("Invalid twitterBearerToken - must not be NULL.")
  }

  if (is.null(batchBaseLabel) || nchar(trimws(batchBaseLabel)) == 0) {
    stop("Please provide a valid batchBaseLabel - must not be NULL or empty.")
  }

  # initialise control variables
  retry_limit <- 10
  next_token <- NA
  next_page <- 0
  json_batches <- NULL
  n_batch_results_cumulative <- 0
  n_results_total <- 0
  batch_id <- 1

  # date range labels to specify the batches
  to_label <- ifelse(is.null(toDate),
                     format(lubridate::now(), "%Y%m%d"),
                     gsub("-", "", toDate))

  if (is.null(toDate)) {
    from_label <- ifelse(is.null(fromDate),
                         format((lubridate::now() - lubridate::duration(30, "days")),
                                "%Y%m%d"),
                         gsub("-", "", fromDate))
  } else {
    from_label <- ifelse(is.null(fromDate),
                         format((lubridate::ymd(toDate) - lubridate::duration(30, "days")),
                                "%Y%m%d"),
                         gsub("-", "", fromDate))
  }

  # print infor message and create progress bar
  writeLines(paste("Pulling Tweets posted from ", lubridate::ymd(from_label),
                   " to ", lubridate::ymd(to_label), " and matching the query \"",
                   queryString, "\".\n", sep = ""))

  search_progressbar <- utils::txtProgressBar(min = 0, max = 100, initial = 0,
                                              char = "=", style = 3)

  while (!is.na(next_token) || next_page == 0) {
    next_page <- next_page + 1

    api_response <- get_v2_tweets_search_all(queryString = queryString,
                                             fromDate = fromDate,
                                             toDate = toDate,
                                             nextToken = next_token,
                                             twitterBearerToken = twitterBearerToken)

    # default wait of one second to ensure that the endpoint rate limit of
    # at most one call per second is observed
    Sys.sleep(1)

    # wait and retry if there is an error, otherwise extract the results
    if (httr::http_error(api_response)) {
      # increase the wait time with repeated failures
      if (verbose) {
        writeLines(paste("\nHTTP error:", httr::http_status(api_response)$message,
                         "Retrying in", floor(30/retry_limit), "seconds.\n"))
      }
      Sys.sleep(30/retry_limit)
      retry_limit <- retry_limit - 1

    } else {
      retry_limit <- 10

      # collect the JSON from the response
      json <- httr::content(api_response, as = "text")

      if (!is.null(json_batches)) {
        json_batches <- paste(json_batches, json, sep = "\n")
      } else {
        json_batches <- json
      }

      # get next token and other search result metadata
      search_metadata <- .search_metadata(json)
      next_token <- .next_token(search_metadata)

      n_batch_results_cumulative <- n_batch_results_cumulative + .result_count(search_metadata)

      # get the date for the oldest Tweet to update the progress indicator
      latest_date <- .oldest_tweet_date(json, .oldest_id(search_metadata))

      if (!is.null(latest_date)) {
        total_duration <- as.numeric(lubridate::ymd(to_label) - lubridate::ymd(from_label))
        covered_duration <- as.numeric(lubridate::ymd(to_label) - as.Date(latest_date))
        utils::setTxtProgressBar(search_progressbar,
                                 value = (100 * covered_duration/total_duration))
      }
    }

    # store batch if threshold is met or no more results
    if (n_batch_results_cumulative >= maxBatchSize || is.na(next_token) || retry_limit == 0) {

      if (retry_limit == 0) {
        writeLines("\nRepeated HTTP errors. Aborting data collection.")
      }

      batch_name <- paste(batchBaseLabel,"_",
                          from_label, "_", to_label, "_",
                          batch_id,"_",
                          n_batch_results_cumulative,
                          ".json",
                          sep = "")


      # storing with 'useBytes = TRUE' is required when running on Win systems
      # to avoid encoding issues when reading the stored files

      writeLines(json_batches, batch_name, useBytes = TRUE)

      if(verbose) {
        writeLines(paste("\n\n", Sys.time(), " - STORING BATCH ",
                         batch_id, " with ", n_batch_results_cumulative,
                         " Tweets: ", batch_name,
                         "\n(Oldest Tweet in this batch created_at: ",
                         latest_date, "). Resuming data collection ...\n",
                         sep = ""))
      }

      # keep a count of results total
      n_results_total <- n_results_total + n_batch_results_cumulative

      if (!is.na(next_token)) {
        batch_id <- batch_id + 1
        json_batches <- NULL
        n_batch_results_cumulative <- 0
      } else {
        return(paste("Retrieved", n_results_total, "Tweets.",
                     ifelse(retry_limit == 10,
                            "No further results available.",
                            "Results are probably incomplete!")))
      }
    }

    # check the rate limiting parameters (300 calls per 15 minute window) and
    # wait if necessary (this corresponds to ~150.000 Tweets every 15 minutes)

    if (!httr::http_error(api_response)) {
      http_headers <- httr::headers(api_response)
      rate_limit_remaining <- as.integer(http_headers$`x-rate-limit-remaining`)

      if (!is.na(rate_limit_remaining) && (rate_limit_remaining == 0) && !is.na(next_token)) {
        rate_limit_reset <- as.integer(http_headers$`x-rate-limit-reset`)
        current_epochs <- as.integer(as.POSIXct(Sys.time()))

        if(verbose) {
          writeLines(paste("\nRATE LIMIT REACHED! Need to wait:",
                           ((rate_limit_reset - current_epochs) + 1),
                           "seconds before resuming.\n"))
        }

        Sys.sleep((rate_limit_reset - current_epochs) + 1)
      }
    }
  }
}





#' Create parameterised URL for v2 API full archive Tweet search
#'
#' @keywords internal
#'
url_v2_search_tweets_all <- function(queryString, maxResult = 500, nextToken = NA,
                                     fromDate = NULL, toDate = NULL) {

  TWEET_FIELDS <- c("created_at", "lang", "conversation_id", "author_id",
                    "public_metrics", "in_reply_to_user_id", "referenced_tweets",
                    "entities", "attachments", "context_annotations", "geo",
                    "possibly_sensitive", "source", "withheld")

  USER_FIELDS <- c("id", "username", "created_at", "location", "description",
                   "public_metrics", "name", "entities", "pinned_tweet_id",
                   "profile_image_url", "protected", "url", "verified", "withheld")

  EXPANSIONS <- c("author_id", "in_reply_to_user_id", "entities.mentions.username",
                  "referenced_tweets.id", "geo.place_id",
                  "referenced_tweets.id.author_id")

  PLACE_FIELDS <- c("full_name", "id", "contained_within", "country",
                    "country_code", "name", "place_type")

  # dates are interpreted as inclusive, i.e. all Tweets posted during the days
  # specified by fromDate and toDate are to be included in the search results
  end_time <- toDate
  if(!is.null(end_time)) {
    end_time <- strptime(paste(end_time, "T24:00:00.000Z", sep = ""),
                         tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS")
    end_time <- lubridate::format_ISO8601(end_time)
    end_time <- utils::URLencode(paste(end_time, "Z", sep = ""), reserved = TRUE)
  }

  start_time <- fromDate
  if(!is.null(start_time)) {
    start_time <- strptime(paste(start_time, "T00:00:00.000Z", sep = ""),
                           tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS")
    start_time <- lubridate::format_ISO8601(start_time)
    start_time <- utils::URLencode(paste(start_time, "Z", sep = ""), reserved = TRUE)
  }

  # assemble the URL
  full_archive_base_url <- "https://api.twitter.com/2/tweets/search/all?"

  url <- paste(full_archive_base_url,
               "query=", utils::URLencode(queryString, reserved = TRUE),
               "&max_results=", maxResult,
               ifelse(!is.null(start_time), paste("&start_time=", start_time, sep = ""), ""),
               ifelse(!is.null(end_time), paste("&end_time=", end_time, sep = ""), ""),
               "&tweet.fields=", paste0(TWEET_FIELDS, collapse = ","),
               "&user.fields=", paste0(USER_FIELDS, collapse = ","),
               "&place.fields=", paste0(PLACE_FIELDS, collapse = ","),
               "&expansions=", paste0(EXPANSIONS, collapse = ","),
               ifelse(is.na(nextToken), "", paste("&next_token=", nextToken, sep = "")),
               sep = "")
}
