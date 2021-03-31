#' Get JSON response for a single full archive Tweet search query
#'
#' \code{v2_tweets_search_all} allows to execute a single Tweet search API call
#' against the
#' \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#' endpoint in the Twitter Academic Research product track.
#'
#'
#' @param fromDate a character string of format ("YYYY-MM-DD") specifying the
#'   date for the oldest Tweets to be included in the search result (interpreted
#'   as inclusive); corresponds to the \code{start_time} parameter of the
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#'    endpoint. Must NOT be a date before "2006-03-21". If no value is supplied
#'   (default), it is interpreted as the 30th day before \code{toDate}.
#'
#' @param toDate a character string of format ("YYYY-MM-DD") specifying the date
#'   for the most recent Tweets to be included in the search result (interpreted
#'   as inclusive); corresponds to the \code{end_time} parameter of the
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
#'    for details.) \code{tweets_search_and_store()} uses this mechanism to
#'   retrieve complete result sets.
#'
#' @param maxResult an integer specifying the maximum number of Tweets returned
#'   in a single search API call (minimum is 10, maximum is 500)
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
v2_tweets_search_all <- function(queryString, fromDate = NULL, toDate = NULL,
                                 nextToken = NA, maxResult = 500,
                                 twitterBearerToken) {
  # create url
  search_url <- .url_v2_search_tweets_all(queryString = queryString,
                                          fromDate = fromDate,
                                          toDate = toDate,
                                          nextToken = nextToken,
                                          maxResult = maxResult)

  # run api call
  bearer <- paste("Bearer", twitterBearerToken)
  api_request <- httr::GET(search_url, httr::add_headers(Authorization = bearer))

  # get and return json
  json <- httr::content(api_request, as = "text")

  return(json)
}



#' Retrieve and store all Tweets for a given full archive search query
#'
#' \code{tweets_search_and_store} allows to execute long-running Tweet searches
#' against the
#' \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{/2/tweets/search/all}
#' endpoint in the Twitter Academic Research product track
#' \href{https://developer.twitter.com/en/solutions/academic-research}{Twitter
#' Academic Research product track} and store the results as batches of the
#' original JSON responses.
#'
#' Based on a given Tweet search query and (optional) date range this function
#' iteratively retrieves all matching Tweets and stores the results in batches
#' as appropriately labelled JSON files; the complete set of results is split in
#' batches containing the (approximate) number of Tweets specified by
#' \code{maxBatchSize} Tweets.
#'
#' The calls to the search API endpoint are timed such the API call limit of at
#' most 300 calls per 15 minute window is observed; this corresponds to a
#' maximum of 150.000 Tweets that can be retrieved every 15 minutes (each
#' individual API call returns at most 500 Tweets).
#'
#' @param queryString a character string specifying a value for the Tweet search
#'   query parameter (e.g. \emph{"sustainability (climate change)"},
#'   \emph{"from:stefandaume"} etc). See
#'   \href{https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all}{here}
#'    for details. Maximum of 1024 characters.
#'
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
#' @export
#'
tweets_search_and_store <- function(queryString, fromDate = NULL, toDate = NULL,
                                    batchBaseLabel = NULL, maxBatchSize = 10000,
                                    twitterBearerToken = NULL) {

  if (is.null(twitterBearerToken)) {
    stop("Invalid twitterBearerToken - must not be NULL.")
  }

  if (is.null(batchBaseLabel) || nchar(trimws(batchBaseLabel)) == 0) {
    stop("Please provide a valid batchBaseLabel - must not be NULL or empty.")
  }
  ####### >>>>>
  #bearer_token <- paste("Bearer", twitterBearerToken)

  # initialise control variables
  next_token <- NA
  next_page <- 0
  json_batches <- NULL
  last_issued_call <- NULL

  n_batch_results_cumulative <- 0
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

  while (!is.na(next_token) || next_page == 0) {
    next_page <- next_page + 1

    ##### >>>
    #search_url <- .url_v2_search_tweets_all(queryString = queryString,
    #                                        fromDate = fromDate,
    #                                        toDate = toDate,
    #                                        nextToken = next_token)

    # time the calls to ensure rate limits are not exceeded, but also not wasted
    # this achieves retrieval of at most 150.000 Tweets per 15 minutes (300 calls)
    current_time <- Sys.time()
    if (!is.null(last_issued_call) && as.numeric(current_time - last_issued_call) < 3) {
      Sys.sleep(3 - (current_time - last_issued_call))
      #print(paste("should sleep", 3 - (current_time - last_issued_call)))
    }
    ##### >>>>
    #api_request <- httr::GET(search_url, httr::add_headers(Authorization = bearer_token))



    ##### >>>>
    #json <- httr::content(api_request, as = "text")

    json <- v2_tweets_search_all(queryString = queryString,
                                 fromDate = fromDate,
                                 toDate = toDate,
                                 nextToken = next_token,
                                 twitterBearerToken = twitterBearerToken)

    last_issued_call <- Sys.time()

    if (!is.null(json_batches)) {
      json_batches <- paste(json_batches, json, sep = "\n")
    } else {
      json_batches <- json
    }

    search_metadata <- .search_metadata(json)

    next_token <- .next_token(search_metadata)

    n_batch_results_cumulative <- n_batch_results_cumulative + .result_count(search_metadata)

    cat(paste(n_batch_results_cumulative, "...", sep = ""))

    if (n_batch_results_cumulative >= maxBatchSize || is.na(next_token)) {
      batch_name <- paste(batchBaseLabel,"_",
                          from_label, "_", to_label, "_",
                          batch_id,"_",
                          n_batch_results_cumulative,
                          ".json",
                          sep = "")

      cat(paste("\nSTORING BATCH", batch_id, "...", batch_name, Sys.time()))

      # storing with 'useBytes = TRUE' is required when running on Win systems
      # to avoid encoding issues when reading the stored files
      writeLines(json_batches, batch_name, useBytes = TRUE)

      cat(paste("... DONE", Sys.time(), "\n\n"))

      if (!is.na(next_token)) {
        batch_id <- batch_id + 1
        json_batches <- NULL
        n_batch_results_cumulative <- 0
      } else {
        return("No further results available.")
      }
    }
  }
}





#' Create parameterised URL for v2 API full archive Tweet search
#'
#' @keywords internal
#'
.url_v2_search_tweets_all <- function(queryString, maxResult = 500, nextToken = NA,
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
    end_time <- URLencode(paste(end_time, "Z", sep = ""), reserved = TRUE)
  }

  start_time <- fromDate
  if(!is.null(start_time)) {
    start_time <- strptime(paste(start_time, "T00:00:00.000Z", sep = ""),
                           tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS")
    start_time <- lubridate::format_ISO8601(start_time)
    start_time <- URLencode(paste(start_time, "Z", sep = ""), reserved = TRUE)
  }

  # assemble the URL
  full_archive_base_url <- "https://api.twitter.com/2/tweets/search/all?"

  url <- paste(full_archive_base_url,
               "query=", URLencode(queryString),
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
