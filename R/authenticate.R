#' Authenticate Twitter API access and get bearer token
#'
#' \code{oauth_twitter_token} authenticates the provided credentials against the
#' Twitter
#' \href{https://developer.twitter.com/en/docs/authentication/api-reference/token}{\code{oauth2/token}}
#' endpoint and returns the authentication (bearer) token required for calls to
#' other Twitter API endpoints (consult the Twitter API documentation on
#' \href{https://developer.twitter.com/en/docs/authentication/oauth-2-0/application-only}{application-only
#' authentication} for details).
#'
#' This authentication method requires a \strong{consumer key} and
#' \strong{consumer secret} for a registered Twitter application (see
#' \href{https://developer.twitter.com/en/docs/apps/overview}{here} for
#' details).
#'
#' @param consumerKey a character string specifying the consumer key for the
#'   registered authenticating Twitter application in the Academic Research
#'   product track
#'
#' @param consumerSecret a character string specifying the consumer secret for
#'   the registered authenticating Twitter application in the Academic Research
#'   product track
#'
#' @return a character string representing a valid bearer token
#'
#' @export
oauth_twitter_token <- function(consumerKey, consumerSecret) {

  # use basic authentication
  secret <- jsonlite::base64_enc(paste(consumerKey, consumerSecret, sep = ":"))

  # Authenticated with Twitter Oauth endpoint
  oauth_request <- httr::POST("https://api.twitter.com/oauth2/token",
                              httr::add_headers(
                                "Authorization" = paste("Basic", gsub("\n", "", secret)),
                                "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"
                              ),
                              body = "grant_type=client_credentials"
  )

  #Extract the access bearer token
  httr::stop_for_status(oauth_request, "authenticate with twitter")

  return(httr::content(oauth_request)$access_token)
}
