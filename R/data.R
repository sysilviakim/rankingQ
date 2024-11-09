#' Relative partisanship data
#'
#' @description
#'
#' Full dataset from survey on relative partisanship used in
#' Atsusaka, Yuki, & Kim, Seo-young Silvia (2024).
#' Addressing Measurement Errors in Ranking Questions for the Social Sciences.
#' Political Analysis (conditionally accepted).
#' https://osf.io/preprints/osf/3ys8x
#' The data contains 1,082 respondents' rankings to the main identity ranking
#' question and the corresponding anchor question, as well as whether they have
#' answered the anchor questions "correctly."
#'
#' @format ## `identity`
#' A data frame with 1,082 rows and 10 columns:
#' \describe{
#'   \item{app_party}{Ranking for party (main identity ranking question).}
#'   \item{app_religion}{Ranking for religion (main identity ranking question).}
#'   \item{app_gender}{Ranking for gender (main identity ranking question).}
#'   \item{app_race}{Ranking for race (main identity ranking question).}
#'   \item{anc_house}{Ranking for household (anchor question).}
#'   \item{anc_neighborhood}{Ranking for neighborhood (anchor question).}
#'   \item{anc_city}{Ranking for city (anchor question).}
#'   \item{anc_state}{Ranking for state (anchor question).}
#'   \item{anc_correct_identity}{Whether the respondent answered the anchor questions correctly.}
#'   \item{s_weight}{Survey weight.}
#' }
#' @source <https://github.com/sysilviakim/ranking_error>
"identity"
