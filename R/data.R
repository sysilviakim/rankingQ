#' Identity-ranking data analyzed in Atsusaka and Kim (2024)
#'
#' @description
#'
#' Full dataset from survey on relative partisanship used in
#' Atsusaka, Yuki, & Kim, Seo-young Silvia (2024).
#' Addressing Measurement Errors in Ranking Questions for the Social Sciences.
#' Political Analysis (conditionally accepted).
#' https://osf.io/preprints/osf/3ys8x
#'
#' This data contains Americans' rankings of four sources of their identity,
#' including political party, religion, gender, and race, for 1,082 respondents.
#' The columns consist of marginal rankings to the main identity ranking
#' question and the corresponding anchor question, as well as whether they have
#' answered the anchor questions "correctly." The anchor ranking question is
#' used to estimate the proportion of random responses and
#' to correct for measurement error bias.
#'
#' @format ## `identity`
#' A data frame with 1,082 rows and 10 columns:
#' \describe{
#'   \item{app_party}{Marginal ranking for party (main identity ranking question).}
#'   \item{app_religion}{Marginal ranking for religion (main identity ranking question).}
#'   \item{app_gender}{Marginal ranking for gender (main identity ranking question).}
#'   \item{app_race}{Marginal ranking for race (main identity ranking question).}
#'   \item{anc_house}{Marginal ranking for household (anchor question).}
#'   \item{anc_neighborhood}{Marginal ranking for neighborhood (anchor question).}
#'   \item{anc_city}{Marginal ranking for city (anchor question).}
#'   \item{anc_state}{Marginal ranking for state (anchor question).}
#'   \item{anc_correct_identity}{Whether the respondent answered the anchor questions correctly. This is a binary variable that 1 if the respondent correctly answers the anchor ranking question and 0 if otherwise.}
#'   \item{s_weight}{Survey weight.}
#' }
#' @source <https://github.com/sysilviakim/ranking_error>
"identity"

#' Identity-ranking data with estimated weights based on inverse probability weighting
#'
#' @description
#'
#' This data has two extra columns from the original `identity` data:
#' `w` and `ranking`, which are respectively the estimated weights based on
#' inverse probability weighting and the ranking pattern that the respondent
#' provided, united into a single column.
#'
#' @format ## `identity_w`
#' A data frame with 1,082 rows and 12 columns:
#' \describe{
#'   \item{w}{Estimated weight based on inverse probability weighting.}
#'   \item{ranking}{Ranking pattern that the respondent provided.}
#'   \item{app_party}{Marginal ranking for party (main identity ranking question).}
#'   \item{app_religion}{Marginal ranking for religion (main identity ranking question).}
#'   \item{app_gender}{Marginal ranking for gender (main identity ranking question).}
#'   \item{app_race}{Marginal ranking for race (main identity ranking question).}
#'   \item{anc_house}{Marginal ranking for household (anchor question).}
#'   \item{anc_neighborhood}{Marginal ranking for neighborhood (anchor question).}
#'   \item{anc_city}{Marginal ranking for city (anchor question).}
#'   \item{anc_state}{Marginal ranking for state (anchor question).}
#'   \item{anc_correct_identity}{Whether the respondent answered the anchor questions correctly. This is a binary variable that 1 if the respondent correctly answers the anchor ranking question and 0 if otherwise.}
#'   \item{s_weight}{Survey weight.}
#' }
#' @source <https://github.com/sysilviakim/ranking_error>
"identity_w"
