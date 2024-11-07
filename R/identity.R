#' Identity-ranking data analyzed in Atsusaka and Kim (2024)
#'
#' A dataset that contains Americans' rankings of four sources of their identity,
#' including political party, religion, gender, and race.
#' The dataset also includes responses to an anchor ranking question that is used to
#' estimate the proportion of random responses and to correct for measurement error bis
#'
#' @docType data
#'
#' @usage data(identity)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{app_party}{Marginal rank of political party (double)}
#'  \item{app_religion}{Marginal rank of religion (double)}
#'  \item{app_gender}{Marginal rank of gender (double)}
#'  \item{app_race}{Marginal rank of race (double)}
#'  \item{anc_house}{Marginal rank of household}
#'  \item{anc_neighborhood}{Marginal rank of neighborhood}
#'  \item{anc_city}{Marginal rank of city}
#'  \item{anc_state}{Marginal rank of state}
#'  \item{anc_correct_identity}{A binary variable that takes 1 if the respondent correctly answers the anchor ranking question}
#'  \item{s_weight}{Survey weight}
#'
#' }
#' @references This data set was collected and analyzed by Atsusaka and Kim (2024) "Addressing Measurement Errors in Ranking Questions for the Social Sciences" Political Analysis (conditionally accepted).
#' @keywords datasets
#' @examples
#'
#' data(identity)
#' head(identity)
#'
"identity"
