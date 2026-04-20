#' Identity-ranking data analyzed in Atsusaka and Kim (2025)
#'
#' @description
#'
#' Full dataset from survey on relative partisanship used in
#' Atsusaka, Yuki, & Kim, Seo-young Silvia (2025).
#' Addressing Measurement Errors in Ranking Questions for the Social Sciences.
#' Political Analysis, 33(4), 339-360.
#' https://doi.org/10.1017/pan.2024.33
#'
#' This data contains Americans' rankings of four sources of their identity,
#' including political party, religion, gender, and race, for
#' 1,082 respondents.
#' The columns consist of marginal rankings to the main identity ranking
#' question and the corresponding anchor question, as well as whether they have
#' answered the anchor questions "correctly." The anchor ranking question is
#' used to estimate the proportion of random responses and
#' to correct for measurement error bias.
#'
#' @format ## `identity`
#' A data frame with 1,082 rows and 16 columns:
#' \describe{
#'   \item{app_identity}{
#'     Full ranking profile for the main identity ranking question.
#'   }
#'   \item{app_identity_1}{
#'     Marginal ranking for party (main identity ranking question).
#'   }
#'   \item{app_identity_2}{
#'     Marginal ranking for religion (main identity ranking question).
#'   }
#'   \item{app_identity_3}{
#'     Marginal ranking for gender (main identity ranking question).
#'   }
#'   \item{app_identity_4}{
#'     Marginal ranking for race (main identity ranking question).
#'   }
#'   \item{anc_identity}{Full ranking profile for the anchor ranking question.}
#'   \item{anc_identity_1}{Marginal ranking for household (anchor question).}
#'   \item{anc_identity_2}{
#'     Marginal ranking for neighborhood (anchor question).
#'   }
#'   \item{anc_identity_3}{Marginal ranking for city (anchor question).}
#'   \item{anc_identity_4}{Marginal ranking for state (anchor question).}
#'   \item{anc_correct_identity}{
#'     Whether the respondent answered the anchor questions correctly.
#'     This is a binary variable that 1 if the respondent correctly answers
#'     the anchor ranking question and 0 if otherwise.
#'   }
#'   \item{app_identity_recorded}{
#'     Recorded responses for the main identity ranking question.
#'   }
#'   \item{anc_identity_recorded}{
#'     Recorded responses for the anchor ranking question.
#'   }
#'   \item{app_identity_row_rnd}{
#'     The order in which the items were randomly presented for the
#'     respondent in the main ranking question.
#'   }
#'   \item{anc_identity_row_rnd}{
#'     The order in which the items were randomly presented for the
#'     respondent in the anchor ranking question.
#'   }
#'   \item{s_weight}{Survey weight.}
#' }
#' @source <https://github.com/sysilviakim/ranking_error>
"identity"

#' Identity-ranking data with estimated weights based on inverse probability
#' weighting
#'
#' @description
#'
#' This data is the `results` element returned by applying
#' `imprr_weights()` to `identity`. It adds two columns to the original
#' `identity` data: `weights`, the estimated inverse probability weights, and
#' `ranking`, the pasted full ranking profile.
#'
#' @format ## `identity_w`
#' A data frame with 1,082 rows and 18 columns:
#' \describe{
#'   \item{weights}{Estimated weights based on inverse probability weighting.}
#'   \item{s_weight}{Survey weight.}
#'   \item{app_identity}{
#'     Full ranking profile for the main identity ranking question.
#'   }
#'   \item{app_identity_1}{
#'     Marginal ranking for party (main identity ranking question).
#'   }
#'   \item{app_identity_2}{
#'     Marginal ranking for religion (main identity ranking question).
#'   }
#'   \item{app_identity_3}{
#'     Marginal ranking for gender (main identity ranking question).
#'   }
#'   \item{app_identity_4}{
#'     Marginal ranking for race (main identity ranking question).
#'   }
#'   \item{anc_identity}{Full ranking profile for the anchor ranking question.}
#'   \item{anc_identity_1}{Marginal ranking for household (anchor question).}
#'   \item{anc_identity_2}{
#'     Marginal ranking for neighborhood (anchor question).
#'   }
#'   \item{anc_identity_3}{Marginal ranking for city (anchor question).}
#'   \item{anc_identity_4}{Marginal ranking for state (anchor question).}
#'   \item{anc_correct_identity}{
#'     Whether the respondent answered the anchor questions correctly.
#'     This is a binary variable that 1 if the respondent correctly answers
#'     the anchor ranking question and 0 if otherwise.
#'   }
#'   \item{app_identity_recorded}{
#'     Recorded responses for the main identity ranking question.
#'   }
#'   \item{anc_identity_recorded}{
#'     Recorded responses for the anchor ranking question.
#'   }
#'   \item{app_identity_row_rnd}{
#'     The order in which the items were randomly presented for the
#'     respondent in the main ranking question.
#'   }
#'   \item{anc_identity_row_rnd}{
#'     The order in which the items were randomly presented for the
#'     respondent in the anchor ranking question.
#'   }
#'   \item{ranking}{
#'     Pasted full ranking profile reconstructed from the marginal ranking
#'     columns. In this dataset, it matches \code{app_identity}.
#'   }
#' }
#' @source <https://github.com/sysilviakim/ranking_error>
"identity_w"
