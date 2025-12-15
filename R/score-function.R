# NEUROBAT Scoring Function -----
#' @title Scoring Function for NEUROBAT Sub-items
#' @description
#'  This function is used to compute item-level subscore in \code{\link{NEUROBAT}()} eCRF.
#'  Please see the \code{Value} section for item-level score description.
#' @param .neurobat Data.frame of \code{\link{NEUROBAT}()} eCRF
#' @return A data frame the same as \code{.neurobat} with the following appended columns
#'   \item{LIMMTOTL }{Logical Memory - Immediate Recall Score: Between 0 and 25}
#'   \item{LDELTOTL }{Logical Memory - Delayed Recall Score: Between 0 and 25}
#'   \item{DIGITSCR }{?? Digit Symbol Substitution}
#'   \item{TRABSCOR }{Time to Complete Trail B Making Test Score}
#'   \item{RAVLTIMM }{Rey Auditory Verbal Learning Test - Immediate Score: Sum of all five trials result}
#'   \item{RAVLTLRN }{Rey Auditory Verbal Learning Test - Learning Score: Difference between the fifth and the first trial result}
#'   \item{RAVLTFG }{Rey Auditory Verbal Learning Test - Forgetting Score: Difference between the fifth trial total score and delayed time}
#'   \item{RAVLTFGP }{Rey Auditory Verbal Learning Test - Forgetting Percentage Score: Relative percentage of forgetting score from the fifth trial result}
#' @rdname compute_neurobat_subscore
#' @family scoring functions
#' @keywords adni_scoring_fun
#' @importFrom dplyr relocate mutate
#' @importFrom assertr assert within_bounds verify
#' @importFrom tidyselect all_of last_col
#' @references [Powell, J.B., Cripe, L.I. and Dodrill, C.B., 1991. Assessment of brain impairment with the Rey Auditory Verbal Learning Test: A comparison with other neuropsychological measures. Archives of Clinical Neuropsychology, 6(4), pp.241-249.](https://doi.org/10.1016/0887-6177(91)90001-P)
#' @export
compute_neurobat_subscore <- function(.neurobat) {
  AVTOT5 <- DIGITSCR <- LDELTOTL <- LIMMTOTL <- RAVLTFG <- RAVLTFGP <- NULL
  RAVLTIMM <- RAVLTLRN <- TRABSCOR <- LIMMTOTAL <- LDELTOTAL <- DIGITSCOR <- NULL
  col_names <- c(paste0("AVTOT", 1:5), "AVDEL30MIN", "LDELTOTAL", "DIGITSCOR", "TRABSCOR", "LIMMTOTAL")
  check_colnames(.data = .neurobat, col_names = col_names, strict = TRUE, stop_message = TRUE)
  dd <- .neurobat %>%
    assert(is.numeric, all_of(col_names))

  dd$RAVLTIMM <- rowSums(dd[, paste0("AVTOT", 1:5)], na.rm = FALSE)
  dd$RAVLTLRN <- dd$AVTOT5 - dd$AVTOT1
  dd$RAVLTFG <- dd$AVTOT5 - dd$AVDEL30MIN
  dd$RAVLTFGP <- round(100 * dd$RAVLTFG / dd$AVTOT5, 2)

  dd <- dd %>%
    mutate(
      RAVLTFGP = ifelse(AVTOT5 == 0, NA_real_, RAVLTFGP),
      LIMMTOTL = LIMMTOTAL,
      LDELTOTL = LDELTOTAL,
      DIGITSCR = DIGITSCOR
    ) %>%
    relocate(LIMMTOTL, RAVLTIMM, RAVLTLRN, RAVLTFG, RAVLTFGP, .after = last_col()) %>%
    assert(within_bounds(0, 25), LIMMTOTL, LDELTOTL) %>%
    verify(min(TRABSCOR, na.rm = TRUE) >= 0) %>%
    verify(min(DIGITSCR, na.rm = TRUE) >= 0) %>%
    assert(within_bounds(0, 15 * 5), RAVLTIMM) %>%
    assert(within_bounds(-15, 15), RAVLTLRN) # %>%
  # assert(within_bounds(), RAVLTFG) %>%
  # assert(within_bounds(0, 100), RAVLTFGP)

  return(dd)
}
