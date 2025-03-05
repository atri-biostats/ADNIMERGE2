# NEUROBAT Scoring Function -----
#' @title Scoring Function for NEUROBAT Sub-Item
#' @description This function is used to compute the item-level subscores in the NEUROBAT eCRF.
#' @param dd Data frame of NEUROBAT eCRF
#' @return A data frame the same as `dd` with the following appended columns
#'  \itemize{
#'    \item{LIMMTOTAL }{Logical Memory - Immediate Recall Score: Between 0 and 25}
#'     \item{LDELTOTAL }{Logical Memory - Delayed Recall Score: Between 0 and 25}
#'     \item{DIGITSCOR }{?? Digit Symbol Substitution}
#'     \item{TRABSCOR }{Time to Complete Trail B Making Test Score}
#'     \item{RAVLTIM }{Rey Auditory Verbal Learning Test - Immediate Score: Sum of all five trials result}
#'     \item{RAVLTLEARN }{Rey Auditory Verbal Learning Test - Learning Score: Difference between the fifth and the first trial result}
#'     \item{RAVLTFORG }{Rey Auditory Verbal Learning Test - Forgetting Score: Difference between the fifth trial total score and delayed time}
#'     \item{RAVLTFORGOPERC }{Rey Auditory Verbal Learning Test - Forgetting Percentage Score: Relative percentage of forgetting score from the fifth trial result}
#'  }
#' @family Scoring Function
#' @rdname compute_neurobat_subscore
#' @importFrom dplyr relocate mutate
#' @importFrom assertr assert within_bounds verify
#' @importFrom tidyselect all_of last_col
#' @references \href{https://doi.org/10.1016/0887-6177(91)90001-P}{Powell, J.B., Cripe, L.I. and Dodrill, C.B., 1991. Assessment of brain impairment with the Rey Auditory Verbal Learning Test: A comparison with other neuropsychological measures. Archives of Clinical Neuropsychology, 6(4), pp.241-249.}
#' @export
compute_neurobat_subscore <- function(dd) {
  AVTOT5 <- DIGITSCOR <- LDELTOTAL <- LIMMTOTAL <- RAVLTFORG <- RAVLTFORGOPERC <- NULL
  RAVLTIM <- RAVLTLEARN <- TRABSCOR <- NULL
  col_names <- c(paste0("AVTOT", 1:5), "AVDEL30MIN", "LDELTOTAL", "DIGITSCOR", "TRABSCOR", "LIMMTOTAL")
  check_colnames(data = dd, col_names = col_names, strict = TRUE, stop_message = TRUE)
  dd <- dd %>%
    assert(is.numeric, all_of(col_names))

  dd$RAVLTIM <- rowSums(dd[, paste0("AVTOT", 1:5)], na.rm = FALSE)
  dd$RAVLTLEARN <- dd$AVTOT5 - dd$AVTOT1
  dd$RAVLTFORG <- dd$AVTOT5 - dd$AVDEL30MIN
  dd$RAVLTFORGOPERC <- 100 * dd$RAVLTFORG / dd$AVTOT5

  dd <- dd %>%
    mutate(RAVLTFORGOPERC = ifelse(AVTOT5 == 0, NA_real_, RAVLTFORGOPERC)) %>%
    relocate(LIMMTOTAL, RAVLTIM, RAVLTLEARN, RAVLTFORG, RAVLTFORGOPERC, .after = last_col()) %>%
    assert(within_bounds(0, 25), LIMMTOTAL, LDELTOTAL) %>%
    verify(min(TRABSCOR, na.rm = TRUE) >= 0) %>%
    verify(min(DIGITSCOR, na.rm = TRUE) >= 0) %>%
    assert(within_bounds(0, 15 * 5), RAVLTIM) %>%
    assert(within_bounds(-15, 15), RAVLTLEARN) # %>%
  # assert(within_bounds(), RAVLTFORG) %>%
  # assert(within_bounds(0, 100), RAVLTFORGOPERC)

  return(dd)
}
