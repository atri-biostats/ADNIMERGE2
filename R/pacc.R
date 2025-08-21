# Compute PACC score ----

#' @title Generate ADNI modified versions of the Preclinical Alzheimer's Cognitive Composite (PACC)
#'
#' @description
#' The Preclinical Alzheimer's Cognitive Composite (\href{https://doi.org/10.1001/jamaneurol.2014.803}{PACC})
#' is a baseline standardized composite of
#'
#' \itemize{
#'   \item Free and Cued Selective Reminding Test (FCSRT)
#'   \item Logical Memory IIa Delayed Recall (LM)
#'   \item Digit Symbol Substitution Test (DSST)
#'   \item Mini-Mental State Examination (MMSE)
#' }
#'
#' @details
#'
#' This function generates two modified versions of PACC scores based on ADNI data. FCSRT is not used in ADNI, so we use the
#' Delayed Recall portion of the Alzheimer's Disease Assessment Scale (ADAS) as a proxy. Score \code{mPACCdigit} uses the
#' DSST when available (ADNI1). \code{mPACCtrailsB} uses (log transformed) Trails B as a proxy for DSST. Raw component scores
#' standardized according to the mean and standard deviation of baseline scores of ADNI subjects with normal cognition
#' to create \code{Z} scores for each component \code{(Z=(raw - mean(raw.bl))/sd(raw.bl))}.
#' The Z scores are reoriented if necessary so that greater scores reflect better performance.
#' The composite is the sum of these Z scores.
#'
#' \strong{Missing components:} At least two components must be present to produce a score.
#' If more than two components are missing, the PACC will be \code{NA}.
#'
#' @param .data_wide Data.frame in wide format contains the PACC component score
#'
#' @param bl.summary Baseline component score summary
#'
#'  It can be created either using `get_baseline_score_summary` function or
#'  a component score summary at baseline visit grouped by baseline diagnostics summary.
#'  See \code{\link{get_score_summary_stats}()}
#'
#' @param componentVars Character vector of component score variable names, Default: c("ADASQ4", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR")
#'
#' The component score variable names should be arranged based the following order.
#' Otherwise, an \strong{invalid} total score will be calculated.
#'
#' \itemize{
#'   \item Delayed Recall portion from `ADAS`, can be named as `ADASQ4`
#'   \item Mini-Mental State Examination Score, can be named as `MMSE`
#'   \item Logical Memory IIa Delayed Recall Score, can be named as `LDELTOTL`
#'   \item Digit Symbol Substitution Test Score from `NEUROBAT`, can be named as `DIGITSCR`
#'   \item Trails B Score from `NEUROBAT`, can be named as `TRABSCOR`
#' }
#'
#' @param rescale_trialB A Boolean value to change the `Trails B` score in log scale, Default: TRUE
#'
#' @param keepComponents A Boolean to keep component score, Default: FALSE
#'
#' @param wideFormat A Boolean value whether the data.frame is in \emph{wide} or \emph{long} format, Default: TRUE
#'
#' @param varName Column name that contain the component score names for long format data, Default = NULL
#' Only applicable for long format data and it should be not missing if `wideFormat = FALSE`.
#'
#' @param scoreCol Variable names that contains component score/numeric value for long format data, Default = NULL
#' Only applicable for long format data and it should be not missing if `wideFormat = FALSE`.
#'
#' @param idCols Character vector of ID columns for long format data, Default: NULL
#' Only applicable for long format data and it should be not missing if `wideFormat = FALSE`.
#'
#' @return
#'
#' The \code{data.frame} with appended columns for \code{mPACCdigit} and \code{mPACCtrailsB} for wide format input data.
#' The \code{data.frame} with additional rows of \code{mPACCdigit} and \code{mPACCtrailsB} for long format input data.
#'
#' @references
#' \itemize{
#'   \item Donohue MC, et al. The Preclinical Alzheimer Cognitive Composite: Measuring Amyloid-Related Decline. \emph{JAMA Neurol}. 2014;71(8):961–970. doi:10.1001/jamaneurol.2014.803 \url{http://dx.doi.org/10.1001/jamaneurol.2014.803}
#'   \item Donohue MC, Sperling RA, Petersen R, Sun C, Weiner MW, Aisen PS, for the Alzheimer’s Disease Neuroimaging Initiative. Association Between Elevated Brain Amyloid and Subsequent Cognitive Decline Among Cognitively Normal Persons. \emph{JAMA}. 2017;317(22):2305–2316. \url{http://dx.doi.org/10.1001/jama.2017.6669}
#' }
#'
#' @author Michael Donohue \email{mdonohue@@usc.edu}
#'
#' @examples
#' \dontrun{
#' library(nlme)
#' library(dplyr)
#' library(multcomp)
#' library(Hmisc)
#' library(ADNIMERGE)
#'
#' csf2numeric <- function(x) {
#'   as.numeric(gsub("<", "", gsub(">", "", x)))
#' }
#'
#' dd <- subset(adnimerge, DX.bl %in% c("CN", "SMC") & !is.na(mPACCtrailsB)) %>%
#'   mutate(
#'     ABETA = csf2numeric(ABETA)
#'   )
#'
#' # identify those with elevated PIB PET at ANY visit OR
#' # elevated BASELINE AV45 PET OR elevated BASELINE FBB PET OR
#' # low BASELINE CSF Abeta
#' # AV45 ~ PIB regression from Landau et al 2012:
#' elevatedAmyloid <- unique(c(
#'   subset(dd, 0.67 * PIB + 0.15 > 1.11)$RID,
#'   subset(dd, VISCODE == "bl" & AV45 > 1.11)$RID,
#'   subset(dd, VISCODE == "bl" & FBB > 1.08)$RID,
#'   subset(dd, VISCODE == "bl" & ABETA < 900)$RID
#' ))
#' anyAmyloid <- unique(subset(dd, !is.na(AV45.bl) | !is.na(PIB) | !is.na(FBB.bl) | !is.na(ABETA.bl))$RID)
#'
#' dd <- dd %>%
#'   mutate(
#'     ElevatedAmyloid = ifelse(RID %in% elevatedAmyloid, 1,
#'       ifelse(RID %in% anyAmyloid, 0, NA)
#'     ),
#'     m = Month.bl,
#'     m2 = Month.bl^2,
#'     APOEe4 = APOE4 > 0
#'   )
#'
#' summary(
#'   ElevatedAmyloid ~ APOEe4 + AGE + PTEDUCAT,
#'   data = dd,
#'   subset = VISCODE == "bl",
#'   method = "reverse",
#'   overall = TRUE
#' )
#'
#' # Quadratic time model:
#' fit <- lme(
#'   fixed = mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT + m + m2 + (m + m2):ElevatedAmyloid,
#'   random = ~ m | RID,
#'   data = dd,
#'   na.action = na.omit
#' )
#'
#' Months <- seq(12, 96, 12)
#' elevated.design <- model.matrix(
#'   mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT + m + m2 + (m + m2):ElevatedAmyloid,
#'   data = data.frame(
#'     mPACCtrailsB = 0, mPACCtrailsB.bl = 0, APOEe4 = TRUE, AGE = 75,
#'     PTEDUCAT = 12, ElevatedAmyloid = 1, m = Months, m2 = Months^2
#'   )
#' )
#'
#' normal.design <- model.matrix(
#'   mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT + m + m2 + (m + m2):ElevatedAmyloid,
#'   data = data.frame(
#'     mPACCtrailsB = 0, mPACCtrailsB.bl = 0, APOEe4 = TRUE, AGE = 75,
#'     PTEDUCAT = 12, ElevatedAmyloid = 0, m = Months, m2 = Months^2
#'   )
#' )
#'
#' contrast.data <- elevated.design - normal.design
#' summary(multcomp::glht(fit, linfct = contrast.data))
#' }
#' @seealso
#'  \code{\link[cli]{cli_abort}}
#' @rdname compute_pacc_score
#' @family scoring function
#' @keywords adni_scoring_fun pacc_score_utils_fun
#' @export
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom dplyr mutate across select relocate pivot_wider pivot_longer bind_rows
#' @importFrom tidyselect all_of any_of ends_with contains last_col

compute_pacc_score <- function(.data,
                               bl.summary,
                               componentVars = c("ADASQ4", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR"),
                               rescale_trialB = FALSE,
                               keepComponents = FALSE,
                               wideFormat = TRUE,
                               varName = NULL,
                               scoreCol = NULL,
                               idCols = NULL) {
  require(tidyverse)

  mPACCdigit <- mPACCtrailsB <- NULL
  check_is_logical(keepComponents)
  check_is_logical(rescale_trialB)
  check_is_logical(wideFormat)

  var_names <- componentVars
  if (length(var_names) != 5) {
    cli::cli_abort(
      message = c(
        "The length of {.var var_name} must be 7. \n",
        "{.val {var_name}} are provided"
      )
    )
  }

  if (!all(var_names %in% bl.summary$VAR)) {
    cli::cli_abort(
      message = c(
        "All {.var componentVars} not found in {.var bl.summary$VAR}. \n",
        "Component variables are: {.val {componentVars}}, and \n",
        "The baseline summary data {.var bl.summary} contains: {.val {unique(bl.summary$VAR)}} variable(s)."
      )
    )
  }

  if (!wideFormat) {
    check_non_missing_value(varName)
    check_non_missing_value(scoreCol)
    check_non_missing_value(idCols)
    check_colnames(
      .data = .data,
      col_names = c(varName, scoreCol, idCols),
      stop_message = TRUE,
      strict = TRUE
    )
  }

  check_colnames(
    .data = bl.summary,
    col_names = c("VAR", "MEAN", "SD"),
    stop_message = TRUE,
    strict = TRUE
  )

  # Change long format data into wide format
  if (!wideFormat) {
    .data_wide <- .data %>%
      pivot_wider(
        id_cols = all_of(idCols),
        names_from = all_of(varName),
        values_from = all_of(scoreCol)
      ) %>%
      select(all_of(c(idCols, var_names)))
  } else {
    .data_wide <- .data
  }

  check_colnames(
    .data = .data_wide,
    col_names = var_names,
    stop_message = TRUE,
    strict = TRUE
  )

  # Log transformed Trial B score
  if (!rescale_trialB) {
    trailB_score <- .data_wide %>%
      select(all_of(var_names[5])) %>%
      pull()

    if (any(trailB_score < 0)) {
      cli::cli_abort(
        message = c(
          "{.var trailB_score} represents the Trial B score. \n",
          "{.var trailB_score} must not contains any negative values for log rescale/transformation. \n",
          "Did you set {.var rescale_trialB} to {.val {FALSE}}?"
        )
      )
    }
  }

  .data_wide <- .data_wide %>%
    {
      if (rescale_trialB) {
        # Create log transformation for Trial B Scores
        mutate(., across(all_of(var_names[5]), ~ log(.x + 1), .names = "LOG.{col}"))
      } else {
        (.)
      }
    }

  if (rescale_trialB) {
    var_names[5] <- paste0("LOG.", var_names[5])
  }

  # Check for any pre-existing standardized variables
  check.zscore_var <- .data_wide %>%
    select(any_of(ends_with(".zscore"))) %>%
    colnames()
  check.zscore_var <- check.zscore_var[check.zscore_var %in% paste0(var_names, ".zscore")]
  if (length(check.zscore_var) != 0) {
    cli::cli_alert_warning(
      message = c(
        "{var .data_wide} must not contains pre-existed {.val {paste0(var_names, '.zscore')}} variables. \n",
        "Caution: these variables will be overwriting! \n",
        "{var .data_wide} contains pre-existed {.val {check.zscore_var}} variable(s)."
      )
    )
  }

  # Normalized item-level score by corresponding baseline score among cognitive normal subjects
  .data_wide <- .data_wide %>%
    mutate(across(all_of(var_names),
      ~ {
        # Adjust for LOG.trialB score
        if (rescale_trialB) {
          col_name <- gsub("LOG\\.", "", cur_column())
        } else {
          col_name <- cur_column()
        }
        normalize_var_by_baseline_score(x = .x, baseline_summary = bl.summary, varName = col_name)
      },
      .names = "{col}.zscore"
    )) %>%
    # Adjust direction
    mutate(across(all_of(paste0(var_names[c(1, 5)], ".zscore")), ~ -.x))

  # check that all measure are positively correlated:
  corTest <- .data_wide %>%
    select(all_of(paste0(var_names, ".zscore"))) %>%
    cor(., use = "pairwise.complete.obs")

  if (any(corTest < 0)) {
    cli::cli_abort(
      message = "Some PACC z scores are negatively correlated!"
    )
  }
  # # visualization/plot checks
  # GGally::ggpairs(
  #   data = .data_wide %>%
  #     select(all_of(ends_with(".zscore")))
  # )

  compscore <- function(x, n.components = 4, n.missing = 2) {
    ifelse(sum(is.na(x)) > n.missing, NA, mean(x, na.rm = TRUE)) * n.components
  }

  .data_wide$mPACCdigit <- apply(.data_wide[, paste0(var_names[-5], ".zscore")], 1, compscore)
  .data_wide$mPACCtrailsB <- apply(.data_wide[, paste0(var_names[-4], ".zscore")], 1, compscore)

  .data_wide <- .data_wide %>%
    relocate(all_of(c("mPACCdigit", "mPACCtrailsB")), .after = last_col()) %>%
    {
      if (!keepComponents) {
        select(., -all_of(paste0(var_names, ".zscore")))
      } else {
        (.)
      }
    }

  if (!wideFormat) {
    .data_long <- .data_wide %>%
      pivot_longer(
        cols = -all_of(idCols),
        names_to = varName,
        values_to = scoreCol
      )

    output_data <- .data %>%
      bind_rows(.data_long)
  }

  if (wideFormat) {
    output_data <- .data_wide
  }

  output_data <- as_tibble(output_data)

  return(output_data)
}

# Get summary statistic -----
#' @title Get Grouped Score/Numeric Summary Stats
#'
#' @description
#'  This function is used to get the numeric variable summary statistic grouped by certain variable.
#'
#' @param .data Data.frame
#'
#' @param wideFormat A Boolean value whether the data.frame is in \emph{wide} or \emph{long} format, Default: TRUE
#'
#' @param scoreVar Character vector of variable(s) that contain the actual score/numeric values,
#' Default: c("ADASQ4", "LDELTOTL", "DIGITSCR", "LOG.TRABSCOR", "MMSE")
#'
#' For long format data, `scoreVar` should be a length of one character vector
#' of variable that contains the score/numeric values.
#'
#' @param groupVar1 Additional grouping variable, only applicable for \emph{long} format data.frame.
#'
#' @param groupVar Group variable, Default: 'DX'
#'
#' @param filterGroup Filter value of group variable `groupVar`, Default: NULL
#' Only applicable if the `groupVar` is a length of one character vector.
#'
#' @return A data.frame with the following columns:
#'
#' \itemsize{
#'   \item \code{groupVar}: Grouping variable
#'   \item VAR: Score/numeric variable name
#'   \item N: Number of non-missing observation
#'   \item MEAN: Mean score
#'   \item SD: Standard deviation value
#' }
#'
#' @details
#'  All computed summary statistic are based on non-missing observation.
#'  The result summary will be filter by the corresponding `filterGroup` value(s).
#'
#' @examples
#' \dontrun{
#' # For long format data
#' # Suppose we wanted to compute the baseline summary score of
#' # all available assessments in `ADNIMERGE2::ADQS`
#'
#' # By baseline diagnosis status.
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' long_format_example <- ADNIMERGE2::ADQS %>%
#'   filter(ABLFL %in% "Y") %>%
#'   #  Check there is only one baseline record per assessment type per subject
#'   ADNIMERGE2::assert_uniq(USUBJID, PARAMCD)
#'
#' get_score_summary_stats(
#'   .data = long_format_example,
#'   wideFormat = FALSE,
#'   scoreVar = "AVAL",
#'   groupVar1 = "PARAMCD",
#'   groupVar = "DX",
#'   filterGroup = NULL
#' )
#'
#' # For only cognitive normal (CN) subjects
#' get_score_summary_stats(
#'   .data = long_format_example,
#'   wideFormat = FALSE,
#'   scoreVar = "AVAL",
#'   groupVar1 = "PARAMCD",
#'   groupVar = "DX",
#'   filterGroup = "CN"
#' )
#'
#' # For wide format data
#' # Suppose we wanted to compute the baseline summary statistic of `AGE`, `BMI`, `ADASTT11` and `ADASTT13`
#' wide_format_example <- ADNIMERGE2::ADSL %>%
#'   filter(ENRLFL %in% "Y")
#'
#' # By baseline diagnostics status
#' get_score_summary_stats(
#'   .data = wide_format_example,
#'   wideFormat = TRUE,
#'   scoreVar = c("AGE", "BMI", "ADASTT11", "ADASTT13"),
#'   groupVar1 = NULL,
#'   groupVar = "DX",
#'   filterGroup = NULL
#' )
#'
#'
#' get_score_summary_stats(
#'   .data = wide_format_example,
#'   wideFormat = TRUE,
#'   scoreVar = c("AGE", "BMI", "ADASTT11", "ADASTT13"),
#'   groupVar1 = NULL,
#'   groupVar = "DX",
#'   filterGroup = "CN"
#' )
#'
#' # By SEX
#' get_score_summary_stats(
#'   .data = wide_format_example,
#'   wideFormat = TRUE,
#'   scoreVar = c("AGE", "BMI", "ADASTT11", "ADASTT13"),
#'   groupVar1 = NULL,
#'   groupVar = "SEX",
#'   filterGroup = NULL
#' )
#' }
#' @seealso
#'  \code{\link{get_baseline_score_summary_stats}()}
#' @rdname get_score_summary_stats
#' @keywords pacc_score_utils_fun
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter if_all pivot_longer group_by across ungroup if_any select mutate
#' @importFrom tidyselect all_of

get_score_summary_stats <- function(.data,
                                    wideFormat = TRUE,
                                    scoreVar = c("ADASQ4", "LDELTOTL", "DIGITSCR", "LOG.TRABSCOR", "MMSE"),
                                    groupVar1 = NULL,
                                    groupVar = "DX",
                                    filterGroup = NULL) {
  require(tidyverse)
  N <- MEAN <- SD <- NULL
  check_is_logical(wideFormat)
  if (wideFormat) {
    check_colnames(
      .data = .data,
      col_names = c(scoreVar, groupVar),
      strict = TRUE,
      stop_message = TRUE
    )
  }
  # For long format data
  if (!wideFormat) {
    check_non_missing_value(scoreVar)
    check_non_missing_value(groupVar1)
    check_colnames(
      .data = .data,
      col_names = c(scoreVar, groupVar1, groupVar),
      strict = TRUE,
      stop_message = TRUE
    )
  }

  .data <- .data %>%
    as_tibble() %>%
    {
      if (wideFormat) {
        # Change into a long format
        pivot_longer(
          .,
          cols = all_of(scoreVar),
          names_to = "VAR",
          values_to = "SCORE"
        )
      } else {
        mutate(., across(all_of(groupVar1), ~., .names = "VAR")) %>%
          mutate(., across(all_of(scoreVar), ~., .names = "SCORE"))
      }
    }

  score_summary <- .data %>%
    group_by(across(all_of(c(groupVar, "VAR")))) %>%
    dplyr::summarize(
      N = sum(!is.na(SCORE)),
      MEAN = mean(SCORE, na.rm = TRUE),
      SD = sd(SCORE, na.rm = TRUE)
    ) %>%
    ungroup()

  if (!is.null(filterGroup)) {
    if (length(groupVar) != 1) {
      cli::cli_abort(
        message = c(
          "{.var groupVar} must be a length of one character vector. \n",
          "{.var groupVar} contains {.val {groupVar}} variables."
        )
      )
    }
    score_summary <- score_summary %>%
      filter(if_any(.cols = all_of(groupVar), ~ .x %in% filterGroup))
  }

  score_summary <- score_summary %>%
    select(all_of(c(groupVar, "VAR", "N", "MEAN", "SD")))

  return(score_summary)
}

#' @title Get Baseline Grouped Score/Numeric Summary Stats
#'
#' @description This function is used to get baseline grouped summary statistic of numeric/score variable(s).
#'
#' @inheritParams get_score_summary_stats
#'
#' @param filterBy Character vector of baseline record identifier variable
#'
#' @param filterValue Baseline record identifier values, Default: c("Y", "Yes", "bl")
#'
#' @param ... \code{\link[assertr]{get_score_summary_stats}} arguments
#'
#' @return Similar to \code{\link[assertr]{get_score_summary_stats}} result
#'
#' @examples
#' \dontrun{
#' # For long format data
#' # Suppose we wanted to compute the baseline summary score of
#' # all available assessments in `ADNIMERGE2::ADQS` by baseline diagnosis status.
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' get_baseline_score_summary_stats(
#'   .data = ADNIMERGE2::ADQS,
#'   filterBy = "ABLFL",
#'   filterValue = "Y",
#'   wideFormat = FALSE,
#'   scoreVar = "AVAL",
#'   groupVar1 = "PARAMCD",
#'   groupVar = "DX",
#'   filterGroup = NULL
#' )
#'
#' get_baseline_score_summary_stats(
#'   .data = ADNIMERGE2::ADQS,
#'   filterBy = "ABLFL",
#'   filterValue = "Y",
#'   wideFormat = FALSE,
#'   scoreVar = "AVAL",
#'   groupVar1 = "PARAMCD",
#'   groupVar = "DX",
#'   filterGroup = "CN"
#' )
#' }
#' @seealso
#'  \code{\link{get_score_summary_stats}()}
#' @rdname get_baseline_score_summary_stats
#' @keywords pacc_score_utils_fun
#' @export
#' @importFrom cli cli_abort
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter if_all
#' @importFrom tidyselect all_of

get_baseline_score_summary_stats <- function(.data, filterBy, filterValue = c("Y", "Yes", "bl"), ...) {
  if (length(filterBy) != 1) {
    cli::cli_abort(
      message = c(
        "{.var filterBy} must be a length of one character vector. \n",
        "{.var filterBy} contains {.val {filterBy}} variables."
      )
    )
  }
  check_colnames(
    .data = .data,
    col_names = filterBy,
    strict = TRUE,
    stop_message = TRUE
  )

  .data <- .data %>%
    as_tibble() %>%
    filter(if_all(all_of(filterBy), ~ .x %in% filterValue))

  bl_summary <- get_score_summary_stats(.data = .data, ...)

  return(bl_summary)
}

# Standardize/Normalize values ----
#' @title Standardize/normalize numeric/score value by baseline summary
#'
#' @param x Numeric/score value
#'
#' @param baseline_summary A data.frame of baseline score summary.
#'  `baseline_summary` should contains the following variables:
#'
#' \itemsize{
#'   \item VAR: Contains variable names
#'   \item MEAN: Mean value
#'   \item SD: Standard deviation value
#' }
#' The `baseline_summary` can be generated using get_baseline_score_summary function.
#'
#' @param varName Variable name
#'
#' @return A numeric vector
#'
#' @examples
#' \dontrun{
#' # Suppose we wanted to standardize/normalize ADASTT13 scores in `ADNIMERGE2:ADQS` by
#' # baseline summary score of enrolled `CN` subjects
#' library(tidyverse)
#' library(assertr)
#' library(ADNIMERGE2)
#'
#' bl.summary <- ADNIMERGE2::ADSL %>%
#'   get_baseline_score_summary_stats(
#'     .data = ADNIMERGE2::ADSL,
#'     filterBy = "ENRLFL",
#'     filterValue = "Y",
#'     wideFormat = TRUE,
#'     scoreVar = "ADASTT13",
#'     groupVar = "DX",
#'     filterGroup = "CN"
#'   )
#'
#' # Using numeric vector
#' example_data1 <- ADNIMERGE2::ADQS %>%
#'   filter(PARAMCD %in% "ADASTT13")
#' rsample_adas13 <- example_data1$AVAL
#' rsample_adas13 <- sample(rsample_adas13, size = 100)
#' normalize_var_by_baseline_score(
#'   x = rsample_adas13,
#'   baseline_summary = bl.summary,
#'   varName = NULL
#' )
#'
#' # Using data.frame format
#'
#' example_data2 <- ADNIMERGE2::ADQS %>%
#'   mutate(across(AVAL, ~ normalize_var_by_baseline_score(x = .x, baseline_summary = bl.summary, varName = "ADASTT13"),
#'     .names = "{col}.zscore"
#'   ))
#'
#' example_data2 %>%
#'   group_by(PARAMCD, !is.na(AVAL), !is.na(AVAL.zscore)) %>%
#'   count() %>%
#'   ungroup()
#'
#' library(ggplot2)
#'
#' example_data2 %>%
#'   filter(PARAMCD %in% "ADASTT13") %>%
#'   pivot_longer(
#'     cols = c("AVAL", "AVAL.zscore"),
#'     names_to = "SOURCE",
#'     values_to = "VALUE"
#'   ) %>%
#'   ggplot(aes(x = VALUE)) +
#'   geom_histogram() +
#'   facet_wrap(~SOURCE)
#' }
#' @rdname normalize_var_by_baseline_score
#' @keywords pacc_score_utils_fun
#' @family Utility functions
#' @importFrom dplyr filter nrow
#' @importFrom tibble as_tibble
#' @importFrom assertr verify

normalize_var_by_baseline_score <- function(x, baseline_summary, varName = NULL) {
  VAR <- MEAN <- SD <- NULL
  check_is_data.frame(baseline_summary)
  col_names <- c("MEAN", "SD")
  if (!is.null(varName)) {
    col_names <- c("VAR", col_names)
  }
  col_names <-
    check_colnames(
      .data = baseline_summary,
      col_names = col_names,
      stop_message = TRUE,
      strict = TRUE
    )

  baseline_summary <- baseline_summary %>%
    as_tibble() %>%
    {
      if (!is.null(varName)) {
        filter(., VAR %in% varName)
      } else {
        (.)
      }
    } %>%
    verify(nrow(.) <= 1)

  x <- compute_zscore(
    x = x,
    mean = baseline_summary$MEAN,
    sd = baseline_summary$SD
  )

  return(x)
}

#' @title Calculate Standardized Z-Score
#' @param x Numeric value
#' @param mean Mean value
#' @param sd Standard deviation value
#' @return A numeric vector
#' @examples
#' \dontrun{
#' mean <- 10
#' sd <- 2
#' x <- rnorm(n = 100, mean = mean, sd = sd)
#' compute_zscore(x = x, mean = mean, sd = sd)
#' compute_zscore(x = x, mean = 5, sd = 1.5)
#' compute_zscore(x = c(1, 0.5, NA, 3), mean = 5, sd = 1.5)
#' }
#' @rdname compute_zscore
#' @family utility functions
#' @keywords utils_fun

compute_zscore <- function(x, mean, sd) {
  if (!is.numeric(x) & any(!is.na(x))) {
    cli::cli_abort(
      message = c(
        "{.var x} must be a numeric object. \n",
        "{.var x} is a {.cls {class(x)}} object."
      )
    )
  }
  x <- (x - as.numeric(mean)) / as.numeric(sd)
  return(x)
}

# Utility functions ----

#' @title Is a data.frame object?
#' @param x Object
#' @return A stop error if the object is not a data.frame class
#' @examples
#' \dontrun{
#' mean <- 10
#' sd <- 2
#' x <- rnorm(n = 100, mean = 5, sd = 2)
#' df <- tibble(random_sample = x)
#' check_is_data.frame(x = x)
#' check_is_data.frame(x = df)
#' }
#' @rdname check_is_data.frame
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort

check_is_data.frame <- function(x) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      message = c(
        "{.var x} must be a data.frame object. \n",
        "{.var x} is a {.cls {class(x)}} object."
      )
    )
  }
  invisible(x)
}

#' @title Check for non-missing value
#' @param x Input value
#' @return A stop error if the value is missing value \emph(NULL)
#' @examples
#' \dontrun{
#' check_non_missing_value(x = LETTERS[1:10])
#' check_non_missing_value(x = NULL)
#' }
#' @rdname check_non_missing_value
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort

check_non_missing_value <- function(x) {
  if (is.null(x)) {
    cli::cli_abort(
      message = c(
        "{.var x} must not be missing value."
      )
    )
  }
  invisible(x)
}


## Get variable common date -----
#' @title Get Variables Common Date
#'
#' @description
#'  This function is used to get a common value across multiple date columns and compared
#'  with reference date column if it is provided.
#'
#' @param .data A wide format data.frame
#'
#' @param date_cols Character vector of date column names
#'
#' @param select_method
#' Selection method if there is more than one unique non-missing date, Default: 'min'
#' Either the minimum date `'min`, or the maximum date `'max'`.
#'
#' @param compared_ref_date A Boolean to compared the common date with the reference date if it is provided, Default: FALSE
#'
#' @param ref_date_col Reference date column name, Default: NULL
#'
#' @param preferred_date_col
#' Preferred date when common date and reference date are different.
#' Only applicable if `compared_ref_date` is `TRUE`.
#'
#' @return A data.frame with the appended columns:
#' \itemsiz{
#'  \item COMMON_DATE: Common date among the provided date columns
#'  \item FINAL_DATE: Final date after comparing with reference date if `compared_ref_date` is `TRUE`. Otherwise the same as `COMMON_DATE`
#'  \item DATE_RECORD_TYPE: Record type to identify whether the date columns are the same across row or not
#'  }
#'
#' @details
#'  The comparison algorithm is based on rowwise operation and presented as follow:
#'
#'  For records that have at least one non-missing date columns based on the list of provided date columns:
#'  \itemsiz{
#'    \item If all date columns are the same/equal, then it select one unique date values.
#'    \item If at least one date column is differ from the other columns, then it select either the minimum or maximum date based on the selection method (`select_method`)
#'  }
#'
#' The reference date column should be present for any comparison with reference date.
#'
#' Otherwise, the date will considered as missing.
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @rdname get_vars_common_date
#'
#' @export
#' @importFrom rlang arg_match0
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate row_number filter group_by ungroup n_distinct distinct left_join case_when select
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of

get_vars_common_date <- function(.data,
                                 date_cols,
                                 select_method = "min",
                                 compared_ref_date = FALSE,
                                 ref_date_col = NULL,
                                 preferred_date_col = NULL) {
  require(tidyverse)

  TEMP_ID <- DATE_COLS <- DATES <- ALL_SAME_DATE_SATUS <- NUM_RECORDS <- NULL
  COMMON_DATE <- DATE_RECORD_TYPE <- REF_DATE_COL <- FINAL_DATE <- NULL

  rlang::arg_match0(arg = select_method, values = c("min", "max"))
  check_is_logical(compared_ref_date)

  # For records that have at least one non-missing date
  .data <- .data %>%
    as_tibble() %>%
    mutate(TEMP_ID = row_number())

  .data_long <- .data %>%
    pivot_longer(
      cols = all_of(date_cols),
      names_to = "DATE_COLS",
      values_to = "DATES"
    ) %>%
    mutate(DATES = as.Date(DATES)) %>%
    filter(!is.na(DATES)) %>%
    # Check for similar values
    group_by(TEMP_ID) %>%
    mutate(ALL_SAME_DATE_SATUS = n_distinct(DATES) == 1) %>%
    ungroup()

  ## If all dates are the same/equal
  .data_long_equal <- .data_long %>%
    filter(ALL_SAME_DATE_SATUS == TRUE) %>%
    group_by(TEMP_ID) %>%
    mutate(
      COMMON_DATE = unique(DATES),
      DATE_RECORD_TYPE = "Same"
    ) %>%
    ungroup() %>%
    distinct(TEMP_ID, COMMON_DATE, DATE_RECORD_TYPE)

  ## For any non-unique dates
  .data_long_unequal <- .data_long %>%
    filter(ALL_SAME_DATE_SATUS == FALSE) %>%
    group_by(TEMP_ID) %>%
    {
      if (select_method %in% "max") {
        mutate(., COMMON_DATE = min(DATES))
      } else {
        mutate(., COMMON_DATE = max(DATES))
      }
    } %>%
    mutate(NUM_RECORDS = n()) %>%
    ungroup() %>%
    mutate(DATE_RECORD_TYPE = case_when(NUM_RECORDS >= 1 ~ "Unequal")) %>%
    distinct(TEMP_ID, COMMON_DATE, DATE_RECORD_TYPE)

  .data_combined_date <- bind_rows(.data_long_equal, .data_long_unequal)

  output_data <- .data %>%
    left_join(.data_combined_date,
      by = "TEMP_ID"
    ) %>%
    assert_uniq(TEMP_ID)

  # Comparison with reference date column
  if (!compared_ref_date) ref_date_col <- NULL
  if (compared_ref_date) {
    if (is.null(ref_date_col)) {
      cli::cli_abort(
        message = c(
          "{.var ref_date_col} must not be missing!\n",
          "Otherwise {.var compared_ref_date} must be {.val {FALSE}} value."
        )
      )
    }

    check_non_missing_value(preferred_date_col)
    rlang::arg_match0(arg = preferred_date_col, values = c("COMMON_DATE", "REF_DATE_COL"))

    output_data <- output_data %>%
      mutate(REF_DATE_COL = as.Date(get(ref_date_col))) %>%
      mutate(
        FINAL_DATE = case_when(
          REF_DATE_COL == COMMON_DATE ~ COMMON_DATE,
          !is.na(REF_DATE_COL) & is.na(COMMON_DATE) ~ REF_DATE_COL,
          is.na(REF_DATE_COL) & !is.na(COMMON_DATE) ~ COMMON_DATE,
          !is.na(REF_DATE_COL) & !is.na(COMMON_DATE) & REF_DATE_COL != COMMON_DATE ~ get(preferred_date_col)
        )
      )
  } else {
    output_data <- output_data %>%
      mutate(FINAL_DATE = COMMON_DATE)
  }

  output_data <- output_data %>%
    select(-TEMP_ID)

  return(output_data)
}

# Rename columns, convert into character type and tibble object ----
#' @title Make Similar Format
#' @param .data Data.frame
#' @return A tibble/data.frame object with upper-case column names and character type.
#' @rdname set_as_tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename_with mutate across
#' @importFrom tidyselect everything
#' @export

set_as_tibble <- function(.data) {
  require(tidyverse)
  .data <- .data %>%
    as_tibble() %>%
    rename_with(~ toupper(.x), everything()) %>%
    mutate(across(everything(), as.character))
  return(.data)
}

#
#' @title Carrying Forward Screening Record as Baseline Record
#'
#' @description
#' This function is used to carry forward screening record as baseline record per
#' subject id (`RID`) and study phase (`COLPROT`).
#'
#' @param .data Data.frame
#'
#' @return A data.frame that contains adjusted baseline record and screening record.
#'
#' @examples
#' \dontrun{
#' pacc_mmse_long_file <- system.file(
#'   "/extradata/pacc-raw-input/pacc_mmse_long.csv",
#'   package = "ADNIMERGE2"
#' )
#' pacc_mmse_long <- readr::read_csv(
#'   file = pacc_mmse_long_file,
#'   guess_max = Inf
#' )
#' baseline_screening_mmse_record <- adjust_screening_record(.data = pacc_mmse_long)
#' }
#' @rdname adjust_screening_record
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter mutate across case_when select distinct left_join group_by ungroup
#' @importFrom tidyselect all_of
#' @importFrom tidyr expand_grid fill
#' @export

adjust_screening_record <- function(.data) {
  # Screening visit code
  screenVisit <- c("sc", "v01", "4_sc")
  # baseline visit code
  baselineVisit <- c("bl", "v03", "4_bl")
  join_by_vars <- c("RID", "COLPROT", "VISCODE")
  check_colnames(
    .data = .data,
    col_names = join_by_vars,
    strict = TRUE,
    stop_message = TRUE
  )
  .data <- .data %>%
    as_tibble() %>%
    filter(if_any(all_of(join_by_vars[3]), ~ .x %in% c(screenVisit, baselineVisit))) %>%
    mutate(across(all_of(join_by_vars[3]), ~ case_when(
      .x %in% screenVisit ~ "sc",
      .x %in% baselineVisit ~ "bl"
    ))) %>%
    assert_non_missing(all_of(join_by_vars[3])) %>%
    assert_uniq(all_of(join_by_vars))

  output_data <- .data %>%
    select(COLPROT, RID) %>%
    distinct() %>%
    expand_grid(VISCODE = c("sc", "bl")) %>%
    left_join(.data,
      by = join_by_vars
    ) %>%
    group_by(across(all_of(join_by_vars[1:2]))) %>%
    fill(-all_of(c(join_by_vars)), .direction = "down") %>%
    ungroup()

  output_data <- output_data %>%
    mutate(across(all_of(join_by_vars[3]), ~ case_when(
      .x %in% "sc" & get(join_by_vars[2]) %in% adni_phase()[3] ~ "v01",
      .x %in% "bl" & get(join_by_vars[2]) %in% adni_phase()[3] ~ "v03",
      .x %in% "sc" & get(join_by_vars[2]) %in% adni_phase()[5] ~ "4_sc",
      .x %in% "bl" & get(join_by_vars[2]) %in% adni_phase()[5] ~ "4_bl",
      TRUE ~ .x
    )))

  return(output_data)
}
