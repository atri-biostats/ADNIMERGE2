# Compute PACC score ----

#' @title Compute ADNI modified versions of the Preclinical Alzheimer's Cognitive Composite (PACC)
#'
#' @description
#'
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
#' See \code{Details} section for more information.
#'
#' @details
#'
#' This function generates two modified versions of PACC scores based on ADNI data. FCSRT is not used in ADNI, so we use the
#' Delayed Recall portion of the Alzheimer's Disease Assessment Scale (ADAS) as a proxy. Score \code{mPACCdigit} uses the
#' DSST and only computed for \code{ADNI1} study phase. \code{mPACCtrailsB} uses (log transformed) Trails B as a proxy for DSST. Raw component scores
#' standardized according to the mean and standard deviation of baseline scores of ADNI subjects with normal cognition
#' to create \code{Z} scores for each component \code{(Z=(raw - mean(raw.bl))/sd(raw.bl))}.
#' The Z scores are reoriented if necessary so that greater scores reflect better performance.
#' The composite is the sum of these Z scores.
#'
#' \strong{Missing components:} At least two components must be present to produce a score.
#' If more than two components are missing, the PACC will be \code{NA}.
#'
#' \strong{mPACCdigit} only will be calculated for the study phase when DSST was collected, which is in ADNI1 study phase.
#'  Otherwise the \code{mPACCdigit} score will be missing (i.e., \code{NA}) even though the remaining PACC component scores are non-missing.
#'
#' Please see \code{vignette(topic = "ADNIMERGE2-PACC-SCORE", package = "ADNIMERGE2")}
#' how \code{\link{compute_pacc_score}()} function can be used.
#'
#' @param .data A data.frame either in wide or long format. Please see the other arguments.
#'
#' @param bl.summary Baseline component score summary
#'  It can be created either using \code{\link{compute_score_summary}()} function.
#'  Or a data.frame of component score summary that contains the following variables:
#' \itemize{
#'   \item {\code{VAR}}: Contains PACC component variable names
#'   \item {\code{MEAN}}: Mean score
#'   \item {\code{SD}}: Standard deviation value
#' }
#'  Often recommended to use the baseline component score that summarized by
#'  baseline diagnostics status.
#'
#' @param componentVars Character vector of component score variable names.
#' The component score variable names should be arranged based the following order.
#' Otherwise, \strong{invalid} composite score will be calculated.
#'
#' \itemize{
#'   \item Delayed Recall portion from ADAS Cognitive Behavior assessment (ADAS-Cog), see \code{\link{ADAS}()}
#'   \item Mini-Mental State Examination Score, see \code{\link{MMSE}()}
#'   \item Logical Memory IIa Delayed Recall Score, see \code{LDELTOTAL} score in \code{\link{NEUROBAT}()}
#'   \item Digit Symbol Substitution Test Score, see \code{DIGITSCOR} score in \code{\link{NEUROBAT}()}
#'   \item Trails B Score, see see \code{TRABSCOR} score in \code{\link{NEUROBAT}()}
#' }
#'
#' @param rescale_trialsB A Boolean value to change the \code{Trails B} score in log scale, Default: TRUE
#'
#' @param keepComponents A Boolean to keep component score, Default: FALSE
#'
#' @param wideFormat A Boolean value whether the data.frame is in \code{wide} or \code{long} format, Default: TRUE
#'
#' @param varName Column name that contain the component score names for long format data, Default = NULL.
#' Only applicable for a \code{long} format data and \code{varName} must not be missing if \code{wideFormat} is \code{FALSE}.
#'
#' @param scoreCol Variable names that contains component score/numeric value for long format data, Default = NULL.
#' Only applicable for a \code{long} format data and \code{scoreCol} must not be missing if \code{wideFormat} is \code{FALSE}.
#'
#' @param idCols Character vector of ID columns for long format data, Default: NULL.
#' Only applicable for a \code{long} format data and \code{idCols} must not be missing if \code{wideFormat} is \code{FALSE}.
#'
#' @return
#' \itemize{
#' \item For a {\code{wide}} format input data: A \code{data.frame} with appended columns for \code{mPACCdigit} and \code{mPACCtrailsB}.
#' \item For a {\code{long}} format input data: A \code{data.frame} with additional rows of \code{mPACCdigit} and \code{mPACCtrailsB}.
#' }
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
#' # Please see 'Details' section or 'ADNIMERGE2-PACC-SCORE' vignette
#' vignette(topic = "ADNIMERGE2-PACC-SCORE", package = "ADNIMERGE2")
#'
#' # Additional examples about PACC score -----
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
#' anyAmyloid <- unique(subset(dd, !is.na(AV45.bl) | !is.na(PIB) |
#'   !is.na(FBB.bl) | !is.na(ABETA.bl))$RID)
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
#'   fixed = mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE +
#'     PTEDUCAT + m + m2 + (m + m2):ElevatedAmyloid,
#'   random = ~ m | RID,
#'   data = dd,
#'   na.action = na.omit
#' )
#'
#' Months <- seq(12, 96, 12)
#' elevated.design <- model.matrix(
#'   mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT +
#'     m + m2 + (m + m2):ElevatedAmyloid,
#'   data = data.frame(
#'     mPACCtrailsB = 0, mPACCtrailsB.bl = 0, APOEe4 = TRUE, AGE = 75,
#'     PTEDUCAT = 12, ElevatedAmyloid = 1, m = Months, m2 = Months^2
#'   )
#' )
#'
#' normal.design <- model.matrix(
#'   mPACCtrailsB ~ mPACCtrailsB.bl + APOEe4 + AGE + PTEDUCAT +
#'     m + m2 + (m + m2):ElevatedAmyloid,
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
#'   \code{vignette(topic = "ADNIMERGE2-PACC-SCORE", package = "ADNIMERGE2")}
#' @rdname compute_pacc_score
#' @keywords adni_scoring_fun pacc_score_utils_fun
#' @export
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom dplyr mutate across select relocate bind_rows
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tidyselect all_of any_of ends_with contains last_col
#' @importFrom stats cor

compute_pacc_score <- function(.data,
                               bl.summary,
                               componentVars,
                               rescale_trialsB = FALSE,
                               keepComponents = FALSE,
                               wideFormat = TRUE,
                               varName = NULL,
                               scoreCol = NULL,
                               idCols = NULL) {
  mPACCdigit <- mPACCtrailsB <- NULL
  check_object_type(keepComponents, "logical")
  check_object_type(rescale_trialsB, "logical")
  check_object_type(wideFormat, "logical")

  var_names <- componentVars
  if (length(var_names) != 5) {
    cli::cli_abort(
      message = c(
        "{.var var_name} must be length of 5. \n",
        "{.val {var_name}} are provided"
      )
    )
  }

  if (!all(var_names %in% bl.summary$VAR)) {
    cli::cli_abort(
      message = c(
        "All {.var componentVars} not found in {.var bl.summary$VAR}. \n",
        "Component variables are: {.val {componentVars}}, and \n",
        paste0(
          "The baseline summary data {.var bl.summary} contains:",
          " {.val {unique(bl.summary$VAR)}} variable{?s}."
        )
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

  # Get phase var names
  phase_vars <- c("Phase", "PHASE", "COLPROT")
  phaseVar <- get_cols_name(.data = .data_wide, col_name = phase_vars)
  if (length(phaseVar) == 0) {
    cli_abort(message = "{.var phaseVar} must be a length of 1 character vector.")
  }

  if (length(phaseVar) != 1) {
    cli_abort(message = "{.var phaseVar} must be a length of 1 character vector of {.val {phase_vars}}.")
  }

  check_colnames(
    .data = .data_wide,
    col_names = var_names,
    stop_message = TRUE,
    strict = TRUE
  )

  # Log transformed Trial B score
  if (!rescale_trialsB) {
    trailB_score <- .data_wide %>%
      select(all_of(var_names[5])) %>%
      pull()

    if (any(trailB_score < 0)) {
      cli::cli_abort(
        message = c(
          "{.var trailB_score} represents the Trial B score. \n",
          paste0(
            "{.var trailB_score} must not contains any negative value for",
            " log rescale/transformation. \n"
          ),
          "Do you want to set {.var rescale_trialsB} = {.val {FALSE}}?"
        )
      )
    }
  }

  .data_wide <- .data_wide %>%
    {
      if (rescale_trialsB) {
        # Create log transformation for Trial B Scores
        mutate(., across(all_of(var_names[5]), ~ log(.x + 1), .names = "LOG.{col}"))
      } else {
        (.)
      }
    }

  if (rescale_trialsB) {
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
        "{var .data_wide} must not contains pre-existing {.val {paste0(var_names, '.zscore')}} variable{?s}. \n",
        "Caution: these variables will be overwriting! \n",
        "{var .data_wide} contains pre-existed {.val {check.zscore_var}} variable{?s}."
      )
    )
  }

  # Normalized item-level score by corresponding baseline score among cognitive normal subjects
  .data_wide <- .data_wide %>%
    mutate(across(all_of(var_names),
      ~ {
        # Adjust for LOG.trialB score
        if (rescale_trialsB) {
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
    stats::cor(., use = "pairwise.complete.obs")

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
    mutate(across(all_of(c("mPACCdigit", "mPACCtrailsB")), ~ round(.x, 6))) %>%
    # mPACCdigit only in ADNI1 phase
    mutate(across(all_of("mPACCdigit"), ~ case_when(get(phaseVar) %in% adni_phase()[1] ~ .x))) %>%
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
#' @title Compute Grouped Numeric Summary Statistic
#'
#' @description
#'  This function is used to compute the numeric variable summary statistic grouped
#'  by categorical variable.
#'
#' @param .data Data.frame
#'
#' @param wideFormat A Boolean value whether the input data.frame is in a \code{wide} or \code{long} format, Default: TRUE
#'
#' @param scoreVar Character vector of variable(s) that contain the actual score/numeric values
#'
#' When \code{wideFormat} is \code{FALSE} (i.e., for a long format data), \code{scoreVar}
#' must be a length of one character vector of variable name that contains
#' the score/numeric values.
#'
#' @param groupVar Group variable, Default: 'DX'
#'
#' @param filterGroup Filter value of group variable \code{groupVar}, Default: NULL
#' Only applicable if the \code{groupVar} is a length of one character vector.
#'
#' @param groupVar1 Additional grouping variable, only applicable for \code{long} format data.frame.
#'
#' @return A data.frame with the following columns:
#'
#' \itemize{
#'   \item \code{groupVar}: Grouping variable
#'   \item \code{VAR}: Score/numeric variable name
#'   \item \code{N}: Number of non-missing observation
#'   \item \code{MEAN}: Mean score
#'   \item \code{SD}: Standard deviation value
#' }
#'
#' @details
#'  All computed summary statistic are based on non-missing observation.
#'  The result summary will be filter by the corresponding \code{filterGroup} value(s).
#'
#' @examples
#' \dontrun{
#' # For long format data
#' # Suppose we wanted to compute the baseline summary score of
#' # all available assessments in \code{ADNIMERGE2::ADQS}
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
#' compute_score_summary(
#'   .data = long_format_example,
#'   wideFormat = FALSE,
#'   scoreVar = "AVAL",
#'   groupVar1 = "PARAMCD",
#'   groupVar = "DX",
#'   filterGroup = NULL
#' )
#'
#' # For only cognitive normal (CN) subjects
#' compute_score_summary(
#'   .data = long_format_example,
#'   wideFormat = FALSE,
#'   scoreVar = "AVAL",
#'   groupVar1 = "PARAMCD",
#'   groupVar = "DX",
#'   filterGroup = "CN"
#' )
#'
#' # For wide format data
#' # Suppose we wanted to compute the baseline summary statistic of
#' # \code{AGE}, \code{BMI}, \code{ADASTT11} and \code{ADASTT13}
#' wide_format_example <- ADNIMERGE2::ADSL %>%
#'   filter(ENRLFL %in% "Y")
#'
#' # By baseline diagnostics status
#' compute_score_summary(
#'   .data = wide_format_example,
#'   wideFormat = TRUE,
#'   scoreVar = c("AGE", "BMI", "ADASTT11", "ADASTT13"),
#'   groupVar1 = NULL,
#'   groupVar = "DX",
#'   filterGroup = NULL
#' )
#'
#'
#' compute_score_summary(
#'   .data = wide_format_example,
#'   wideFormat = TRUE,
#'   scoreVar = c("AGE", "BMI", "ADASTT11", "ADASTT13"),
#'   groupVar1 = NULL,
#'   groupVar = "DX",
#'   filterGroup = "CN"
#' )
#'
#' # By SEX
#' compute_score_summary(
#'   .data = wide_format_example,
#'   wideFormat = TRUE,
#'   scoreVar = c("AGE", "BMI", "ADASTT11", "ADASTT13"),
#'   groupVar1 = NULL,
#'   groupVar = "SEX",
#'   filterGroup = NULL
#' )
#' }
#' @seealso
#'  \code{\link{compute_baseline_score_summary}()}
#'  \code{vignette(topic = "ADNIMERGE2-PACC-SCORE", package = "ADNIMERGE2")}
#' @rdname compute_score_summary
#' @keywords pacc_score_utils_fun utils_fun
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter if_all group_by across ungroup if_any select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom stats sd

compute_score_summary <- function(.data,
                                  wideFormat = TRUE,
                                  scoreVar,
                                  groupVar = "DX",
                                  filterGroup = NULL,
                                  groupVar1 = NULL) {
  N <- MEAN <- SD <- VAR <- SCORE <- NULL
  check_object_type(wideFormat, "logical")
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
      SD = stats::sd(SCORE, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(across(c("MEAN", "SD"), ~ ifelse(.x %in% NaN, NA_real_, .x))) %>%
    mutate(
      MEAN = round(MEAN, 4),
      SD = round(SD, 6)
    )

  if (!is.null(filterGroup)) {
    if (length(groupVar) != 1) {
      cli::cli_abort(
        message = c(
          "{.var groupVar} must be a length of one character vector. \n",
          "{.var groupVar} contains {.val {groupVar}} variable{?s}."
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

#' @title Compute Baseline Grouped Score/Numeric Summary Statistic
#'
#' @description
#' A wrapper function to calculate baseline grouped summary statistic of
#' numeric variable(s).
#'
#' @inheritParams compute_score_summary
#'
#' @param filterBy Character vector of baseline record identifier variable
#'
#' @param filterValue Baseline record identifier values, Default: c("Y", "Yes", "bl")
#'
#' @param ... \code{\link{compute_score_summary}()} arguments
#'
#' @return Similar to \code{\link{compute_score_summary}()} result
#'
#' @examples
#' \dontrun{
#' # For long format data
#' # Suppose we wanted to compute the baseline summary score of available
#' # assessment score in 'ADNIMERGE2::ADQS' by baseline diagnosis status.
#' library(tidyverse)
#' library(ADNIMERGE2)
#'
#' compute_baseline_score_summary(
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
#' compute_baseline_score_summary(
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
#'  \code{\link{compute_score_summary}()}
#' @rdname compute_baseline_score_summary
#' @keywords pacc_score_utils_fun utils_fun
#' @export
#' @importFrom cli cli_abort
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter if_all
#' @importFrom tidyselect all_of

compute_baseline_score_summary <- function(.data, filterBy, filterValue = c("Y", "Yes", "bl"), ...) {
  if (length(filterBy) != 1) {
    cli::cli_abort(
      message = c(
        "{.var filterBy} must be a length of one character vector. \n",
        "{.var filterBy} contains {.val {filterBy}} variable{?s}."
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

  bl_summary <- compute_score_summary(.data = .data, ...)

  return(bl_summary)
}

# Standardize/Normalize values ----
#' @title Standardize/normalize numeric value by baseline summary
#'
#' @param x Numeric value
#'
#' @param baseline_summary A data.frame of baseline score summary that contains the following variables:
#'
#' \itemize{
#'   \item \code{MEAN}: Mean value
#'   \item \code{SD}: Standard deviation value
#'   \item \code{VAR}: Contains variable name. Only applicable for non-missing \code{varName} value.
#' }
#'
#' The \code{baseline_summary} can be generated using
#' \code{\link{compute_baseline_score_summary}()} function.
#'
#' @param varName Variable name
#'
#' @return A numeric vector
#'
#' @examples
#' \dontrun{
#' # Suppose we wanted to standardize/normalize ADASTT13 scores in
#' # ADNIMERGE2:ADQS by baseline summary score of Cognitive Normal (CN)
#' # enrolled subjects.
#'
#' library(tidyverse)
#' library(assertr)
#' library(ADNIMERGE2)
#'
#' bl.summary <- ADNIMERGE2::ADSL %>%
#'   compute_baseline_score_summary(
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
#'   mutate(across(AVAL,
#'     ~ normalize_var_by_baseline_score(
#'       x = .x,
#'       baseline_summary = bl.summary,
#'       varName = "ADASTT13"
#'     ),
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
#' @family utility functions
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#' @importFrom assertr verify

normalize_var_by_baseline_score <- function(x, baseline_summary, varName = NULL) {
  VAR <- MEAN <- SD <- NULL
  check_object_type(baseline_summary, "data.frame")
  col_names <- c("MEAN", "SD")
  if (!is.null(varName)) {
    col_names <- c("VAR", col_names)
  }
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

  x <- calculate_zscore(
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
#' calculate_zscore(x = x, mean = mean, sd = sd)
#' calculate_zscore(x = x, mean = 5, sd = 1.5)
#' calculate_zscore(x = c(1, 0.5, NA, 3), mean = 5, sd = 1.5)
#' }
#' @rdname calculate_zscore
#' @family utility functions
#' @keywords utils_fun
#' @export

calculate_zscore <- function(x, mean, sd) {
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
#' @title Check for non-missing value
#' @param x Input value
#' @return A stop error if the value is missing value \code{'NULL'}
#' @examples
#' \dontrun{
#' check_non_missing_value(x = LETTERS[1:10])
#' check_non_missing_value(x = NULL)
#' }
#' @rdname check_non_missing_value
#' @family checks function
#' @keywords utils_fun
#' @importFrom cli cli_abort
#' @export

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
#' @title Get Common Date Across Variables
#'
#' @param .data A wide format data.frame
#'
#' @param date_cols Character vector of date column names
#'
#' @param select_method
#' Selection method if there is more than one unique non-missing date, Default: 'min'
#' Either the minimum date (\code{'min'}), or the maximum date (\code{'max'}).
#'
#' @param compared_ref_date A Boolean to compared the common date with the reference date if it is provided, Default: FALSE
#'
#' @param ref_date_col Reference date column name, Default: NULL
#'
#' @param preferred_date_col
#' Preferred date when common date and reference date are different.
#' Only applicable if \code{compared_ref_date} is \code{TRUE}.
#'
#' @return A data.frame with the appended columns:
#' \itemize{
#'  \item COMMON_DATE Common date among the provided date columns
#'  \item FINAL_DATE Final date after comparing with reference date if \code{compared_ref_date} is \code{TRUE}. Otherwise the same as \code{COMMON_DATE}.
#'  \item DATE_RECORD_TYPE Record type to indicate whether the date columns are the same rowwise or not.
#'  }
#'
#' @details
#'  This function is used to get a common date value across multiple date columns and
#'  compared with reference date column if it is provided. The comparison algorithm
#'  is based on rowwise operation and presented as follow:
#'
#'  For records that have at least one non-missing date columns based on the list of
#'  provided date columns:
#'  \itemize{
#'    \item Select one unique date value if all date columns are the same/equal.
#'    \item Select either the minimum or maximum date based on the selection method (\code{select_method}) if at least one date column is differ from the remaining provided date columns.
#'  }
#'
#' The reference date column must be present for any comparison with reference date. Then, the reference date will be used for any missing common date.
#'
#' Otherwise, the date will considered as missing \code{NA}.
#'
#' @examples
#' \dontrun{
#' # Please see the \code{ADNIMERGE2-PACC-SCORE} vignette
#' vignette(topic = "ADNIMERGE2-PACC-SCORE", package = "ADNIMERGE2")
#' }
#'
#' @rdname get_vars_common_date
#' @keywords utils_fun
#' @importFrom rlang arg_match0
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate row_number filter group_by ungroup n_distinct
#' @importFrom dplyr distinct left_join case_when select
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @export

get_vars_common_date <- function(.data,
                                 date_cols,
                                 select_method = "min",
                                 compared_ref_date = FALSE,
                                 ref_date_col = NULL,
                                 preferred_date_col = NULL) {
  TEMP_ID <- DATE_COLS <- DATES <- ALL_SAME_DATE_SATUS <- NUM_RECORDS <- NULL
  COMMON_DATE <- DATE_RECORD_TYPE <- REF_DATE_COL <- FINAL_DATE <- NULL

  rlang::arg_match0(arg = select_method, values = c("min", "max"))
  check_object_type(compared_ref_date, "logical")

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
        mutate(., COMMON_DATE = max(DATES))
      } else {
        mutate(., COMMON_DATE = min(DATES))
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

## Rename columns, convert into character type and tibble object ----
#' @title Make Similar Format
#' @description
#' A function to convert a data.frame object into tibble format with upper-case
#' column name and character type.
#' @param .data Data.frame
#' @return A tibble/data.frame object with upper-case column name and character type.
#' @rdname set_as_tibble
#' @keywords utils_fun
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename_with mutate across
#' @importFrom tidyselect everything
#' @export

set_as_tibble <- function(.data) {
  .data <- .data %>%
    as_tibble() %>%
    rename_with(~ toupper(.x), everything()) %>%
    mutate(across(everything(), as.character))
  return(.data)
}

# Baseline record adjustment from screening visit -----
#' @title Carrying Forward Screening Record as Baseline Record
#'
#' @description
#' This function is used to carry forward screening record as baseline record per
#' subject id (\code{RID}) and study data collection phase (\code{COLPROT}).
#'
#' @param .data Data.frame
#'
#' @param wide_format A Boolean indicator of wide data format, Default: TRUE
#'
#' @param extra_id_cols ID variable names in addition to \code{RID}, \code{COLPROT} and \code{VISCODE}.
#'  Only applicable for a long format data.
#'
#' @param adjust_date_col Adjusting date column values.
#'   Only applicable if \code{adjust_date_col} is non-missing
#'
#' @param check_col Column name which values used to determine date column adjustment.
#' Only applicable if \code{adjust_date_col} is non-missing

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
#' # For a wide format data
#' bl_sc_mmse_record <- adjust_scbl_record(
#'   .data = pacc_mmse_long %>%
#'     pivot_wider(names_from = SCORE_SOURCE, values_from = SCORE),
#'   wide_format = TRUE
#' )
#' head(bl_sc_mmse_record)
#'
#' # For a long format data without adjusting for visit date
#' bl_sc_mmse_record_long <- adjust_scbl_record(
#'   .data = pacc_mmse_long,
#'   wide_format = FALSE,
#'   extra_id_cols = "SCORE_SOURCE"
#' )
#' head(bl_sc_mmse_record_long)
#'
#' # For a long format data with adjusting for visit date
#' bl_sc_mmse_record_long <- adjust_scbl_record(
#'   .data = pacc_mmse_long,
#'   wide_format = FALSE,
#'   extra_id_cols = "SCORE_SOURCE",
#'   adjust_date_col = "VISDATE",
#'   check_col = "SCORE"
#' )
#' }
#' @rdname adjust_scbl_record
#' @keywords adni_enroll_fun
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter mutate across case_when select distinct left_join
#' @importFrom dplyr group_by ungroup bind_rows
#' @importFrom tidyselect all_of
#' @importFrom tidyr expand_grid fill
#' @importFrom assertr verify
#' @export

adjust_scbl_record <- function(.data, wide_format = TRUE, extra_id_cols = NULL,
                               adjust_date_col = NULL, check_col = NULL) {
  VISCODE <- NULL
  check_object_type(wide_format, "logical")
  if (!wide_format) {
    check_non_missing_value(extra_id_cols)
  }
  # Screening and baseline visits
  sc_bl_visit <- c(get_screen_vistcode(type = "first"), get_baseline_vistcode())
  join_by_vars <- c("RID", "COLPROT", "VISCODE")
  if (!is.null(extra_id_cols)) join_by_vars <- c(join_by_vars, extra_id_cols)
  check_colnames(
    .data = .data,
    col_names = join_by_vars,
    strict = TRUE,
    stop_message = TRUE
  )

  scbl_data <- .data %>%
    as_tibble() %>%
    filter(if_any(all_of(join_by_vars[3]), ~ .x %in% c(sc_bl_visit))) %>%
    mutate(across(
      all_of(join_by_vars[3]),
      ~ case_when(
        .x %in% get_screen_vistcode(type = "first") ~ "sc",
        .x %in% get_baseline_vistcode() ~ "bl"
      )
    )) %>%
    assert_non_missing(all_of(join_by_vars[3])) %>%
    assert_uniq(all_of(join_by_vars))

  output_data <- scbl_data %>%
    select(all_of(join_by_vars[!join_by_vars %in% "VISCODE"])) %>%
    distinct() %>%
    expand_grid(VISCODE = c("sc", "bl")) %>%
    left_join(scbl_data,
      by = join_by_vars
    ) %>%
    group_by(across(all_of(join_by_vars[!join_by_vars %in% "VISCODE"]))) %>%
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

  if (!is.null(adjust_date_col)) {
    check_non_missing_value(check_col)
    output_data <- output_data %>%
      filter(if_any(all_of(join_by_vars[3]), ~ .x %in% get_screen_vistcode())) %>%
      bind_rows(
        adjust_scbl_visitdate(
          .data = .data,
          .adj_scbl_data = output_data,
          date_col = adjust_date_col,
          check_col = check_col,
          extra_id_cols = extra_id_cols
        )
      ) %>%
      verify(nrow(.) == nrow(output_data))
  }

  return(output_data)
}

#' @title Adjust Baseline Visit/Assessment Collection Date
#'
#' @description
#'  This function is used to adjust the visit/assessment collection date
#'  of a baseline record that was carried forward from a screening visit.
#'
#' @inheritParams adjust_scbl_record
#'
#' @param .adj_scbl_data A data.frame generated using \code{\link{adjust_scbl_record}()} function
#'
#' @param date_col Date column name
#'
#' @return A data.frame the same as \code{.adj_scbl_data} input data with adjusted values.
#'
#' @examples
#' \dontrun{
#' pacc_neurobat_long_file <- system.file(
#'   "/extradata/pacc-raw-input/pacc_neurobat_long.csv",
#'   package = "ADNIMERGE2"
#' )
#' pacc_neurobat_long <- readr::read_csv(
#'   file = pacc_neurobat_long_file,
#'   guess_max = Inf
#' )
#' bl_sc_neurobat_record_long <- adjust_scbl_record(
#'   .data = pacc_neurobat_long,
#'   wide_format = FALSE,
#'   extra_id_cols = "SCORE_SOURCE",
#'   adjust_date_col = NULL
#' )
#' # Only for baseline visit records
#' adjust_scbl_visitdate(
#'   .data = pacc_neurobat_long,
#'   .adj_scbl_data = bl_sc_neurobat_record_long,
#'   date_col = "VISDATE",
#'   check_col = "SCORE",
#'   id_cols = "SCORE_SOURCE"
#' )
#' }
#' @seealso
#'  \code{\link{adjust_scbl_record}()}
#' @rdname adjust_scbl_visitdate
#' @keywords internal

adjust_scbl_visitdate <- function(.data, .adj_scbl_data, date_col, check_col, extra_id_cols = NULL) {
  IS_ORIGINAL_BL <- IS_KNOWN_BL_SCORE <- NULL

  checks <- lapply(c("date_col", "check_col"), function(i) {
    temp_value <- get(i)
    if (length(temp_value) != 1) {
      cli_abort(
        message = paste0(
          "{.var {i}} must be a length of 1 character vector. \n",
          "{.var {i}} contains {.val {temp_value}} value{?s}."
        )
      )
    }
  })

  join_by_vars <- c("RID", "COLPROT", "VISCODE", date_col, check_col)

  if (!is.null(extra_id_cols)) join_by_vars <- c(join_by_vars, extra_id_cols)

  .adj_scbl_data <- .adj_scbl_data %>%
    # Only baseline visit
    filter(if_any(all_of(join_by_vars[3]), ~ .x %in% get_baseline_vistcode()))

  .adj_scbl_data <- .adj_scbl_data %>%
    # Add first screening visit date
    left_join(
      .data %>%
        filter(if_any(all_of(join_by_vars[3]), ~ .x %in% get_screen_vistcode(type = "first"))) %>%
        mutate(across(all_of(date_col), ~., .names = "SC_{col}")) %>%
        select(all_of(join_by_vars[!join_by_vars %in% c("VISCODE", check_col)])) %>%
        rename_with(~ paste0("SC_", .x), all_of(date_col)),
      by = join_by_vars[join_by_vars %in% c(join_by_vars[1:2], extra_id_cols)]
    ) %>%
    verify(nrow(.) == nrow(.adj_scbl_data)) %>%
    # Add actual baseline visit record and date
    left_join(
      .data %>%
        filter(if_any(all_of(join_by_vars[3]), ~ .x %in% get_baseline_vistcode())) %>%
        mutate(across(all_of(check_col), ~ case_when(!is.na(.x) ~ "Yes", TRUE ~ "No"), .names = "IS_KNOWN_BL_SCORE")) %>%
        mutate(IS_ORIGINAL_BL = "Yes") %>%
        select(all_of(c(join_by_vars[!join_by_vars %in% check_col], "IS_ORIGINAL_BL", "IS_KNOWN_BL_SCORE"))) %>%
        rename_with(~ paste0("BL_", .x), all_of(date_col)),
      by = join_by_vars[join_by_vars %in% c(join_by_vars[1:3], extra_id_cols)]
    ) %>%
    mutate(across(all_of(date_col), ~ case_when(
      IS_ORIGINAL_BL %in% "Yes" & IS_KNOWN_BL_SCORE %in% "No" & !is.na(get(check_col)) ~ get(paste0("SC_", date_col)),
      is.na(IS_ORIGINAL_BL) ~ get(paste0("SC_", date_col)),
      TRUE ~ .x
    ))) %>%
    select(-any_of(c("IS_ORIGINAL_BL", "IS_KNOWN_BL_SCORE", paste0("SC_", date_col), paste0("BL_", date_col))))

  return(.adj_scbl_data)
}
