library(testthat)

# Synthetic PACC component data ----
make_pacc_data <- function(n = 200, seed = 123) {
  set.seed(seed)
  latent <- rnorm(n)
  tibble::tibble(
    RID = seq_len(n),
    COLPROT = rep(c("ADNI1", "ADNI2"), length.out = n),
    ADASQ4SCORE = 5 - latent + rnorm(n, sd = 0.5),
    MMSE = 28 + latent + rnorm(n, sd = 0.5),
    LDELTOTL = 10 + 2 * latent + rnorm(n, sd = 0.5),
    DIGITSCR = 40 + 5 * latent + rnorm(n, sd = 1),
    TRABSCOR = exp(4.5 - 0.3 * latent + rnorm(n, sd = 0.1))
  )
}

make_bl_summary <- function(.data, log_trailsB = FALSE, log_var_name = "TRABSCOR") {
  comp_vars <- c("ADASQ4SCORE", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR")
  out <- lapply(comp_vars, function(v) {
    x <- .data[[v]]
    var_name <- v
    if (v == "TRABSCOR" && log_trailsB) {
      x <- log(x + 1)
      var_name <- log_var_name
    }
    tibble::tibble(VAR = var_name, MEAN = mean(x, na.rm = TRUE), SD = stats::sd(x, na.rm = TRUE))
  })
  dplyr::bind_rows(out)
}

test_that("compute_pacc_score standardizes and orients component scores", {
  dd <- make_pacc_data()
  bl_summary <- make_bl_summary(dd)
  result <- compute_pacc_score(
    .data = dd,
    bl.summary = bl_summary,
    rescale_trailsB = FALSE,
    keepComponents = TRUE,
    wideFormat = TRUE
  )
  mmse_summary <- bl_summary[bl_summary$VAR == "MMSE", ]
  adas_summary <- bl_summary[bl_summary$VAR == "ADASQ4SCORE", ]
  expect_equal(result$MMSE.zscore, (dd$MMSE - mmse_summary$MEAN) / mmse_summary$SD)
  # ADAS Q4 score is reoriented so that greater value reflects better performance
  expect_equal(result$ADASQ4SCORE.zscore, -(dd$ADASQ4SCORE - adas_summary$MEAN) / adas_summary$SD)
  # Composite score of the reference group should be centered at zero
  expect_equal(mean(result$mPACCtrailsB), 0, tolerance = 1e-6)
  # mPACCdigit is only computed for ADNI1 study phase
  expect_true(all(is.na(result$mPACCdigit[result$COLPROT == "ADNI2"])))
  expect_false(any(is.na(result$mPACCdigit[result$COLPROT == "ADNI1"])))
})

test_that("compute_pacc_score requires at least two non-missing components", {
  dd <- make_pacc_data()
  bl_summary <- make_bl_summary(dd)
  dd$ADASQ4SCORE[1:2] <- NA
  dd$MMSE[1] <- NA
  dd$LDELTOTL[1] <- NA
  result <- compute_pacc_score(.data = dd, bl.summary = bl_summary)
  expect_true(is.na(result$mPACCtrailsB[1]))
  expect_false(is.na(result$mPACCtrailsB[2]))
})

test_that("compute_pacc_score handles missing Trails B scores", {
  dd <- make_pacc_data()
  dd$TRABSCOR[1:3] <- NA
  expect_no_error(
    compute_pacc_score(.data = dd, bl.summary = make_bl_summary(dd), rescale_trailsB = FALSE)
  )
  expect_no_error(
    compute_pacc_score(.data = dd, bl.summary = make_bl_summary(dd, log_trailsB = TRUE), rescale_trailsB = TRUE)
  )
})

test_that("compute_pacc_score log-scale Trails B summary", {
  dd <- make_pacc_data()
  # Log-scale summary provided with either `TRABSCOR` or `LOG_TRABSCOR` name
  result1 <- compute_pacc_score(
    .data = dd,
    bl.summary = make_bl_summary(dd, log_trailsB = TRUE, log_var_name = "TRABSCOR"),
    rescale_trailsB = TRUE
  )
  result2 <- compute_pacc_score(
    .data = dd,
    bl.summary = make_bl_summary(dd, log_trailsB = TRUE, log_var_name = "LOG_TRABSCOR"),
    rescale_trailsB = TRUE
  )
  expect_equal(result1$mPACCtrailsB, result2$mPACCtrailsB)
  expect_equal(mean(result1$mPACCtrailsB), 0, tolerance = 1e-6)
  # Negative Trails B score is not allowed for log transformation
  dd$TRABSCOR[1] <- -5
  expect_error(
    compute_pacc_score(.data = dd, bl.summary = make_bl_summary(make_pacc_data(), log_trailsB = TRUE), rescale_trailsB = TRUE)
  )
})

test_that("compute_pacc_score handles a component with all missing values", {
  dd <- make_pacc_data()
  bl_summary <- make_bl_summary(dd)
  dd$DIGITSCR <- NA_real_
  result <- compute_pacc_score(.data = dd, bl.summary = bl_summary)
  # Three of four components are still available
  expect_false(any(is.na(result$mPACCdigit[result$COLPROT == "ADNI1"])))
  expect_false(any(is.na(result$mPACCtrailsB)))
})

test_that("compute_pacc_score long format only appends the composite scores", {
  dd <- make_pacc_data(n = 50)
  bl_summary <- make_bl_summary(dd)
  dd_long <- tidyr::pivot_longer(
      data = dd,
      cols = c("ADASQ4SCORE", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR"),
      names_to = "PARAMCD",
      values_to = "AVAL"
    )
  result <- compute_pacc_score(
    .data = dd_long,
    bl.summary = bl_summary,
    wideFormat = FALSE,
    varName = "PARAMCD",
    scoreCol = "AVAL",
    idCols = c("RID", "COLPROT")
  )
  expect_equal(nrow(result), nrow(dd_long) + 2 * nrow(dd))
  expect_setequal(
    unique(result$PARAMCD),
    c("ADASQ4SCORE", "MMSE", "LDELTOTL", "DIGITSCR", "TRABSCOR", "mPACCdigit", "mPACCtrailsB")
  )
})

test_that("compute_pacc_score deprecated rescale_trialsB argument", {
  dd <- make_pacc_data()
  expect_warning(
    compute_pacc_score(.data = dd, bl.summary = make_bl_summary(dd, log_trailsB = TRUE), rescale_trialsB = TRUE),
    "deprecated"
  )
})

# Other utility functions ----
test_that("original_study_protocol covers all RID ranges", {
  expect_equal(
    original_study_protocol(c(1, 2001, 4001, 6001, 10001, 11999, 12000, 12001)),
    c("ADNI1", "ADNIGO", "ADNI2", "ADNI3", "ADNI4", "ADNI4", "TEAM", "TEAM")
  )
})

test_that("check_overall_phase allows multiple phases", {
  expect_no_error(check_overall_phase(c("ADNIGO", "ADNI2")))
  expect_no_error(check_overall_phase("Overall"))
  expect_error(check_overall_phase(c("Overall", "ADNI2")))
})

test_that("detect_baseline_score handles missing dates", {
  expect_equal(
    detect_baseline_score(
      cur_record_date = as.Date(c("2020-01-05", NA, "2020-01-20")),
      enroll_date = as.Date("2020-01-01"),
      time_interval = 30
    ),
    c("Yes", NA_character_, NA_character_)
  )
})
