# ============================================================
# scripts/daily_competitor_forecasting.R
# ============================================================
#
# Purpose:
#
# Production forecasting infrastructure for the two daily
# leaderboard interval-PnL processes:
#
#   q_daily = [h1, h2]'
#
# where:
#
#   h1 = Rank1 interval daily PnL
#   h2 = Rank2 interval daily PnL
#
# The retained models are fixed from Notebooks 26-28:
#
# Mean models:
#   Rank1: hurdle_persistence
#   Rank2: hurdle_persistence
#
# Variance models:
#   Rank1: constant
#   Rank2: quadratic_progress
#
# Standardized residual correlation:
#
#   rho_daily = 0.4811939
#
# Daily state:
#
#   Delta_Daily_H1  = h1
#   Delta_Daily_D12 = h1 - h2
#
# Therefore:
#
#   Delta_S_daily = M_d q_daily
#
# with:
#
#   M_d =
#   [ 1,  0]
#   [ 1, -1]
#
# This script:
#
#   1. Loads the daily data/model infrastructure.
#   2. Fits the retained mean models on all available data.
#   3. Constructs full-sample mean residuals.
#   4. Fits the retained variance models.
#   5. Forecasts future marginal means.
#   6. Forecasts future marginal variances.
#   7. Constructs the raw-process covariance matrix.
#   8. Transforms raw-process forecasts into daily state space.
#
# It does NOT:
#
#   - reselect mean models;
#   - reselect variance models;
#   - re-estimate the cross-series correlation;
#   - combine daily ranks with the six final-rank processes;
#   - write the final production optimizer CSV files.
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

suppressPackageStartupMessages(
  library(
    tidyverse
  )
)

suppressPackageStartupMessages(
  library(
    here
  )
)


# ------------------------------------------------------------
# 2. Source Existing Daily Infrastructure
# ------------------------------------------------------------

source(
  here(
    "scripts",
    "daily_competitor_data.R"
  )
)

source(
  here(
    "scripts",
    "daily_competitor_models.R"
  )
)


# ------------------------------------------------------------
# 3. Fixed Production Model Choices
# ------------------------------------------------------------

DAILY_SELECTED_MEAN_MODELS <-
  c(
    Rank1 =
      "hurdle_persistence",
    Rank2 =
      "hurdle_persistence"
  )

DAILY_SELECTED_VARIANCE_MODELS <-
  c(
    Rank1 =
      "constant",
    Rank2 =
      "quadratic_progress"
  )

DAILY_STANDARDIZED_RESIDUAL_CORRELATION <-
  0.4811939


# ------------------------------------------------------------
# 4. Numerical Settings
# ------------------------------------------------------------

DAILY_VARIANCE_RESPONSE_FLOOR <-
  1e-4

DAILY_VARIANCE_FORECAST_FLOOR <-
  1e-8

DAILY_PROBABILITY_FLOOR <-
  1e-8

DAILY_PROBABILITY_CEILING <-
  1 -
  DAILY_PROBABILITY_FLOOR


# ------------------------------------------------------------
# 5. Daily State Transformation
# ------------------------------------------------------------

DAILY_STATE_TRANSFORM <-
  matrix(
    c(
      1,
      0,
      1,
      -1
    ),
    nrow =
      2L,
    ncol =
      2L,
    byrow =
      TRUE
  )

rownames(
  DAILY_STATE_TRANSFORM
) <-
  c(
    "Delta_Daily_H1",
    "Delta_Daily_D12"
  )

colnames(
  DAILY_STATE_TRANSFORM
) <-
  c(
    "h1",
    "h2"
  )


# ------------------------------------------------------------
# 6. Daily Standardized Residual Correlation Matrix
# ------------------------------------------------------------

DAILY_RESIDUAL_CORRELATION_MATRIX <-
  matrix(
    c(
      1,
      DAILY_STANDARDIZED_RESIDUAL_CORRELATION,
      DAILY_STANDARDIZED_RESIDUAL_CORRELATION,
      1
    ),
    nrow =
      2L,
    ncol =
      2L,
    byrow =
      TRUE
  )

rownames(
  DAILY_RESIDUAL_CORRELATION_MATRIX
) <-
  c(
    "h1",
    "h2"
  )

colnames(
  DAILY_RESIDUAL_CORRELATION_MATRIX
) <-
  c(
    "h1",
    "h2"
  )


# ------------------------------------------------------------
# 7. Basic Scalar Validation Helpers
# ------------------------------------------------------------

assert_one_finite_numeric <- function(
    x,
    name
) {
  if (
    length(
      x
    ) !=
    1L ||
    !is.numeric(
      x
    ) ||
    !is.finite(
      x
    )
  ) {
    stop(
      paste0(
        name,
        " must be one finite numeric value."
      ),
      call. =
        FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


assert_binary_scalar <- function(
    x,
    name
) {
  assert_one_finite_numeric(
    x,
    name
  )
  
  if (
    !x %in%
    c(
      0,
      1
    )
  ) {
    stop(
      paste0(
        name,
        " must equal 0 or 1."
      ),
      call. =
        FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ------------------------------------------------------------
# 8. Prepare Historical Modeling Data
# ------------------------------------------------------------

get_daily_forecasting_training_data <- function() {
  data <-
    prepare_daily_mean_model_data()
  
  required_columns <-
    c(
      "DailyRank1Pnl",
      "DailyRank2Pnl",
      "Rank1Nonzero",
      "Rank2Nonzero",
      "PreviousRank1Nonzero",
      "PreviousRank2Nonzero",
      "SessionProgress",
      "SessionProgressSquared",
      "LogElapsedMinutes",
      "SessionStart"
    )
  
  missing_columns <-
    setdiff(
      required_columns,
      names(
        data
      )
    )
  
  if (
    length(
      missing_columns
    ) >
    0L
  ) {
    stop(
      paste0(
        "Daily forecasting training data are missing required columns: ",
        paste(
          missing_columns,
          collapse =
            ", "
        )
      ),
      call. =
        FALSE
    )
  }
  
  data
}


# ------------------------------------------------------------
# 9. Fit Rank1 Retained Mean Model
# ------------------------------------------------------------

fit_daily_rank1_mean_model <- function(
    data
) {
  probability_model <-
    suppressWarnings(
      glm(
        Rank1Nonzero ~
          SessionProgress +
          SessionProgressSquared +
          PreviousRank1Nonzero +
          SessionStart,
        family =
          binomial(
            link =
              "logit"
          ),
        data =
          data
      )
    )
  
  nonzero_data <-
    data |>
    filter(
      Rank1Nonzero ==
        1
    )
  
  if (
    nrow(
      nonzero_data
    ) <
    2L
  ) {
    stop(
      "Insufficient nonzero Rank1 observations to fit magnitude model.",
      call. =
        FALSE
    )
  }
  
  magnitude_model <-
    lm(
      DailyRank1Pnl ~
        LogElapsedMinutes,
      data =
        nonzero_data
    )
  
  list(
    rank =
      "Rank1",
    model_id =
      "hurdle_persistence",
    probability_fit =
      probability_model,
    magnitude_fit =
      magnitude_model
  )
}


# ------------------------------------------------------------
# 10. Fit Rank2 Retained Mean Model
# ------------------------------------------------------------

fit_daily_rank2_mean_model <- function(
    data
) {
  probability_model <-
    suppressWarnings(
      glm(
        Rank2Nonzero ~
          SessionProgress +
          SessionProgressSquared +
          PreviousRank2Nonzero +
          SessionStart,
        family =
          binomial(
            link =
              "logit"
          ),
        data =
          data
      )
    )
  
  nonzero_data <-
    data |>
    filter(
      Rank2Nonzero ==
        1
    )
  
  if (
    nrow(
      nonzero_data
    ) <
    2L
  ) {
    stop(
      "Insufficient nonzero Rank2 observations to fit magnitude model.",
      call. =
        FALSE
    )
  }
  
  magnitude_model <-
    lm(
      DailyRank2Pnl ~
        SessionProgress +
        SessionProgressSquared +
        LogElapsedMinutes,
      data =
        nonzero_data
    )
  
  list(
    rank =
      "Rank2",
    model_id =
      "hurdle_persistence",
    probability_fit =
      probability_model,
    magnitude_fit =
      magnitude_model
  )
}


# ------------------------------------------------------------
# 11. Fit Both Retained Mean Models
# ------------------------------------------------------------

fit_daily_selected_mean_models <- function(
    data =
      get_daily_forecasting_training_data()
) {
  list(
    Rank1 =
      fit_daily_rank1_mean_model(
        data
      ),
    Rank2 =
      fit_daily_rank2_mean_model(
        data
      )
  )
}


# ------------------------------------------------------------
# 12. Validate Future Feature Row
# ------------------------------------------------------------

validate_daily_future_features <- function(
    newdata
) {
  if (
    !is.data.frame(
      newdata
    )
  ) {
    stop(
      "newdata must be a data.frame or tibble.",
      call. =
        FALSE
    )
  }
  
  if (
    nrow(
      newdata
    ) !=
    1L
  ) {
    stop(
      "newdata must contain exactly one future interval.",
      call. =
        FALSE
    )
  }
  
  required_columns <-
    c(
      "SessionProgress",
      "SessionProgressSquared",
      "ElapsedMinutes",
      "LogElapsedMinutes",
      "PreviousRank1Nonzero",
      "PreviousRank2Nonzero",
      "SessionStart"
    )
  
  missing_columns <-
    setdiff(
      required_columns,
      names(
        newdata
      )
    )
  
  if (
    length(
      missing_columns
    ) >
    0L
  ) {
    stop(
      paste0(
        "Future feature row is missing required columns: ",
        paste(
          missing_columns,
          collapse =
            ", "
        )
      ),
      call. =
        FALSE
    )
  }
  
  finite_columns <-
    c(
      "SessionProgress",
      "SessionProgressSquared",
      "ElapsedMinutes",
      "LogElapsedMinutes",
      "PreviousRank1Nonzero",
      "PreviousRank2Nonzero",
      "SessionStart"
    )
  
  for (
    column_name in
    finite_columns
  ) {
    value <-
      newdata[[column_name]][1]
    
    assert_one_finite_numeric(
      value,
      column_name
    )
  }
  
  assert_binary_scalar(
    newdata[["PreviousRank1Nonzero"]][1],
    "PreviousRank1Nonzero"
  )
  
  assert_binary_scalar(
    newdata[["PreviousRank2Nonzero"]][1],
    "PreviousRank2Nonzero"
  )
  
  assert_binary_scalar(
    newdata[["SessionStart"]][1],
    "SessionStart"
  )
  
  if (
    newdata[["SessionProgress"]][1] <
    0 ||
    newdata[["SessionProgress"]][1] >
    1
  ) {
    stop(
      "SessionProgress must be between 0 and 1.",
      call. =
        FALSE
    )
  }
  
  if (
    newdata[["ElapsedMinutes"]][1] <=
    0
  ) {
    stop(
      "ElapsedMinutes must be positive.",
      call. =
        FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ------------------------------------------------------------
# 13. Build One Future Feature Row
# ------------------------------------------------------------

build_daily_future_feature_row <- function(
    session_progress,
    elapsed_minutes,
    previous_rank1_nonzero,
    previous_rank2_nonzero,
    session_start =
      0
) {
  assert_one_finite_numeric(
    session_progress,
    "session_progress"
  )
  
  assert_one_finite_numeric(
    elapsed_minutes,
    "elapsed_minutes"
  )
  
  assert_binary_scalar(
    previous_rank1_nonzero,
    "previous_rank1_nonzero"
  )
  
  assert_binary_scalar(
    previous_rank2_nonzero,
    "previous_rank2_nonzero"
  )
  
  assert_binary_scalar(
    session_start,
    "session_start"
  )
  
  if (
    session_progress <
    0 ||
    session_progress >
    1
  ) {
    stop(
      "session_progress must be between 0 and 1.",
      call. =
        FALSE
    )
  }
  
  if (
    elapsed_minutes <=
    0
  ) {
    stop(
      "elapsed_minutes must be positive.",
      call. =
        FALSE
    )
  }
  
  newdata <-
    tibble(
      SessionProgress =
        session_progress,
      SessionProgressSquared =
        session_progress^2,
      ElapsedMinutes =
        elapsed_minutes,
      LogElapsedMinutes =
        log1p(
          elapsed_minutes
        ),
      PreviousRank1Nonzero =
        previous_rank1_nonzero,
      PreviousRank2Nonzero =
        previous_rank2_nonzero,
      SessionStart =
        session_start
    )
  
  validate_daily_future_features(
    newdata
  )
  
  newdata
}


# ------------------------------------------------------------
# 14. Predict One Hurdle Mean
# ------------------------------------------------------------

predict_daily_hurdle_mean_production <- function(
    fitted_model,
    newdata
) {
  
  probability_fit <-
    fitted_model[["probability_fit"]]
  
  magnitude_fit <-
    fitted_model[["magnitude_fit"]]
  
  if (
    is.null(
      probability_fit
    )
  ) {
    stop(
      "Fitted hurdle model is missing probability_fit.",
      call. =
        FALSE
    )
  }
  
  if (
    is.null(
      magnitude_fit
    )
  ) {
    stop(
      "Fitted hurdle model is missing magnitude_fit.",
      call. =
        FALSE
    )
  }
  
  probability_nonzero <-
    as.numeric(
      stats::predict(
        probability_fit,
        newdata =
          newdata,
        type =
          "response"
      )
    )
  
  conditional_nonzero_mean <-
    as.numeric(
      stats::predict(
        magnitude_fit,
        newdata =
          newdata
      )
    )
  
  if (
    length(
      probability_nonzero
    ) !=
    1L ||
    !is.finite(
      probability_nonzero
    )
  ) {
    stop(
      "Predicted hurdle probability is invalid.",
      call. =
        FALSE
    )
  }
  
  if (
    length(
      conditional_nonzero_mean
    ) !=
    1L ||
    !is.finite(
      conditional_nonzero_mean
    )
  ) {
    stop(
      "Predicted conditional nonzero mean is invalid.",
      call. =
        FALSE
    )
  }
  
  probability_nonzero <-
    min(
      max(
        probability_nonzero,
        DAILY_PROBABILITY_FLOOR
      ),
      DAILY_PROBABILITY_CEILING
    )
  
  unconditional_mean <-
    probability_nonzero *
    conditional_nonzero_mean
  
  list(
    probability_nonzero =
      probability_nonzero,
    conditional_nonzero_mean =
      conditional_nonzero_mean,
    unconditional_mean =
      unconditional_mean
  )
}


# ------------------------------------------------------------
# 15. Forecast Both Daily Marginal Means
# ------------------------------------------------------------

forecast_daily_raw_means <- function(
    fitted_mean_models,
    newdata
) {
  validate_daily_future_features(
    newdata
  )
  
  rank1_forecast <-
    predict_daily_hurdle_mean_production(
      fitted_model =
        fitted_mean_models[["Rank1"]],
      newdata =
        newdata
    )
  
  rank2_forecast <-
    predict_daily_hurdle_mean_production(
      fitted_model =
        fitted_mean_models[["Rank2"]],
      newdata =
        newdata
    )
  
  raw_mean_vector <-
    c(
      h1 =
        rank1_forecast[["unconditional_mean"]],
      h2 =
        rank2_forecast[["unconditional_mean"]]
    )
  
  list(
    raw_mean =
      raw_mean_vector,
    Rank1 =
      rank1_forecast,
    Rank2 =
      rank2_forecast
  )
}


# ------------------------------------------------------------
# 16. Construct Full-Sample Mean Residuals
# ------------------------------------------------------------

construct_daily_full_sample_mean_residuals <- function(
    data,
    fitted_mean_models
) {
  rank1_probability <-
    as.numeric(
      stats::predict(
        fitted_mean_models[["Rank1"]][["probability_fit"]],
        newdata =
          data,
        type =
          "response"
      )
    )
  
  rank1_nonzero_mean <-
    as.numeric(
      stats::predict(
        fitted_mean_models[["Rank1"]][["magnitude_fit"]],
        newdata =
          data
      )
    )
  
  rank2_probability <-
    as.numeric(
      stats::predict(
        fitted_mean_models[["Rank2"]][["probability_fit"]],
        newdata =
          data,
        type =
          "response"
      )
    )
  
  rank2_nonzero_mean <-
    as.numeric(
      stats::predict(
        fitted_mean_models[["Rank2"]][["magnitude_fit"]],
        newdata =
          data
      )
    )
  
  rank1_probability <-
    pmin(
      pmax(
        rank1_probability,
        DAILY_PROBABILITY_FLOOR
      ),
      DAILY_PROBABILITY_CEILING
    )
  
  rank2_probability <-
    pmin(
      pmax(
        rank2_probability,
        DAILY_PROBABILITY_FLOOR
      ),
      DAILY_PROBABILITY_CEILING
    )
  
  rank1_mean <-
    rank1_probability *
    rank1_nonzero_mean
  
  rank2_mean <-
    rank2_probability *
    rank2_nonzero_mean
  
  rank1_residuals <-
    data[["DailyRank1Pnl"]] -
    rank1_mean
  
  rank2_residuals <-
    data[["DailyRank2Pnl"]] -
    rank2_mean
  
  if (
    any(
      !is.finite(
        rank1_residuals
      )
    ) ||
    any(
      !is.finite(
        rank2_residuals
      )
    )
  ) {
    stop(
      "Full-sample mean residuals contain non-finite values.",
      call. = FALSE
    )
  }
  
  list(
    Rank1 =
      data |>
      mutate(
        ConditionalMean =
          rank1_mean,
        MeanResidual =
          rank1_residuals,
        SquaredResidual =
          rank1_residuals^2
      ),
    Rank2 =
      data |>
      mutate(
        ConditionalMean =
          rank2_mean,
        MeanResidual =
          rank2_residuals,
        SquaredResidual =
          rank2_residuals^2
      )
  )
}


# ------------------------------------------------------------
# 17. Fit Rank1 Retained Variance Model
# ------------------------------------------------------------

fit_daily_rank1_variance_model <- function(
    residual_data
) {
  
  required_columns <-
    c(
      "SquaredResidual"
    )
  
  missing_columns <-
    setdiff(
      required_columns,
      names(
        residual_data
      )
    )
  
  if (
    length(
      missing_columns
    ) >
    0
  ) {
    
    stop(
      paste0(
        "Missing required Rank1 variance columns: ",
        paste(
          missing_columns,
          collapse =
            ", "
        )
      )
    )
  }
  
  squared_residual <-
    residual_data[["SquaredResidual"]]
  
  if (
    !all(
      is.finite(
        squared_residual
      )
    )
  ) {
    
    stop(
      "Rank1 squared residuals contain non-finite values."
    )
  }
  
  variance_scale <-
    mean(
      squared_residual
    )
  
  if (
    !is.finite(
      variance_scale
    ) ||
    variance_scale <=
    0
  ) {
    
    stop(
      "Rank1 variance scale must be positive and finite."
    )
  }
  
  fit_data <-
    residual_data |>
    mutate(
      VarianceTarget =
        pmax(
          SquaredResidual /
            variance_scale,
          DAILY_VARIANCE_RESPONSE_FLOOR
        )
    )
  
  if (
    !all(
      is.finite(
        fit_data[["VarianceTarget"]]
      )
    )
  ) {
    
    stop(
      "Rank1 scaled variance target contains non-finite values."
    )
  }
  
  if (
    any(
      fit_data[["VarianceTarget"]] <=
      0
    )
  ) {
    
    stop(
      "Rank1 scaled variance target must be strictly positive."
    )
  }
  
  stable_start <-
    c(
      log(
        mean(
          fit_data[["VarianceTarget"]]
        )
      )
    )
  
  fitted_model <-
    glm(
      VarianceTarget ~ 1,
      data =
        fit_data,
      family =
        Gamma(
          link =
            "log"
        ),
      start =
        stable_start,
      control =
        glm.control(
          maxit =
            200,
          epsilon =
            1e-8
        )
    )
  
  if (
    !isTRUE(
      fitted_model[["converged"]]
    )
  ) {
    
    stop(
      "Rank1 constant variance model did not converge."
    )
  }
  
  fitted_response <-
    as.numeric(
      predict(
        fitted_model,
        newdata =
          fit_data,
        type =
          "response"
      )
    )
  
  if (
    !all(
      is.finite(
        fitted_response
      )
    )
  ) {
    
    stop(
      "Rank1 fitted scaled variances contain non-finite values."
    )
  }
  
  if (
    any(
      fitted_response <=
      0
    )
  ) {
    
    stop(
      "Rank1 fitted scaled variances must be strictly positive."
    )
  }
  
  list(
    rank =
      "Rank1",
    model_id =
      "constant",
    fitted_model =
      fitted_model,
    variance_scale =
      variance_scale
  )
}


# ------------------------------------------------------------
# 18. Fit Rank2 Retained Variance Model
# ------------------------------------------------------------

fit_daily_rank2_variance_model <- function(
    residual_data
) {
  variance_scale <-
    mean(
      residual_data[["SquaredResidual"]]
    )
  
  if (
    !is.finite(
      variance_scale
    ) ||
    variance_scale <=
    0
  ) {
    stop(
      "Rank2 variance scale must be finite and positive.",
      call. =
        FALSE
    )
  }
  
  fit_data <-
    residual_data |>
    mutate(
      VarianceTarget =
        pmax(
          SquaredResidual /
            variance_scale,
          DAILY_VARIANCE_RESPONSE_FLOOR
        )
    )
  
  stable_start <-
    c(
      log(
        mean(
          fit_data[["VarianceTarget"]]
        )
      ),
      0,
      0
    )
  
  fitted_model <-
    glm(
      VarianceTarget ~
        SessionProgress +
        SessionProgressSquared,
      family =
        Gamma(
          link =
            "log"
        ),
      data =
        fit_data,
      start =
        stable_start,
      control =
        glm.control(
          maxit =
            200,
          epsilon =
            1e-8
        )
    )
  
  if (
    !isTRUE(
      fitted_model[["converged"]]
    )
  ) {
    stop(
      "Rank2 production variance model did not converge.",
      call. =
        FALSE
    )
  }
  
  list(
    rank =
      "Rank2",
    model_id =
      "quadratic_progress",
    fitted_model =
      fitted_model,
    variance_scale =
      variance_scale
  )
}

# ------------------------------------------------------------
# 19. Fit Both Retained Variance Models
# ------------------------------------------------------------

fit_daily_selected_variance_models <- function(
    residual_data
) {
  list(
    Rank1 =
      fit_daily_rank1_variance_model(
        residual_data[["Rank1"]]
      ),
    Rank2 =
      fit_daily_rank2_variance_model(
        residual_data[["Rank2"]]
      )
  )
}


# ------------------------------------------------------------
# 20. Predict One Marginal Variance
# ------------------------------------------------------------

predict_daily_marginal_variance <- function(
    fitted_variance_model,
    newdata
) {
  scaled_variance <-
    as.numeric(
      predict(
        fitted_variance_model[["fitted_model"]],
        newdata =
          newdata,
        type =
          "response"
      )
    )
  
  variance_forecast <-
    scaled_variance *
    fitted_variance_model[["variance_scale"]]
  
  if (
    length(
      variance_forecast
    ) !=
    1L ||
    !is.finite(
      variance_forecast
    )
  ) {
    stop(
      "Marginal variance forecast is invalid.",
      call. =
        FALSE
    )
  }
  
  variance_forecast <-
    max(
      variance_forecast,
      DAILY_VARIANCE_FORECAST_FLOOR
    )
  
  variance_forecast
}


# ------------------------------------------------------------
# 21. Forecast Both Daily Marginal Variances
# ------------------------------------------------------------

forecast_daily_raw_variances <- function(
    fitted_variance_models,
    newdata
) {
  validate_daily_future_features(
    newdata
  )
  
  rank1_variance <-
    predict_daily_marginal_variance(
      fitted_variance_model =
        fitted_variance_models[["Rank1"]],
      newdata =
        newdata
    )
  
  rank2_variance <-
    predict_daily_marginal_variance(
      fitted_variance_model =
        fitted_variance_models[["Rank2"]],
      newdata =
        newdata
    )
  
  c(
    h1 =
      rank1_variance,
    h2 =
      rank2_variance
  )
}


# ------------------------------------------------------------
# 22. Construct Raw-Process Covariance Matrix
# ------------------------------------------------------------

construct_daily_raw_covariance <- function(
    marginal_variances
) {
  if (
    length(
      marginal_variances
    ) !=
    2L ||
    any(
      !is.finite(
        marginal_variances
      )
    ) ||
    any(
      marginal_variances <=
      0
    )
  ) {
    stop(
      "marginal_variances must contain two finite positive values.",
      call. =
        FALSE
    )
  }
  
  marginal_volatilities <-
    sqrt(
      marginal_variances
    )
  
  D_t <-
    diag(
      marginal_volatilities,
      nrow =
        2L,
      ncol =
        2L
    )
  
  rownames(
    D_t
  ) <-
    c(
      "h1",
      "h2"
    )
  
  colnames(
    D_t
  ) <-
    c(
      "h1",
      "h2"
    )
  
  covariance_matrix <-
    D_t %*%
    DAILY_RESIDUAL_CORRELATION_MATRIX %*%
    D_t
  
  rownames(
    covariance_matrix
  ) <-
    c(
      "h1",
      "h2"
    )
  
  colnames(
    covariance_matrix
  ) <-
    c(
      "h1",
      "h2"
    )
  
  covariance_matrix
}


# ------------------------------------------------------------
# 23. Validate Covariance Matrix
# ------------------------------------------------------------

validate_daily_covariance_matrix <- function(
    covariance_matrix,
    expected_names =
      NULL
) {
  if (
    !is.matrix(
      covariance_matrix
    ) ||
    !all(
      dim(
        covariance_matrix
      ) ==
      c(
        2L,
        2L
      )
    )
  ) {
    stop(
      "Daily covariance matrix must be 2x2.",
      call. =
        FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        covariance_matrix
      )
    )
  ) {
    stop(
      "Daily covariance matrix contains non-finite values.",
      call. =
        FALSE
    )
  }
  
  if (
    !isTRUE(
      all.equal(
        covariance_matrix,
        t(
          covariance_matrix
        ),
        tolerance =
        1e-10
      )
    )
  ) {
    stop(
      "Daily covariance matrix is not symmetric.",
      call. =
        FALSE
    )
  }
  
  eigenvalues <-
    eigen(
      covariance_matrix,
      symmetric =
        TRUE,
      only.values =
        TRUE
    )[["values"]]
  
  if (
    any(
      !is.finite(
        eigenvalues
      )
    ) ||
    any(
      eigenvalues <=
      0
    )
  ) {
    stop(
      "Daily covariance matrix is not positive definite.",
      call. =
        FALSE
    )
  }
  
  if (
    !is.null(
      expected_names
    )
  ) {
    if (
      !identical(
        rownames(
          covariance_matrix
        ),
        expected_names
      ) ||
      !identical(
        colnames(
          covariance_matrix
        ),
        expected_names
      )
    ) {
      stop(
        "Daily covariance matrix names do not match expected ordering.",
        call. =
          FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}


# ------------------------------------------------------------
# 24. Transform Raw Means Into Daily State Means
# ------------------------------------------------------------

transform_daily_raw_mean_to_state <- function(
    raw_mean
) {
  if (
    length(
      raw_mean
    ) !=
    2L ||
    any(
      !is.finite(
        raw_mean
      )
    )
  ) {
    stop(
      "raw_mean must contain two finite values.",
      call. =
        FALSE
    )
  }
  
  raw_mean_matrix <-
    matrix(
      raw_mean,
      ncol =
        1L
    )
  
  state_mean <-
    DAILY_STATE_TRANSFORM %*%
    raw_mean_matrix
  
  state_mean <-
    as.numeric(
      state_mean
    )
  
  names(
    state_mean
  ) <-
    rownames(
      DAILY_STATE_TRANSFORM
    )
  
  state_mean
}


# ------------------------------------------------------------
# 25. Transform Raw Covariance Into Daily State Covariance
# ------------------------------------------------------------

transform_daily_raw_covariance_to_state <- function(
    raw_covariance
) {
  validate_daily_covariance_matrix(
    raw_covariance,
    expected_names =
      c(
        "h1",
        "h2"
      )
  )
  
  state_covariance <-
    DAILY_STATE_TRANSFORM %*%
    raw_covariance %*%
    t(
      DAILY_STATE_TRANSFORM
    )
  
  rownames(
    state_covariance
  ) <-
    rownames(
      DAILY_STATE_TRANSFORM
    )
  
  colnames(
    state_covariance
  ) <-
    rownames(
      DAILY_STATE_TRANSFORM
    )
  
  validate_daily_covariance_matrix(
    state_covariance,
    expected_names =
      c(
        "Delta_Daily_H1",
        "Delta_Daily_D12"
      )
  )
  
  state_covariance
}


# ------------------------------------------------------------
# 26. Fit Complete Daily Production System
# ------------------------------------------------------------

fit_daily_competitor_forecasting_system <- function(
    data =
      get_daily_forecasting_training_data()
) {
  mean_models <-
    fit_daily_selected_mean_models(
      data
    )
  
  residual_data <-
    construct_daily_full_sample_mean_residuals(
      data =
        data,
      fitted_mean_models =
        mean_models
    )
  
  variance_models <-
    fit_daily_selected_variance_models(
      residual_data =
        residual_data
    )
  
  list(
    training_data =
      data,
    selected_mean_models =
      DAILY_SELECTED_MEAN_MODELS,
    selected_variance_models =
      DAILY_SELECTED_VARIANCE_MODELS,
    standardized_residual_correlation =
      DAILY_STANDARDIZED_RESIDUAL_CORRELATION,
    residual_correlation_matrix =
      DAILY_RESIDUAL_CORRELATION_MATRIX,
    state_transform =
      DAILY_STATE_TRANSFORM,
    fitted_mean_models =
      mean_models,
    mean_residual_data =
      residual_data,
    fitted_variance_models =
      variance_models
  )
}


# ------------------------------------------------------------
# 27. Forecast One Future Daily Interval
# ------------------------------------------------------------

forecast_daily_competitor_interval <- function(
    fitted_system,
    newdata
) {
  validate_daily_future_features(
    newdata
  )
  
  mean_forecasts <-
    forecast_daily_raw_means(
      fitted_mean_models =
        fitted_system[["fitted_mean_models"]],
      newdata =
        newdata
    )
  
  raw_mean <-
    mean_forecasts[["raw_mean"]]
  
  marginal_variances <-
    forecast_daily_raw_variances(
      fitted_variance_models =
        fitted_system[["fitted_variance_models"]],
      newdata =
        newdata
    )
  
  marginal_volatilities <-
    sqrt(
      marginal_variances
    )
  
  raw_covariance <-
    construct_daily_raw_covariance(
      marginal_variances =
        marginal_variances
    )
  
  validate_daily_covariance_matrix(
    raw_covariance,
    expected_names =
      c(
        "h1",
        "h2"
      )
  )
  
  state_mean <-
    transform_daily_raw_mean_to_state(
      raw_mean =
        raw_mean
    )
  
  state_covariance <-
    transform_daily_raw_covariance_to_state(
      raw_covariance =
        raw_covariance
    )
  
  list(
    features =
      newdata,
    probability_nonzero =
      c(
        Rank1 =
          mean_forecasts[["Rank1"]][["probability_nonzero"]],
        Rank2 =
          mean_forecasts[["Rank2"]][["probability_nonzero"]]
      ),
    conditional_nonzero_mean =
      c(
        Rank1 =
          mean_forecasts[["Rank1"]][["conditional_nonzero_mean"]],
        Rank2 =
          mean_forecasts[["Rank2"]][["conditional_nonzero_mean"]]
      ),
    raw_mean =
      raw_mean,
    marginal_variance =
      marginal_variances,
    marginal_volatility =
      marginal_volatilities,
    residual_correlation =
      fitted_system[["residual_correlation_matrix"]],
    raw_covariance =
      raw_covariance,
    state_mean =
      state_mean,
    state_covariance =
      state_covariance
  )
}


# ------------------------------------------------------------
# 28. Convenience Wrapper From Scalar Inputs
# ------------------------------------------------------------

forecast_daily_competitor_from_inputs <- function(
    fitted_system,
    session_progress,
    elapsed_minutes,
    previous_rank1_nonzero,
    previous_rank2_nonzero,
    session_start =
      0
) {
  newdata <-
    build_daily_future_feature_row(
      session_progress =
        session_progress,
      elapsed_minutes =
        elapsed_minutes,
      previous_rank1_nonzero =
        previous_rank1_nonzero,
      previous_rank2_nonzero =
        previous_rank2_nonzero,
      session_start =
        session_start
    )
  
  forecast_daily_competitor_interval(
    fitted_system =
      fitted_system,
    newdata =
      newdata
  )
}


# ------------------------------------------------------------
# 29. Forecast Multiple Pre-Built Future Rows
# ------------------------------------------------------------

forecast_daily_competitor_path <- function(
    fitted_system,
    future_features
) {
  if (
    !is.data.frame(
      future_features
    ) ||
    nrow(
      future_features
    ) <
    1L
  ) {
    stop(
      "future_features must contain at least one row.",
      call. =
        FALSE
    )
  }
  
  purrr::map(
    seq_len(
      nrow(
        future_features
      )
    ),
    function(i) {
      forecast_daily_competitor_interval(
        fitted_system =
          fitted_system,
        newdata =
          future_features[
            i,
            ,
            drop =
              FALSE
          ]
      )
    }
  )
}


# ------------------------------------------------------------
# 30. Convert Forecast List To Mean Table
# ------------------------------------------------------------

daily_forecasts_to_mean_table <- function(
    forecasts
) {
  purrr::map_dfr(
    seq_along(
      forecasts
    ),
    function(i) {
      forecast <-
        forecasts[[i]]
      
      tibble(
        ForecastIndex =
          i,
        Mean_h1 =
          forecast[["raw_mean"]][["h1"]],
        Mean_h2 =
          forecast[["raw_mean"]][["h2"]],
        Mean_Delta_Daily_H1 =
          forecast[["state_mean"]][["Delta_Daily_H1"]],
        Mean_Delta_Daily_D12 =
          forecast[["state_mean"]][["Delta_Daily_D12"]]
      )
    }
  )
}


# ------------------------------------------------------------
# 31. Convert Forecast List To Raw Covariance Array
# ------------------------------------------------------------

daily_forecasts_to_raw_covariance_array <- function(
    forecasts
) {
  n_forecasts <-
    length(
      forecasts
    )
  
  covariance_array <-
    array(
      NA_real_,
      dim =
        c(
          2L,
          2L,
          n_forecasts
        ),
      dimnames =
        list(
          c(
            "h1",
            "h2"
          ),
          c(
            "h1",
            "h2"
          ),
          NULL
        )
    )
  
  for (
    i in
    seq_len(
      n_forecasts
    )
  ) {
    covariance_array[, , i] <-
      forecasts[[i]][["raw_covariance"]]
  }
  
  covariance_array
}


# ------------------------------------------------------------
# 32. Convert Forecast List To State Covariance Array
# ------------------------------------------------------------

daily_forecasts_to_state_covariance_array <- function(
    forecasts
) {
  n_forecasts <-
    length(
      forecasts
    )
  
  covariance_array <-
    array(
      NA_real_,
      dim =
        c(
          2L,
          2L,
          n_forecasts
        ),
      dimnames =
        list(
          c(
            "Delta_Daily_H1",
            "Delta_Daily_D12"
          ),
          c(
            "Delta_Daily_H1",
            "Delta_Daily_D12"
          ),
          NULL
        )
    )
  
  for (
    i in
    seq_len(
      n_forecasts
    )
  ) {
    covariance_array[, , i] <-
      forecasts[[i]][["state_covariance"]]
  }
  
  covariance_array
}


# ------------------------------------------------------------
# 33. Update Daily State Levels
# ------------------------------------------------------------

update_daily_state_levels <- function(
    current_H1,
    current_D12,
    realized_h1,
    realized_h2,
    session_start =
      0
) {
  assert_one_finite_numeric(
    current_H1,
    "current_H1"
  )
  
  assert_one_finite_numeric(
    current_D12,
    "current_D12"
  )
  
  assert_one_finite_numeric(
    realized_h1,
    "realized_h1"
  )
  
  assert_one_finite_numeric(
    realized_h2,
    "realized_h2"
  )
  
  assert_binary_scalar(
    session_start,
    "session_start"
  )
  
  if (
    current_D12 <
    0
  ) {
    stop(
      "current_D12 cannot be negative.",
      call. =
        FALSE
    )
  }
  
  if (
    session_start ==
    1
  ) {
    next_H1 <-
      realized_h1
    
    next_D12 <-
      max(
        0,
        realized_h1 -
          realized_h2
      )
  } else {
    next_H1 <-
      current_H1 +
      realized_h1
    
    next_D12 <-
      max(
        0,
        current_D12 +
          realized_h1 -
          realized_h2
      )
  }
  
  next_H2 <-
    next_H1 -
    next_D12
  
  c(
    Daily_H1 =
      next_H1,
    Daily_D12 =
      next_D12,
    Daily_H2 =
      next_H2
  )
}


# ------------------------------------------------------------
# 34. Validate Fixed Production Objects
# ------------------------------------------------------------

validate_daily_competitor_forecasting_constants <- function() {
  if (
    !identical(
      names(
        DAILY_SELECTED_MEAN_MODELS
      ),
      c(
        "Rank1",
        "Rank2"
      )
    )
  ) {
    stop(
      "Mean-model ordering is invalid.",
      call. =
        FALSE
    )
  }
  
  if (
    !identical(
      names(
        DAILY_SELECTED_VARIANCE_MODELS
      ),
      c(
        "Rank1",
        "Rank2"
      )
    )
  ) {
    stop(
      "Variance-model ordering is invalid.",
      call. =
        FALSE
    )
  }
  
  if (
    DAILY_SELECTED_MEAN_MODELS[["Rank1"]] !=
    "hurdle_persistence" ||
    DAILY_SELECTED_MEAN_MODELS[["Rank2"]] !=
    "hurdle_persistence"
  ) {
    stop(
      "Unexpected retained daily mean models.",
      call. =
        FALSE
    )
  }
  
  if (
    DAILY_SELECTED_VARIANCE_MODELS[["Rank1"]] !=
    "constant" ||
    DAILY_SELECTED_VARIANCE_MODELS[["Rank2"]] !=
    "quadratic_progress"
  ) {
    stop(
      "Unexpected retained daily variance models.",
      call. =
        FALSE
    )
  }
  
  if (
    !is.finite(
      DAILY_STANDARDIZED_RESIDUAL_CORRELATION
    ) ||
    abs(
      DAILY_STANDARDIZED_RESIDUAL_CORRELATION
    ) >=
    1
  ) {
    stop(
      "Daily standardized residual correlation is invalid.",
      call. =
        FALSE
    )
  }
  
  if (
    !isTRUE(
      all.equal(
        DAILY_RESIDUAL_CORRELATION_MATRIX,
        t(
          DAILY_RESIDUAL_CORRELATION_MATRIX
        ),
        tolerance =
        1e-12
      )
    )
  ) {
    stop(
      "Daily residual correlation matrix is not symmetric.",
      call. =
        FALSE
    )
  }
  
  if (
    any(
      abs(
        diag(
          DAILY_RESIDUAL_CORRELATION_MATRIX
        ) -
        1
      ) >
      1e-12
    )
  ) {
    stop(
      "Daily residual correlation matrix does not have unit diagonal.",
      call. =
        FALSE
    )
  }
  
  correlation_eigenvalues <-
    eigen(
      DAILY_RESIDUAL_CORRELATION_MATRIX,
      symmetric =
        TRUE,
      only.values =
        TRUE
    )[["values"]]
  
  if (
    any(
      correlation_eigenvalues <=
      0
    )
  ) {
    stop(
      "Daily residual correlation matrix is not positive definite.",
      call. =
        FALSE
    )
  }
  
  expected_transform <-
    matrix(
      c(
        1,
        0,
        1,
        -1
      ),
      nrow =
        2L,
      byrow =
        TRUE
    )
  
  if (
    !isTRUE(
      all.equal(
        unname(
          DAILY_STATE_TRANSFORM
        ),
        expected_transform,
        tolerance =
        0
      )
    )
  ) {
    stop(
      "Daily state transformation is invalid.",
      call. =
        FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ------------------------------------------------------------
# 35. Validate Fitted Production System
# ------------------------------------------------------------

validate_daily_competitor_forecasting_system <- function(
    fitted_system
) {
  required_objects <-
    c(
      "training_data",
      "selected_mean_models",
      "selected_variance_models",
      "standardized_residual_correlation",
      "residual_correlation_matrix",
      "state_transform",
      "fitted_mean_models",
      "mean_residual_data",
      "fitted_variance_models"
    )
  
  missing_objects <-
    setdiff(
      required_objects,
      names(
        fitted_system
      )
    )
  
  if (
    length(
      missing_objects
    ) >
    0L
  ) {
    stop(
      paste0(
        "Fitted daily forecasting system is missing: ",
        paste(
          missing_objects,
          collapse =
            ", "
        )
      ),
      call. =
        FALSE
    )
  }
  
  validate_daily_competitor_forecasting_constants()
  
  if (
    nrow(
      fitted_system[["training_data"]]
    ) <
    1L
  ) {
    stop(
      "Daily forecasting system contains no training observations.",
      call. =
        FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ------------------------------------------------------------
# 36. Main Production Fit Convenience Function
# ------------------------------------------------------------

build_daily_competitor_forecasting_system <- function() {
  fitted_system <-
    fit_daily_competitor_forecasting_system()
  
  validate_daily_competitor_forecasting_system(
    fitted_system
  )
  
  fitted_system
}


# ------------------------------------------------------------
# 37. Script-Level Validation
# ------------------------------------------------------------

validate_daily_competitor_forecasting_constants()


# ------------------------------------------------------------
# 38. Completion Message
# ------------------------------------------------------------

message(
  paste0(
    "daily_competitor_forecasting.R loaded successfully. ",
    "Retained models: Rank1 mean=hurdle_persistence, ",
    "Rank2 mean=hurdle_persistence, ",
    "Rank1 variance=constant, ",
    "Rank2 variance=quadratic_progress, ",
    "rho=",
    round(
      DAILY_STANDARDIZED_RESIDUAL_CORRELATION,
      7
    ),
    "."
  )
)
