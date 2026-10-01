# ============================================================
# daily_competitor_models.R
# ============================================================
#
# Purpose:
#   Define candidate conditional-mean models for the two
#   daily leaderboard interval P&L processes:
#
#       h1_t = DailyRank1Pnl
#       h2_t = DailyRank2Pnl
#
# Important interpretation:
#   Rank 1 and Rank 2 are leaderboard POSITIONS at each
#   timestamp, not fixed competitors.
#
#   Therefore, these models forecast changes in the first-
#   and second-place daily leaderboard levels, not the P&L
#   of persistent individual traders.
#
# This script:
#   1. prepares model features;
#   2. defines evidence-based candidate mean models;
#   3. fits continuous and hurdle models;
#   4. produces one-step conditional-mean predictions;
#   5. supports expanding-origin out-of-sample forecasting;
#   6. evaluates point forecasts.
#
# This script DOES NOT:
#   - select the winning model;
#   - fit production variance models;
#   - estimate cross-rank dependence;
#   - produce final production forecast CSVs.
#
# Model selection will occur in:
#
#   26_DailyRank_Mean_Models.Rmd
#
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# ============================================================
# Numerical Settings
# ============================================================

DAILY_PROBABILITY_FLOOR <- 1e-8

DAILY_PROBABILITY_CEILING <-
  1 -
  DAILY_PROBABILITY_FLOOR

# ============================================================
# 2. Source Daily Data Pipeline
# ============================================================

source(
  here(
    "scripts",
    "daily_competitor_data.R"
  )
)


# ============================================================
# 3. Supported Daily Rank Processes
# ============================================================

DAILY_MEAN_RANKS <- c(
  "Rank1",
  "Rank2"
)


DAILY_MEAN_RESPONSE_COLUMNS <- c(
  Rank1 = "DailyRank1Pnl",
  Rank2 = "DailyRank2Pnl"
)


# ============================================================
# 4. Modeling Data Preparation
# ============================================================

prepare_daily_mean_model_data <- function(
    data = NULL
) {
  
  if (is.null(data)) {
    
    prepared <-
      prepare_daily_competitor_data()
    
    data <-
      prepared$data
  }
  
  
  required_columns <- c(
    "Observation",
    "Timestamp",
    "DailySession",
    "DailySessionObservation",
    "DailySessionObservations",
    "ElapsedMinutes",
    "LogElapsedMinutes",
    "LongInterval",
    "DailyRank1Pnl",
    "DailyRank2Pnl"
  )
  
  
  missing_columns <-
    setdiff(
      required_columns,
      names(data)
    )
  
  
  if (length(missing_columns) > 0L) {
    
    stop(
      paste0(
        "Daily mean-model data are missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  model_data <-
    data |>
    arrange(
      Observation
    ) |>
    group_by(
      DailySession
    ) |>
    mutate(
      SessionProgress =
        if_else(
          DailySessionObservations > 1L,
          (
            DailySessionObservation - 1
          ) /
            (
              DailySessionObservations - 1
            ),
          0
        ),
      
      SessionProgressSquared =
        SessionProgress^2,
      
      SessionStart =
        as.integer(
          DailySessionObservation == 1L
        ),
      
      Rank1Nonzero =
        as.integer(
          DailyRank1Pnl != 0
        ),
      
      Rank2Nonzero =
        as.integer(
          DailyRank2Pnl != 0
        ),
      
      PreviousRank1Pnl =
        dplyr::lag(
          DailyRank1Pnl
        ),
      
      PreviousRank2Pnl =
        dplyr::lag(
          DailyRank2Pnl
        ),
      
      PreviousRank1Nonzero =
        dplyr::lag(
          Rank1Nonzero
        ),
      
      PreviousRank2Nonzero =
        dplyr::lag(
          Rank2Nonzero
        )
    ) |>
    ungroup() |>
    mutate(
      PreviousRank1Pnl =
        if_else(
          SessionStart == 1L,
          0,
          PreviousRank1Pnl
        ),
      
      PreviousRank2Pnl =
        if_else(
          SessionStart == 1L,
          0,
          PreviousRank2Pnl
        ),
      
      PreviousRank1Nonzero =
        if_else(
          SessionStart == 1L,
          0L,
          PreviousRank1Nonzero
        ),
      
      PreviousRank2Nonzero =
        if_else(
          SessionStart == 1L,
          0L,
          PreviousRank2Nonzero
        ),
      
      ClockMinutes =
        lubridate::hour(
          Timestamp
        ) * 60 +
        lubridate::minute(
          Timestamp
        ) +
        lubridate::second(
          Timestamp
        ) / 60,
      
      ClockSin =
        sin(
          2 * pi *
            ClockMinutes /
            1440
        ),
      
      ClockCos =
        cos(
          2 * pi *
            ClockMinutes /
            1440
        )
    )
  
  
  numeric_feature_columns <- c(
    "SessionProgress",
    "SessionProgressSquared",
    "ElapsedMinutes",
    "LogElapsedMinutes",
    "LongInterval",
    "SessionStart",
    "PreviousRank1Pnl",
    "PreviousRank2Pnl",
    "PreviousRank1Nonzero",
    "PreviousRank2Nonzero",
    "ClockSin",
    "ClockCos"
  )
  
  
  finite_check <-
    vapply(
      model_data[
        numeric_feature_columns
      ],
      function(x) {
        all(
          is.finite(x)
        )
      },
      logical(1)
    )
  
  
  if (!all(finite_check)) {
    
    stop(
      paste0(
        "Non-finite daily mean-model features found in: ",
        paste(
          names(finite_check)[
            !finite_check
          ],
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  model_data
}


# ============================================================
# 5. Rank-Specific Variable Names
# ============================================================

get_daily_rank_variables <- function(
    rank
) {
  
  if (!rank %in% DAILY_MEAN_RANKS) {
    
    stop(
      paste0(
        "Unsupported daily rank: ",
        rank
      ),
      call. = FALSE
    )
  }
  
  
  if (rank == "Rank1") {
    
    return(
      list(
        response =
          "DailyRank1Pnl",
        
        nonzero =
          "Rank1Nonzero",
        
        previous_pnl =
          "PreviousRank1Pnl",
        
        previous_nonzero =
          "PreviousRank1Nonzero"
      )
    )
  }
  
  
  list(
    response =
      "DailyRank2Pnl",
    
    nonzero =
      "Rank2Nonzero",
    
    previous_pnl =
      "PreviousRank2Pnl",
    
    previous_nonzero =
      "PreviousRank2Nonzero"
  )
}


# ============================================================
# 6. Candidate Model Registry
# ============================================================
#
# Evidence from Notebooks 25 and 25.5 motivates:
#
# Core continuous candidates:
#   - constant benchmark
#   - linear progress
#   - quadratic progress
#   - log elapsed time
#   - quadratic progress + log elapsed
#
# Robustness continuous candidates:
#   - quadratic progress + long interval
#   - quadratic progress + cyclical clock terms
#
# Serial candidate:
#   - lag-1 P&L + session-start indicator
#
# Hurdle candidates:
#   - deterministic hurdle
#   - persistence hurdle using previous nonzero state
#
# Rank-specific hurdle magnitude equations:
#
#   Rank 1:
#       nonzero probability:
#           quadratic progress
#
#       nonzero magnitude:
#           log elapsed time
#
#   Rank 2:
#       nonzero probability:
#           quadratic progress
#
#       nonzero magnitude:
#           quadratic progress + log elapsed time
#
# These are CANDIDATES only.
#
# Final selection must use expanding-origin OOS performance.
#
# ============================================================

get_daily_mean_candidate_registry <- function() {
  
  tibble(
    Rank = c(
      rep(
        "Rank1",
        10L
      ),
      rep(
        "Rank2",
        10L
      )
    ),
    
    ModelID = rep(
      c(
        "constant",
        "linear_progress",
        "quadratic_progress",
        "log_elapsed",
        "quadratic_progress_log_elapsed",
        "quadratic_progress_long_interval",
        "quadratic_progress_clock",
        "ar1",
        "hurdle_deterministic",
        "hurdle_persistence"
      ),
      times = 2L
    ),
    
    ModelClass = rep(
      c(
        rep(
          "continuous",
          8L
        ),
        rep(
          "hurdle",
          2L
        )
      ),
      times = 2L
    ),
    
    Description = c(
      
      # Rank 1
      "Constant unconditional-mean benchmark.",
      
      "Linear normalized session-progress model.",
      
      "Quadratic normalized session-progress model.",
      
      "Log elapsed-time model.",
      
      paste0(
        "Quadratic session progress plus log elapsed time; ",
        "best diagnostic unconditional-mean family."
      ),
      
      paste0(
        "Quadratic session progress plus long-interval indicator; ",
        "robustness alternative."
      ),
      
      paste0(
        "Quadratic session progress plus cyclical clock terms; ",
        "clock-time robustness alternative."
      ),
      
      paste0(
        "Within-session lag-1 leaderboard P&L with explicit ",
        "session-start indicator."
      ),
      
      paste0(
        "Hurdle model: quadratic-progress nonzero probability ",
        "and log-elapsed conditional nonzero Rank 1 magnitude."
      ),
      
      paste0(
        "Hurdle model with previous nonzero-state persistence: ",
        "quadratic progress plus previous state in the probability ",
        "equation and log-elapsed conditional nonzero magnitude."
      ),
      
      # Rank 2
      "Constant unconditional-mean benchmark.",
      
      "Linear normalized session-progress model.",
      
      "Quadratic normalized session-progress model.",
      
      "Log elapsed-time model.",
      
      paste0(
        "Quadratic session progress plus log elapsed time; ",
        "best diagnostic unconditional-mean family."
      ),
      
      paste0(
        "Quadratic session progress plus long-interval indicator; ",
        "robustness alternative."
      ),
      
      paste0(
        "Quadratic session progress plus cyclical clock terms; ",
        "clock-time robustness alternative."
      ),
      
      paste0(
        "Within-session lag-1 leaderboard P&L with explicit ",
        "session-start indicator."
      ),
      
      paste0(
        "Hurdle model: quadratic-progress nonzero probability ",
        "and quadratic-progress-plus-log-elapsed conditional ",
        "nonzero Rank 2 magnitude."
      ),
      
      paste0(
        "Hurdle model with previous nonzero-state persistence: ",
        "quadratic progress plus previous state in the probability ",
        "equation and quadratic-progress-plus-log-elapsed ",
        "conditional nonzero magnitude."
      )
    )
  )
}


# ============================================================
# 7. Candidate Formula Definitions
# ============================================================

get_daily_mean_model_definition <- function(
    rank,
    model_id
) {
  
  variables <-
    get_daily_rank_variables(
      rank
    )
  
  
  response <-
    variables[["response"]]
  
  
  nonzero <-
    variables[["nonzero"]]
  
  
  previous_pnl <-
    variables[["previous_pnl"]]
  
  
  previous_nonzero <-
    variables[["previous_nonzero"]]
  
  
  if (model_id == "constant") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = character(0)
      )
    )
  }
  
  
  if (model_id == "linear_progress") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = c(
          "SessionProgress"
        )
      )
    )
  }
  
  
  if (model_id == "quadratic_progress") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = c(
          "SessionProgress",
          "SessionProgressSquared"
        )
      )
    )
  }
  
  
  if (model_id == "log_elapsed") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = c(
          "LogElapsedMinutes"
        )
      )
    )
  }
  
  
  if (model_id == "quadratic_progress_log_elapsed") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = c(
          "SessionProgress",
          "SessionProgressSquared",
          "LogElapsedMinutes"
        )
      )
    )
  }
  
  
  if (model_id == "quadratic_progress_long_interval") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = c(
          "SessionProgress",
          "SessionProgressSquared",
          "LongInterval"
        )
      )
    )
  }
  
  
  if (model_id == "quadratic_progress_clock") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = c(
          "SessionProgress",
          "SessionProgressSquared",
          "ClockSin",
          "ClockCos"
        )
      )
    )
  }
  
  
  if (model_id == "ar1") {
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "continuous",
        response = response,
        mean_terms = c(
          previous_pnl,
          "SessionStart"
        )
      )
    )
  }
  
  
  if (model_id == "hurdle_deterministic") {
    
    if (rank == "Rank1") {
      
      magnitude_terms <- c(
        "LogElapsedMinutes"
      )
      
    } else {
      
      magnitude_terms <- c(
        "SessionProgress",
        "SessionProgressSquared",
        "LogElapsedMinutes"
      )
    }
    
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "hurdle",
        response = response,
        nonzero_response = nonzero,
        probability_terms = c(
          "SessionProgress",
          "SessionProgressSquared"
        ),
        magnitude_terms = magnitude_terms
      )
    )
  }
  
  
  if (model_id == "hurdle_persistence") {
    
    if (rank == "Rank1") {
      
      magnitude_terms <- c(
        "LogElapsedMinutes"
      )
      
    } else {
      
      magnitude_terms <- c(
        "SessionProgress",
        "SessionProgressSquared",
        "LogElapsedMinutes"
      )
    }
    
    
    return(
      list(
        rank = rank,
        model_id = model_id,
        model_class = "hurdle",
        response = response,
        nonzero_response = nonzero,
        probability_terms = c(
          "SessionProgress",
          "SessionProgressSquared",
          previous_nonzero,
          "SessionStart"
        ),
        magnitude_terms = magnitude_terms
      )
    )
  }
  
  
  stop(
    paste0(
      "Unknown daily mean model ID: ",
      model_id
    ),
    call. = FALSE
  )
}


# ============================================================
# 8. Formula Construction
# ============================================================

build_daily_formula <- function(
    response,
    terms
) {
  
  if (length(terms) == 0L) {
    
    return(
      stats::as.formula(
        paste0(
          response,
          " ~ 1"
        )
      )
    )
  }
  
  
  stats::reformulate(
    termlabels = terms,
    response = response
  )
}


# ============================================================
# 9. Validate Candidate Request
# ============================================================

validate_daily_mean_candidate <- function(
    rank,
    model_id
) {
  
  registry <-
    get_daily_mean_candidate_registry()
  
  
  valid <-
    any(
      registry[["Rank"]] == rank &
        registry[["ModelID"]] == model_id
    )
  
  
  if (!valid) {
    
    stop(
      paste0(
        "Unsupported daily mean candidate: rank = ",
        rank,
        ", model_id = ",
        model_id
      ),
      call. = FALSE
    )
  }
  
  
  invisible(
    TRUE
  )
}


# ============================================================
# 10. Fit Continuous Candidate
# ============================================================

fit_daily_continuous_mean_model <- function(
    data,
    definition
) {
  
  formula <-
    build_daily_formula(
      response =
        definition[["response"]],
      terms =
        definition[["mean_terms"]]
    )
  
  
  model <-
    stats::lm(
      formula = formula,
      data = data
    )
  
  
  list(
    model_class =
      "continuous",
    
    rank =
      definition[["rank"]],
    
    model_id =
      definition[["model_id"]],
    
    definition =
      definition,
    
    fit =
      model,
    
    training_observations =
      nrow(
        data
      )
  )
}


# ============================================================
# 11. Fit Hurdle Candidate
# ============================================================

fit_daily_hurdle_mean_model <- function(
    data,
    definition
) {
  
  nonzero_response <-
    definition[["nonzero_response"]]
  
  
  response <-
    definition[["response"]]
  
  
  probability_formula <-
    build_daily_formula(
      response =
        nonzero_response,
      terms =
        definition[["probability_terms"]]
    )
  
  
  magnitude_formula <-
    build_daily_formula(
      response =
        response,
      terms =
        definition[["magnitude_terms"]]
    )
  
  
  nonzero_values <-
    unique(
      data[[nonzero_response]]
    )
  
  
  if (length(nonzero_values) < 2L) {
    
    stop(
      paste0(
        "Hurdle probability model cannot be fitted because ",
        nonzero_response,
        " has only one observed state in the training sample."
      ),
      call. = FALSE
    )
  }
  
  
  probability_fit <-
    stats::glm(
      formula =
        probability_formula,
      family =
        stats::binomial(),
      data =
        data
    )
  
  
  magnitude_data <-
    data[
      data[[response]] != 0,
      ,
      drop = FALSE
    ]
  
  
  minimum_magnitude_observations <-
    length(
      definition[["magnitude_terms"]]
    ) + 2L
  
  
  if (
    nrow(
      magnitude_data
    ) <
    minimum_magnitude_observations
  ) {
    
    stop(
      paste0(
        "Insufficient nonzero observations to fit hurdle ",
        "magnitude model for ",
        definition[["rank"]],
        "."
      ),
      call. = FALSE
    )
  }
  
  
  magnitude_fit <-
    stats::lm(
      formula =
        magnitude_formula,
      data =
        magnitude_data
    )
  
  
  list(
    model_class =
      "hurdle",
    
    rank =
      definition[["rank"]],
    
    model_id =
      definition[["model_id"]],
    
    definition =
      definition,
    
    probability_fit =
      probability_fit,
    
    magnitude_fit =
      magnitude_fit,
    
    training_observations =
      nrow(
        data
      ),
    
    nonzero_training_observations =
      nrow(
        magnitude_data
      )
  )
}


# ============================================================
# 12. Fit Any Daily Mean Candidate
# ============================================================

fit_daily_mean_model <- function(
    data,
    rank,
    model_id
) {
  
  validate_daily_mean_candidate(
    rank = rank,
    model_id = model_id
  )
  
  
  definition <-
    get_daily_mean_model_definition(
      rank = rank,
      model_id = model_id
    )
  
  
  if (
    definition[["model_class"]] ==
    "continuous"
  ) {
    
    return(
      fit_daily_continuous_mean_model(
        data = data,
        definition = definition
      )
    )
  }
  
  
  if (
    definition[["model_class"]] ==
    "hurdle"
  ) {
    
    return(
      fit_daily_hurdle_mean_model(
        data = data,
        definition = definition
      )
    )
  }
  
  
  stop(
    paste0(
      "Unsupported model class: ",
      definition[["model_class"]]
    ),
    call. = FALSE
  )
}


# ============================================================
# 13. Predict Continuous Conditional Mean
# ============================================================

predict_daily_continuous_mean <- function(
    fitted_model,
    newdata
) {
  
  prediction <-
    stats::predict(
      fitted_model[["fit"]],
      newdata = newdata
    )
  
  
  as.numeric(
    prediction
  )
}


# ============================================================
# 14. Predict Hurdle Conditional Mean
# ============================================================
#
# If:
#
#   p_t = P(h_t != 0 | information)
#
# and:
#
#   m_t = E[h_t | h_t != 0, information]
#
# then:
#
#   E[h_t | information] = p_t * m_t
#
# ============================================================

predict_daily_hurdle_mean <- function(
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
  
  tibble(
    ProbabilityNonzero =
      probability_nonzero,
    ConditionalNonzeroMean =
      conditional_nonzero_mean,
    MeanForecast =
      unconditional_mean
  )
}


# ============================================================
# 15. Predict Any Daily Mean Candidate
# ============================================================

predict_daily_mean_model <- function(
    fitted_model,
    newdata
) {
  
  if (
    fitted_model[["model_class"]] ==
    "continuous"
  ) {
    
    mean_forecast <-
      predict_daily_continuous_mean(
        fitted_model =
          fitted_model,
        newdata =
          newdata
      )
    
    
    return(
      tibble(
        ProbabilityNonzero =
          NA_real_,
        
        ConditionalNonzeroMean =
          NA_real_,
        
        MeanForecast =
          mean_forecast
      )
    )
  }
  
  
  if (
    fitted_model[["model_class"]] ==
    "hurdle"
  ) {
    
    return(
      predict_daily_hurdle_mean(
        fitted_model =
          fitted_model,
        newdata =
          newdata
      )
    )
  }
  
  
  stop(
    paste0(
      "Unsupported fitted model class: ",
      fitted_model[["model_class"]]
    ),
    call. = FALSE
  )
}


# ============================================================
# 16. Safe One-Step Fit and Prediction
# ============================================================

safe_daily_mean_fit_predict <- function(
    training_data,
    forecast_row,
    rank,
    model_id
) {
  
  tryCatch(
    {
      
      fitted_model <-
        fit_daily_mean_model(
          data =
            training_data,
          rank =
            rank,
          model_id =
            model_id
        )
      
      
      prediction <-
        predict_daily_mean_model(
          fitted_model =
            fitted_model,
          newdata =
            forecast_row
        )
      
      
      tibble(
        FitSuccess =
          TRUE,
        
        ErrorMessage =
          NA_character_,
        
        ProbabilityNonzero =
          prediction[["ProbabilityNonzero"]][1],
        
        ConditionalNonzeroMean =
          prediction[["ConditionalNonzeroMean"]][1],
        
        MeanForecast =
          prediction[["MeanForecast"]][1]
      )
      
    },
    
    error = function(e) {
      
      tibble(
        FitSuccess =
          FALSE,
        
        ErrorMessage =
          conditionMessage(
            e
          ),
        
        ProbabilityNonzero =
          NA_real_,
        
        ConditionalNonzeroMean =
          NA_real_,
        
        MeanForecast =
          NA_real_
      )
    }
  )
}


# ============================================================
# 17. Expanding-Origin One-Step Forecasts
# ============================================================
#
# No look-ahead:
#
#   Forecast for observation t is fitted only using
#   observations 1,...,t-1.
#
# initial_window is intentionally supplied by the notebook
# rather than hard-coded here.
#
# ============================================================

expanding_daily_mean_forecasts <- function(
    data,
    rank,
    model_id,
    initial_window
) {
  
  if (
    length(initial_window) != 1L ||
    !is.numeric(initial_window) ||
    !is.finite(initial_window)
  ) {
    
    stop(
      "initial_window must be one finite numeric value.",
      call. = FALSE
    )
  }
  
  
  initial_window <-
    as.integer(
      initial_window
    )
  
  
  if (initial_window < 2L) {
    
    stop(
      "initial_window must be at least 2.",
      call. = FALSE
    )
  }
  
  
  if (
    initial_window >= nrow(data)
  ) {
    
    stop(
      "initial_window must be smaller than the number of observations.",
      call. = FALSE
    )
  }
  
  
  validate_daily_mean_candidate(
    rank = rank,
    model_id = model_id
  )
  
  
  variables <-
    get_daily_rank_variables(
      rank
    )
  
  
  response <-
    variables[["response"]]
  
  
  forecast_indices <-
    seq.int(
      from =
        initial_window + 1L,
      to =
        nrow(
          data
        )
    )
  
  
  results <-
    purrr::map_dfr(
      forecast_indices,
      function(forecast_index) {
        
        training_data <-
          data[
            seq_len(
              forecast_index - 1L
            ),
            ,
            drop = FALSE
          ]
        
        
        forecast_row <-
          data[
            forecast_index,
            ,
            drop = FALSE
          ]
        
        
        forecast_result <-
          safe_daily_mean_fit_predict(
            training_data =
              training_data,
            forecast_row =
              forecast_row,
            rank =
              rank,
            model_id =
              model_id
          )
        
        
        tibble(
          Rank =
            rank,
          
          ModelID =
            model_id,
          
          ForecastIndex =
            forecast_index,
          
          Observation =
            forecast_row[["Observation"]][1],
          
          Timestamp =
            forecast_row[["Timestamp"]][1],
          
          DailySession =
            forecast_row[["DailySession"]][1],
          
          DailySessionObservation =
            forecast_row[["DailySessionObservation"]][1],
          
          Actual =
            forecast_row[[response]][1],
          
          FitSuccess =
            forecast_result[["FitSuccess"]][1],
          
          ErrorMessage =
            forecast_result[["ErrorMessage"]][1],
          
          ProbabilityNonzero =
            forecast_result[["ProbabilityNonzero"]][1],
          
          ConditionalNonzeroMean =
            forecast_result[["ConditionalNonzeroMean"]][1],
          
          MeanForecast =
            forecast_result[["MeanForecast"]][1]
        )
      }
    )
  
  
  results |>
    mutate(
      ForecastError =
        Actual -
        MeanForecast,
      
      SquaredError =
        ForecastError^2,
      
      AbsoluteError =
        abs(
          ForecastError
        )
    )
}


# ============================================================
# 18. Expanding Forecasts for All Candidates of One Rank
# ============================================================

expanding_daily_mean_forecasts_all_models <- function(
    data,
    rank,
    initial_window
) {
  
  registry <-
    get_daily_mean_candidate_registry() |>
    filter(
      Rank == rank
    )
  
  
  model_ids <-
    registry[["ModelID"]]
  
  
  purrr::map_dfr(
    model_ids,
    function(model_id) {
      
      expanding_daily_mean_forecasts(
        data =
          data,
        rank =
          rank,
        model_id =
          model_id,
        initial_window =
          initial_window
      )
    }
  )
}


# ============================================================
# 19. Expanding Forecasts for Both Daily Rank Processes
# ============================================================

expanding_daily_mean_forecasts_all_ranks <- function(
    data,
    initial_window
) {
  
  bind_rows(
    expanding_daily_mean_forecasts_all_models(
      data =
        data,
      rank =
        "Rank1",
      initial_window =
        initial_window
    ),
    
    expanding_daily_mean_forecasts_all_models(
      data =
        data,
      rank =
        "Rank2",
      initial_window =
        initial_window
    )
  )
}


# ============================================================
# 20. Point-Forecast Evaluation
# ============================================================

evaluate_daily_mean_forecasts <- function(
    forecasts
) {
  
  forecasts |>
    group_by(
      Rank,
      ModelID
    ) |>
    summarise(
      ForecastsAttempted =
        n(),
      
      SuccessfulForecasts =
        sum(
          FitSuccess &
            is.finite(
              MeanForecast
            )
        ),
      
      FailedForecasts =
        sum(
          !FitSuccess |
            !is.finite(
              MeanForecast
            )
        ),
      
      SuccessRate =
        mean(
          FitSuccess &
            is.finite(
              MeanForecast
            )
        ),
      
      RMSE =
        sqrt(
          mean(
            SquaredError[
              FitSuccess &
                is.finite(
                  SquaredError
                )
            ],
            na.rm = TRUE
          )
        ),
      
      MAE =
        mean(
          AbsoluteError[
            FitSuccess &
              is.finite(
                AbsoluteError
              )
          ],
          na.rm = TRUE
        ),
      
      MeanError =
        mean(
          ForecastError[
            FitSuccess &
              is.finite(
                ForecastError
              )
          ],
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
}


# ============================================================
# 21. Hurdle Probability Evaluation
# ============================================================
#
# This is secondary to unconditional-mean forecasting but is
# useful for diagnosing the probability component of hurdle
# candidates.
#
# Brier score:
#
#   mean((I[h_t != 0] - p_t)^2)
#
# ============================================================

evaluate_daily_hurdle_probability <- function(
    forecasts,
    data
) {
  
  rank1_actual_nonzero <-
    data |>
    transmute(
      Observation,
      Rank =
        "Rank1",
      ActualNonzero =
        as.integer(
          DailyRank1Pnl != 0
        )
    )
  
  
  rank2_actual_nonzero <-
    data |>
    transmute(
      Observation,
      Rank =
        "Rank2",
      ActualNonzero =
        as.integer(
          DailyRank2Pnl != 0
        )
    )
  
  
  actual_nonzero <-
    bind_rows(
      rank1_actual_nonzero,
      rank2_actual_nonzero
    )
  
  
  forecasts |>
    filter(
      !is.na(
        ProbabilityNonzero
      ),
      FitSuccess
    ) |>
    left_join(
      actual_nonzero,
      by = c(
        "Observation",
        "Rank"
      )
    ) |>
    mutate(
      BrierError =
        (
          ActualNonzero -
            ProbabilityNonzero
        )^2
    ) |>
    group_by(
      Rank,
      ModelID
    ) |>
    summarise(
      ProbabilityForecasts =
        n(),
      
      BrierScore =
        mean(
          BrierError,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
}


# ============================================================
# 22. Model Ranking Helper
# ============================================================

rank_daily_mean_models <- function(
    evaluation,
    metric = "RMSE"
) {
  
  supported_metrics <- c(
    "RMSE",
    "MAE",
    "MeanError"
  )
  
  
  if (!metric %in% supported_metrics) {
    
    stop(
      paste0(
        "Unsupported ranking metric: ",
        metric
      ),
      call. = FALSE
    )
  }
  
  
  if (metric == "MeanError") {
    
    return(
      evaluation |>
        mutate(
          RankingMetric =
            abs(
              MeanError
            )
        ) |>
        group_by(
          Rank
        ) |>
        arrange(
          RankingMetric,
          .by_group = TRUE
        ) |>
        mutate(
          ModelRank =
            row_number()
        ) |>
        ungroup()
    )
  }
  
  
  evaluation |>
    mutate(
      RankingMetric =
        .data[[metric]]
    ) |>
    group_by(
      Rank
    ) |>
    arrange(
      RankingMetric,
      .by_group = TRUE
    ) |>
    mutate(
      ModelRank =
        row_number()
    ) |>
    ungroup()
}


# ============================================================
# 23. Fit Full-Sample Candidate
# ============================================================
#
# Useful only AFTER a model has been selected out of sample.
#
# Do not use this function to choose the winning model.
#
# ============================================================

fit_daily_mean_model_full_sample <- function(
    data,
    rank,
    model_id
) {
  
  fit_daily_mean_model(
    data =
      data,
    rank =
      rank,
    model_id =
      model_id
  )
}


# ============================================================
# 24. Candidate Registry Summary
# ============================================================

summarize_daily_mean_candidates <- function() {
  
  get_daily_mean_candidate_registry() |>
    arrange(
      Rank,
      ModelClass,
      ModelID
    )
}


# ============================================================
# 25. Script Validation
# ============================================================

validate_daily_competitor_models_script <- function(
    data = NULL
) {
  
  if (is.null(data)) {
    
    data <-
      prepare_daily_mean_model_data()
  }
  
  
  registry <-
    get_daily_mean_candidate_registry()
  
  
  expected_models_per_rank <-
    10L
  
  
  registry_check <-
    registry |>
    count(
      Rank,
      name = "Models"
    )
  
  
  if (
    !all(
      registry_check[["Models"]] ==
      expected_models_per_rank
    )
  ) {
    
    stop(
      "Unexpected number of daily mean candidates.",
      call. = FALSE
    )
  }
  
  
  if (
    anyDuplicated(
      registry[
        c(
          "Rank",
          "ModelID"
        )
      ]
    ) > 0L
  ) {
    
    stop(
      "Duplicate Rank / ModelID combinations found.",
      call. = FALSE
    )
  }
  
  
  if (
    !all(
      registry[["ModelClass"]] %in%
      c(
        "continuous",
        "hurdle"
      )
    )
  ) {
    
    stop(
      "Invalid model class found in candidate registry.",
      call. = FALSE
    )
  }
  
  
  definition_checks <-
    purrr::map_lgl(
      seq_len(
        nrow(
          registry
        )
      ),
      function(i) {
        
        definition <-
          get_daily_mean_model_definition(
            rank =
              registry[["Rank"]][i],
            model_id =
              registry[["ModelID"]][i]
          )
        
        
        is.list(
          definition
        ) &&
          !is.null(
            definition[["model_class"]]
          )
      }
    )
  
  
  if (!all(definition_checks)) {
    
    stop(
      "At least one model definition failed validation.",
      call. = FALSE
    )
  }
  
  
  required_model_columns <- c(
    "SessionProgress",
    "SessionProgressSquared",
    "LogElapsedMinutes",
    "LongInterval",
    "SessionStart",
    "PreviousRank1Pnl",
    "PreviousRank2Pnl",
    "PreviousRank1Nonzero",
    "PreviousRank2Nonzero",
    "ClockSin",
    "ClockCos"
  )
  
  
  missing_model_columns <-
    setdiff(
      required_model_columns,
      names(
        data
      )
    )
  
  
  if (
    length(
      missing_model_columns
    ) > 0L
  ) {
    
    stop(
      paste0(
        "Prepared modeling data are missing: ",
        paste(
          missing_model_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  list(
    valid =
      TRUE,
    
    observations =
      nrow(
        data
      ),
    
    ranks =
      unique(
        registry[["Rank"]]
      ),
    
    models_per_rank =
      registry_check,
    
    candidate_registry =
      registry
  )
}


# ============================================================
# 26. Load Message
# ============================================================

message(
  "daily_competitor_models.R loaded successfully."
)


# ============================================================
# End of daily_competitor_models.R
# ============================================================
