# ============================================================
# competitor_models.R
# Candidate models for final-rank leaderboard return processes
# ============================================================
#
# PURPOSE
#
# This script contains reusable functions for fitting candidate
# conditional-mean models to the final-rank log-return series.
#
# The models reflect the evidence from:
#
#   17_FinalRank_TimeSeries.Rmd
#   17.5_FinalRank_Seasonality.Rmd
#
# Main findings motivating these candidates:
#
#   1. Little evidence of conventional return autocorrelation.
#   2. Competition progression appears important for several ranks.
#   3. Elapsed interval duration is strongly related to returns.
#   4. The duration effect may be nonlinear / long-interval driven.
#   5. Exact zeros are common for some rank processes.
#   6. Model specification should remain rank-specific.
#
# This script DEFINES AND FITS candidate models.
#
# It does NOT:
#
#   - select the final model;
#   - perform rolling forecast evaluation;
#   - fit volatility models;
#   - construct cross-rank dependence models;
#   - generate final competition simulations.
#
# Those tasks belong in later notebooks / forecasting scripts.
# ============================================================


# ============================================================
# 1. Required Package Checks
# ============================================================

if (!requireNamespace("forecast", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'forecast' is required by competitor_models.R. ",
      "Install it with install.packages('forecast')."
    )
  )
}


# ============================================================
# 2. Supported Candidate Model Types
# ============================================================

COMPETITOR_MEAN_MODEL_TYPES <- c(
  "constant_mean",
  "linear_time",
  "quadratic_time",
  "elapsed_linear",
  "elapsed_log",
  "long_interval",
  "time_elapsed",
  "time_long",
  "ar1",
  "ar1_time_elapsed",
  "hurdle_time_duration"
)


# ============================================================
# 3. Model Labels
# ============================================================

COMPETITOR_MEAN_MODEL_LABELS <- c(
  constant_mean =
    "Constant Mean",
  
  linear_time =
    "Linear Competition Time",
  
  quadratic_time =
    "Quadratic Competition Time",
  
  elapsed_linear =
    "Linear Elapsed Time",
  
  elapsed_log =
    "Log Elapsed Time",
  
  long_interval =
    "Long-Interval Indicator",
  
  time_elapsed =
    "Competition Time + Elapsed Time",
  
  time_long =
    "Competition Time + Long-Interval Indicator",
  
  ar1 =
    "AR(1)",
  
  ar1_time_elapsed =
    "AR(1) + Competition Time + Elapsed Time",
  
  hurdle_time_duration =
    "Hurdle: Zero Probability + Nonzero Mean"
)


# ============================================================
# 4. Add Modeling Features
# ============================================================
#
# IMPORTANT:
#
# CompetitionProgress must be based on the KNOWN total number
# of observations in the competition, not on the current size
# of an expanding training sample.
#
# Example:
#
#   total_observations = 147
#
# gives
#
#   observation 1   -> progress = 0
#   observation 147 -> progress = 1
#
# This keeps the definition of competition progress fixed when
# models are later estimated recursively.
# ============================================================

add_competitor_model_features <- function(
    data,
    total_observations
) {
  
  data <- as.data.frame(
    data
  )
  
  if (!"Observation" %in% names(data)) {
    stop(
      "data must contain an 'Observation' column."
    )
  }
  
  if (
    length(total_observations) != 1 ||
    !is.numeric(total_observations) ||
    is.na(total_observations) ||
    total_observations <= 1
  ) {
    stop(
      "total_observations must be a single numeric value greater than 1."
    )
  }
  
  if (
    any(
      !is.finite(
        data$Observation
      )
    )
  ) {
    stop(
      "'Observation' contains non-finite values."
    )
  }
  
  data$CompetitionProgress <- (
    data$Observation - 1
  ) / (
    total_observations - 1
  )
  
  data$CompetitionProgress2 <-
    data$CompetitionProgress^2
  
  data$RemainingCompetition <-
    1 - data$CompetitionProgress
  
  if (
    "ElapsedMinutes" %in% names(data)
  ) {
    
    data$LogElapsedMinutes <-
      log1p(
        data$ElapsedMinutes
      )
    
    data$LongInterval <-
      ifelse(
        is.na(
          data$ElapsedMinutes
        ),
        NA_real_,
        as.numeric(
          data$ElapsedMinutes > 60
        )
      )
    
  } else {
    
    data$ElapsedMinutes <-
      NA_real_
    
    data$LogElapsedMinutes <-
      NA_real_
    
    data$LongInterval <-
      NA_real_
  }
  
  data
}


# ============================================================
# 5. Validate Rank Series
# ============================================================

validate_competitor_rank <- function(
    data,
    rank_name
) {
  
  if (
    length(rank_name) != 1 ||
    !is.character(rank_name)
  ) {
    stop(
      "rank_name must be a single character string."
    )
  }
  
  if (
    !rank_name %in% names(data)
  ) {
    stop(
      paste0(
        "Rank column '",
        rank_name,
        "' was not found in data."
      )
    )
  }
  
  values <- data[[rank_name]]
  
  if (
    !is.numeric(values)
  ) {
    stop(
      paste0(
        "Rank column '",
        rank_name,
        "' must be numeric."
      )
    )
  }
  
  if (
    any(
      !is.finite(values)
    )
  ) {
    stop(
      paste0(
        "Rank column '",
        rank_name,
        "' contains non-finite values."
      )
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 6. Validate Model Type
# ============================================================

validate_competitor_model_type <- function(
    model_type
) {
  
  if (
    length(model_type) != 1 ||
    !is.character(model_type)
  ) {
    stop(
      "model_type must be a single character string."
    )
  }
  
  if (
    !model_type %in%
    COMPETITOR_MEAN_MODEL_TYPES
  ) {
    stop(
      paste0(
        "Unsupported model_type: ",
        model_type,
        ". Supported types are: ",
        paste(
          COMPETITOR_MEAN_MODEL_TYPES,
          collapse = ", "
        ),
        "."
      )
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 7. Prepare One Rank for Modeling
# ============================================================

prepare_competitor_rank_model_data <- function(
    data,
    rank_name,
    total_observations
) {
  
  validate_competitor_rank(
    data = data,
    rank_name = rank_name
  )
  
  model_data <-
    add_competitor_model_features(
      data = data,
      total_observations =
        total_observations
    )
  
  model_data$Return <-
    model_data[[rank_name]]
  
  model_data$ZeroReturn <-
    as.integer(
      model_data$Return == 0
    )
  
  model_data$NonZeroReturn <-
    as.integer(
      model_data$Return != 0
    )
  
  model_data
}


# ============================================================
# 8. Constant-Mean Model
# ============================================================
#
# r_t = mu + epsilon_t
#
# This is the main historical-mean benchmark.
# ============================================================

fit_constant_mean_model <- function(
    model_data
) {
  
  stats::lm(
    Return ~ 1,
    data = model_data
  )
}


# ============================================================
# 9. Linear Competition-Time Model
# ============================================================
#
# r_t = alpha
#       + beta * CompetitionProgress_t
#       + epsilon_t
# ============================================================

fit_linear_time_model <- function(
    model_data
) {
  
  stats::lm(
    Return ~ CompetitionProgress,
    data = model_data
  )
}


# ============================================================
# 10. Quadratic Competition-Time Model
# ============================================================
#
# r_t = alpha
#       + beta1 * CompetitionProgress_t
#       + beta2 * CompetitionProgress_t^2
#       + epsilon_t
#
# This allows non-monotonic competition-time behavior while
# remaining very parsimonious.
# ============================================================

fit_quadratic_time_model <- function(
    model_data
) {
  
  stats::lm(
    Return ~
      CompetitionProgress +
      CompetitionProgress2,
    data = model_data
  )
}


# ============================================================
# 11. Linear Elapsed-Time Model
# ============================================================
#
# r_t = alpha
#       + beta * ElapsedMinutes_t
#       + epsilon_t
# ============================================================

fit_elapsed_linear_model <- function(
    model_data
) {
  
  usable_data <-
    model_data[
      is.finite(
        model_data$ElapsedMinutes
      ),
      ,
      drop = FALSE
    ]
  
  stats::lm(
    Return ~ ElapsedMinutes,
    data = usable_data
  )
}


# ============================================================
# 12. Log Elapsed-Time Model
# ============================================================
#
# r_t = alpha
#       + beta * log(1 + ElapsedMinutes_t)
#       + epsilon_t
#
# This is a simple nonlinear duration specification.
#
# It allows the effect of additional elapsed time to diminish
# rather than imposing a constant effect per additional minute.
# ============================================================

fit_elapsed_log_model <- function(
    model_data
) {
  
  usable_data <-
    model_data[
      is.finite(
        model_data$LogElapsedMinutes
      ),
      ,
      drop = FALSE
    ]
  
  stats::lm(
    Return ~ LogElapsedMinutes,
    data = usable_data
  )
}


# ============================================================
# 13. Long-Interval Indicator Model
# ============================================================
#
# r_t = alpha
#       + beta * I(ElapsedMinutes > 60)
#       + epsilon_t
#
# This treats long intervals as a distinct schedule regime
# rather than assuming a smooth relationship with duration.
# ============================================================

fit_long_interval_model <- function(
    model_data
) {
  
  usable_data <-
    model_data[
      is.finite(
        model_data$LongInterval
      ),
      ,
      drop = FALSE
    ]
  
  stats::lm(
    Return ~ LongInterval,
    data = usable_data
  )
}


# ============================================================
# 14. Competition Time + Elapsed Time
# ============================================================
#
# r_t = alpha
#       + beta1 * CompetitionProgress_t
#       + beta2 * ElapsedMinutes_t
#       + epsilon_t
#
# NOTE:
#
# We intentionally do NOT automatically include both
# ElapsedMinutes and LongInterval in the same candidate.
#
# Notebook 17.5 showed that these variables contain strongly
# overlapping information, making the separate coefficients
# difficult to interpret.
# ============================================================

fit_time_elapsed_model <- function(
    model_data
) {
  
  usable_data <-
    model_data[
      is.finite(
        model_data$ElapsedMinutes
      ),
      ,
      drop = FALSE
    ]
  
  stats::lm(
    Return ~
      CompetitionProgress +
      ElapsedMinutes,
    data = usable_data
  )
}


# ============================================================
# 15. Competition Time + Long-Interval Indicator
# ============================================================
#
# r_t = alpha
#       + beta1 * CompetitionProgress_t
#       + beta2 * LongInterval_t
#       + epsilon_t
#
# This is an alternative to time_elapsed.
# ============================================================

fit_time_long_model <- function(
    model_data
) {
  
  usable_data <-
    model_data[
      is.finite(
        model_data$LongInterval
      ),
      ,
      drop = FALSE
    ]
  
  stats::lm(
    Return ~
      CompetitionProgress +
      LongInterval,
    data = usable_data
  )
}


# ============================================================
# 16. AR(1) Challenger
# ============================================================
#
# r_t = mu
#       + phi * r_(t-1)
#       + epsilon_t
#
# Notebook 17 did not provide strong evidence of serial
# dependence, so this is a CHALLENGER model rather than the
# central specification.
# ============================================================

fit_ar1_model <- function(
    model_data
) {
  
  forecast::Arima(
    model_data$Return,
    order = c(
      1,
      0,
      0
    ),
    include.mean = TRUE,
    method = "ML"
  )
}


# ============================================================
# 17. AR(1) + Competition Time + Elapsed Time
# ============================================================
#
# This asks whether a small autoregressive component adds
# useful information after accounting for the deterministic
# effects already identified.
# ============================================================

fit_ar1_time_elapsed_model <- function(
    model_data
) {
  
  usable_rows <-
    is.finite(
      model_data$ElapsedMinutes
    )
  
  usable_data <-
    model_data[
      usable_rows,
      ,
      drop = FALSE
    ]
  
  xreg <-
    cbind(
      CompetitionProgress =
        usable_data$CompetitionProgress,
      
      ElapsedMinutes =
        usable_data$ElapsedMinutes
    )
  
  forecast::Arima(
    usable_data$Return,
    order = c(
      1,
      0,
      0
    ),
    xreg = xreg,
    include.mean = TRUE,
    method = "ML"
  )
}


# ============================================================
# 18. Determine Hurdle-Model Eligibility
# ============================================================
#
# A hurdle model is only reasonable when there are enough:
#
#   - exact-zero observations;
#   - nonzero observations.
#
# This prevents attempting a zero-probability regression for a
# rank such as Rank16 when almost every observation is nonzero.
# ============================================================

competitor_hurdle_eligible <- function(
    model_data,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  number_zero <-
    sum(
      model_data$Return == 0
    )
  
  number_nonzero <-
    sum(
      model_data$Return != 0
    )
  
  (
    number_zero >=
      min_zero_observations
  ) &&
    (
      number_nonzero >=
        min_nonzero_observations
    )
}


# ============================================================
# 19. Hurdle / Two-Part Mean Model
# ============================================================
#
# PART 1
#
# Model the probability that the return is NONZERO:
#
# P(r_t != 0)
#
# using:
#
#   CompetitionProgress
#   CompetitionProgress^2
#   log(1 + ElapsedMinutes)
#
#
# PART 2
#
# Conditional on r_t != 0:
#
# E[r_t | r_t != 0]
#
# using:
#
#   CompetitionProgress
#   log(1 + ElapsedMinutes)
#
#
# The implied unconditional mean is:
#
# E[r_t]
#
# =
#
# P(r_t != 0)
#
# *
#
# E[r_t | r_t != 0]
#
#
# This is a candidate specifically motivated by the large
# number of exact-zero returns in several upper-rank series.
# ============================================================

fit_hurdle_time_duration_model <- function(
    model_data,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  usable_data <-
    model_data[
      is.finite(
        model_data$LogElapsedMinutes
      ),
      ,
      drop = FALSE
    ]
  
  if (
    !competitor_hurdle_eligible(
      model_data = usable_data,
      min_zero_observations =
      min_zero_observations,
      min_nonzero_observations =
      min_nonzero_observations
    )
  ) {
    
    return(
      NULL
    )
  }
  
  zero_model <-
    stats::glm(
      NonZeroReturn ~
        CompetitionProgress +
        CompetitionProgress2 +
        LogElapsedMinutes,
      family = stats::binomial(
        link = "logit"
      ),
      data = usable_data
    )
  
  nonzero_data <-
    usable_data[
      usable_data$NonZeroReturn == 1,
      ,
      drop = FALSE
    ]
  
  nonzero_mean_model <-
    stats::lm(
      Return ~
        CompetitionProgress +
        LogElapsedMinutes,
      data = nonzero_data
    )
  
  structure(
    list(
      zero_model =
        zero_model,
      
      nonzero_mean_model =
        nonzero_mean_model,
      
      number_zero =
        sum(
          usable_data$Return == 0
        ),
      
      number_nonzero =
        sum(
          usable_data$Return != 0
        )
    ),
    class = "competitor_hurdle_model"
  )
}


# ============================================================
# 20. Fit One Candidate Model
# ============================================================

fit_competitor_mean_model <- function(
    data,
    rank_name,
    model_type,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  validate_competitor_model_type(
    model_type
  )
  
  model_data <-
    prepare_competitor_rank_model_data(
      data = data,
      rank_name = rank_name,
      total_observations =
        total_observations
    )
  
  fitted_model <-
    switch(
      model_type,
      
      constant_mean =
        fit_constant_mean_model(
          model_data
        ),
      
      linear_time =
        fit_linear_time_model(
          model_data
        ),
      
      quadratic_time =
        fit_quadratic_time_model(
          model_data
        ),
      
      elapsed_linear =
        fit_elapsed_linear_model(
          model_data
        ),
      
      elapsed_log =
        fit_elapsed_log_model(
          model_data
        ),
      
      long_interval =
        fit_long_interval_model(
          model_data
        ),
      
      time_elapsed =
        fit_time_elapsed_model(
          model_data
        ),
      
      time_long =
        fit_time_long_model(
          model_data
        ),
      
      ar1 =
        fit_ar1_model(
          model_data
        ),
      
      ar1_time_elapsed =
        fit_ar1_time_elapsed_model(
          model_data
        ),
      
      hurdle_time_duration =
        fit_hurdle_time_duration_model(
          model_data =
            model_data,
          min_zero_observations =
            min_zero_observations,
          min_nonzero_observations =
            min_nonzero_observations
        )
    )
  
  structure(
    list(
      rank =
        rank_name,
      
      model_type =
        model_type,
      
      model_label =
        unname(
          COMPETITOR_MEAN_MODEL_LABELS[
            model_type
          ]
        ),
      
      fit =
        fitted_model,
      
      hurdle_eligible =
        competitor_hurdle_eligible(
          model_data =
            model_data,
          min_zero_observations =
            min_zero_observations,
          min_nonzero_observations =
            min_nonzero_observations
        ),
      
      total_observations =
        total_observations,
      
      training_observations =
        nrow(
          model_data
        )
    ),
    class =
      "competitor_mean_model"
  )
}


# ============================================================
# 21. Fit All Candidate Models for One Rank
# ============================================================

fit_competitor_mean_candidates <- function(
    data,
    rank_name,
    total_observations,
    model_types =
      COMPETITOR_MEAN_MODEL_TYPES,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  validate_competitor_rank(
    data = data,
    rank_name = rank_name
  )
  
  invalid_models <-
    setdiff(
      model_types,
      COMPETITOR_MEAN_MODEL_TYPES
    )
  
  if (
    length(
      invalid_models
    ) > 0
  ) {
    stop(
      paste0(
        "Unsupported model type(s): ",
        paste(
          invalid_models,
          collapse = ", "
        )
      )
    )
  }
  
  candidate_models <-
    lapply(
      model_types,
      function(model_type) {
        
        fit_competitor_mean_model(
          data = data,
          rank_name = rank_name,
          model_type = model_type,
          total_observations =
            total_observations,
          min_zero_observations =
            min_zero_observations,
          min_nonzero_observations =
            min_nonzero_observations
        )
      }
    )
  
  names(
    candidate_models
  ) <-
    model_types
  
  candidate_models
}


# ============================================================
# 22. Fit Candidate Models for All Ranks
# ============================================================

fit_all_competitor_mean_candidates <- function(
    data,
    rank_columns,
    total_observations,
    model_types =
      COMPETITOR_MEAN_MODEL_TYPES,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  missing_ranks <-
    setdiff(
      rank_columns,
      names(
        data
      )
    )
  
  if (
    length(
      missing_ranks
    ) > 0
  ) {
    stop(
      paste0(
        "The following rank columns are missing from data: ",
        paste(
          missing_ranks,
          collapse = ", "
        )
      )
    )
  }
  
  all_models <-
    lapply(
      rank_columns,
      function(rank_name) {
        
        fit_competitor_mean_candidates(
          data = data,
          rank_name = rank_name,
          total_observations =
            total_observations,
          model_types =
            model_types,
          min_zero_observations =
            min_zero_observations,
          min_nonzero_observations =
            min_nonzero_observations
        )
      }
    )
  
  names(
    all_models
  ) <-
    rank_columns
  
  all_models
}


# ============================================================
# 23. Extract AIC Safely
# ============================================================

safe_competitor_aic <- function(
    model
) {
  
  if (
    is.null(
      model
    )
  ) {
    return(
      NA_real_
    )
  }
  
  if (
    inherits(
      model,
      "competitor_hurdle_model"
    )
  ) {
    
    return(
      NA_real_
    )
  }
  
  result <-
    tryCatch(
      stats::AIC(
        model
      ),
      error = function(e) {
        NA_real_
      }
    )
  
  as.numeric(
    result
  )
}


# ============================================================
# 24. Extract BIC Safely
# ============================================================

safe_competitor_bic <- function(
    model
) {
  
  if (
    is.null(
      model
    )
  ) {
    return(
      NA_real_
    )
  }
  
  if (
    inherits(
      model,
      "competitor_hurdle_model"
    )
  ) {
    
    return(
      NA_real_
    )
  }
  
  result <-
    tryCatch(
      stats::BIC(
        model
      ),
      error = function(e) {
        NA_real_
      }
    )
  
  as.numeric(
    result
  )
}


# ============================================================
# 25. Extract Number of Estimated Parameters
# ============================================================

count_competitor_parameters <- function(
    model
) {
  
  if (
    is.null(
      model
    )
  ) {
    return(
      NA_integer_
    )
  }
  
  if (
    inherits(
      model,
      "competitor_hurdle_model"
    )
  ) {
    
    return(
      length(
        stats::coef(
          model$zero_model
        )
      ) +
        length(
          stats::coef(
            model$nonzero_mean_model
          )
        )
    )
  }
  
  length(
    stats::coef(
      model
    )
  )
}


# ============================================================
# 26. Extract Number of Observations Used
# ============================================================

get_competitor_model_nobs <- function(
    model
) {
  
  if (
    is.null(
      model
    )
  ) {
    return(
      NA_integer_
    )
  }
  
  if (
    inherits(
      model,
      "competitor_hurdle_model"
    )
  ) {
    
    return(
      stats::nobs(
        model$zero_model
      )
    )
  }
  
  result <-
    tryCatch(
      stats::nobs(
        model
      ),
      error = function(e) {
        NA_integer_
      }
    )
  
  as.integer(
    result
  )
}


# ============================================================
# 27. Create In-Sample Candidate Summary
# ============================================================
#
# IMPORTANT:
#
# AIC and BIC are included only as descriptive diagnostics.
#
# They are NOT the final model-selection criterion.
#
# The hurdle model deliberately receives NA for overall AIC/BIC
# because its two-part likelihood is not directly represented
# by the separate glm and lm AIC values.
#
# Final selection should use out-of-sample forecast performance.
# ============================================================

summarize_competitor_mean_candidates <- function(
    all_models
) {
  
  rows <- list()
  
  row_number <- 1
  
  for (
    rank_name in names(
      all_models
    )
  ) {
    
    rank_models <-
      all_models[[rank_name]]
    
    for (
      model_type in names(
        rank_models
      )
    ) {
      
      model_object <-
        rank_models[[model_type]]
      
      fitted_model <-
        model_object$fit
      
      rows[[row_number]] <-
        data.frame(
          Rank =
            rank_name,
          
          ModelType =
            model_type,
          
          ModelLabel =
            model_object$model_label,
          
          ModelAvailable =
            !is.null(
              fitted_model
            ),
          
          HurdleEligible =
            model_object$hurdle_eligible,
          
          N =
            get_competitor_model_nobs(
              fitted_model
            ),
          
          Parameters =
            count_competitor_parameters(
              fitted_model
            ),
          
          AIC =
            safe_competitor_aic(
              fitted_model
            ),
          
          BIC =
            safe_competitor_bic(
              fitted_model
            ),
          
          stringsAsFactors =
            FALSE
        )
      
      row_number <-
        row_number + 1
    }
  }
  
  summary_table <-
    do.call(
      rbind,
      rows
    )
  
  rownames(
    summary_table
  ) <-
    NULL
  
  summary_table
}


# ============================================================
# 28. Extract Coefficients from One Candidate Model
# ============================================================

extract_competitor_model_coefficients <- function(
    model_object
) {
  
  if (
    !inherits(
      model_object,
      "competitor_mean_model"
    )
  ) {
    stop(
      "model_object must be created by fit_competitor_mean_model()."
    )
  }
  
  fitted_model <-
    model_object$fit
  
  if (
    is.null(
      fitted_model
    )
  ) {
    
    return(
      data.frame(
        Rank =
          model_object$rank,
        
        ModelType =
          model_object$model_type,
        
        Component =
          NA_character_,
        
        Term =
          NA_character_,
        
        Estimate =
          NA_real_,
        
        stringsAsFactors =
          FALSE
      )
    )
  }
  
  if (
    inherits(
      fitted_model,
      "competitor_hurdle_model"
    )
  ) {
    
    zero_coefficients <-
      stats::coef(
        fitted_model$zero_model
      )
    
    nonzero_coefficients <-
      stats::coef(
        fitted_model$nonzero_mean_model
      )
    
    zero_rows <-
      data.frame(
        Rank =
          model_object$rank,
        
        ModelType =
          model_object$model_type,
        
        Component =
          "Nonzero Probability",
        
        Term =
          names(
            zero_coefficients
          ),
        
        Estimate =
          as.numeric(
            zero_coefficients
          ),
        
        stringsAsFactors =
          FALSE
      )
    
    nonzero_rows <-
      data.frame(
        Rank =
          model_object$rank,
        
        ModelType =
          model_object$model_type,
        
        Component =
          "Conditional Nonzero Mean",
        
        Term =
          names(
            nonzero_coefficients
          ),
        
        Estimate =
          as.numeric(
            nonzero_coefficients
          ),
        
        stringsAsFactors =
          FALSE
      )
    
    return(
      rbind(
        zero_rows,
        nonzero_rows
      )
    )
  }
  
  coefficients <-
    stats::coef(
      fitted_model
    )
  
  data.frame(
    Rank =
      model_object$rank,
    
    ModelType =
      model_object$model_type,
    
    Component =
      "Mean",
    
    Term =
      names(
        coefficients
      ),
    
    Estimate =
      as.numeric(
        coefficients
      ),
    
    stringsAsFactors =
      FALSE
  )
}


# ============================================================
# 29. Extract Coefficients from All Candidate Models
# ============================================================

extract_all_competitor_model_coefficients <- function(
    all_models
) {
  
  coefficient_tables <- list()
  
  table_number <- 1
  
  for (
    rank_name in names(
      all_models
    )
  ) {
    
    for (
      model_type in names(
        all_models[[rank_name]]
      )
    ) {
      
      coefficient_tables[[table_number]] <-
        extract_competitor_model_coefficients(
          all_models[[rank_name]][[model_type]]
        )
      
      table_number <-
        table_number + 1
    }
  }
  
  output <-
    do.call(
      rbind,
      coefficient_tables
    )
  
  rownames(
    output
  ) <-
    NULL
  
  output
}


# ============================================================
# 30. Predict from Ordinary LM Candidate
# ============================================================

predict_competitor_lm <- function(
    fitted_model,
    new_data
) {
  
  as.numeric(
    stats::predict(
      fitted_model,
      newdata = new_data
    )
  )
}


# ============================================================
# 31. Predict from Hurdle Model
# ============================================================
#
# Expected return:
#
# E[r]
#
# =
#
# P(r != 0)
#
# *
#
# E[r | r != 0]
# ============================================================

predict_competitor_hurdle <- function(
    fitted_model,
    new_data
) {
  
  probability_nonzero <-
    as.numeric(
      stats::predict(
        fitted_model$zero_model,
        newdata = new_data,
        type = "response"
      )
    )
  
  conditional_nonzero_mean <-
    as.numeric(
      stats::predict(
        fitted_model$nonzero_mean_model,
        newdata = new_data
      )
    )
  
  probability_nonzero *
    conditional_nonzero_mean
}


# ============================================================
# 32. Predict Mean from One Candidate Model
# ============================================================
#
# This helper is useful for deterministic / regression models.
#
# ARIMA forecasting will ultimately be handled more fully in
# competitor_forecasting.R because future AR forecasts require
# horizon-specific recursive treatment and future xreg values.
# ============================================================

predict_competitor_mean_model <- function(
    model_object,
    new_data
) {
  
  if (
    !inherits(
      model_object,
      "competitor_mean_model"
    )
  ) {
    stop(
      "model_object must be created by fit_competitor_mean_model()."
    )
  }
  
  if (
    is.null(
      model_object$fit
    )
  ) {
    return(
      rep(
        NA_real_,
        nrow(
          new_data
        )
      )
    )
  }
  
  prepared_new_data <-
    add_competitor_model_features(
      data = new_data,
      total_observations =
        model_object$total_observations
    )
  
  model_type <-
    model_object$model_type
  
  fitted_model <-
    model_object$fit
  
  if (
    model_type %in%
    c(
      "constant_mean",
      "linear_time",
      "quadratic_time",
      "elapsed_linear",
      "elapsed_log",
      "long_interval",
      "time_elapsed",
      "time_long"
    )
  ) {
    
    return(
      predict_competitor_lm(
        fitted_model =
          fitted_model,
        new_data =
          prepared_new_data
      )
    )
  }
  
  if (
    model_type ==
    "hurdle_time_duration"
  ) {
    
    return(
      predict_competitor_hurdle(
        fitted_model =
          fitted_model,
        new_data =
          prepared_new_data
      )
    )
  }
  
  if (
    model_type %in%
    c(
      "ar1",
      "ar1_time_elapsed"
    )
  ) {
    
    stop(
      paste0(
        "AR-model forecasting will be handled in ",
        "competitor_forecasting.R. ",
        "predict_competitor_mean_model() is currently for ",
        "regression and hurdle candidates."
      )
    )
  }
  
  stop(
    paste0(
      "Prediction is not implemented for model type: ",
      model_type
    )
  )
}


# ============================================================
# 33. Print Method for One Competitor Mean Model
# ============================================================

print.competitor_mean_model <- function(
    x,
    ...
) {
  
  cat(
    "\nCompetitor Mean Model\n"
  )
  
  cat(
    "---------------------\n"
  )
  
  cat(
    "Rank: ",
    x$rank,
    "\n",
    sep = ""
  )
  
  cat(
    "Model type: ",
    x$model_type,
    "\n",
    sep = ""
  )
  
  cat(
    "Model label: ",
    x$model_label,
    "\n",
    sep = ""
  )
  
  cat(
    "Available: ",
    !is.null(
      x$fit
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Hurdle eligible: ",
    x$hurdle_eligible,
    "\n",
    sep = ""
  )
  
  invisible(
    x
  )
}


# ============================================================
# 34. Default Candidate Sets
# ============================================================
#
# CORE candidates:
#
# These are the models most directly motivated by the current
# diagnostics and should form the main Notebook 18 comparison.
#
# EXTENDED candidates:
#
# These add the low-order AR challenger and hurdle candidate.
# ============================================================

COMPETITOR_CORE_MEAN_MODELS <- c(
  "constant_mean",
  "linear_time",
  "quadratic_time",
  "elapsed_linear",
  "elapsed_log",
  "long_interval",
  "time_elapsed",
  "time_long"
)


COMPETITOR_EXTENDED_MEAN_MODELS <- c(
  COMPETITOR_CORE_MEAN_MODELS,
  "ar1",
  "ar1_time_elapsed",
  "hurdle_time_duration"
)


# ============================================================
# 35. Script Completion Message
# ============================================================

message(
  paste0(
    "competitor_models.R loaded successfully. ",
    "Candidate mean-model functions are available."
  )
)
