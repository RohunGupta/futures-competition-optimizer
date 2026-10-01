# ============================================================
# competitor_forecasting.R
# Reusable forecasting functions for final-rank competitors
# ============================================================
#
# PURPOSE
#
# This script contains reusable functions for:
#
#   1. one-step-ahead competitor mean forecasting;
#   2. recursive / expanding-origin forecast evaluation;
#   3. deterministic conditional-variance modeling;
#   4. one-step-ahead variance forecasting;
#   5. forecast-error construction;
#   6. forecast-performance summaries;
#   7. transformation from six rank returns to the ordered
#      six-dimensional final-rank state;
#   8. covariance transformation into ordered-state space.
#
# This script is designed to work with:
#
#   scripts/competitor_data.R
#   scripts/competitor_models.R
#
# It does NOT select the final forecasting model.
#
# Final model selection belongs in:
#
#   22_FinalRank_Model_Selection.Rmd
#
# The primary out-of-sample design is EXPANDING ORIGIN:
#
#   fit using observations 1,...,t
#   forecast observation t+1
#   observe actual t+1
#   expand the training sample
#   repeat
#
# IMPORTANT:
#
# CompetitionProgress is always defined relative to the known
# TOTAL competition horizon, not the size of the current
# expanding training sample.
#
# ============================================================


# ============================================================
# 1. Required Package Checks
# ============================================================

if (!requireNamespace("forecast", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'forecast' is required by ",
      "competitor_forecasting.R. ",
      "Install it with install.packages('forecast')."
    ),
    call. = FALSE
  )
}


# ============================================================
# 2. Check Required Functions from Earlier Scripts
# ============================================================

.required_competitor_functions <- c(
  "add_competitor_model_features",
  "prepare_competitor_rank_model_data",
  "fit_competitor_mean_model",
  "predict_competitor_mean_model"
)

.missing_competitor_functions <-
  .required_competitor_functions[
    !vapply(
      .required_competitor_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (length(.missing_competitor_functions) > 0) {
  stop(
    paste0(
      "The following required functions are not available: ",
      paste(
        .missing_competitor_functions,
        collapse = ", "
      ),
      ". Source competitor_data.R and competitor_models.R ",
      "before sourcing competitor_forecasting.R."
    ),
    call. = FALSE
  )
}


# ============================================================
# 3. Constants
# ============================================================

COMPETITOR_VARIANCE_MODEL_TYPES <- c(
  "constant",
  "elapsed_linear",
  "elapsed_log",
  "long_interval",
  "garch11_student_t"
)

COMPETITOR_VARIANCE_MODEL_LABELS <- c(
  constant =
    "Constant Variance",
  
  elapsed_linear =
    "Linear Elapsed-Time Variance",
  
  elapsed_log =
    "Log Elapsed-Time Variance",
  
  long_interval =
    "Long-Interval Variance",
  
  garch11_student_t =
    "GARCH(1,1), Student-t"
)

FINAL_RANK_STATE_NAMES <- c(
  "Delta_X1",
  "Delta_D12",
  "Delta_D23",
  "Delta_D34",
  "Delta_D4_15",
  "Delta_D15_16"
)

FINAL_RANK_RETURN_NAMES <- c(
  "Rank1",
  "Rank2",
  "Rank3",
  "Rank4",
  "Rank15",
  "Rank16"
)

FINAL_RANK_TRANSFORMATION_MATRIX <-
  matrix(
    c(
      1,  0,  0,  0,  0,  0,
      1, -1,  0,  0,  0,  0,
      0,  1, -1,  0,  0,  0,
      0,  0,  1, -1,  0,  0,
      0,  0,  0,  1, -1,  0,
      0,  0,  0,  0,  1, -1
    ),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(
      FINAL_RANK_STATE_NAMES,
      FINAL_RANK_RETURN_NAMES
    )
  )


# ============================================================
# 4. Validate Total Competition Observations
# ============================================================

validate_total_competition_observations <- function(
    total_observations
) {
  
  if (
    length(total_observations) != 1 ||
    !is.numeric(total_observations) ||
    is.na(total_observations) ||
    !is.finite(total_observations) ||
    total_observations <= 1
  ) {
    stop(
      paste0(
        "total_observations must be one finite numeric ",
        "value greater than 1."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 5. Validate Forecast Origin
# ============================================================

validate_competitor_forecast_origin <- function(
    data,
    forecast_origin
) {
  
  if (
    length(forecast_origin) != 1 ||
    !is.numeric(forecast_origin) ||
    is.na(forecast_origin) ||
    !is.finite(forecast_origin) ||
    forecast_origin != as.integer(forecast_origin)
  ) {
    stop(
      "forecast_origin must be a single integer.",
      call. = FALSE
    )
  }
  
  forecast_origin <- as.integer(
    forecast_origin
  )
  
  if (
    forecast_origin < 1 ||
    forecast_origin >= nrow(data)
  ) {
    stop(
      paste0(
        "forecast_origin must be between 1 and ",
        nrow(data) - 1,
        " so that observation t+1 exists."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 6. Prepare One Future Observation
# ============================================================
#
# The row already contains the known future schedule
# information such as Observation and ElapsedMinutes.
#
# Return values are not used to generate the forecast.
# They are retained only so that the actual realization can
# later be attached to the forecast record.
# ============================================================

prepare_competitor_future_row <- function(
    data,
    target_observation,
    total_observations
) {
  
  validate_total_competition_observations(
    total_observations
  )
  
  if (
    target_observation < 1 ||
    target_observation > nrow(data)
  ) {
    stop(
      "target_observation is outside the available data.",
      call. = FALSE
    )
  }
  
  future_row <-
    data[
      target_observation,
      ,
      drop = FALSE
    ]
  
  future_row <-
    add_competitor_model_features(
      data = future_row,
      total_observations =
        total_observations
    )
  
  future_row
}


# ============================================================
# 7. Forecast AR(1) Candidate
# ============================================================

forecast_competitor_ar1 <- function(
    model_object,
    horizon = 1
) {
  
  if (
    is.null(model_object$fit)
  ) {
    return(
      rep(
        NA_real_,
        horizon
      )
    )
  }
  
  forecast_object <-
    forecast::forecast(
      model_object$fit,
      h = horizon
    )
  
  as.numeric(
    forecast_object$mean
  )
}


# ============================================================
# 8. Forecast AR(1) + Competition Time + Elapsed Time
# ============================================================

forecast_competitor_ar1_time_elapsed <- function(
    model_object,
    future_data
) {
  
  if (
    is.null(model_object$fit)
  ) {
    return(
      rep(
        NA_real_,
        nrow(future_data)
      )
    )
  }
  
  prepared_future_data <-
    add_competitor_model_features(
      data = future_data,
      total_observations =
        model_object$total_observations
    )
  
  if (
    any(
      !is.finite(
        prepared_future_data$CompetitionProgress
      )
    ) ||
    any(
      !is.finite(
        prepared_future_data$ElapsedMinutes
      )
    )
  ) {
    stop(
      paste0(
        "Future CompetitionProgress and ElapsedMinutes ",
        "must be finite for ar1_time_elapsed forecasts."
      ),
      call. = FALSE
    )
  }
  
  future_xreg <-
    cbind(
      CompetitionProgress =
        prepared_future_data$CompetitionProgress,
      
      ElapsedMinutes =
        prepared_future_data$ElapsedMinutes
    )
  
  forecast_object <-
    forecast::forecast(
      model_object$fit,
      h = nrow(
        prepared_future_data
      ),
      xreg = future_xreg
    )
  
  as.numeric(
    forecast_object$mean
  )
}


# ============================================================
# 9. Forecast Mean from Any Candidate
# ============================================================

forecast_competitor_mean <- function(
    model_object,
    future_data
) {
  
  if (
    !inherits(
      model_object,
      "competitor_mean_model"
    )
  ) {
    stop(
      paste0(
        "model_object must be created by ",
        "fit_competitor_mean_model()."
      ),
      call. = FALSE
    )
  }
  
  model_type <-
    model_object$model_type
  
  if (
    model_type ==
    "ar1"
  ) {
    return(
      forecast_competitor_ar1(
        model_object = model_object,
        horizon = nrow(
          future_data
        )
      )
    )
  }
  
  if (
    model_type ==
    "ar1_time_elapsed"
  ) {
    return(
      forecast_competitor_ar1_time_elapsed(
        model_object = model_object,
        future_data = future_data
      )
    )
  }
  
  predict_competitor_mean_model(
    model_object = model_object,
    new_data = future_data
  )
}


# ============================================================
# 10. Safe Mean-Model Fit
# ============================================================
#
# Expanding samples can temporarily make a model unavailable,
# especially hurdle specifications.
#
# Rather than terminate the entire rolling evaluation, return
# NULL and record an unavailable forecast.
# ============================================================

safe_fit_competitor_mean_model <- function(
    data,
    rank_name,
    model_type,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  tryCatch(
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
    ),
    error = function(e) {
      NULL
    }
  )
}


# ============================================================
# 11. One-Step-Ahead Mean Forecast
# ============================================================

one_step_competitor_mean_forecast <- function(
    data,
    rank_name,
    model_type,
    forecast_origin,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  validate_competitor_forecast_origin(
    data = data,
    forecast_origin =
      forecast_origin
  )
  
  training_data <-
    data[
      seq_len(
        forecast_origin
      ),
      ,
      drop = FALSE
    ]
  
  target_observation <-
    forecast_origin + 1L
  
  future_data <-
    data[
      target_observation,
      ,
      drop = FALSE
    ]
  
  model_object <-
    safe_fit_competitor_mean_model(
      data = training_data,
      rank_name = rank_name,
      model_type = model_type,
      total_observations =
        total_observations,
      min_zero_observations =
        min_zero_observations,
      min_nonzero_observations =
        min_nonzero_observations
    )
  
  if (
    is.null(model_object) ||
    is.null(model_object$fit)
  ) {
    
    mean_forecast <-
      NA_real_
    
    model_available <-
      FALSE
    
  } else {
    
    mean_forecast <-
      tryCatch(
        forecast_competitor_mean(
          model_object =
            model_object,
          future_data =
            future_data
        )[1],
        error = function(e) {
          NA_real_
        }
      )
    
    model_available <-
      is.finite(
        mean_forecast
      )
  }
  
  actual_return <-
    as.numeric(
      data[
        target_observation,
        rank_name
      ][[1]]
    )
  
  forecast_error <-
    if (
      is.finite(mean_forecast) &&
      is.finite(actual_return)
    ) {
      actual_return -
        mean_forecast
    } else {
      NA_real_
    }
  
  data.frame(
    ForecastOrigin =
      as.integer(
        forecast_origin
      ),
    
    TargetObservation =
      as.integer(
        target_observation
      ),
    
    Rank =
      rank_name,
    
    MeanModel =
      model_type,
    
    ModelAvailable =
      model_available,
    
    MeanForecast =
      mean_forecast,
    
    ActualReturn =
      actual_return,
    
    ForecastError =
      forecast_error,
    
    SquaredError =
      forecast_error^2,
    
    AbsoluteError =
      abs(
        forecast_error
      ),
    
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 12. Expanding-Origin Mean Forecasts for One Model
# ============================================================

expanding_competitor_mean_forecasts <- function(
    data,
    rank_name,
    model_type,
    initial_window,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  validate_total_competition_observations(
    total_observations
  )
  
  if (
    length(initial_window) != 1 ||
    !is.numeric(initial_window) ||
    is.na(initial_window) ||
    !is.finite(initial_window) ||
    initial_window != as.integer(initial_window)
  ) {
    stop(
      "initial_window must be a single integer.",
      call. = FALSE
    )
  }
  
  initial_window <-
    as.integer(
      initial_window
    )
  
  if (
    initial_window < 2 ||
    initial_window >= nrow(data)
  ) {
    stop(
      paste0(
        "initial_window must be at least 2 and smaller ",
        "than the number of observations."
      ),
      call. = FALSE
    )
  }
  
  forecast_origins <-
    seq.int(
      from = initial_window,
      to = nrow(data) - 1L
    )
  
  forecast_rows <-
    lapply(
      forecast_origins,
      function(origin) {
        
        one_step_competitor_mean_forecast(
          data = data,
          rank_name = rank_name,
          model_type = model_type,
          forecast_origin = origin,
          total_observations =
            total_observations,
          min_zero_observations =
            min_zero_observations,
          min_nonzero_observations =
            min_nonzero_observations
        )
      }
    )
  
  output <-
    do.call(
      rbind,
      forecast_rows
    )
  
  rownames(output) <-
    NULL
  
  output
}


# ============================================================
# 13. Expanding-Origin Mean Forecasts for All Candidates
# ============================================================

expanding_competitor_mean_candidates <- function(
    data,
    rank_names,
    model_types,
    initial_window,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20,
    verbose = TRUE
) {
  
  results <-
    list()
  
  result_index <-
    1L
  
  for (
    rank_name in rank_names
  ) {
    
    for (
      model_type in model_types
    ) {
      
      if (isTRUE(verbose)) {
        message(
          paste0(
            "Expanding mean forecasts: ",
            rank_name,
            " | ",
            model_type
          )
        )
      }
      
      current_result <-
        expanding_competitor_mean_forecasts(
          data = data,
          rank_name = rank_name,
          model_type = model_type,
          initial_window =
            initial_window,
          total_observations =
            total_observations,
          min_zero_observations =
            min_zero_observations,
          min_nonzero_observations =
            min_nonzero_observations
        )
      
      results[[result_index]] <-
        current_result
      
      result_index <-
        result_index + 1L
    }
  }
  
  output <-
    do.call(
      rbind,
      results
    )
  
  rownames(output) <-
    NULL
  
  output
}


# ============================================================
# 14. Mean Forecast Performance Summary
# ============================================================

summarize_competitor_mean_forecasts <- function(
    forecast_table
) {
  
  required_columns <- c(
    "Rank",
    "MeanModel",
    "ModelAvailable",
    "ForecastError",
    "SquaredError",
    "AbsoluteError"
  )
  
  missing_columns <-
    setdiff(
      required_columns,
      names(
        forecast_table
      )
    )
  
  if (
    length(
      missing_columns
    ) > 0
  ) {
    stop(
      paste0(
        "forecast_table is missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  split_data <-
    split(
      forecast_table,
      interaction(
        forecast_table$Rank,
        forecast_table$MeanModel,
        drop = TRUE
      )
    )
  
  summaries <-
    lapply(
      split_data,
      function(x) {
        
        usable <-
          is.finite(
            x$ForecastError
          )
        
        number_forecasts <-
          sum(
            usable
          )
        
        data.frame(
          Rank =
            x$Rank[1],
          
          MeanModel =
            x$MeanModel[1],
          
          NumberForecasts =
            number_forecasts,
          
          AvailabilityRate =
            mean(
              x$ModelAvailable
            ),
          
          Bias =
            if (
              number_forecasts > 0
            ) {
              mean(
                x$ForecastError[
                  usable
                ]
              )
            } else {
              NA_real_
            },
          
          RMSE =
            if (
              number_forecasts > 0
            ) {
              sqrt(
                mean(
                  x$SquaredError[
                    usable
                  ]
                )
              )
            } else {
              NA_real_
            },
          
          MAE =
            if (
              number_forecasts > 0
            ) {
              mean(
                x$AbsoluteError[
                  usable
                ]
              )
            } else {
              NA_real_
            },
          
          stringsAsFactors =
            FALSE
        )
      }
    )
  
  output <-
    do.call(
      rbind,
      summaries
    )
  
  rownames(output) <-
    NULL
  
  output
}


# ============================================================
# 15. Validate Variance Model Type
# ============================================================

validate_competitor_variance_model_type <- function(
    variance_model_type
) {
  
  if (
    length(variance_model_type) != 1 ||
    !is.character(variance_model_type)
  ) {
    stop(
      paste0(
        "variance_model_type must be one character string."
      ),
      call. = FALSE
    )
  }
  
  if (
    !variance_model_type %in%
    COMPETITOR_VARIANCE_MODEL_TYPES
  ) {
    stop(
      paste0(
        "Unsupported variance model type: ",
        variance_model_type,
        ". Supported types are: ",
        paste(
          COMPETITOR_VARIANCE_MODEL_TYPES,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 16. Fit Positive Deterministic Variance Regression
# ============================================================
#
# The deterministic variance models use a Gamma GLM with a
# log link:
#
#   E[e_t^2 | X_t] > 0
#
# This ensures positive fitted conditional variances.
#
# A very small positive floor is used only when an observed
# squared residual is exactly zero, because Gamma regression
# requires a strictly positive response.
# ============================================================

fit_positive_variance_glm <- function(
    residual_data,
    variance_model_type,
    variance_floor = 1e-12
) {
  
  validate_competitor_variance_model_type(
    variance_model_type
  )
  
  if (
    variance_model_type ==
    "garch11_student_t"
  ) {
    stop(
      paste0(
        "garch11_student_t must be fitted with ",
        "fit_competitor_variance_model()."
      ),
      call. = FALSE
    )
  }
  
  model_data <-
    as.data.frame(
      residual_data
    )
  
  if (
    !"SquaredResidual" %in%
    names(model_data)
  ) {
    stop(
      paste0(
        "residual_data must contain ",
        "'SquaredResidual'."
      ),
      call. = FALSE
    )
  }
  
  model_data$PositiveSquaredResidual <-
    pmax(
      model_data$SquaredResidual,
      variance_floor
    )
  
  if (
    variance_model_type ==
    "constant"
  ) {
    
    fit <-
      stats::glm(
        PositiveSquaredResidual ~ 1,
        data = model_data,
        family = stats::Gamma(
          link = "log"
        )
      )
    
  } else if (
    variance_model_type ==
    "elapsed_linear"
  ) {
    
    usable_rows <-
      is.finite(
        model_data$ElapsedMinutes
      )
    
    fit <-
      stats::glm(
        PositiveSquaredResidual ~ ElapsedMinutes,
        data =
          model_data[
            usable_rows,
            ,
            drop = FALSE
          ],
        family = stats::Gamma(
          link = "log"
        )
      )
    
  } else if (
    variance_model_type ==
    "elapsed_log"
  ) {
    
    usable_rows <-
      is.finite(
        model_data$ElapsedMinutes
      )
    
    model_data$LogElapsedMinutes <-
      log1p(
        model_data$ElapsedMinutes
      )
    
    fit <-
      stats::glm(
        PositiveSquaredResidual ~ LogElapsedMinutes,
        data =
          model_data[
            usable_rows,
            ,
            drop = FALSE
          ],
        family = stats::Gamma(
          link = "log"
        )
      )
    
  } else if (
    variance_model_type ==
    "long_interval"
  ) {
    
    usable_rows <-
      is.finite(
        model_data$ElapsedMinutes
      )
    
    model_data$LongInterval <-
      as.numeric(
        model_data$ElapsedMinutes > 60
      )
    
    fit <-
      stats::glm(
        PositiveSquaredResidual ~ LongInterval,
        data =
          model_data[
            usable_rows,
            ,
            drop = FALSE
          ],
        family = stats::Gamma(
          link = "log"
        )
      )
    
  } else {
    
    stop(
      paste0(
        "Unsupported deterministic variance model: ",
        variance_model_type
      ),
      call. = FALSE
    )
  }
  
  fit
}


# ============================================================
# 17. Fit GARCH(1,1) Student-t Variance Candidate
# ============================================================

fit_competitor_garch11_student_t <- function(
    residuals
) {
  
  if (
    !requireNamespace(
      "rugarch",
      quietly = TRUE
    )
  ) {
    return(
      NULL
    )
  }
  
  residuals <-
    as.numeric(
      residuals
    )
  
  residuals <-
    residuals[
      is.finite(
        residuals
      )
    ]
  
  if (
    length(
      residuals
    ) < 30
  ) {
    return(
      NULL
    )
  }
  
  specification <-
    rugarch::ugarchspec(
      variance.model =
        list(
          model = "sGARCH",
          garchOrder = c(
            1,
            1
          )
        ),
      mean.model =
        list(
          armaOrder = c(
            0,
            0
          ),
          include.mean = FALSE
        ),
      distribution.model =
        "std"
    )
  
  fit <-
    tryCatch(
      rugarch::ugarchfit(
        spec = specification,
        data = residuals,
        solver = "hybrid",
        solver.control =
          list(
            trace = 0
          )
      ),
      error = function(e) {
        NULL
      }
    )
  
  fit
}


# ============================================================
# 18. Fit One Competitor Variance Model
# ============================================================

fit_competitor_variance_model <- function(
    residual_data,
    variance_model_type,
    variance_floor = 1e-12
) {
  
  validate_competitor_variance_model_type(
    variance_model_type
  )
  
  if (
    variance_model_type ==
    "garch11_student_t"
  ) {
    
    if (
      !"Residual" %in%
      names(
        residual_data
      )
    ) {
      stop(
        paste0(
          "residual_data must contain 'Residual' ",
          "for GARCH fitting."
        ),
        call. = FALSE
      )
    }
    
    fit <-
      fit_competitor_garch11_student_t(
        residuals =
          residual_data$Residual
      )
    
  } else {
    
    fit <-
      fit_positive_variance_glm(
        residual_data =
          residual_data,
        variance_model_type =
          variance_model_type,
        variance_floor =
          variance_floor
      )
  }
  
  structure(
    list(
      model_type =
        variance_model_type,
      
      model_label =
        unname(
          COMPETITOR_VARIANCE_MODEL_LABELS[
            variance_model_type
          ]
        ),
      
      fit =
        fit,
      
      variance_floor =
        variance_floor,
      
      training_observations =
        nrow(
          residual_data
        )
    ),
    class =
      "competitor_variance_model"
  )
}


# ============================================================
# 19. Forecast One Conditional Variance
# ============================================================

forecast_competitor_variance <- function(
    variance_model_object,
    future_data
) {
  
  if (
    !inherits(
      variance_model_object,
      "competitor_variance_model"
    )
  ) {
    stop(
      paste0(
        "variance_model_object must be created by ",
        "fit_competitor_variance_model()."
      ),
      call. = FALSE
    )
  }
  
  if (
    is.null(
      variance_model_object$fit
    )
  ) {
    return(
      rep(
        NA_real_,
        nrow(
          future_data
        )
      )
    )
  }
  
  model_type <-
    variance_model_object$model_type
  
  variance_floor <-
    variance_model_object$variance_floor
  
  if (
    model_type ==
    "garch11_student_t"
  ) {
    
    garch_forecast <-
      tryCatch(
        rugarch::ugarchforecast(
          variance_model_object$fit,
          n.ahead =
            nrow(
              future_data
            )
        ),
        error = function(e) {
          NULL
        }
      )
    
    if (
      is.null(
        garch_forecast
      )
    ) {
      return(
        rep(
          NA_real_,
          nrow(
            future_data
          )
        )
      )
    }
    
    sigma_forecast <-
      as.numeric(
        rugarch::sigma(
          garch_forecast
        )
      )
    
    return(
      pmax(
        sigma_forecast^2,
        variance_floor
      )
    )
  }
  
  prediction_data <-
    as.data.frame(
      future_data
    )
  
  if (
    "ElapsedMinutes" %in%
    names(
      prediction_data
    )
  ) {
    
    prediction_data$LogElapsedMinutes <-
      log1p(
        prediction_data$ElapsedMinutes
      )
    
    prediction_data$LongInterval <-
      as.numeric(
        prediction_data$ElapsedMinutes > 60
      )
  }
  
  variance_forecast <-
    tryCatch(
      as.numeric(
        stats::predict(
          variance_model_object$fit,
          newdata =
            prediction_data,
          type = "response"
        )
      ),
      error = function(e) {
        rep(
          NA_real_,
          nrow(
            prediction_data
          )
        )
      }
    )
  
  pmax(
    variance_forecast,
    variance_floor
  )
}


# ============================================================
# 20. Construct In-Sample Mean Residual Data
# ============================================================
#
# This is used inside each expanding training window to fit
# the conditional-variance model after fitting the specified
# conditional-mean model.
# ============================================================

construct_competitor_mean_residual_data <- function(
    data,
    rank_name,
    mean_model_type,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20
) {
  
  mean_model <-
    safe_fit_competitor_mean_model(
      data = data,
      rank_name = rank_name,
      model_type =
        mean_model_type,
      total_observations =
        total_observations,
      min_zero_observations =
        min_zero_observations,
      min_nonzero_observations =
        min_nonzero_observations
    )
  
  if (
    is.null(mean_model) ||
    is.null(mean_model$fit)
  ) {
    return(NULL)
  }
  
  model_data <-
    prepare_competitor_rank_model_data(
      data = data,
      rank_name = rank_name,
      total_observations =
        total_observations
    )
  
  model_type <-
    mean_model$model_type
  
  fitted_mean <-
    rep(
      NA_real_,
      nrow(
        model_data
      )
    )
  
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
    
    fitted_values <-
      tryCatch(
        as.numeric(
          stats::predict(
            mean_model$fit,
            newdata =
              model_data
          )
        ),
        error = function(e) {
          rep(
            NA_real_,
            nrow(
              model_data
            )
          )
        }
      )
    
    fitted_mean <-
      fitted_values
    
  } else if (
    model_type ==
    "hurdle_time_duration"
  ) {
    
    fitted_mean <-
      tryCatch(
        predict_competitor_hurdle(
          fitted_model =
            mean_model$fit,
          new_data =
            model_data
        ),
        error = function(e) {
          rep(
            NA_real_,
            nrow(
              model_data
            )
          )
        }
      )
    
  } else if (
    model_type ==
    "ar1"
  ) {
    
    ar_fitted <-
      as.numeric(
        stats::fitted(
          mean_model$fit
        )
      )
    
    if (
      length(ar_fitted) ==
      nrow(model_data)
    ) {
      fitted_mean <-
        ar_fitted
    }
    
  } else if (
    model_type ==
    "ar1_time_elapsed"
  ) {
    
    usable_rows <-
      is.finite(
        model_data$ElapsedMinutes
      )
    
    ar_fitted <-
      as.numeric(
        stats::fitted(
          mean_model$fit
        )
      )
    
    if (
      length(ar_fitted) ==
      sum(
        usable_rows
      )
    ) {
      fitted_mean[
        usable_rows
      ] <-
        ar_fitted
    }
  }
  
  residual <-
    model_data$Return -
    fitted_mean
  
  output <-
    model_data
  
  output$FittedMean <-
    fitted_mean
  
  output$Residual <-
    residual
  
  output$SquaredResidual <-
    residual^2
  
  output$AbsoluteResidual <-
    abs(
      residual
    )
  
  output
}


# ============================================================
# 21. One-Step-Ahead Joint Mean + Variance Forecast
# ============================================================

one_step_competitor_distribution_forecast <- function(
    data,
    rank_name,
    mean_model_type,
    variance_model_type,
    forecast_origin,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20,
    variance_floor = 1e-12
) {
  
  validate_competitor_forecast_origin(
    data = data,
    forecast_origin =
      forecast_origin
  )
  
  training_data <-
    data[
      seq_len(
        forecast_origin
      ),
      ,
      drop = FALSE
    ]
  
  target_observation <-
    forecast_origin + 1L
  
  future_data <-
    data[
      target_observation,
      ,
      drop = FALSE
    ]
  
  mean_model <-
    safe_fit_competitor_mean_model(
      data = training_data,
      rank_name = rank_name,
      model_type =
        mean_model_type,
      total_observations =
        total_observations,
      min_zero_observations =
        min_zero_observations,
      min_nonzero_observations =
        min_nonzero_observations
    )
  
  if (
    is.null(mean_model) ||
    is.null(mean_model$fit)
  ) {
    
    mean_forecast <-
      NA_real_
    
    mean_available <-
      FALSE
    
  } else {
    
    mean_forecast <-
      tryCatch(
        forecast_competitor_mean(
          model_object =
            mean_model,
          future_data =
            future_data
        )[1],
        error = function(e) {
          NA_real_
        }
      )
    
    mean_available <-
      is.finite(
        mean_forecast
      )
  }
  
  residual_data <-
    construct_competitor_mean_residual_data(
      data = training_data,
      rank_name = rank_name,
      mean_model_type =
        mean_model_type,
      total_observations =
        total_observations,
      min_zero_observations =
        min_zero_observations,
      min_nonzero_observations =
        min_nonzero_observations
    )
  
  if (
    is.null(
      residual_data
    )
  ) {
    
    variance_forecast <-
      NA_real_
    
    variance_available <-
      FALSE
    
  } else {
    
    usable_residual_data <-
      residual_data[
        is.finite(
          residual_data$Residual
        ) &
          is.finite(
            residual_data$SquaredResidual
          ),
        ,
        drop = FALSE
      ]
    
    if (
      nrow(
        usable_residual_data
      ) < 2
    ) {
      
      variance_forecast <-
        NA_real_
      
      variance_available <-
        FALSE
      
    } else {
      
      variance_model <-
        tryCatch(
          fit_competitor_variance_model(
            residual_data =
              usable_residual_data,
            variance_model_type =
              variance_model_type,
            variance_floor =
              variance_floor
          ),
          error = function(e) {
            NULL
          }
        )
      
      if (
        is.null(variance_model) ||
        is.null(variance_model$fit)
      ) {
        
        variance_forecast <-
          NA_real_
        
        variance_available <-
          FALSE
        
      } else {
        
        variance_forecast <-
          tryCatch(
            forecast_competitor_variance(
              variance_model_object =
                variance_model,
              future_data =
                future_data
            )[1],
            error = function(e) {
              NA_real_
            }
          )
        
        variance_available <-
          is.finite(
            variance_forecast
          )
      }
    }
  }
  
  actual_return <-
    as.numeric(
      data[
        target_observation,
        rank_name
      ][[1]]
    )
  
  forecast_error <-
    if (
      is.finite(mean_forecast) &&
      is.finite(actual_return)
    ) {
      actual_return -
        mean_forecast
    } else {
      NA_real_
    }
  
  realized_squared_error <-
    forecast_error^2
  
  standardized_error <-
    if (
      is.finite(forecast_error) &&
      is.finite(variance_forecast) &&
      variance_forecast > 0
    ) {
      forecast_error /
        sqrt(
          variance_forecast
        )
    } else {
      NA_real_
    }
  
  qlike <-
    if (
      is.finite(realized_squared_error) &&
      is.finite(variance_forecast) &&
      variance_forecast > 0
    ) {
      log(
        variance_forecast
      ) +
        realized_squared_error /
        variance_forecast
    } else {
      NA_real_
    }
  
  data.frame(
    ForecastOrigin =
      as.integer(
        forecast_origin
      ),
    
    TargetObservation =
      as.integer(
        target_observation
      ),
    
    Rank =
      rank_name,
    
    MeanModel =
      mean_model_type,
    
    VarianceModel =
      variance_model_type,
    
    MeanAvailable =
      mean_available,
    
    VarianceAvailable =
      variance_available,
    
    MeanForecast =
      mean_forecast,
    
    VarianceForecast =
      variance_forecast,
    
    VolatilityForecast =
      if (
        is.finite(
          variance_forecast
        )
      ) {
        sqrt(
          variance_forecast
        )
      } else {
        NA_real_
      },
    
    ActualReturn =
      actual_return,
    
    ForecastError =
      forecast_error,
    
    SquaredError =
      realized_squared_error,
    
    AbsoluteError =
      abs(
        forecast_error
      ),
    
    StandardizedError =
      standardized_error,
    
    QLIKE =
      qlike,
    
    stringsAsFactors =
      FALSE
  )
}


# ============================================================
# 22. Expanding-Origin Joint Mean + Variance Forecasts
# ============================================================

expanding_competitor_distribution_forecasts <- function(
    data,
    rank_name,
    mean_model_type,
    variance_model_type,
    initial_window,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20,
    variance_floor = 1e-12,
    verbose = FALSE
) {
  
  if (
    initial_window < 2 ||
    initial_window >= nrow(data)
  ) {
    stop(
      paste0(
        "initial_window must be at least 2 and smaller ",
        "than the number of observations."
      ),
      call. = FALSE
    )
  }
  
  forecast_origins <-
    seq.int(
      from =
        as.integer(
          initial_window
        ),
      to =
        nrow(data) - 1L
    )
  
  results <-
    vector(
      "list",
      length(
        forecast_origins
      )
    )
  
  for (
    i in seq_along(
      forecast_origins
    )
  ) {
    
    origin <-
      forecast_origins[i]
    
    if (
      isTRUE(verbose)
    ) {
      message(
        paste0(
          rank_name,
          " | ",
          mean_model_type,
          " | ",
          variance_model_type,
          " | origin ",
          origin
        )
      )
    }
    
    results[[i]] <-
      one_step_competitor_distribution_forecast(
        data = data,
        rank_name = rank_name,
        mean_model_type =
          mean_model_type,
        variance_model_type =
          variance_model_type,
        forecast_origin =
          origin,
        total_observations =
          total_observations,
        min_zero_observations =
          min_zero_observations,
        min_nonzero_observations =
          min_nonzero_observations,
        variance_floor =
          variance_floor
      )
  }
  
  output <-
    do.call(
      rbind,
      results
    )
  
  rownames(output) <-
    NULL
  
  output
}


# ============================================================
# 23. Expanding Forecasts Across Candidate Grid
# ============================================================

expanding_competitor_candidate_grid <- function(
    data,
    rank_names,
    mean_model_types,
    variance_model_types,
    initial_window,
    total_observations,
    min_zero_observations = 5,
    min_nonzero_observations = 20,
    variance_floor = 1e-12,
    verbose = TRUE
) {
  
  results <-
    list()
  
  result_index <-
    1L
  
  for (
    rank_name in rank_names
  ) {
    
    for (
      mean_model_type in mean_model_types
    ) {
      
      for (
        variance_model_type in variance_model_types
      ) {
        
        if (
          isTRUE(verbose)
        ) {
          message(
            paste0(
              "Forecasting: ",
              rank_name,
              " | mean=",
              mean_model_type,
              " | variance=",
              variance_model_type
            )
          )
        }
        
        current_result <-
          expanding_competitor_distribution_forecasts(
            data = data,
            rank_name = rank_name,
            mean_model_type =
              mean_model_type,
            variance_model_type =
              variance_model_type,
            initial_window =
              initial_window,
            total_observations =
              total_observations,
            min_zero_observations =
              min_zero_observations,
            min_nonzero_observations =
              min_nonzero_observations,
            variance_floor =
              variance_floor,
            verbose = FALSE
          )
        
        results[[result_index]] <-
          current_result
        
        result_index <-
          result_index + 1L
      }
    }
  }
  
  output <-
    do.call(
      rbind,
      results
    )
  
  rownames(output) <-
    NULL
  
  output
}


# ============================================================
# 24. Summarize Joint Forecast Performance
# ============================================================

summarize_competitor_distribution_forecasts <- function(
    forecast_table
) {
  
  required_columns <- c(
    "Rank",
    "MeanModel",
    "VarianceModel",
    "ForecastError",
    "SquaredError",
    "AbsoluteError",
    "QLIKE"
  )
  
  missing_columns <-
    setdiff(
      required_columns,
      names(
        forecast_table
      )
    )
  
  if (
    length(
      missing_columns
    ) > 0
  ) {
    stop(
      paste0(
        "forecast_table is missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  grouping_variable <-
    interaction(
      forecast_table$Rank,
      forecast_table$MeanModel,
      forecast_table$VarianceModel,
      drop = TRUE
    )
  
  split_data <-
    split(
      forecast_table,
      grouping_variable
    )
  
  summaries <-
    lapply(
      split_data,
      function(x) {
        
        mean_usable <-
          is.finite(
            x$ForecastError
          )
        
        variance_usable <-
          is.finite(
            x$QLIKE
          )
        
        n_mean <-
          sum(
            mean_usable
          )
        
        n_variance <-
          sum(
            variance_usable
          )
        
        data.frame(
          Rank =
            x$Rank[1],
          
          MeanModel =
            x$MeanModel[1],
          
          VarianceModel =
            x$VarianceModel[1],
          
          NumberMeanForecasts =
            n_mean,
          
          NumberVarianceForecasts =
            n_variance,
          
          Bias =
            if (
              n_mean > 0
            ) {
              mean(
                x$ForecastError[
                  mean_usable
                ]
              )
            } else {
              NA_real_
            },
          
          RMSE =
            if (
              n_mean > 0
            ) {
              sqrt(
                mean(
                  x$SquaredError[
                    mean_usable
                  ]
                )
              )
            } else {
              NA_real_
            },
          
          MAE =
            if (
              n_mean > 0
            ) {
              mean(
                x$AbsoluteError[
                  mean_usable
                ]
              )
            } else {
              NA_real_
            },
          
          MeanQLIKE =
            if (
              n_variance > 0
            ) {
              mean(
                x$QLIKE[
                  variance_usable
                ]
              )
            } else {
              NA_real_
            },
          
          stringsAsFactors =
            FALSE
        )
      }
    )
  
  output <-
    do.call(
      rbind,
      summaries
    )
  
  rownames(output) <-
    NULL
  
  output
}


# ============================================================
# 25. Transform One Rank-Return Vector to Ordered-State Move
# ============================================================
#
# Raw rank return vector:
#
#   r =
#   [r1, r2, r3, r4, r15, r16]'
#
# Ordered-state increment:
#
#   Delta S =
#   [
#     Delta X1,
#     Delta D12,
#     Delta D23,
#     Delta D34,
#     Delta D4_15,
#     Delta D15_16
#   ]'
#
# where:
#
#   Delta X1      = r1
#   Delta D12     = r1  - r2
#   Delta D23     = r2  - r3
#   Delta D34     = r3  - r4
#   Delta D4_15   = r4  - r15
#   Delta D15_16  = r15 - r16
#
# ============================================================

transform_rank_returns_to_final_state <- function(
    rank_returns
) {
  
  rank_returns <-
    as.numeric(
      rank_returns
    )
  
  if (
    length(
      rank_returns
    ) != 6
  ) {
    stop(
      "rank_returns must contain exactly six values.",
      call. = FALSE
    )
  }
  
  state_increment <-
    as.numeric(
      FINAL_RANK_TRANSFORMATION_MATRIX %*%
        rank_returns
    )
  
  names(
    state_increment
  ) <-
    FINAL_RANK_STATE_NAMES
  
  state_increment
}


# ============================================================
# 26. Transform Matrix of Rank Returns to Ordered-State Moves
# ============================================================
#
# Input:
#
#   n x 6 matrix
#
# Output:
#
#   n x 6 matrix
#
# ============================================================

transform_rank_return_matrix_to_final_state <- function(
    rank_return_matrix
) {
  
  rank_return_matrix <-
    as.matrix(
      rank_return_matrix
    )
  
  if (
    ncol(
      rank_return_matrix
    ) != 6
  ) {
    stop(
      "rank_return_matrix must have exactly six columns.",
      call. = FALSE
    )
  }
  
  state_matrix <-
    rank_return_matrix %*%
    t(
      FINAL_RANK_TRANSFORMATION_MATRIX
    )
  
  colnames(
    state_matrix
  ) <-
    FINAL_RANK_STATE_NAMES
  
  state_matrix
}


# ============================================================
# 27. Transform Mean Vector to Ordered-State Mean
# ============================================================

transform_rank_mean_to_final_state_mean <- function(
    rank_mean
) {
  
  transform_rank_returns_to_final_state(
    rank_returns =
      rank_mean
  )
}


# ============================================================
# 28. Transform Rank Covariance to Ordered-State Covariance
# ============================================================
#
# Sigma_state =
#
#   M_f %*% Sigma_rank %*% t(M_f)
#
# ============================================================

transform_rank_covariance_to_final_state <- function(
    rank_covariance
) {
  
  rank_covariance <-
    as.matrix(
      rank_covariance
    )
  
  if (
    !all(
      dim(
        rank_covariance
      ) ==
      c(
        6,
        6
      )
    )
  ) {
    stop(
      "rank_covariance must be a 6 x 6 matrix.",
      call. = FALSE
    )
  }
  
  state_covariance <-
    FINAL_RANK_TRANSFORMATION_MATRIX %*%
    rank_covariance %*%
    t(
      FINAL_RANK_TRANSFORMATION_MATRIX
    )
  
  rownames(
    state_covariance
  ) <-
    FINAL_RANK_STATE_NAMES
  
  colnames(
    state_covariance
  ) <-
    FINAL_RANK_STATE_NAMES
  
  state_covariance
}


# ============================================================
# 29. Transform Rank/Futures Cross-Covariance
# ============================================================
#
# If:
#
#   C_rank_futures = Cov(r_rank, R_futures)
#
# then:
#
#   C_state_futures =
#
#     M_f %*% C_rank_futures
#
# ============================================================

transform_rank_futures_covariance_to_final_state <- function(
    rank_futures_covariance
) {
  
  rank_futures_covariance <-
    as.matrix(
      rank_futures_covariance
    )
  
  if (
    nrow(
      rank_futures_covariance
    ) != 6
  ) {
    stop(
      paste0(
        "rank_futures_covariance must have six rows, ",
        "one for each final-rank return process."
      ),
      call. = FALSE
    )
  }
  
  state_futures_covariance <-
    FINAL_RANK_TRANSFORMATION_MATRIX %*%
    rank_futures_covariance
  
  rownames(
    state_futures_covariance
  ) <-
    FINAL_RANK_STATE_NAMES
  
  state_futures_covariance
}


# ============================================================
# 30. Construct Rank Covariance from Volatilities + Correlation
# ============================================================
#
# Sigma =
#
#   diag(sigma) %*% Corr %*% diag(sigma)
#
# This will later allow Notebook 21/22/23 to combine
# rank-specific conditional volatility forecasts with a
# cross-rank standardized-residual correlation estimate.
# ============================================================

construct_rank_covariance <- function(
    volatility_forecast,
    correlation_matrix
) {
  
  volatility_forecast <-
    as.numeric(
      volatility_forecast
    )
  
  correlation_matrix <-
    as.matrix(
      correlation_matrix
    )
  
  if (
    length(
      volatility_forecast
    ) != 6
  ) {
    stop(
      paste0(
        "volatility_forecast must contain exactly ",
        "six values."
      ),
      call. = FALSE
    )
  }
  
  if (
    !all(
      dim(
        correlation_matrix
      ) ==
      c(
        6,
        6
      )
    )
  ) {
    stop(
      "correlation_matrix must be 6 x 6.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        volatility_forecast
      )
    ) ||
    any(
      volatility_forecast < 0
    )
  ) {
    stop(
      paste0(
        "volatility_forecast must contain finite, ",
        "nonnegative values."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        correlation_matrix
      )
    )
  ) {
    stop(
      "correlation_matrix contains non-finite values.",
      call. = FALSE
    )
  }
  
  volatility_matrix <-
    diag(
      volatility_forecast,
      nrow = 6,
      ncol = 6
    )
  
  covariance_matrix <-
    volatility_matrix %*%
    correlation_matrix %*%
    volatility_matrix
  
  rownames(
    covariance_matrix
  ) <-
    FINAL_RANK_RETURN_NAMES
  
  colnames(
    covariance_matrix
  ) <-
    FINAL_RANK_RETURN_NAMES
  
  covariance_matrix
}


# ============================================================
# 31. Estimate Standardized-Residual Correlation
# ============================================================

estimate_standardized_residual_correlation <- function(
    standardized_residual_matrix,
    use = "pairwise.complete.obs"
) {
  
  standardized_residual_matrix <-
    as.matrix(
      standardized_residual_matrix
    )
  
  if (
    ncol(
      standardized_residual_matrix
    ) != 6
  ) {
    stop(
      paste0(
        "standardized_residual_matrix must have ",
        "six columns."
      ),
      call. = FALSE
    )
  }
  
  correlation_matrix <-
    stats::cor(
      standardized_residual_matrix,
      use = use
    )
  
  correlation_matrix <-
    (
      correlation_matrix +
        t(
          correlation_matrix
        )
    ) / 2
  
  diag(
    correlation_matrix
  ) <-
    1
  
  rownames(
    correlation_matrix
  ) <-
    FINAL_RANK_RETURN_NAMES
  
  colnames(
    correlation_matrix
  ) <-
    FINAL_RANK_RETURN_NAMES
  
  correlation_matrix
}


# ============================================================
# 32. QLIKE Loss
# ============================================================

competitor_qlike_loss <- function(
    realized_squared_error,
    variance_forecast,
    variance_floor = 1e-12
) {
  
  realized_squared_error <-
    as.numeric(
      realized_squared_error
    )
  
  variance_forecast <-
    pmax(
      as.numeric(
        variance_forecast
      ),
      variance_floor
    )
  
  if (
    length(
      realized_squared_error
    ) !=
    length(
      variance_forecast
    )
  ) {
    stop(
      paste0(
        "realized_squared_error and variance_forecast ",
        "must have equal length."
      ),
      call. = FALSE
    )
  }
  
  log(
    variance_forecast
  ) +
    realized_squared_error /
    variance_forecast
}


# ============================================================
# 33. Mean Forecast Losses
# ============================================================

competitor_mean_forecast_losses <- function(
    actual,
    forecast
) {
  
  actual <-
    as.numeric(
      actual
    )
  
  forecast <-
    as.numeric(
      forecast
    )
  
  if (
    length(actual) !=
    length(forecast)
  ) {
    stop(
      "actual and forecast must have equal length.",
      call. = FALSE
    )
  }
  
  error <-
    actual -
    forecast
  
  data.frame(
    Error =
      error,
    
    SquaredError =
      error^2,
    
    AbsoluteError =
      abs(
        error
      )
  )
}


# ============================================================
# 34. Script Completion Message
# ============================================================

message(
  paste0(
    "competitor_forecasting.R loaded successfully. ",
    "Expanding-origin mean, variance, and ordered-state ",
    "forecasting functions are available."
  )
)
