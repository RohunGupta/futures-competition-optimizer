# ============================================================
# futures_forecasting.R
#
# Reusable expanding-origin forecasting helpers for the
# competition-aligned intraday futures pipeline.
#
# This script:
#   1. fits mean models on expanding training windows;
#   2. generates one-step-ahead conditional-mean forecasts;
#   3. constructs strictly lagged mean residual information;
#   4. fits candidate conditional-variance models;
#   5. generates one-step-ahead variance forecasts;
#   6. constructs standardized OOS innovations;
#   7. records model failures explicitly;
#   8. never uses the current realized return to forecast
#      that same timestamp.
#
# This script DOES NOT select final production models.
# Final selection occurs later from expanding-origin OOS results.
# ============================================================


# ============================================================
# 1. Dependencies
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(lubridate)
  library(here)
})


# ============================================================
# 2. Source futures modeling helpers when needed
# ============================================================

required_futures_model_functions <- c(
  "add_futures_model_features",
  "validate_futures_model_data",
  "get_all_futures_mean_model_names",
  "get_futures_variance_model_specs",
  "fit_futures_mean_model",
  "predict_futures_mean_model",
  "attach_futures_mean_residuals",
  "fit_all_futures_variance_candidates",
  "attach_futures_variance_predictions"
)

missing_futures_model_functions <-
  required_futures_model_functions[
    !vapply(
      required_futures_model_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (
  length(
    missing_futures_model_functions
  ) > 0L
) {
  
  futures_models_path <-
    here(
      "scripts",
      "futures_models.R"
    )
  
  if (
    !file.exists(
      futures_models_path
    )
  ) {
    stop(
      paste0(
        "Required futures modeling helpers are missing and ",
        "scripts/futures_models.R could not be found."
      ),
      call. = FALSE
    )
  }
  
  source(
    futures_models_path
  )
}

missing_futures_model_functions <-
  required_futures_model_functions[
    !vapply(
      required_futures_model_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (
  length(
    missing_futures_model_functions
  ) > 0L
) {
  stop(
    paste0(
      "The following required futures modeling functions ",
      "are still unavailable after sourcing futures_models.R: ",
      paste(
        missing_futures_model_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 3. Constants
# ============================================================

if (
  !exists(
    "FUTURES_MODEL_SYMBOLS"
  )
) {
  FUTURES_MODEL_SYMBOLS <- c(
    "CL",
    "GC",
    "SR3",
    "ZN",
    "KE",
    "HO",
    "ZW",
    "NQ"
  )
}

DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS <- 1000L

DEFAULT_FUTURES_OOS_REFIT_EVERY <- 1L

DEFAULT_FUTURES_REPRESENTATIVE_MEAN_MODELS <- c(
  "constant",
  "lag1",
  "interval",
  "seasonal_compact_lag1",
  "hurdle_seasonal_persistence"
)

FUTURES_FORECASTING_EPSILON <- 1e-12


# ============================================================
# 4. Error-object helper
# ============================================================

make_futures_forecast_error <- function(
    stage,
    symbol = NA_character_,
    mean_model_name = NA_character_,
    variance_model_name = NA_character_,
    forecast_timestamp = as.POSIXct(NA),
    message
) {
  
  structure(
    list(
      Stage =
        stage,
      Symbol =
        symbol,
      MeanModel =
        mean_model_name,
      VarianceModel =
        variance_model_name,
      ForecastTimestamp =
        forecast_timestamp,
      ErrorMessage =
        as.character(
          message
        )
    ),
    class =
      "futures_forecast_error"
  )
}


is_futures_forecast_error <- function(
    object
) {
  
  inherits(
    object,
    "futures_forecast_error"
  )
}


# ============================================================
# 5. Basic validation helpers
# ============================================================

validate_futures_forecasting_data <- function(
    data,
    require_all_symbols = FALSE
) {
  
  required_columns <- c(
    "Symbol",
    "CompetitionTimestampCT",
    "CandidateLogReturn"
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
    ) > 0L
  ) {
    stop(
      paste0(
        "Forecasting data are missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (
    nrow(
      data
    ) == 0L
  ) {
    stop(
      "Forecasting data contain zero rows.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(
        data$Symbol
      )
    )
  ) {
    stop(
      "Forecasting data contain missing Symbol values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(
        data$CompetitionTimestampCT
      )
    )
  ) {
    stop(
      "Forecasting data contain missing competition timestamps.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        data$CandidateLogReturn
      )
    )
  ) {
    stop(
      "Forecasting data contain nonfinite CandidateLogReturn values.",
      call. = FALSE
    )
  }
  
  duplicate_rows <-
    data |>
    count(
      Symbol,
      CompetitionTimestampCT,
      name = "N"
    ) |>
    filter(
      N > 1L
    )
  
  if (
    nrow(
      duplicate_rows
    ) > 0L
  ) {
    stop(
      paste0(
        "Forecasting data contain duplicate ",
        "Symbol/timestamp rows."
      ),
      call. = FALSE
    )
  }
  
  if (
    require_all_symbols
  ) {
    
    missing_symbols <-
      setdiff(
        FUTURES_MODEL_SYMBOLS,
        unique(
          data$Symbol
        )
      )
    
    if (
      length(
        missing_symbols
      ) > 0L
    ) {
      stop(
        paste0(
          "Forecasting data are missing futures: ",
          paste(
            missing_symbols,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 6. Prepare one symbol for expanding-origin forecasting
# ============================================================

prepare_futures_forecasting_symbol_data <- function(
    data,
    symbol
) {
  
  validate_futures_forecasting_data(
    data = data,
    require_all_symbols = FALSE
  )
  
  symbol_data <-
    data |>
    filter(
      Symbol ==
        symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  if (
    nrow(
      symbol_data
    ) == 0L
  ) {
    stop(
      paste0(
        "No observations available for symbol ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  symbol_data <-
    add_futures_model_features(
      symbol_data
    )
  
  symbol_data <-
    symbol_data |>
    arrange(
      CompetitionTimestampCT
    )
  
  duplicate_timestamps <-
    anyDuplicated(
      symbol_data$CompetitionTimestampCT
    )
  
  if (
    duplicate_timestamps > 0L
  ) {
    stop(
      paste0(
        "Duplicate timestamps remain after preparing ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  symbol_data
}


# ============================================================
# 7. Expanding-origin index construction
# ============================================================

get_futures_expanding_origin_indices <- function(
    data,
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS
) {
  
  min_training_observations <-
    as.integer(
      min_training_observations
    )
  
  if (
    min_training_observations < 1L
  ) {
    stop(
      "min_training_observations must be at least 1.",
      call. = FALSE
    )
  }
  
  n_observations <-
    nrow(
      data
    )
  
  if (
    n_observations <=
    min_training_observations
  ) {
    stop(
      paste0(
        "Not enough observations for expanding-origin ",
        "forecasting. N = ",
        n_observations,
        ", minimum training observations = ",
        min_training_observations,
        "."
      ),
      call. = FALSE
    )
  }
  
  seq.int(
    from =
      min_training_observations + 1L,
    to =
      n_observations
  )
}


# ============================================================
# 8. Check strict no-look-ahead split
# ============================================================

validate_futures_forecast_split <- function(
    training_data,
    forecast_row
) {
  
  if (
    nrow(
      forecast_row
    ) != 1L
  ) {
    stop(
      "forecast_row must contain exactly one row.",
      call. = FALSE
    )
  }
  
  if (
    nrow(
      training_data
    ) == 0L
  ) {
    stop(
      "training_data must contain at least one row.",
      call. = FALSE
    )
  }
  
  last_training_timestamp <-
    max(
      training_data$CompetitionTimestampCT
    )
  
  forecast_timestamp <-
    forecast_row$CompetitionTimestampCT[[1]]
  
  if (
    !(
      last_training_timestamp <
      forecast_timestamp
    )
  ) {
    stop(
      paste0(
        "Look-ahead validation failed: the final training ",
        "timestamp is not strictly before the forecast timestamp."
      ),
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 9. Fit one mean model safely
# ============================================================

safe_fit_futures_mean_model <- function(
    training_data,
    model_name,
    symbol = NA_character_,
    forecast_timestamp = as.POSIXct(NA)
) {
  
  tryCatch(
    {
      fit_futures_mean_model(
        data =
          training_data,
        model_name =
          model_name
      )
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage =
          "mean_fit",
        symbol =
          symbol,
        mean_model_name =
          model_name,
        forecast_timestamp =
          forecast_timestamp,
        message =
          conditionMessage(
            error_object
          )
      )
    }
  )
}


# ============================================================
# 10. Predict one conditional mean safely
# ============================================================

safe_predict_futures_mean_model <- function(
    model,
    forecast_row,
    symbol = NA_character_,
    model_name = NA_character_
) {
  
  forecast_timestamp <-
    forecast_row$CompetitionTimestampCT[[1]]
  
  if (
    is_futures_forecast_error(
      model
    )
  ) {
    return(
      model
    )
  }
  
  tryCatch(
    {
      
      prediction <-
        predict_futures_mean_model(
          model =
            model,
          new_data =
            forecast_row
        )
      
      if (
        length(
          prediction
        ) != 1L
      ) {
        stop(
          paste0(
            "Mean prediction did not return exactly one value."
          )
        )
      }
      
      prediction <-
        as.numeric(
          prediction[[1]]
        )
      
      if (
        !is.finite(
          prediction
        )
      ) {
        stop(
          "Mean prediction is nonfinite."
        )
      }
      
      prediction
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage =
          "mean_prediction",
        symbol =
          symbol,
        mean_model_name =
          model_name,
        forecast_timestamp =
          forecast_timestamp,
        message =
          conditionMessage(
            error_object
          )
      )
    }
  )
}


# ============================================================
# 11. Forecast one mean model at one origin
# ============================================================

forecast_one_futures_mean_origin <- function(
    symbol_data,
    forecast_index,
    mean_model_name
) {
  
  forecast_index <-
    as.integer(
      forecast_index
    )
  
  if (
    forecast_index <= 1L ||
    forecast_index >
    nrow(
      symbol_data
    )
  ) {
    stop(
      "forecast_index is outside the valid one-step-ahead range.",
      call. = FALSE
    )
  }
  
  training_data <-
    symbol_data[
      seq_len(
        forecast_index - 1L
      ),
      ,
      drop = FALSE
    ]
  
  forecast_row <-
    symbol_data[
      forecast_index,
      ,
      drop = FALSE
    ]
  
  validate_futures_forecast_split(
    training_data =
      training_data,
    forecast_row =
      forecast_row
  )
  
  symbol <-
    forecast_row$Symbol[[1]]
  
  forecast_timestamp <-
    forecast_row$CompetitionTimestampCT[[1]]
  
  fitted_model <-
    safe_fit_futures_mean_model(
      training_data =
        training_data,
      model_name =
        mean_model_name,
      symbol =
        symbol,
      forecast_timestamp =
        forecast_timestamp
    )
  
  if (
    is_futures_forecast_error(
      fitted_model
    )
  ) {
    
    return(
      tibble(
        Symbol =
          symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          nrow(
            training_data
          ),
        MeanModel =
          mean_model_name,
        ActualLogReturn =
          forecast_row$CandidateLogReturn[[1]],
        MeanForecast =
          NA_real_,
        MeanForecastError =
          NA_real_,
        MeanSquaredForecastError =
          NA_real_,
        MeanAbsoluteForecastError =
          NA_real_,
        FitSucceeded =
          FALSE,
        PredictionSucceeded =
          FALSE,
        ErrorStage =
          fitted_model$Stage,
        ErrorMessage =
          fitted_model$ErrorMessage
      )
    )
  }
  
  mean_forecast <-
    safe_predict_futures_mean_model(
      model =
        fitted_model,
      forecast_row =
        forecast_row,
      symbol =
        symbol,
      model_name =
        mean_model_name
    )
  
  if (
    is_futures_forecast_error(
      mean_forecast
    )
  ) {
    
    return(
      tibble(
        Symbol =
          symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          nrow(
            training_data
          ),
        MeanModel =
          mean_model_name,
        ActualLogReturn =
          forecast_row$CandidateLogReturn[[1]],
        MeanForecast =
          NA_real_,
        MeanForecastError =
          NA_real_,
        MeanSquaredForecastError =
          NA_real_,
        MeanAbsoluteForecastError =
          NA_real_,
        FitSucceeded =
          TRUE,
        PredictionSucceeded =
          FALSE,
        ErrorStage =
          mean_forecast$Stage,
        ErrorMessage =
          mean_forecast$ErrorMessage
      )
    )
  }
  
  actual_return <-
    forecast_row$CandidateLogReturn[[1]]
  
  forecast_error <-
    actual_return -
    mean_forecast
  
  tibble(
    Symbol =
      symbol,
    CompetitionTimestampCT =
      forecast_timestamp,
    ForecastIndex =
      forecast_index,
    TrainingObservations =
      nrow(
        training_data
      ),
    MeanModel =
      mean_model_name,
    ActualLogReturn =
      actual_return,
    MeanForecast =
      mean_forecast,
    MeanForecastError =
      forecast_error,
    MeanSquaredForecastError =
      forecast_error^2,
    MeanAbsoluteForecastError =
      abs(
        forecast_error
      ),
    FitSucceeded =
      TRUE,
    PredictionSucceeded =
      TRUE,
    ErrorStage =
      NA_character_,
    ErrorMessage =
      NA_character_
  )
}


# ============================================================
# 12. Expanding-origin mean forecasts for one model
# ============================================================

run_futures_mean_expanding_origin <- function(
    data,
    symbol,
    mean_model_name,
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  symbol_data <-
    prepare_futures_forecasting_symbol_data(
      data =
        data,
      symbol =
        symbol
    )
  
  forecast_indices <-
    get_futures_expanding_origin_indices(
      data =
        symbol_data,
      min_training_observations =
        min_training_observations
    )
  
  forecast_rows <-
    vector(
      mode = "list",
      length = length(
        forecast_indices
      )
    )
  
  for (
    iteration_index in
    seq_along(
      forecast_indices
    )
  ) {
    
    forecast_index <-
      forecast_indices[[iteration_index]]
    
    if (
      verbose &&
      (
        iteration_index == 1L ||
        iteration_index %%
        100L ==
        0L ||
        iteration_index ==
        length(
          forecast_indices
        )
      )
    ) {
      cat(
        "Mean OOS:",
        symbol,
        mean_model_name,
        iteration_index,
        "of",
        length(
          forecast_indices
        ),
        "\n"
      )
    }
    
    forecast_rows[[iteration_index]] <-
      forecast_one_futures_mean_origin(
        symbol_data =
          symbol_data,
        forecast_index =
          forecast_index,
        mean_model_name =
          mean_model_name
      )
  }
  
  bind_rows(
    forecast_rows
  )
}


# ============================================================
# 13. Run all mean candidates for one future
# ============================================================

run_all_futures_mean_expanding_origin <- function(
    data,
    symbol,
    mean_model_names =
      get_all_futures_mean_model_names(),
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  result_list <-
    vector(
      mode = "list",
      length = length(
        mean_model_names
      )
    )
  
  names(
    result_list
  ) <-
    mean_model_names
  
  for (
    mean_model_name in
    mean_model_names
  ) {
    
    if (
      verbose
    ) {
      cat(
        "\nStarting mean model:",
        symbol,
        mean_model_name,
        "\n"
      )
    }
    
    result_list[[mean_model_name]] <-
      run_futures_mean_expanding_origin(
        data =
          data,
        symbol =
          symbol,
        mean_model_name =
          mean_model_name,
        min_training_observations =
          min_training_observations,
        verbose =
          verbose
      )
  }
  
  bind_rows(
    result_list
  )
}


# ============================================================
# 14. Run mean OOS forecasts across all futures
# ============================================================

run_all_futures_mean_oos <- function(
    data,
    symbols =
      FUTURES_MODEL_SYMBOLS,
    mean_model_names =
      get_all_futures_mean_model_names(),
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  output_list <-
    vector(
      mode = "list",
      length = length(
        symbols
      )
    )
  
  names(
    output_list
  ) <-
    symbols
  
  for (
    symbol in
    symbols
  ) {
    
    if (
      verbose
    ) {
      cat(
        "\n========================================\n"
      )
      
      cat(
        "Starting all mean OOS forecasts for",
        symbol,
        "\n"
      )
      
      cat(
        "========================================\n"
      )
    }
    
    output_list[[symbol]] <-
      run_all_futures_mean_expanding_origin(
        data =
          data,
        symbol =
          symbol,
        mean_model_names =
          mean_model_names,
        min_training_observations =
          min_training_observations,
        verbose =
          verbose
      )
  }
  
  bind_rows(
    output_list
  )
}


# ============================================================
# 15. Mean OOS evaluation summary
# ============================================================

summarize_futures_mean_oos <- function(
    mean_oos_forecasts
) {
  
  mean_oos_forecasts |>
    group_by(
      Symbol,
      MeanModel
    ) |>
    summarise(
      ForecastAttempts =
        n(),
      SuccessfulForecasts =
        sum(
          PredictionSucceeded,
          na.rm = TRUE
        ),
      FailedForecasts =
        sum(
          !PredictionSucceeded,
          na.rm = TRUE
        ),
      SuccessRate =
        mean(
          PredictionSucceeded,
          na.rm = TRUE
        ),
      OOSMeanError =
        mean(
          MeanForecastError[
            PredictionSucceeded
          ],
          na.rm = TRUE
        ),
      OOSRMSE =
        sqrt(
          mean(
            MeanSquaredForecastError[
              PredictionSucceeded
            ],
            na.rm = TRUE
          )
        ),
      OOSMAE =
        mean(
          MeanAbsoluteForecastError[
            PredictionSucceeded
          ],
          na.rm = TRUE
        ),
      FirstForecast =
        min(
          CompetitionTimestampCT[
            PredictionSucceeded
          ],
          na.rm = TRUE
        ),
      LastForecast =
        max(
          CompetitionTimestampCT[
            PredictionSucceeded
          ],
          na.rm = TRUE
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 16. Construct full-sample residuals for a training window
# ============================================================

build_futures_training_residual_data <- function(
    training_data,
    fitted_mean_model
) {
  
  if (
    is_futures_forecast_error(
      fitted_mean_model
    )
  ) {
    stop(
      "Cannot construct residuals from a failed mean model.",
      call. = FALSE
    )
  }
  
  residual_data <-
    attach_futures_mean_residuals(
      data =
        training_data,
      model =
        fitted_mean_model
    )
  
  residual_data <-
    residual_data |>
    filter(
      is.finite(
        MeanResidual
      ),
      is.finite(
        SquaredMeanResidual
      )
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  if (
    nrow(
      residual_data
    ) == 0L
  ) {
    stop(
      "No finite training residuals remain.",
      call. = FALSE
    )
  }
  
  residual_data
}


# ============================================================
# 17. Build lagged residual predictors for one forecast row
# ============================================================

add_futures_forecast_residual_lags <- function(
    forecast_row,
    training_residual_data
) {
  
  training_residual_data <-
    training_residual_data |>
    arrange(
      CompetitionTimestampCT
    )
  
  n_residuals <-
    nrow(
      training_residual_data
    )
  
  lag_residual_1 <-
    if (
      n_residuals >= 1L
    ) {
      training_residual_data$MeanResidual[[n_residuals]]
    } else {
      NA_real_
    }
  
  lag_residual_2 <-
    if (
      n_residuals >= 2L
    ) {
      training_residual_data$MeanResidual[[n_residuals - 1L]]
    } else {
      NA_real_
    }
  
  forecast_row$LagAbsoluteResidual1 <-
    if (
      is.finite(
        lag_residual_1
      )
    ) {
      abs(
        lag_residual_1
      )
    } else {
      NA_real_
    }
  
  forecast_row$LagSquaredResidual1 <-
    if (
      is.finite(
        lag_residual_1
      )
    ) {
      lag_residual_1^2
    } else {
      NA_real_
    }
  
  forecast_row$LagSquaredResidual2 <-
    if (
      is.finite(
        lag_residual_2
      )
    ) {
      lag_residual_2^2
    } else {
      NA_real_
    }
  
  forecast_row
}


# ============================================================
# 18. Extract one variance model from an all-candidate fit
# ============================================================

extract_futures_variance_candidate <- function(
    fitted_variance_candidates,
    variance_model_name
) {
  
  if (
    is.null(
      fitted_variance_candidates[[variance_model_name]]
    )
  ) {
    stop(
      paste0(
        "Variance model ",
        variance_model_name,
        " was not returned by ",
        "fit_all_futures_variance_candidates()."
      ),
      call. = FALSE
    )
  }
  
  fitted_variance_candidates[[variance_model_name]]
}


# ============================================================
# 19. Fit one variance candidate safely
# ============================================================

safe_fit_futures_variance_model <- function(
    training_residual_data,
    variance_model_name,
    symbol = NA_character_,
    mean_model_name = NA_character_,
    forecast_timestamp = as.POSIXct(NA)
) {
  
  tryCatch(
    {
      
      fitted_variance_model <-
        fit_futures_variance_model(
          data =
            training_residual_data,
          model_name =
            variance_model_name
        )
      
      if (
        inherits(
          fitted_variance_model,
          "futures_model_error"
        )
      ) {
        stop(
          fitted_variance_model$ErrorMessage
        )
      }
      
      fitted_variance_model
      
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage =
          "variance_fit",
        symbol =
          symbol,
        mean_model_name =
          mean_model_name,
        variance_model_name =
          variance_model_name,
        forecast_timestamp =
          forecast_timestamp,
        message =
          conditionMessage(
            error_object
          )
      )
    }
  )
}

# ============================================================
# 20. Predict one variance safely
# ============================================================

safe_predict_futures_variance_model <- function(
    variance_model,
    forecast_row,
    symbol = NA_character_,
    mean_model_name = NA_character_,
    variance_model_name = NA_character_
) {
  
  forecast_timestamp <-
    forecast_row$CompetitionTimestampCT[[1]]
  
  if (
    is_futures_forecast_error(
      variance_model
    )
  ) {
    return(
      variance_model
    )
  }
  
  tryCatch(
    {
      
      prediction_data <-
        forecast_row
      
      prediction_data$MeanResidual <-
        0
      
      prediction_data$SquaredMeanResidual <-
        0
      
      predicted_data <-
        attach_futures_variance_predictions(
          data =
            prediction_data,
          model =
            variance_model
        )
      
      if (
        !(
          "ConditionalVariance" %in%
          names(
            predicted_data
          )
        )
      ) {
        stop(
          paste0(
            "attach_futures_variance_predictions() did not ",
            "return ConditionalVariance."
          )
        )
      }
      
      variance_forecast <-
        as.numeric(
          predicted_data$ConditionalVariance[[1]]
        )
      
      if (
        !is.finite(
          variance_forecast
        )
      ) {
        stop(
          "Variance forecast is nonfinite."
        )
      }
      
      if (
        variance_forecast <=
        0
      ) {
        stop(
          "Variance forecast is not strictly positive."
        )
      }
      
      variance_forecast
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage =
          "variance_prediction",
        symbol =
          symbol,
        mean_model_name =
          mean_model_name,
        variance_model_name =
          variance_model_name,
        forecast_timestamp =
          forecast_timestamp,
        message =
          conditionMessage(
            error_object
          )
      )
    }
  )
}


# ============================================================
# 21. Forecast one mean-variance pair at one origin
# ============================================================

forecast_one_futures_mean_variance_origin <- function(
    symbol_data,
    forecast_index,
    mean_model_name,
    variance_model_name
) {
  
  forecast_index <-
    as.integer(
      forecast_index
    )
  
  if (
    forecast_index <= 2L ||
    forecast_index >
    nrow(
      symbol_data
    )
  ) {
    stop(
      "forecast_index is outside the valid range.",
      call. = FALSE
    )
  }
  
  training_data <-
    symbol_data[
      seq_len(
        forecast_index - 1L
      ),
      ,
      drop = FALSE
    ]
  
  forecast_row <-
    symbol_data[
      forecast_index,
      ,
      drop = FALSE
    ]
  
  validate_futures_forecast_split(
    training_data =
      training_data,
    forecast_row =
      forecast_row
  )
  
  symbol <-
    forecast_row$Symbol[[1]]
  
  forecast_timestamp <-
    forecast_row$CompetitionTimestampCT[[1]]
  
  actual_return <-
    forecast_row$CandidateLogReturn[[1]]
  
  fitted_mean_model <-
    safe_fit_futures_mean_model(
      training_data =
        training_data,
      model_name =
        mean_model_name,
      symbol =
        symbol,
      forecast_timestamp =
        forecast_timestamp
    )
  
  if (
    is_futures_forecast_error(
      fitted_mean_model
    )
  ) {
    
    return(
      tibble(
        Symbol =
          symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          nrow(
            training_data
          ),
        MeanModel =
          mean_model_name,
        VarianceModel =
          variance_model_name,
        ActualLogReturn =
          actual_return,
        MeanForecast =
          NA_real_,
        VarianceForecast =
          NA_real_,
        VolatilityForecast =
          NA_real_,
        MeanForecastError =
          NA_real_,
        SquaredMeanForecastError =
          NA_real_,
        VarianceForecastError =
          NA_real_,
        VarianceSquaredForecastError =
          NA_real_,
        VarianceAbsoluteForecastError =
          NA_real_,
        StandardizedInnovation =
          NA_real_,
        MeanFitSucceeded =
          FALSE,
        MeanPredictionSucceeded =
          FALSE,
        VarianceFitSucceeded =
          FALSE,
        VariancePredictionSucceeded =
          FALSE,
        ErrorStage =
          fitted_mean_model$Stage,
        ErrorMessage =
          fitted_mean_model$ErrorMessage
      )
    )
  }
  
  mean_forecast <-
    safe_predict_futures_mean_model(
      model =
        fitted_mean_model,
      forecast_row =
        forecast_row,
      symbol =
        symbol,
      model_name =
        mean_model_name
    )
  
  if (
    is_futures_forecast_error(
      mean_forecast
    )
  ) {
    
    return(
      tibble(
        Symbol =
          symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          nrow(
            training_data
          ),
        MeanModel =
          mean_model_name,
        VarianceModel =
          variance_model_name,
        ActualLogReturn =
          actual_return,
        MeanForecast =
          NA_real_,
        VarianceForecast =
          NA_real_,
        VolatilityForecast =
          NA_real_,
        MeanForecastError =
          NA_real_,
        SquaredMeanForecastError =
          NA_real_,
        VarianceForecastError =
          NA_real_,
        VarianceSquaredForecastError =
          NA_real_,
        VarianceAbsoluteForecastError =
          NA_real_,
        StandardizedInnovation =
          NA_real_,
        MeanFitSucceeded =
          TRUE,
        MeanPredictionSucceeded =
          FALSE,
        VarianceFitSucceeded =
          FALSE,
        VariancePredictionSucceeded =
          FALSE,
        ErrorStage =
          mean_forecast$Stage,
        ErrorMessage =
          mean_forecast$ErrorMessage
      )
    )
  }
  
  training_residual_data <-
    tryCatch(
      {
        build_futures_training_residual_data(
          training_data =
            training_data,
          fitted_mean_model =
            fitted_mean_model
        )
      },
      error = function(
    error_object
      ) {
        make_futures_forecast_error(
          stage =
            "residual_construction",
          symbol =
            symbol,
          mean_model_name =
            mean_model_name,
          variance_model_name =
            variance_model_name,
          forecast_timestamp =
            forecast_timestamp,
          message =
            conditionMessage(
              error_object
            )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      training_residual_data
    )
  ) {
    
    return(
      tibble(
        Symbol =
          symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          nrow(
            training_data
          ),
        MeanModel =
          mean_model_name,
        VarianceModel =
          variance_model_name,
        ActualLogReturn =
          actual_return,
        MeanForecast =
          mean_forecast,
        VarianceForecast =
          NA_real_,
        VolatilityForecast =
          NA_real_,
        MeanForecastError =
          actual_return -
          mean_forecast,
        SquaredMeanForecastError =
          (
            actual_return -
              mean_forecast
          )^2,
        VarianceForecastError =
          NA_real_,
        VarianceSquaredForecastError =
          NA_real_,
        VarianceAbsoluteForecastError =
          NA_real_,
        StandardizedInnovation =
          NA_real_,
        MeanFitSucceeded =
          TRUE,
        MeanPredictionSucceeded =
          TRUE,
        VarianceFitSucceeded =
          FALSE,
        VariancePredictionSucceeded =
          FALSE,
        ErrorStage =
          training_residual_data$Stage,
        ErrorMessage =
          training_residual_data$ErrorMessage
      )
    )
  }
  
  variance_forecast_row <-
    add_futures_forecast_residual_lags(
      forecast_row =
        forecast_row,
      training_residual_data =
        training_residual_data
    )
  
  fitted_variance_model <-
    safe_fit_futures_variance_model(
      training_residual_data =
        training_residual_data,
      variance_model_name =
        variance_model_name,
      symbol =
        symbol,
      mean_model_name =
        mean_model_name,
      forecast_timestamp =
        forecast_timestamp
    )
  
  if (
    is_futures_forecast_error(
      fitted_variance_model
    )
  ) {
    
    mean_error <-
      actual_return -
      mean_forecast
    
    return(
      tibble(
        Symbol =
          symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          nrow(
            training_data
          ),
        MeanModel =
          mean_model_name,
        VarianceModel =
          variance_model_name,
        ActualLogReturn =
          actual_return,
        MeanForecast =
          mean_forecast,
        VarianceForecast =
          NA_real_,
        VolatilityForecast =
          NA_real_,
        MeanForecastError =
          mean_error,
        SquaredMeanForecastError =
          mean_error^2,
        VarianceForecastError =
          NA_real_,
        VarianceSquaredForecastError =
          NA_real_,
        VarianceAbsoluteForecastError =
          NA_real_,
        StandardizedInnovation =
          NA_real_,
        MeanFitSucceeded =
          TRUE,
        MeanPredictionSucceeded =
          TRUE,
        VarianceFitSucceeded =
          FALSE,
        VariancePredictionSucceeded =
          FALSE,
        ErrorStage =
          fitted_variance_model$Stage,
        ErrorMessage =
          fitted_variance_model$ErrorMessage
      )
    )
  }
  
  variance_forecast <-
    safe_predict_futures_variance_model(
      variance_model =
        fitted_variance_model,
      forecast_row =
        variance_forecast_row,
      symbol =
        symbol,
      mean_model_name =
        mean_model_name,
      variance_model_name =
        variance_model_name
    )
  
  if (
    is_futures_forecast_error(
      variance_forecast
    )
  ) {
    
    mean_error <-
      actual_return -
      mean_forecast
    
    return(
      tibble(
        Symbol =
          symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          nrow(
            training_data
          ),
        MeanModel =
          mean_model_name,
        VarianceModel =
          variance_model_name,
        ActualLogReturn =
          actual_return,
        MeanForecast =
          mean_forecast,
        VarianceForecast =
          NA_real_,
        VolatilityForecast =
          NA_real_,
        MeanForecastError =
          mean_error,
        SquaredMeanForecastError =
          mean_error^2,
        VarianceForecastError =
          NA_real_,
        VarianceSquaredForecastError =
          NA_real_,
        VarianceAbsoluteForecastError =
          NA_real_,
        StandardizedInnovation =
          NA_real_,
        MeanFitSucceeded =
          TRUE,
        MeanPredictionSucceeded =
          TRUE,
        VarianceFitSucceeded =
          TRUE,
        VariancePredictionSucceeded =
          FALSE,
        ErrorStage =
          variance_forecast$Stage,
        ErrorMessage =
          variance_forecast$ErrorMessage
      )
    )
  }
  
  mean_error <-
    actual_return -
    mean_forecast
  
  realized_squared_innovation <-
    mean_error^2
  
  variance_error <-
    realized_squared_innovation -
    variance_forecast
  
  volatility_forecast <-
    sqrt(
      variance_forecast
    )
  
  standardized_innovation <-
    mean_error /
    volatility_forecast
  
  tibble(
    Symbol =
      symbol,
    CompetitionTimestampCT =
      forecast_timestamp,
    ForecastIndex =
      forecast_index,
    TrainingObservations =
      nrow(
        training_data
      ),
    MeanModel =
      mean_model_name,
    VarianceModel =
      variance_model_name,
    ActualLogReturn =
      actual_return,
    MeanForecast =
      mean_forecast,
    VarianceForecast =
      variance_forecast,
    VolatilityForecast =
      volatility_forecast,
    MeanForecastError =
      mean_error,
    SquaredMeanForecastError =
      realized_squared_innovation,
    VarianceForecastError =
      variance_error,
    VarianceSquaredForecastError =
      variance_error^2,
    VarianceAbsoluteForecastError =
      abs(
        variance_error
      ),
    StandardizedInnovation =
      standardized_innovation,
    MeanFitSucceeded =
      TRUE,
    MeanPredictionSucceeded =
      TRUE,
    VarianceFitSucceeded =
      TRUE,
    VariancePredictionSucceeded =
      TRUE,
    ErrorStage =
      NA_character_,
    ErrorMessage =
      NA_character_
  )
}


# ============================================================
# 22. Run one mean-variance pair over expanding origins
# ============================================================

run_futures_mean_variance_expanding_origin <- function(
    data,
    symbol,
    mean_model_name,
    variance_model_name,
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  symbol_data <-
    prepare_futures_forecasting_symbol_data(
      data =
        data,
      symbol =
        symbol
    )
  
  forecast_indices <-
    get_futures_expanding_origin_indices(
      data =
        symbol_data,
      min_training_observations =
        min_training_observations
    )
  
  result_rows <-
    vector(
      mode = "list",
      length = length(
        forecast_indices
      )
    )
  
  for (
    iteration_index in
    seq_along(
      forecast_indices
    )
  ) {
    
    forecast_index <-
      forecast_indices[[iteration_index]]
    
    if (
      verbose &&
      (
        iteration_index == 1L ||
        iteration_index %%
        100L ==
        0L ||
        iteration_index ==
        length(
          forecast_indices
        )
      )
    ) {
      cat(
        "Mean-variance OOS:",
        symbol,
        mean_model_name,
        variance_model_name,
        iteration_index,
        "of",
        length(
          forecast_indices
        ),
        "\n"
      )
    }
    
    result_rows[[iteration_index]] <-
      forecast_one_futures_mean_variance_origin(
        symbol_data =
          symbol_data,
        forecast_index =
          forecast_index,
        mean_model_name =
          mean_model_name,
        variance_model_name =
          variance_model_name
      )
  }
  
  bind_rows(
    result_rows
  )
}


# ============================================================
# 23. Run variance candidates for one representative mean
# ============================================================

run_all_futures_variance_oos_for_mean <- function(
    data,
    symbol,
    mean_model_name,
    variance_model_names =
      get_futures_variance_model_specs()$ModelName,
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  output_list <-
    vector(
      mode = "list",
      length = length(
        variance_model_names
      )
    )
  
  names(
    output_list
  ) <-
    variance_model_names
  
  for (
    variance_model_name in
    variance_model_names
  ) {
    
    if (
      verbose
    ) {
      cat(
        "\nStarting variance candidate:",
        symbol,
        mean_model_name,
        variance_model_name,
        "\n"
      )
    }
    
    output_list[[variance_model_name]] <-
      run_futures_mean_variance_expanding_origin(
        data =
          data,
        symbol =
          symbol,
        mean_model_name =
          mean_model_name,
        variance_model_name =
          variance_model_name,
        min_training_observations =
          min_training_observations,
        verbose =
          verbose
      )
  }
  
  bind_rows(
    output_list
  )
}


# ============================================================
# 24. Run representative mean-variance OOS system
# ============================================================

run_futures_representative_mean_variance_oos <- function(
    data,
    symbols =
      FUTURES_MODEL_SYMBOLS,
    mean_model_names =
      DEFAULT_FUTURES_REPRESENTATIVE_MEAN_MODELS,
    variance_model_names =
      get_futures_variance_model_specs()$ModelName,
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  output_rows <-
    list()
  
  result_index <-
    0L
  
  for (
    symbol in
    symbols
  ) {
    
    for (
      mean_model_name in
      mean_model_names
    ) {
      
      if (
        verbose
      ) {
        cat(
          "\n========================================\n"
        )
        
        cat(
          "Variance OOS system:",
          symbol,
          mean_model_name,
          "\n"
        )
        
        cat(
          "========================================\n"
        )
      }
      
      current_result <-
        tryCatch(
          {
            run_all_futures_variance_oos_for_mean(
              data =
                data,
              symbol =
                symbol,
              mean_model_name =
                mean_model_name,
              variance_model_names =
                variance_model_names,
              min_training_observations =
                min_training_observations,
              verbose =
                verbose
            )
          },
          error = function(
    error_object
          ) {
            
            tibble(
              Symbol =
                symbol,
              CompetitionTimestampCT =
                as.POSIXct(
                  NA
                ),
              ForecastIndex =
                NA_integer_,
              TrainingObservations =
                NA_integer_,
              MeanModel =
                mean_model_name,
              VarianceModel =
                NA_character_,
              ActualLogReturn =
                NA_real_,
              MeanForecast =
                NA_real_,
              VarianceForecast =
                NA_real_,
              VolatilityForecast =
                NA_real_,
              MeanForecastError =
                NA_real_,
              SquaredMeanForecastError =
                NA_real_,
              VarianceForecastError =
                NA_real_,
              VarianceSquaredForecastError =
                NA_real_,
              VarianceAbsoluteForecastError =
                NA_real_,
              StandardizedInnovation =
                NA_real_,
              MeanFitSucceeded =
                FALSE,
              MeanPredictionSucceeded =
                FALSE,
              VarianceFitSucceeded =
                FALSE,
              VariancePredictionSucceeded =
                FALSE,
              ErrorStage =
                "mean_variance_system",
              ErrorMessage =
                conditionMessage(
                  error_object
                )
            )
          }
        )
      
      result_index <-
        result_index + 1L
      
      output_rows[[result_index]] <-
        current_result
    }
  }
  
  bind_rows(
    output_rows
  )
}


# ============================================================
# 25. Variance OOS evaluation summary
# ============================================================

summarize_futures_variance_oos <- function(
    variance_oos_forecasts
) {
  
  variance_oos_forecasts |>
    group_by(
      Symbol,
      MeanModel,
      VarianceModel
    ) |>
    summarise(
      ForecastAttempts =
        n(),
      SuccessfulForecasts =
        sum(
          VariancePredictionSucceeded,
          na.rm = TRUE
        ),
      FailedForecasts =
        sum(
          !VariancePredictionSucceeded,
          na.rm = TRUE
        ),
      SuccessRate =
        mean(
          VariancePredictionSucceeded,
          na.rm = TRUE
        ),
      OOSVarianceRMSE =
        sqrt(
          mean(
            VarianceSquaredForecastError[
              VariancePredictionSucceeded
            ],
            na.rm = TRUE
          )
        ),
      OOSVarianceMAE =
        mean(
          VarianceAbsoluteForecastError[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      MeanVarianceForecast =
        mean(
          VarianceForecast[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      MeanRealizedSquaredInnovation =
        mean(
          SquaredMeanForecastError[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      MeanStandardizedInnovation =
        mean(
          StandardizedInnovation[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      SDStandardizedInnovation =
        sd(
          StandardizedInnovation[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 26. QLIKE variance loss
# ============================================================

futures_qlike_loss <- function(
    realized_squared_innovation,
    variance_forecast,
    epsilon =
      FUTURES_FORECASTING_EPSILON
) {
  
  realized_squared_innovation <-
    pmax(
      as.numeric(
        realized_squared_innovation
      ),
      epsilon
    )
  
  variance_forecast <-
    pmax(
      as.numeric(
        variance_forecast
      ),
      epsilon
    )
  
  log(
    variance_forecast
  ) +
    realized_squared_innovation /
    variance_forecast
}


# ============================================================
# 27. Add QLIKE to OOS variance results
# ============================================================

add_futures_variance_qlike <- function(
    variance_oos_forecasts
) {
  
  variance_oos_forecasts |>
    mutate(
      QLIKE =
        if_else(
          VariancePredictionSucceeded &
            is.finite(
              SquaredMeanForecastError
            ) &
            is.finite(
              VarianceForecast
            ) &
            VarianceForecast >
            0,
          futures_qlike_loss(
            realized_squared_innovation =
              SquaredMeanForecastError,
            variance_forecast =
              VarianceForecast
          ),
          NA_real_
        )
    )
}


# ============================================================
# 28. Full variance OOS evaluation with QLIKE
# ============================================================

summarize_futures_variance_oos_with_qlike <- function(
    variance_oos_forecasts
) {
  
  evaluated_data <-
    add_futures_variance_qlike(
      variance_oos_forecasts
    )
  
  evaluated_data |>
    group_by(
      Symbol,
      MeanModel,
      VarianceModel
    ) |>
    summarise(
      ForecastAttempts =
        n(),
      SuccessfulForecasts =
        sum(
          VariancePredictionSucceeded,
          na.rm = TRUE
        ),
      FailedForecasts =
        sum(
          !VariancePredictionSucceeded,
          na.rm = TRUE
        ),
      SuccessRate =
        mean(
          VariancePredictionSucceeded,
          na.rm = TRUE
        ),
      OOSVarianceRMSE =
        sqrt(
          mean(
            VarianceSquaredForecastError[
              VariancePredictionSucceeded
            ],
            na.rm = TRUE
          )
        ),
      OOSVarianceMAE =
        mean(
          VarianceAbsoluteForecastError[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      OOSQLIKE =
        mean(
          QLIKE[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      MeanVarianceForecast =
        mean(
          VarianceForecast[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      MeanRealizedSquaredInnovation =
        mean(
          SquaredMeanForecastError[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      MeanStandardizedInnovation =
        mean(
          StandardizedInnovation[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      SDStandardizedInnovation =
        sd(
          StandardizedInnovation[
            VariancePredictionSucceeded
          ],
          na.rm = TRUE
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 29. Validate OOS mean forecast table
# ============================================================

validate_futures_mean_oos_results <- function(
    mean_oos_forecasts
) {
  
  required_columns <- c(
    "Symbol",
    "CompetitionTimestampCT",
    "MeanModel",
    "ActualLogReturn",
    "MeanForecast",
    "MeanForecastError",
    "PredictionSucceeded"
  )
  
  missing_columns <-
    setdiff(
      required_columns,
      names(
        mean_oos_forecasts
      )
    )
  
  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      paste0(
        "Mean OOS results are missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  successful_rows <-
    mean_oos_forecasts |>
    filter(
      PredictionSucceeded
    )
  
  if (
    nrow(
      successful_rows
    ) == 0L
  ) {
    stop(
      "Mean OOS results contain no successful forecasts.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        successful_rows$MeanForecast
      )
    )
  ) {
    stop(
      "Successful mean OOS rows contain nonfinite forecasts.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        successful_rows$MeanForecastError
      )
    )
  ) {
    stop(
      "Successful mean OOS rows contain nonfinite forecast errors.",
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 30. Validate OOS variance forecast table
# ============================================================

validate_futures_variance_oos_results <- function(
    variance_oos_forecasts
) {
  
  required_columns <- c(
    "Symbol",
    "CompetitionTimestampCT",
    "MeanModel",
    "VarianceModel",
    "ActualLogReturn",
    "MeanForecast",
    "VarianceForecast",
    "StandardizedInnovation",
    "VariancePredictionSucceeded"
  )
  
  missing_columns <-
    setdiff(
      required_columns,
      names(
        variance_oos_forecasts
      )
    )
  
  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      paste0(
        "Variance OOS results are missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  successful_rows <-
    variance_oos_forecasts |>
    filter(
      VariancePredictionSucceeded
    )
  
  if (
    nrow(
      successful_rows
    ) == 0L
  ) {
    stop(
      "Variance OOS results contain no successful forecasts.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        successful_rows$MeanForecast
      )
    )
  ) {
    stop(
      "Successful variance OOS rows contain nonfinite mean forecasts.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        successful_rows$VarianceForecast
      )
    )
  ) {
    stop(
      "Successful variance OOS rows contain nonfinite variance forecasts.",
      call. = FALSE
    )
  }
  
  if (
    any(
      successful_rows$VarianceForecast <=
      0
    )
  ) {
    stop(
      "Successful variance OOS rows contain nonpositive variances.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        successful_rows$StandardizedInnovation
      )
    )
  ) {
    stop(
      paste0(
        "Successful variance OOS rows contain nonfinite ",
        "standardized innovations."
      ),
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 31. Align one selected OOS innovation series per future
# ============================================================

align_futures_standardized_oos_innovations <- function(
    variance_oos_forecasts,
    model_selection
) {
  
  required_selection_columns <- c(
    "Symbol",
    "MeanModel",
    "VarianceModel"
  )
  
  missing_selection_columns <-
    setdiff(
      required_selection_columns,
      names(
        model_selection
      )
    )
  
  if (
    length(
      missing_selection_columns
    ) > 0L
  ) {
    stop(
      paste0(
        "model_selection is missing columns: ",
        paste(
          missing_selection_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (
    anyDuplicated(
      model_selection$Symbol
    ) > 0L
  ) {
    stop(
      "model_selection must contain one row per Symbol.",
      call. = FALSE
    )
  }
  
  selected_innovations <-
    variance_oos_forecasts |>
    inner_join(
      model_selection,
      by = c(
        "Symbol",
        "MeanModel",
        "VarianceModel"
      )
    ) |>
    filter(
      VariancePredictionSucceeded,
      is.finite(
        StandardizedInnovation
      )
    ) |>
    select(
      CompetitionTimestampCT,
      Symbol,
      StandardizedInnovation
    )
  
  duplicate_rows <-
    selected_innovations |>
    count(
      CompetitionTimestampCT,
      Symbol,
      name = "N"
    ) |>
    filter(
      N > 1L
    )
  
  if (
    nrow(
      duplicate_rows
    ) > 0L
  ) {
    stop(
      paste0(
        "Selected OOS innovations contain duplicate ",
        "timestamp/symbol observations."
      ),
      call. = FALSE
    )
  }
  
  aligned_wide <-
    selected_innovations |>
    pivot_wider(
      names_from =
        Symbol,
      values_from =
        StandardizedInnovation
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  required_symbols <-
    intersect(
      FUTURES_MODEL_SYMBOLS,
      model_selection$Symbol
    )
  
  aligned_complete <-
    aligned_wide |>
    filter(
      if_all(
        all_of(
          required_symbols
        ),
        ~ is.finite(
          .x
        )
      )
    )
  
  aligned_complete
}


# ============================================================
# 32. Long-form aligned OOS innovations
# ============================================================

make_futures_standardized_oos_innovations_long <- function(
    aligned_wide
) {
  
  aligned_wide |>
    pivot_longer(
      cols =
        -CompetitionTimestampCT,
      names_to =
        "Symbol",
      values_to =
        "StandardizedInnovation"
    ) |>
    arrange(
      CompetitionTimestampCT,
      Symbol
    )
}


# ============================================================
# 33. OOS correlation matrix
# ============================================================

estimate_futures_oos_correlation <- function(
    aligned_wide,
    symbols =
      FUTURES_MODEL_SYMBOLS
) {
  
  available_symbols <-
    intersect(
      symbols,
      names(
        aligned_wide
      )
    )
  
  if (
    length(
      available_symbols
    ) < 2L
  ) {
    stop(
      "At least two aligned futures are required.",
      call. = FALSE
    )
  }
  
  correlation_matrix <-
    stats::cor(
      as.matrix(
        aligned_wide[
          ,
          available_symbols,
          drop = FALSE
        ]
      ),
      use =
        "complete.obs"
    )
  
  correlation_matrix
}


# ============================================================
# 34. Correlation-matrix validation
# ============================================================

validate_futures_correlation_matrix <- function(
    correlation_matrix,
    tolerance = 1e-8
) {
  
  if (
    !is.matrix(
      correlation_matrix
    )
  ) {
    stop(
      "correlation_matrix must be a matrix.",
      call. = FALSE
    )
  }
  
  if (
    nrow(
      correlation_matrix
    ) !=
    ncol(
      correlation_matrix
    )
  ) {
    stop(
      "Correlation matrix must be square.",
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
      "Correlation matrix contains nonfinite values.",
      call. = FALSE
    )
  }
  
  if (
    max(
      abs(
        correlation_matrix -
        t(
          correlation_matrix
        )
      )
    ) >
    tolerance
  ) {
    stop(
      "Correlation matrix is not symmetric.",
      call. = FALSE
    )
  }
  
  if (
    max(
      abs(
        diag(
          correlation_matrix
        ) -
        1
      )
    ) >
    tolerance
  ) {
    stop(
      "Correlation matrix diagonal is not equal to one.",
      call. = FALSE
    )
  }
  
  eigenvalues <-
    eigen(
      correlation_matrix,
      symmetric = TRUE,
      only.values = TRUE
    )$values
  
  list(
    IsSymmetric =
      TRUE,
    UnitDiagonal =
      TRUE,
    MinimumEigenvalue =
      min(
        eigenvalues
      ),
    PositiveDefinite =
      all(
        eigenvalues >
          tolerance
      ),
    PositiveSemidefinite =
      all(
        eigenvalues >=
          -tolerance
      ),
    Eigenvalues =
      eigenvalues
  )
}


# ============================================================
# 35. Forecast failure summary
# ============================================================

summarize_futures_forecast_failures <- function(
    forecast_results
) {
  
  if (
    !(
      "ErrorStage" %in%
      names(
        forecast_results
      )
    )
  ) {
    stop(
      "forecast_results do not contain ErrorStage.",
      call. = FALSE
    )
  }
  
  forecast_results |>
    filter(
      !is.na(
        ErrorStage
      )
    ) |>
    count(
      Symbol,
      ErrorStage,
      ErrorMessage,
      sort = TRUE,
      name = "Failures"
    )
}


# ============================================================
# 36. Forecast coverage summary
# ============================================================

summarize_futures_forecast_coverage <- function(
    forecast_results,
    success_column
) {
  
  if (
    !(
      success_column %in%
      names(
        forecast_results
      )
    )
  ) {
    stop(
      paste0(
        "Success column ",
        success_column,
        " is not present."
      ),
      call. = FALSE
    )
  }
  
  success_values <-
    forecast_results[[success_column]]
  
  forecast_results |>
    mutate(
      ForecastSucceeded =
        as.logical(
          success_values
        )
    ) |>
    group_by(
      Symbol
    ) |>
    summarise(
      ForecastAttempts =
        n(),
      SuccessfulForecasts =
        sum(
          ForecastSucceeded,
          na.rm = TRUE
        ),
      FailedForecasts =
        sum(
          !ForecastSucceeded,
          na.rm = TRUE
        ),
      SuccessRate =
        mean(
          ForecastSucceeded,
          na.rm = TRUE
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 37. Compare mean model against constant benchmark
# ============================================================

add_futures_mean_oos_benchmark_comparison <- function(
    mean_oos_summary
) {
  
  constant_benchmark <-
    mean_oos_summary |>
    filter(
      MeanModel ==
        "constant"
    ) |>
    select(
      Symbol,
      ConstantOOSRMSE =
        OOSRMSE,
      ConstantOOSMAE =
        OOSMAE
    )
  
  mean_oos_summary |>
    left_join(
      constant_benchmark,
      by =
        "Symbol"
    ) |>
    mutate(
      OOSRMSEImprovementPercent =
        100 *
        (
          ConstantOOSRMSE -
            OOSRMSE
        ) /
        ConstantOOSRMSE,
      OOSMAEImprovementPercent =
        100 *
        (
          ConstantOOSMAE -
            OOSMAE
        ) /
        ConstantOOSMAE
    )
}


# ============================================================
# 38. Compare variance model against constant benchmark
# ============================================================

add_futures_variance_oos_benchmark_comparison <- function(
    variance_oos_summary
) {
  
  constant_benchmark <-
    variance_oos_summary |>
    filter(
      VarianceModel ==
        "constant"
    ) |>
    select(
      Symbol,
      MeanModel,
      ConstantOOSVarianceRMSE =
        OOSVarianceRMSE,
      ConstantOOSVarianceMAE =
        OOSVarianceMAE,
      ConstantOOSQLIKE =
        OOSQLIKE
    )
  
  variance_oos_summary |>
    left_join(
      constant_benchmark,
      by = c(
        "Symbol",
        "MeanModel"
      )
    ) |>
    mutate(
      OOSVarianceRMSEImprovementPercent =
        100 *
        (
          ConstantOOSVarianceRMSE -
            OOSVarianceRMSE
        ) /
        ConstantOOSVarianceRMSE,
      OOSVarianceMAEImprovementPercent =
        100 *
        (
          ConstantOOSVarianceMAE -
            OOSVarianceMAE
        ) /
        ConstantOOSVarianceMAE,
      OOSQLIKEImprovement =
        ConstantOOSQLIKE -
        OOSQLIKE
    )
}


# ============================================================
# 39. Rank OOS mean candidates
# ============================================================

rank_futures_mean_oos_models <- function(
    mean_oos_summary
) {
  
  mean_oos_summary |>
    group_by(
      Symbol
    ) |>
    mutate(
      RMSErank =
        rank(
          OOSRMSE,
          ties.method =
            "min",
          na.last =
            "keep"
        ),
      MAErank =
        rank(
          OOSMAE,
          ties.method =
            "min",
          na.last =
            "keep"
        ),
      MeanRank =
        rowMeans(
          cbind(
            RMSErank,
            MAErank
          ),
          na.rm = TRUE
        )
    ) |>
    ungroup() |>
    arrange(
      Symbol,
      MeanRank,
      OOSRMSE
    )
}


# ============================================================
# 40. Rank OOS variance candidates
# ============================================================

rank_futures_variance_oos_models <- function(
    variance_oos_summary
) {
  
  variance_oos_summary |>
    group_by(
      Symbol,
      MeanModel
    ) |>
    mutate(
      RMSErank =
        rank(
          OOSVarianceRMSE,
          ties.method =
            "min",
          na.last =
            "keep"
        ),
      MAErank =
        rank(
          OOSVarianceMAE,
          ties.method =
            "min",
          na.last =
            "keep"
        ),
      QLIKErank =
        rank(
          OOSQLIKE,
          ties.method =
            "min",
          na.last =
            "keep"
        ),
      MeanRank =
        rowMeans(
          cbind(
            RMSErank,
            MAErank,
            QLIKErank
          ),
          na.rm = TRUE
        )
    ) |>
    ungroup() |>
    arrange(
      Symbol,
      MeanModel,
      MeanRank,
      OOSQLIKE
    )
}


# ============================================================
# 41. Refit schedule helper
# ============================================================

should_refit_futures_model <- function(
    iteration_index,
    refit_every =
      DEFAULT_FUTURES_OOS_REFIT_EVERY
) {
  
  refit_every <-
    as.integer(
      refit_every
    )
  
  if (
    refit_every < 1L
  ) {
    stop(
      "refit_every must be at least 1.",
      call. = FALSE
    )
  }
  
  iteration_index == 1L ||
    (
      (
        iteration_index - 1L
      ) %%
        refit_every
    ) ==
    0L
}


# ============================================================
# 42. Strict chronological validation for OOS results
# ============================================================

validate_futures_oos_chronology <- function(
    forecast_results
) {
  
  relevant_rows <-
    forecast_results |>
    filter(
      !is.na(
        CompetitionTimestampCT
      ),
      !is.na(
        TrainingObservations
      )
    ) |>
    arrange(
      Symbol,
      CompetitionTimestampCT
    )
  
  if (
    nrow(
      relevant_rows
    ) == 0L
  ) {
    stop(
      "No chronological OOS rows are available.",
      call. = FALSE
    )
  }
  
  chronology_checks <-
    relevant_rows |>
    group_by(
      Symbol
    ) |>
    summarise(
      TimestampStrictlyIncreasing =
        all(
          diff(
            as.numeric(
              CompetitionTimestampCT
            )
          ) >=
            0
        ),
      TrainingSizeNondecreasing =
        all(
          diff(
            TrainingObservations
          ) >=
            0
        ),
      .groups =
        "drop"
    )
  
  if (
    !all(
      chronology_checks$TimestampStrictlyIncreasing
    )
  ) {
    stop(
      "OOS forecast timestamps are not chronological.",
      call. = FALSE
    )
  }
  
  if (
    !all(
      chronology_checks$TrainingSizeNondecreasing
    )
  ) {
    stop(
      "OOS training sizes are not nondecreasing.",
      call. = FALSE
    )
  }
  
  chronology_checks
}


# ============================================================
# 43. Final script validation
# ============================================================

required_forecasting_functions <- c(
  "prepare_futures_forecasting_symbol_data",
  "get_futures_expanding_origin_indices",
  "validate_futures_forecast_split",
  "safe_fit_futures_mean_model",
  "safe_predict_futures_mean_model",
  "forecast_one_futures_mean_origin",
  "run_futures_mean_expanding_origin",
  "run_all_futures_mean_expanding_origin",
  "run_all_futures_mean_oos",
  "summarize_futures_mean_oos",
  "build_futures_training_residual_data",
  "add_futures_forecast_residual_lags",
  "safe_fit_futures_variance_model",
  "safe_predict_futures_variance_model",
  "forecast_one_futures_mean_variance_origin",
  "run_futures_mean_variance_expanding_origin",
  "run_all_futures_variance_oos_for_mean",
  "run_futures_representative_mean_variance_oos",
  "summarize_futures_variance_oos",
  "futures_qlike_loss",
  "add_futures_variance_qlike",
  "summarize_futures_variance_oos_with_qlike",
  "validate_futures_mean_oos_results",
  "validate_futures_variance_oos_results",
  "align_futures_standardized_oos_innovations",
  "make_futures_standardized_oos_innovations_long",
  "estimate_futures_oos_correlation",
  "validate_futures_correlation_matrix",
  "summarize_futures_forecast_failures",
  "summarize_futures_forecast_coverage",
  "add_futures_mean_oos_benchmark_comparison",
  "add_futures_variance_oos_benchmark_comparison",
  "rank_futures_mean_oos_models",
  "rank_futures_variance_oos_models",
  "should_refit_futures_model",
  "validate_futures_oos_chronology"
)

missing_forecasting_functions <-
  required_forecasting_functions[
    !vapply(
      required_forecasting_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (
  length(
    missing_forecasting_functions
  ) > 0L
) {
  stop(
    paste0(
      "futures_forecasting.R validation failed. ",
      "Missing functions: ",
      paste(
        missing_forecasting_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

FUTURES_FORECASTING_SCRIPT_VALIDATION <- TRUE

# ============================================================
# Extended conventional-model OOS forecasting layer
# ============================================================
#
# This section is append-only.
#
# It does NOT modify the original futures OOS functions or
# the validated Notebook 40 caches.
#
# It adds support for:
#
# Mean:
#   trimmed_mean_05
#   trimmed_mean_10
#   ar1
#   ar2
#   ma1
#   ma2
#   arma11
#   arma_aic
#
# Variance:
#   rolling_var_20
#   rolling_var_50
#   rolling_var_100
#   ewma_094
#   ewma_097
#   garch11_normal
#   garch11_student
#
# ============================================================


# ============================================================
# 1. Validate extended modeling helpers
# ============================================================

required_extended_model_functions <- c(
  "get_extended_futures_mean_model_names",
  "fit_extended_futures_mean_model",
  "predict_extended_futures_mean_model",
  "attach_extended_futures_mean_residuals",
  "get_extended_futures_variance_model_names",
  "fit_extended_futures_variance_model",
  "predict_extended_futures_variance_model"
)

missing_extended_model_functions <-
  required_extended_model_functions[
    !vapply(
      required_extended_model_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (
  length(
    missing_extended_model_functions
  ) > 0L
) {
  
  futures_models_path <-
    here(
      "scripts",
      "futures_models.R"
    )
  
  if (
    !file.exists(
      futures_models_path
    )
  ) {
    stop(
      paste0(
        "Extended futures modeling helpers are missing and ",
        "scripts/futures_models.R could not be found."
      ),
      call. = FALSE
    )
  }
  
  source(
    futures_models_path
  )
}

missing_extended_model_functions <-
  required_extended_model_functions[
    !vapply(
      required_extended_model_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (
  length(
    missing_extended_model_functions
  ) > 0L
) {
  stop(
    paste0(
      "Missing extended futures modeling functions: ",
      paste(
        missing_extended_model_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 2. Extended mean-model dispatch helpers
# ============================================================

is_extended_futures_mean_model_name <- function(
    model_name
) {
  
  model_name %in%
    get_extended_futures_mean_model_names()
}


fit_any_futures_mean_model <- function(
    data,
    model_name
) {
  
  if (
    is_extended_futures_mean_model_name(
      model_name
    )
  ) {
    
    fit_extended_futures_mean_model(
      data = data,
      model_name = model_name
    )
    
  } else {
    
    fit_futures_mean_model(
      data = data,
      model_name = model_name
    )
  }
}


predict_any_futures_mean_model <- function(
    model,
    new_data
) {
  
  if (
    inherits(
      model,
      "extended_futures_mean_model"
    )
  ) {
    
    predict_extended_futures_mean_model(
      model = model,
      new_data = new_data
    )
    
  } else {
    
    predict_futures_mean_model(
      model = model,
      new_data = new_data
    )
  }
}


attach_any_futures_mean_residuals <- function(
    data,
    model
) {
  
  if (
    inherits(
      model,
      "extended_futures_mean_model"
    )
  ) {
    
    attach_extended_futures_mean_residuals(
      data = data,
      model = model
    )
    
  } else {
    
    attach_futures_mean_residuals(
      data = data,
      model = model
    )
  }
}


# ============================================================
# 3. Safe extended mean fitting
# ============================================================

safe_fit_extended_futures_mean_model <- function(
    training_data,
    model_name,
    symbol = NA_character_,
    forecast_timestamp = as.POSIXct(NA)
) {
  
  tryCatch(
    {
      
      fit_extended_futures_mean_model(
        data = training_data,
        model_name = model_name
      )
      
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage = "mean_fit",
        symbol = symbol,
        mean_model_name = model_name,
        forecast_timestamp = forecast_timestamp,
        message = conditionMessage(
          error_object
        )
      )
    }
  )
}


# ============================================================
# 4. Safe extended mean prediction
# ============================================================

safe_predict_extended_futures_mean_model <- function(
    model,
    forecast_row,
    symbol = NA_character_,
    model_name = NA_character_
) {
  
  forecast_timestamp <-
    forecast_row[["CompetitionTimestampCT"]][[1]]
  
  if (
    is_futures_forecast_error(
      model
    )
  ) {
    return(
      model
    )
  }
  
  tryCatch(
    {
      
      prediction <-
        predict_extended_futures_mean_model(
          model = model,
          new_data = forecast_row
        )
      
      prediction <-
        as.numeric(
          prediction[[1]]
        )
      
      if (
        length(
          prediction
        ) != 1L ||
        !is.finite(
          prediction
        )
      ) {
        stop(
          "Extended mean prediction is invalid."
        )
      }
      
      prediction
      
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage = "mean_prediction",
        symbol = symbol,
        mean_model_name = model_name,
        forecast_timestamp = forecast_timestamp,
        message = conditionMessage(
          error_object
        )
      )
    }
  )
}


# ============================================================
# 5. One extended mean forecast origin
# ============================================================

forecast_one_extended_futures_mean_origin <- function(
    symbol_data,
    forecast_index,
    mean_model_name
) {
  
  forecast_index <-
    as.integer(
      forecast_index
    )
  
  if (
    forecast_index <= 1L ||
    forecast_index >
    nrow(
      symbol_data
    )
  ) {
    stop(
      "forecast_index is outside the valid range.",
      call. = FALSE
    )
  }
  
  training_data <-
    symbol_data[
      seq_len(
        forecast_index - 1L
      ),
      ,
      drop = FALSE
    ]
  
  forecast_row <-
    symbol_data[
      forecast_index,
      ,
      drop = FALSE
    ]
  
  validate_futures_forecast_split(
    training_data = training_data,
    forecast_row = forecast_row
  )
  
  symbol <-
    forecast_row[["Symbol"]][[1]]
  
  forecast_timestamp <-
    forecast_row[["CompetitionTimestampCT"]][[1]]
  
  actual_return <-
    forecast_row[["CandidateLogReturn"]][[1]]
  
  fitted_model <-
    safe_fit_extended_futures_mean_model(
      training_data = training_data,
      model_name = mean_model_name,
      symbol = symbol,
      forecast_timestamp = forecast_timestamp
    )
  
  if (
    is_futures_forecast_error(
      fitted_model
    )
  ) {
    
    return(
      tibble(
        Symbol = symbol,
        CompetitionTimestampCT = forecast_timestamp,
        ForecastIndex = forecast_index,
        TrainingObservations = nrow(
          training_data
        ),
        MeanModel = mean_model_name,
        ActualLogReturn = actual_return,
        MeanForecast = NA_real_,
        MeanForecastError = NA_real_,
        MeanSquaredForecastError = NA_real_,
        MeanAbsoluteForecastError = NA_real_,
        FitSucceeded = FALSE,
        PredictionSucceeded = FALSE,
        ErrorStage = fitted_model[["Stage"]],
        ErrorMessage = fitted_model[["ErrorMessage"]]
      )
    )
  }
  
  mean_forecast <-
    safe_predict_extended_futures_mean_model(
      model = fitted_model,
      forecast_row = forecast_row,
      symbol = symbol,
      model_name = mean_model_name
    )
  
  if (
    is_futures_forecast_error(
      mean_forecast
    )
  ) {
    
    return(
      tibble(
        Symbol = symbol,
        CompetitionTimestampCT = forecast_timestamp,
        ForecastIndex = forecast_index,
        TrainingObservations = nrow(
          training_data
        ),
        MeanModel = mean_model_name,
        ActualLogReturn = actual_return,
        MeanForecast = NA_real_,
        MeanForecastError = NA_real_,
        MeanSquaredForecastError = NA_real_,
        MeanAbsoluteForecastError = NA_real_,
        FitSucceeded = TRUE,
        PredictionSucceeded = FALSE,
        ErrorStage = mean_forecast[["Stage"]],
        ErrorMessage = mean_forecast[["ErrorMessage"]]
      )
    )
  }
  
  mean_error <-
    actual_return -
    mean_forecast
  
  tibble(
    Symbol = symbol,
    CompetitionTimestampCT = forecast_timestamp,
    ForecastIndex = forecast_index,
    TrainingObservations = nrow(
      training_data
    ),
    MeanModel = mean_model_name,
    ActualLogReturn = actual_return,
    MeanForecast = mean_forecast,
    MeanForecastError = mean_error,
    MeanSquaredForecastError = mean_error^2,
    MeanAbsoluteForecastError = abs(
      mean_error
    ),
    FitSucceeded = TRUE,
    PredictionSucceeded = TRUE,
    ErrorStage = NA_character_,
    ErrorMessage = NA_character_
  )
}


# ============================================================
# 6. Extended mean expanding-origin OOS
# ============================================================

run_extended_futures_mean_expanding_origin <- function(
    data,
    symbol,
    mean_model_name,
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  if (
    !is_extended_futures_mean_model_name(
      mean_model_name
    )
  ) {
    stop(
      paste0(
        "Not an extended mean model: ",
        mean_model_name
      ),
      call. = FALSE
    )
  }
  
  symbol_data <-
    prepare_futures_forecasting_symbol_data(
      data = data,
      symbol = symbol
    )
  
  forecast_indices <-
    get_futures_expanding_origin_indices(
      data = symbol_data,
      min_training_observations =
        min_training_observations
    )
  
  result_rows <-
    vector(
      mode = "list",
      length = length(
        forecast_indices
      )
    )
  
  for (
    iteration_index in
    seq_along(
      forecast_indices
    )
  ) {
    
    if (
      verbose &&
      (
        iteration_index == 1L ||
        iteration_index %% 100L == 0L ||
        iteration_index ==
        length(
          forecast_indices
        )
      )
    ) {
      
      cat(
        "Extended mean OOS:",
        symbol,
        mean_model_name,
        iteration_index,
        "of",
        length(
          forecast_indices
        ),
        "\n"
      )
    }
    
    result_rows[[iteration_index]] <-
      forecast_one_extended_futures_mean_origin(
        symbol_data = symbol_data,
        forecast_index =
          forecast_indices[[iteration_index]],
        mean_model_name =
          mean_model_name
      )
  }
  
  bind_rows(
    result_rows
  )
}


# ============================================================
# 7. Run all extended means for one future
# ============================================================

run_all_extended_futures_mean_oos_for_symbol <- function(
    data,
    symbol,
    mean_model_names =
      get_extended_futures_mean_model_names(),
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  result_list <-
    vector(
      mode = "list",
      length = length(
        mean_model_names
      )
    )
  
  names(
    result_list
  ) <-
    mean_model_names
  
  for (
    mean_model_name in
    mean_model_names
  ) {
    
    if (
      verbose
    ) {
      cat(
        "\nStarting extended mean model:",
        symbol,
        mean_model_name,
        "\n"
      )
    }
    
    result_list[[mean_model_name]] <-
      run_extended_futures_mean_expanding_origin(
        data = data,
        symbol = symbol,
        mean_model_name = mean_model_name,
        min_training_observations =
          min_training_observations,
        verbose = verbose
      )
  }
  
  bind_rows(
    result_list
  )
}


# ============================================================
# 8. Extended variance-model dispatch
# ============================================================

is_extended_futures_variance_model_name <- function(
    model_name
) {
  
  model_name %in%
    get_extended_futures_variance_model_names(
      include_garch = TRUE
    )
}


safe_fit_extended_futures_variance_model <- function(
    training_residual_data,
    variance_model_name,
    symbol = NA_character_,
    mean_model_name = NA_character_,
    forecast_timestamp = as.POSIXct(NA)
) {
  
  tryCatch(
    {
      
      fit_extended_futures_variance_model(
        data = training_residual_data,
        model_name = variance_model_name
      )
      
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage = "variance_fit",
        symbol = symbol,
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name,
        forecast_timestamp = forecast_timestamp,
        message = conditionMessage(
          error_object
        )
      )
    }
  )
}


safe_predict_extended_futures_variance_model <- function(
    variance_model,
    forecast_row,
    symbol = NA_character_,
    mean_model_name = NA_character_,
    variance_model_name = NA_character_
) {
  
  forecast_timestamp <-
    forecast_row[["CompetitionTimestampCT"]][[1]]
  
  if (
    is_futures_forecast_error(
      variance_model
    )
  ) {
    return(
      variance_model
    )
  }
  
  tryCatch(
    {
      
      variance_forecast <-
        predict_extended_futures_variance_model(
          model = variance_model,
          new_data = forecast_row
        )
      
      variance_forecast <-
        as.numeric(
          variance_forecast[[1]]
        )
      
      if (
        length(
          variance_forecast
        ) != 1L ||
        !is.finite(
          variance_forecast
        ) ||
        variance_forecast <= 0
      ) {
        stop(
          "Extended variance forecast is invalid."
        )
      }
      
      variance_forecast
      
    },
    error = function(
    error_object
    ) {
      
      make_futures_forecast_error(
        stage = "variance_prediction",
        symbol = symbol,
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name,
        forecast_timestamp = forecast_timestamp,
        message = conditionMessage(
          error_object
        )
      )
    }
  )
}


# ============================================================
# 9. Build residual history using any supported mean model
# ============================================================

build_any_futures_training_residual_data <- function(
    training_data,
    fitted_mean_model
) {
  
  if (
    is_futures_forecast_error(
      fitted_mean_model
    )
  ) {
    stop(
      "Cannot construct residuals from a failed mean model.",
      call. = FALSE
    )
  }
  
  residual_data <-
    attach_any_futures_mean_residuals(
      data = training_data,
      model = fitted_mean_model
    )
  
  residual_data <-
    residual_data |>
    filter(
      is.finite(
        MeanResidual
      ),
      is.finite(
        SquaredMeanResidual
      )
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  if (
    nrow(
      residual_data
    ) == 0L
  ) {
    stop(
      "No finite residuals remain.",
      call. = FALSE
    )
  }
  
  residual_data
}


# ============================================================
# 10. One extended variance forecast at one origin
# ============================================================

forecast_one_extended_futures_variance_origin <- function(
    symbol_data,
    forecast_index,
    mean_model_name,
    variance_model_name
) {
  
  forecast_index <-
    as.integer(
      forecast_index
    )
  
  if (
    forecast_index <= 2L ||
    forecast_index >
    nrow(
      symbol_data
    )
  ) {
    stop(
      "forecast_index is outside the valid range.",
      call. = FALSE
    )
  }
  
  if (
    !is_extended_futures_variance_model_name(
      variance_model_name
    )
  ) {
    stop(
      paste0(
        "Not an extended variance model: ",
        variance_model_name
      ),
      call. = FALSE
    )
  }
  
  training_data <-
    symbol_data[
      seq_len(
        forecast_index - 1L
      ),
      ,
      drop = FALSE
    ]
  
  forecast_row <-
    symbol_data[
      forecast_index,
      ,
      drop = FALSE
    ]
  
  validate_futures_forecast_split(
    training_data = training_data,
    forecast_row = forecast_row
  )
  
  symbol <-
    forecast_row[["Symbol"]][[1]]
  
  forecast_timestamp <-
    forecast_row[["CompetitionTimestampCT"]][[1]]
  
  actual_return <-
    forecast_row[["CandidateLogReturn"]][[1]]
  
  fitted_mean_model <-
    tryCatch(
      {
        
        fit_any_futures_mean_model(
          data = training_data,
          model_name = mean_model_name
        )
        
      },
      error = function(
    error_object
      ) {
        
        make_futures_forecast_error(
          stage = "mean_fit",
          symbol = symbol,
          mean_model_name = mean_model_name,
          variance_model_name = variance_model_name,
          forecast_timestamp = forecast_timestamp,
          message = conditionMessage(
            error_object
          )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      fitted_mean_model
    )
  ) {
    
    return(
      make_extended_futures_variance_error_row(
        symbol = symbol,
        forecast_timestamp = forecast_timestamp,
        forecast_index = forecast_index,
        training_observations = nrow(
          training_data
        ),
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name,
        actual_return = actual_return,
        mean_fit_succeeded = FALSE,
        mean_prediction_succeeded = FALSE,
        variance_fit_succeeded = FALSE,
        variance_prediction_succeeded = FALSE,
        error_stage = fitted_mean_model[["Stage"]],
        error_message =
          fitted_mean_model[["ErrorMessage"]]
      )
    )
  }
  
  mean_forecast <-
    tryCatch(
      {
        
        prediction <-
          predict_any_futures_mean_model(
            model = fitted_mean_model,
            new_data = forecast_row
          )
        
        prediction <-
          as.numeric(
            prediction[[1]]
          )
        
        if (
          length(
            prediction
          ) != 1L ||
          !is.finite(
            prediction
          )
        ) {
          stop(
            "Mean forecast is invalid."
          )
        }
        
        prediction
        
      },
      error = function(
    error_object
      ) {
        
        make_futures_forecast_error(
          stage = "mean_prediction",
          symbol = symbol,
          mean_model_name = mean_model_name,
          variance_model_name = variance_model_name,
          forecast_timestamp = forecast_timestamp,
          message = conditionMessage(
            error_object
          )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      mean_forecast
    )
  ) {
    
    return(
      make_extended_futures_variance_error_row(
        symbol = symbol,
        forecast_timestamp = forecast_timestamp,
        forecast_index = forecast_index,
        training_observations = nrow(
          training_data
        ),
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name,
        actual_return = actual_return,
        mean_fit_succeeded = TRUE,
        mean_prediction_succeeded = FALSE,
        variance_fit_succeeded = FALSE,
        variance_prediction_succeeded = FALSE,
        error_stage = mean_forecast[["Stage"]],
        error_message =
          mean_forecast[["ErrorMessage"]]
      )
    )
  }
  
  training_residual_data <-
    tryCatch(
      {
        
        build_any_futures_training_residual_data(
          training_data = training_data,
          fitted_mean_model = fitted_mean_model
        )
        
      },
      error = function(
    error_object
      ) {
        
        make_futures_forecast_error(
          stage = "residual_construction",
          symbol = symbol,
          mean_model_name = mean_model_name,
          variance_model_name = variance_model_name,
          forecast_timestamp = forecast_timestamp,
          message = conditionMessage(
            error_object
          )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      training_residual_data
    )
  ) {
    
    return(
      make_extended_futures_variance_error_row(
        symbol = symbol,
        forecast_timestamp = forecast_timestamp,
        forecast_index = forecast_index,
        training_observations = nrow(
          training_data
        ),
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name,
        actual_return = actual_return,
        mean_forecast = mean_forecast,
        mean_fit_succeeded = TRUE,
        mean_prediction_succeeded = TRUE,
        variance_fit_succeeded = FALSE,
        variance_prediction_succeeded = FALSE,
        error_stage =
          training_residual_data[["Stage"]],
        error_message =
          training_residual_data[["ErrorMessage"]]
      )
    )
  }
  
  fitted_variance_model <-
    safe_fit_extended_futures_variance_model(
      training_residual_data =
        training_residual_data,
      variance_model_name =
        variance_model_name,
      symbol = symbol,
      mean_model_name = mean_model_name,
      forecast_timestamp = forecast_timestamp
    )
  
  if (
    is_futures_forecast_error(
      fitted_variance_model
    )
  ) {
    
    return(
      make_extended_futures_variance_error_row(
        symbol = symbol,
        forecast_timestamp = forecast_timestamp,
        forecast_index = forecast_index,
        training_observations = nrow(
          training_data
        ),
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name,
        actual_return = actual_return,
        mean_forecast = mean_forecast,
        mean_fit_succeeded = TRUE,
        mean_prediction_succeeded = TRUE,
        variance_fit_succeeded = FALSE,
        variance_prediction_succeeded = FALSE,
        error_stage =
          fitted_variance_model[["Stage"]],
        error_message =
          fitted_variance_model[["ErrorMessage"]]
      )
    )
  }
  
  variance_forecast <-
    safe_predict_extended_futures_variance_model(
      variance_model =
        fitted_variance_model,
      forecast_row = forecast_row,
      symbol = symbol,
      mean_model_name = mean_model_name,
      variance_model_name = variance_model_name
    )
  
  if (
    is_futures_forecast_error(
      variance_forecast
    )
  ) {
    
    return(
      make_extended_futures_variance_error_row(
        symbol = symbol,
        forecast_timestamp = forecast_timestamp,
        forecast_index = forecast_index,
        training_observations = nrow(
          training_data
        ),
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name,
        actual_return = actual_return,
        mean_forecast = mean_forecast,
        mean_fit_succeeded = TRUE,
        mean_prediction_succeeded = TRUE,
        variance_fit_succeeded = TRUE,
        variance_prediction_succeeded = FALSE,
        error_stage =
          variance_forecast[["Stage"]],
        error_message =
          variance_forecast[["ErrorMessage"]]
      )
    )
  }
  
  mean_error <-
    actual_return -
    mean_forecast
  
  realized_squared_innovation <-
    mean_error^2
  
  variance_error <-
    realized_squared_innovation -
    variance_forecast
  
  volatility_forecast <-
    sqrt(
      variance_forecast
    )
  
  standardized_innovation <-
    mean_error /
    volatility_forecast
  
  tibble(
    Symbol = symbol,
    CompetitionTimestampCT = forecast_timestamp,
    ForecastIndex = forecast_index,
    TrainingObservations = nrow(
      training_data
    ),
    MeanModel = mean_model_name,
    VarianceModel = variance_model_name,
    ActualLogReturn = actual_return,
    MeanForecast = mean_forecast,
    VarianceForecast = variance_forecast,
    VolatilityForecast = volatility_forecast,
    MeanForecastError = mean_error,
    SquaredMeanForecastError =
      realized_squared_innovation,
    VarianceForecastError = variance_error,
    VarianceSquaredForecastError =
      variance_error^2,
    VarianceAbsoluteForecastError =
      abs(
        variance_error
      ),
    StandardizedInnovation =
      standardized_innovation,
    MeanFitSucceeded = TRUE,
    MeanPredictionSucceeded = TRUE,
    VarianceFitSucceeded = TRUE,
    VariancePredictionSucceeded = TRUE,
    ErrorStage = NA_character_,
    ErrorMessage = NA_character_
  )
}


# ============================================================
# 11. Extended variance error-row helper
# ============================================================

make_extended_futures_variance_error_row <- function(
    symbol,
    forecast_timestamp,
    forecast_index,
    training_observations,
    mean_model_name,
    variance_model_name,
    actual_return,
    mean_forecast = NA_real_,
    mean_fit_succeeded = FALSE,
    mean_prediction_succeeded = FALSE,
    variance_fit_succeeded = FALSE,
    variance_prediction_succeeded = FALSE,
    error_stage,
    error_message
) {
  
  mean_error <-
    if (
      is.finite(
        mean_forecast
      )
    ) {
      actual_return -
        mean_forecast
    } else {
      NA_real_
    }
  
  tibble(
    Symbol = symbol,
    CompetitionTimestampCT = forecast_timestamp,
    ForecastIndex = forecast_index,
    TrainingObservations = training_observations,
    MeanModel = mean_model_name,
    VarianceModel = variance_model_name,
    ActualLogReturn = actual_return,
    MeanForecast = mean_forecast,
    VarianceForecast = NA_real_,
    VolatilityForecast = NA_real_,
    MeanForecastError = mean_error,
    SquaredMeanForecastError =
      if (
        is.finite(
          mean_error
        )
      ) {
        mean_error^2
      } else {
        NA_real_
      },
    VarianceForecastError = NA_real_,
    VarianceSquaredForecastError = NA_real_,
    VarianceAbsoluteForecastError = NA_real_,
    StandardizedInnovation = NA_real_,
    MeanFitSucceeded = mean_fit_succeeded,
    MeanPredictionSucceeded =
      mean_prediction_succeeded,
    VarianceFitSucceeded =
      variance_fit_succeeded,
    VariancePredictionSucceeded =
      variance_prediction_succeeded,
    ErrorStage = error_stage,
    ErrorMessage = error_message
  )
}


# ============================================================
# 12. Run one extended variance model over OOS origins
# ============================================================

run_extended_futures_variance_expanding_origin <- function(
    data,
    symbol,
    mean_model_name,
    variance_model_name,
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  symbol_data <-
    prepare_futures_forecasting_symbol_data(
      data = data,
      symbol = symbol
    )
  
  forecast_indices <-
    get_futures_expanding_origin_indices(
      data = symbol_data,
      min_training_observations =
        min_training_observations
    )
  
  result_rows <-
    vector(
      mode = "list",
      length = length(
        forecast_indices
      )
    )
  
  for (
    iteration_index in
    seq_along(
      forecast_indices
    )
  ) {
    
    if (
      verbose &&
      (
        iteration_index == 1L ||
        iteration_index %% 100L == 0L ||
        iteration_index ==
        length(
          forecast_indices
        )
      )
    ) {
      
      cat(
        "Extended variance OOS:",
        symbol,
        mean_model_name,
        variance_model_name,
        iteration_index,
        "of",
        length(
          forecast_indices
        ),
        "\n"
      )
    }
    
    result_rows[[iteration_index]] <-
      forecast_one_extended_futures_variance_origin(
        symbol_data = symbol_data,
        forecast_index =
          forecast_indices[[iteration_index]],
        mean_model_name = mean_model_name,
        variance_model_name =
          variance_model_name
      )
  }
  
  bind_rows(
    result_rows
  )
}


# ============================================================
# 13. Extended-model chronology validation
# ============================================================

validate_extended_futures_oos_chronology <- function(
    forecast_data,
    source_data
) {
  
  required_columns <- c(
    "Symbol",
    "CompetitionTimestampCT",
    "ForecastIndex",
    "TrainingObservations"
  )
  
  if (
    !all(
      required_columns %in%
      names(
        forecast_data
      )
    )
  ) {
    stop(
      "Extended OOS forecast data are missing chronology columns.",
      call. = FALSE
    )
  }
  
  symbols <-
    unique(
      forecast_data[["Symbol"]]
    )
  
  for (
    symbol in
    symbols
  ) {
    
    symbol_source <-
      prepare_futures_forecasting_symbol_data(
        data = source_data,
        symbol = symbol
      )
    
    symbol_forecasts <-
      forecast_data |>
      filter(
        Symbol ==
          symbol
      )
    
    for (
      row_index in
      seq_len(
        nrow(
          symbol_forecasts
        )
      )
    ) {
      
      forecast_index <-
        symbol_forecasts[["ForecastIndex"]][[row_index]]
      
      training_observations <-
        symbol_forecasts[["TrainingObservations"]][[row_index]]
      
      forecast_timestamp <-
        symbol_forecasts[["CompetitionTimestampCT"]][[row_index]]
      
      if (
        forecast_index !=
        training_observations + 1L
      ) {
        stop(
          paste0(
            "Extended OOS chronology index failure for ",
            symbol,
            "."
          ),
          call. = FALSE
        )
      }
      
      if (
        forecast_index >
        nrow(
          symbol_source
        )
      ) {
        stop(
          paste0(
            "Extended OOS forecast index exceeds source data for ",
            symbol,
            "."
          ),
          call. = FALSE
        )
      }
      
      expected_timestamp <-
        symbol_source[["CompetitionTimestampCT"]][[forecast_index]]
      
      if (
        !identical(
          forecast_timestamp,
          expected_timestamp
        )
      ) {
        stop(
          paste0(
            "Extended OOS timestamp mismatch for ",
            symbol,
            "."
          ),
          call. = FALSE
        )
      }
      
      last_training_timestamp <-
        symbol_source[["CompetitionTimestampCT"]][[
          training_observations
        ]]
      
      if (
        !(
          last_training_timestamp <
          forecast_timestamp
        )
      ) {
        stop(
          paste0(
            "Extended OOS look-ahead failure for ",
            symbol,
            "."
          ),
          call. = FALSE
        )
      }
    }
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 14. Extended forecasting validation
# ============================================================

required_extended_forecasting_functions <- c(
  "is_extended_futures_mean_model_name",
  "fit_any_futures_mean_model",
  "predict_any_futures_mean_model",
  "attach_any_futures_mean_residuals",
  "safe_fit_extended_futures_mean_model",
  "safe_predict_extended_futures_mean_model",
  "forecast_one_extended_futures_mean_origin",
  "run_extended_futures_mean_expanding_origin",
  "run_all_extended_futures_mean_oos_for_symbol",
  "is_extended_futures_variance_model_name",
  "safe_fit_extended_futures_variance_model",
  "safe_predict_extended_futures_variance_model",
  "build_any_futures_training_residual_data",
  "make_extended_futures_variance_error_row",
  "forecast_one_extended_futures_variance_origin",
  "run_extended_futures_variance_expanding_origin",
  "validate_extended_futures_oos_chronology"
)

missing_extended_forecasting_functions <-
  required_extended_forecasting_functions[
    !vapply(
      required_extended_forecasting_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (
  length(
    missing_extended_forecasting_functions
  ) > 0L
) {
  stop(
    paste0(
      "Extended futures forecasting validation failed. Missing: ",
      paste(
        missing_extended_forecasting_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

FUTURES_EXTENDED_FORECASTING_SCRIPT_VALIDATION <-
  TRUE

# ============================================================
# OPTIMIZED MULTI-VARIANCE EXTENDED OOS FORECASTING
# ============================================================
#
# Purpose:
#
# At each expanding-origin timestamp:
#
#   1. Fit the selected mean model once.
#   2. Forecast the conditional mean once.
#   3. Construct the training residual history once.
#   4. Fit and predict multiple extended variance models.
#
# This avoids repeating the same mean fit and residual
# construction separately for every variance model.
#
# The statistical specification is unchanged.
# ============================================================


# ============================================================
# 1. One origin, multiple extended variance models
# ============================================================

forecast_one_extended_futures_multi_variance_origin <- function(
    symbol_data,
    forecast_index,
    mean_model_name,
    variance_model_names =
      get_extended_futures_variance_model_names(
        include_garch = TRUE
      )
) {
  
  forecast_index <-
    as.integer(
      forecast_index
    )
  
  if (
    forecast_index <= 2L ||
    forecast_index >
    nrow(
      symbol_data
    )
  ) {
    stop(
      "forecast_index is outside the valid range.",
      call. = FALSE
    )
  }
  
  variance_model_names <-
    as.character(
      variance_model_names
    )
  
  if (
    length(
      variance_model_names
    ) == 0L
  ) {
    stop(
      "variance_model_names must contain at least one model.",
      call. = FALSE
    )
  }
  
  invalid_variance_models <-
    variance_model_names[
      !vapply(
        variance_model_names,
        is_extended_futures_variance_model_name,
        logical(1)
      )
    ]
  
  if (
    length(
      invalid_variance_models
    ) > 0L
  ) {
    stop(
      paste0(
        "Unsupported extended variance model(s): ",
        paste(
          invalid_variance_models,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  training_data <-
    symbol_data[
      seq_len(
        forecast_index - 1L
      ),
      ,
      drop = FALSE
    ]
  
  forecast_row <-
    symbol_data[
      forecast_index,
      ,
      drop = FALSE
    ]
  
  validate_futures_forecast_split(
    training_data = training_data,
    forecast_row = forecast_row
  )
  
  symbol <-
    forecast_row[["Symbol"]][[1]]
  
  forecast_timestamp <-
    forecast_row[["CompetitionTimestampCT"]][[1]]
  
  actual_return <-
    forecast_row[["CandidateLogReturn"]][[1]]
  
  training_observations <-
    nrow(
      training_data
    )
  
  fitted_mean_model <-
    tryCatch(
      {
        fit_any_futures_mean_model(
          data = training_data,
          model_name = mean_model_name
        )
      },
      error = function(
    error_object
      ) {
        make_futures_forecast_error(
          stage = "mean_fit",
          symbol = symbol,
          mean_model_name = mean_model_name,
          forecast_timestamp = forecast_timestamp,
          message = conditionMessage(
            error_object
          )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      fitted_mean_model
    )
  ) {
    
    return(
      bind_rows(
        lapply(
          variance_model_names,
          function(
    variance_model_name
          ) {
            
            make_extended_futures_variance_error_row(
              symbol = symbol,
              forecast_timestamp = forecast_timestamp,
              forecast_index = forecast_index,
              training_observations = training_observations,
              mean_model_name = mean_model_name,
              variance_model_name = variance_model_name,
              actual_return = actual_return,
              mean_fit_succeeded = FALSE,
              mean_prediction_succeeded = FALSE,
              variance_fit_succeeded = FALSE,
              variance_prediction_succeeded = FALSE,
              error_stage =
                fitted_mean_model[["Stage"]],
              error_message =
                fitted_mean_model[["ErrorMessage"]]
            )
          }
        )
      )
    )
  }
  
  mean_forecast <-
    tryCatch(
      {
        prediction <-
          predict_any_futures_mean_model(
            model = fitted_mean_model,
            new_data = forecast_row
          )
        
        prediction <-
          as.numeric(
            prediction[[1]]
          )
        
        if (
          length(
            prediction
          ) != 1L ||
          !is.finite(
            prediction
          )
        ) {
          stop(
            "Mean forecast is invalid."
          )
        }
        
        prediction
      },
      error = function(
    error_object
      ) {
        make_futures_forecast_error(
          stage = "mean_prediction",
          symbol = symbol,
          mean_model_name = mean_model_name,
          forecast_timestamp = forecast_timestamp,
          message = conditionMessage(
            error_object
          )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      mean_forecast
    )
  ) {
    
    return(
      bind_rows(
        lapply(
          variance_model_names,
          function(
    variance_model_name
          ) {
            
            make_extended_futures_variance_error_row(
              symbol = symbol,
              forecast_timestamp = forecast_timestamp,
              forecast_index = forecast_index,
              training_observations = training_observations,
              mean_model_name = mean_model_name,
              variance_model_name = variance_model_name,
              actual_return = actual_return,
              mean_fit_succeeded = TRUE,
              mean_prediction_succeeded = FALSE,
              variance_fit_succeeded = FALSE,
              variance_prediction_succeeded = FALSE,
              error_stage =
                mean_forecast[["Stage"]],
              error_message =
                mean_forecast[["ErrorMessage"]]
            )
          }
        )
      )
    )
  }
  
  training_residual_data <-
    tryCatch(
      {
        build_any_futures_training_residual_data(
          training_data = training_data,
          fitted_mean_model = fitted_mean_model
        )
      },
      error = function(
    error_object
      ) {
        make_futures_forecast_error(
          stage = "residual_construction",
          symbol = symbol,
          mean_model_name = mean_model_name,
          forecast_timestamp = forecast_timestamp,
          message = conditionMessage(
            error_object
          )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      training_residual_data
    )
  ) {
    
    return(
      bind_rows(
        lapply(
          variance_model_names,
          function(
    variance_model_name
          ) {
            
            make_extended_futures_variance_error_row(
              symbol = symbol,
              forecast_timestamp = forecast_timestamp,
              forecast_index = forecast_index,
              training_observations = training_observations,
              mean_model_name = mean_model_name,
              variance_model_name = variance_model_name,
              actual_return = actual_return,
              mean_forecast = mean_forecast,
              mean_fit_succeeded = TRUE,
              mean_prediction_succeeded = TRUE,
              variance_fit_succeeded = FALSE,
              variance_prediction_succeeded = FALSE,
              error_stage =
                training_residual_data[["Stage"]],
              error_message =
                training_residual_data[["ErrorMessage"]]
            )
          }
        )
      )
    )
  }
  
  result_rows <-
    vector(
      mode = "list",
      length = length(
        variance_model_names
      )
    )
  
  names(
    result_rows
  ) <-
    variance_model_names
  
  for (
    variance_model_name in
    variance_model_names
  ) {
    
    fitted_variance_model <-
      safe_fit_extended_futures_variance_model(
        training_residual_data =
          training_residual_data,
        variance_model_name =
          variance_model_name,
        symbol = symbol,
        mean_model_name = mean_model_name,
        forecast_timestamp = forecast_timestamp
      )
    
    if (
      is_futures_forecast_error(
        fitted_variance_model
      )
    ) {
      
      result_rows[[variance_model_name]] <-
        make_extended_futures_variance_error_row(
          symbol = symbol,
          forecast_timestamp = forecast_timestamp,
          forecast_index = forecast_index,
          training_observations = training_observations,
          mean_model_name = mean_model_name,
          variance_model_name = variance_model_name,
          actual_return = actual_return,
          mean_forecast = mean_forecast,
          mean_fit_succeeded = TRUE,
          mean_prediction_succeeded = TRUE,
          variance_fit_succeeded = FALSE,
          variance_prediction_succeeded = FALSE,
          error_stage =
            fitted_variance_model[["Stage"]],
          error_message =
            fitted_variance_model[["ErrorMessage"]]
        )
      
      next
    }
    
    variance_forecast <-
      safe_predict_extended_futures_variance_model(
        variance_model =
          fitted_variance_model,
        forecast_row = forecast_row,
        symbol = symbol,
        mean_model_name = mean_model_name,
        variance_model_name = variance_model_name
      )
    
    if (
      is_futures_forecast_error(
        variance_forecast
      )
    ) {
      
      result_rows[[variance_model_name]] <-
        make_extended_futures_variance_error_row(
          symbol = symbol,
          forecast_timestamp = forecast_timestamp,
          forecast_index = forecast_index,
          training_observations = training_observations,
          mean_model_name = mean_model_name,
          variance_model_name = variance_model_name,
          actual_return = actual_return,
          mean_forecast = mean_forecast,
          mean_fit_succeeded = TRUE,
          mean_prediction_succeeded = TRUE,
          variance_fit_succeeded = TRUE,
          variance_prediction_succeeded = FALSE,
          error_stage =
            variance_forecast[["Stage"]],
          error_message =
            variance_forecast[["ErrorMessage"]]
        )
      
      next
    }
    
    mean_error <-
      actual_return -
      mean_forecast
    
    realized_squared_innovation <-
      mean_error^2
    
    variance_error <-
      realized_squared_innovation -
      variance_forecast
    
    volatility_forecast <-
      sqrt(
        variance_forecast
      )
    
    standardized_innovation <-
      mean_error /
      volatility_forecast
    
    result_rows[[variance_model_name]] <-
      tibble(
        Symbol = symbol,
        CompetitionTimestampCT = forecast_timestamp,
        ForecastIndex = forecast_index,
        TrainingObservations = training_observations,
        MeanModel = mean_model_name,
        VarianceModel = variance_model_name,
        ActualLogReturn = actual_return,
        MeanForecast = mean_forecast,
        VarianceForecast = variance_forecast,
        VolatilityForecast = volatility_forecast,
        MeanForecastError = mean_error,
        SquaredMeanForecastError =
          realized_squared_innovation,
        VarianceForecastError = variance_error,
        VarianceSquaredForecastError =
          variance_error^2,
        VarianceAbsoluteForecastError =
          abs(
            variance_error
          ),
        StandardizedInnovation =
          standardized_innovation,
        MeanFitSucceeded = TRUE,
        MeanPredictionSucceeded = TRUE,
        VarianceFitSucceeded = TRUE,
        VariancePredictionSucceeded = TRUE,
        ErrorStage = NA_character_,
        ErrorMessage = NA_character_
      )
  }
  
  bind_rows(
    result_rows
  )
}


# ============================================================
# 2. Expanding-origin multi-variance runner
# ============================================================

run_extended_futures_multi_variance_expanding_origin <- function(
    data,
    symbol,
    mean_model_name,
    variance_model_names =
      get_extended_futures_variance_model_names(
        include_garch = TRUE
      ),
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  symbol_data <-
    prepare_futures_forecasting_symbol_data(
      data = data,
      symbol = symbol
    )
  
  forecast_indices <-
    get_futures_expanding_origin_indices(
      data = symbol_data,
      min_training_observations =
        min_training_observations
    )
  
  result_rows <-
    vector(
      mode = "list",
      length = length(
        forecast_indices
      )
    )
  
  for (
    iteration_index in
    seq_along(
      forecast_indices
    )
  ) {
    
    if (
      verbose &&
      (
        iteration_index == 1L ||
        iteration_index %% 100L == 0L ||
        iteration_index ==
        length(
          forecast_indices
        )
      )
    ) {
      
      cat(
        "Extended multi-variance OOS:",
        symbol,
        mean_model_name,
        iteration_index,
        "of",
        length(
          forecast_indices
        ),
        "-",
        length(
          variance_model_names
        ),
        "variance models\n"
      )
    }
    
    result_rows[[iteration_index]] <-
      forecast_one_extended_futures_multi_variance_origin(
        symbol_data = symbol_data,
        forecast_index =
          forecast_indices[[iteration_index]],
        mean_model_name = mean_model_name,
        variance_model_names =
          variance_model_names
      )
  }
  
  bind_rows(
    result_rows
  )
}


# ============================================================
# 3. Optimized-run validation
# ============================================================

required_multi_variance_functions <- c(
  "forecast_one_extended_futures_multi_variance_origin",
  "run_extended_futures_multi_variance_expanding_origin"
)

missing_multi_variance_functions <-
  required_multi_variance_functions[
    !vapply(
      required_multi_variance_functions,
      exists,
      logical(1),
      mode = "function"
    )
  ]

if (
  length(
    missing_multi_variance_functions
  ) > 0L
) {
  stop(
    paste0(
      "Optimized multi-variance validation failed. Missing: ",
      paste(
        missing_multi_variance_functions,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

FUTURES_EXTENDED_MULTI_VARIANCE_FORECASTING_VALIDATION <-
  TRUE

# ============================================================
# COMBINED ORIGINAL + EXTENDED MULTI-VARIANCE FORECASTING
# ============================================================
#
# This layer evaluates all variance candidates under one fixed
# selected conditional-mean model.
#
# Original variance candidates: 16
# Extended variance candidates: 7
# Total variance candidates: 23
#
# At each OOS origin:
#
#   1. Fit selected mean once.
#   2. Forecast mean once.
#   3. Build residual history once.
#   4. Build residual-lag forecast features once.
#   5. Fit/predict all 23 variance candidates.
#
# ============================================================


# ============================================================
# 1. Original variance-model membership
# ============================================================

is_original_futures_variance_model_name <- function(
    model_name
) {
  
  model_name %in%
    get_futures_variance_model_specs()[["ModelName"]]
}


# ============================================================
# 2. Combined variance-model universe
# ============================================================

get_combined_futures_variance_model_names <- function(
    include_garch = TRUE
) {
  
  original_names <-
    get_futures_variance_model_specs()[["ModelName"]]
  
  extended_names <-
    get_extended_futures_variance_model_names(
      include_garch = include_garch
    )
  
  overlap <-
    intersect(
      original_names,
      extended_names
    )
  
  if (
    length(
      overlap
    ) > 0L
  ) {
    stop(
      paste0(
        "Original and extended variance-model names overlap: ",
        paste(
          overlap,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  c(
    original_names,
    extended_names
  )
}


# ============================================================
# 3. Fit any supported variance model safely
# ============================================================

safe_fit_any_futures_variance_model <- function(
    training_residual_data,
    variance_model_name,
    symbol = NA_character_,
    mean_model_name = NA_character_,
    forecast_timestamp = as.POSIXct(NA)
) {
  
  if (
    is_original_futures_variance_model_name(
      variance_model_name
    )
  ) {
    
    return(
      safe_fit_futures_variance_model(
        training_residual_data =
          training_residual_data,
        variance_model_name =
          variance_model_name,
        symbol = symbol,
        mean_model_name =
          mean_model_name,
        forecast_timestamp =
          forecast_timestamp
      )
    )
  }
  
  if (
    is_extended_futures_variance_model_name(
      variance_model_name
    )
  ) {
    
    return(
      safe_fit_extended_futures_variance_model(
        training_residual_data =
          training_residual_data,
        variance_model_name =
          variance_model_name,
        symbol = symbol,
        mean_model_name =
          mean_model_name,
        forecast_timestamp =
          forecast_timestamp
      )
    )
  }
  
  make_futures_forecast_error(
    stage = "variance_fit",
    symbol = symbol,
    mean_model_name = mean_model_name,
    variance_model_name = variance_model_name,
    forecast_timestamp = forecast_timestamp,
    message = paste0(
      "Unsupported variance model: ",
      variance_model_name
    )
  )
}


# ============================================================
# 4. Predict any supported variance model safely
# ============================================================

safe_predict_any_futures_variance_model <- function(
    variance_model,
    original_forecast_row,
    extended_forecast_row,
    variance_model_name,
    symbol = NA_character_,
    mean_model_name = NA_character_
) {
  
  if (
    is_original_futures_variance_model_name(
      variance_model_name
    )
  ) {
    
    return(
      safe_predict_futures_variance_model(
        variance_model =
          variance_model,
        forecast_row =
          original_forecast_row,
        symbol = symbol,
        mean_model_name =
          mean_model_name,
        variance_model_name =
          variance_model_name
      )
    )
  }
  
  if (
    is_extended_futures_variance_model_name(
      variance_model_name
    )
  ) {
    
    return(
      safe_predict_extended_futures_variance_model(
        variance_model =
          variance_model,
        forecast_row =
          extended_forecast_row,
        symbol = symbol,
        mean_model_name =
          mean_model_name,
        variance_model_name =
          variance_model_name
      )
    )
  }
  
  make_futures_forecast_error(
    stage = "variance_prediction",
    symbol = symbol,
    mean_model_name = mean_model_name,
    variance_model_name = variance_model_name,
    forecast_timestamp =
      extended_forecast_row[[
        "CompetitionTimestampCT"
      ]][[1]],
    message = paste0(
      "Unsupported variance model: ",
      variance_model_name
    )
  )
}


# ============================================================
# 5. Create repeated error rows
# ============================================================

make_combined_futures_variance_error_rows <- function(
    variance_model_names,
    symbol,
    forecast_timestamp,
    forecast_index,
    training_observations,
    mean_model_name,
    actual_return,
    mean_forecast = NA_real_,
    mean_fit_succeeded = FALSE,
    mean_prediction_succeeded = FALSE,
    error_stage,
    error_message
) {
  
  bind_rows(
    lapply(
      variance_model_names,
      function(
    variance_model_name
      ) {
        
        make_extended_futures_variance_error_row(
          symbol = symbol,
          forecast_timestamp =
            forecast_timestamp,
          forecast_index =
            forecast_index,
          training_observations =
            training_observations,
          mean_model_name =
            mean_model_name,
          variance_model_name =
            variance_model_name,
          actual_return =
            actual_return,
          mean_forecast =
            mean_forecast,
          mean_fit_succeeded =
            mean_fit_succeeded,
          mean_prediction_succeeded =
            mean_prediction_succeeded,
          variance_fit_succeeded =
            FALSE,
          variance_prediction_succeeded =
            FALSE,
          error_stage =
            error_stage,
          error_message =
            error_message
        )
      }
    )
  )
}


# ============================================================
# 6. One origin, all requested variance models
# ============================================================

forecast_one_futures_combined_multi_variance_origin <- function(
    symbol_data,
    forecast_index,
    mean_model_name,
    variance_model_names =
      get_combined_futures_variance_model_names(
        include_garch = TRUE
      )
) {
  
  forecast_index <-
    as.integer(
      forecast_index
    )
  
  if (
    forecast_index <= 2L ||
    forecast_index >
    nrow(
      symbol_data
    )
  ) {
    stop(
      "forecast_index is outside the valid range.",
      call. = FALSE
    )
  }
  
  variance_model_names <-
    as.character(
      variance_model_names
    )
  
  valid_variance_names <-
    get_combined_futures_variance_model_names(
      include_garch = TRUE
    )
  
  invalid_variance_names <-
    setdiff(
      variance_model_names,
      valid_variance_names
    )
  
  if (
    length(
      invalid_variance_names
    ) > 0L
  ) {
    stop(
      paste0(
        "Unsupported combined variance model(s): ",
        paste(
          invalid_variance_names,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  training_data <-
    symbol_data[
      seq_len(
        forecast_index - 1L
      ),
      ,
      drop = FALSE
    ]
  
  forecast_row <-
    symbol_data[
      forecast_index,
      ,
      drop = FALSE
    ]
  
  validate_futures_forecast_split(
    training_data = training_data,
    forecast_row = forecast_row
  )
  
  symbol <-
    forecast_row[["Symbol"]][[1]]
  
  forecast_timestamp <-
    forecast_row[[
      "CompetitionTimestampCT"
    ]][[1]]
  
  actual_return <-
    forecast_row[[
      "CandidateLogReturn"
    ]][[1]]
  
  training_observations <-
    nrow(
      training_data
    )
  
  fitted_mean_model <-
    tryCatch(
      {
        fit_any_futures_mean_model(
          data = training_data,
          model_name = mean_model_name
        )
      },
      error = function(
    error_object
      ) {
        make_futures_forecast_error(
          stage = "mean_fit",
          symbol = symbol,
          mean_model_name =
            mean_model_name,
          forecast_timestamp =
            forecast_timestamp,
          message =
            conditionMessage(
              error_object
            )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      fitted_mean_model
    )
  ) {
    
    return(
      make_combined_futures_variance_error_rows(
        variance_model_names =
          variance_model_names,
        symbol = symbol,
        forecast_timestamp =
          forecast_timestamp,
        forecast_index =
          forecast_index,
        training_observations =
          training_observations,
        mean_model_name =
          mean_model_name,
        actual_return =
          actual_return,
        mean_fit_succeeded =
          FALSE,
        mean_prediction_succeeded =
          FALSE,
        error_stage =
          fitted_mean_model[["Stage"]],
        error_message =
          fitted_mean_model[[
            "ErrorMessage"
          ]]
      )
    )
  }
  
  mean_forecast <-
    tryCatch(
      {
        prediction <-
          predict_any_futures_mean_model(
            model = fitted_mean_model,
            new_data = forecast_row
          )
        
        prediction <-
          as.numeric(
            prediction[[1]]
          )
        
        if (
          length(
            prediction
          ) != 1L ||
          !is.finite(
            prediction
          )
        ) {
          stop(
            "Mean forecast is invalid."
          )
        }
        
        prediction
      },
      error = function(
    error_object
      ) {
        make_futures_forecast_error(
          stage = "mean_prediction",
          symbol = symbol,
          mean_model_name =
            mean_model_name,
          forecast_timestamp =
            forecast_timestamp,
          message =
            conditionMessage(
              error_object
            )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      mean_forecast
    )
  ) {
    
    return(
      make_combined_futures_variance_error_rows(
        variance_model_names =
          variance_model_names,
        symbol = symbol,
        forecast_timestamp =
          forecast_timestamp,
        forecast_index =
          forecast_index,
        training_observations =
          training_observations,
        mean_model_name =
          mean_model_name,
        actual_return =
          actual_return,
        mean_fit_succeeded =
          TRUE,
        mean_prediction_succeeded =
          FALSE,
        error_stage =
          mean_forecast[["Stage"]],
        error_message =
          mean_forecast[[
            "ErrorMessage"
          ]]
      )
    )
  }
  
  training_residual_data <-
    tryCatch(
      {
        build_any_futures_training_residual_data(
          training_data =
            training_data,
          fitted_mean_model =
            fitted_mean_model
        )
      },
      error = function(
    error_object
      ) {
        make_futures_forecast_error(
          stage =
            "residual_construction",
          symbol = symbol,
          mean_model_name =
            mean_model_name,
          forecast_timestamp =
            forecast_timestamp,
          message =
            conditionMessage(
              error_object
            )
        )
      }
    )
  
  if (
    is_futures_forecast_error(
      training_residual_data
    )
  ) {
    
    return(
      make_combined_futures_variance_error_rows(
        variance_model_names =
          variance_model_names,
        symbol = symbol,
        forecast_timestamp =
          forecast_timestamp,
        forecast_index =
          forecast_index,
        training_observations =
          training_observations,
        mean_model_name =
          mean_model_name,
        actual_return =
          actual_return,
        mean_forecast =
          mean_forecast,
        mean_fit_succeeded =
          TRUE,
        mean_prediction_succeeded =
          TRUE,
        error_stage =
          training_residual_data[[
            "Stage"
          ]],
        error_message =
          training_residual_data[[
            "ErrorMessage"
          ]]
      )
    )
  }
  
  original_variance_forecast_row <-
    add_futures_forecast_residual_lags(
      forecast_row =
        forecast_row,
      training_residual_data =
        training_residual_data
    )
  
  mean_error <-
    actual_return -
    mean_forecast
  
  realized_squared_innovation <-
    mean_error^2
  
  result_rows <-
    vector(
      mode = "list",
      length = length(
        variance_model_names
      )
    )
  
  names(
    result_rows
  ) <-
    variance_model_names
  
  for (
    variance_model_name in
    variance_model_names
  ) {
    
    fitted_variance_model <-
      safe_fit_any_futures_variance_model(
        training_residual_data =
          training_residual_data,
        variance_model_name =
          variance_model_name,
        symbol = symbol,
        mean_model_name =
          mean_model_name,
        forecast_timestamp =
          forecast_timestamp
      )
    
    if (
      is_futures_forecast_error(
        fitted_variance_model
      )
    ) {
      
      result_rows[[variance_model_name]] <-
        make_extended_futures_variance_error_row(
          symbol = symbol,
          forecast_timestamp =
            forecast_timestamp,
          forecast_index =
            forecast_index,
          training_observations =
            training_observations,
          mean_model_name =
            mean_model_name,
          variance_model_name =
            variance_model_name,
          actual_return =
            actual_return,
          mean_forecast =
            mean_forecast,
          mean_fit_succeeded =
            TRUE,
          mean_prediction_succeeded =
            TRUE,
          variance_fit_succeeded =
            FALSE,
          variance_prediction_succeeded =
            FALSE,
          error_stage =
            fitted_variance_model[[
              "Stage"
            ]],
          error_message =
            fitted_variance_model[[
              "ErrorMessage"
            ]]
        )
      
      next
    }
    
    variance_forecast <-
      safe_predict_any_futures_variance_model(
        variance_model =
          fitted_variance_model,
        original_forecast_row =
          original_variance_forecast_row,
        extended_forecast_row =
          forecast_row,
        variance_model_name =
          variance_model_name,
        symbol = symbol,
        mean_model_name =
          mean_model_name
      )
    
    if (
      is_futures_forecast_error(
        variance_forecast
      )
    ) {
      
      result_rows[[variance_model_name]] <-
        make_extended_futures_variance_error_row(
          symbol = symbol,
          forecast_timestamp =
            forecast_timestamp,
          forecast_index =
            forecast_index,
          training_observations =
            training_observations,
          mean_model_name =
            mean_model_name,
          variance_model_name =
            variance_model_name,
          actual_return =
            actual_return,
          mean_forecast =
            mean_forecast,
          mean_fit_succeeded =
            TRUE,
          mean_prediction_succeeded =
            TRUE,
          variance_fit_succeeded =
            TRUE,
          variance_prediction_succeeded =
            FALSE,
          error_stage =
            variance_forecast[[
              "Stage"
            ]],
          error_message =
            variance_forecast[[
              "ErrorMessage"
            ]]
        )
      
      next
    }
    
    variance_error <-
      realized_squared_innovation -
      variance_forecast
    
    volatility_forecast <-
      sqrt(
        variance_forecast
      )
    
    standardized_innovation <-
      mean_error /
      volatility_forecast
    
    result_rows[[variance_model_name]] <-
      tibble(
        Symbol = symbol,
        CompetitionTimestampCT =
          forecast_timestamp,
        ForecastIndex =
          forecast_index,
        TrainingObservations =
          training_observations,
        MeanModel =
          mean_model_name,
        VarianceModel =
          variance_model_name,
        ActualLogReturn =
          actual_return,
        MeanForecast =
          mean_forecast,
        VarianceForecast =
          variance_forecast,
        VolatilityForecast =
          volatility_forecast,
        MeanForecastError =
          mean_error,
        SquaredMeanForecastError =
          realized_squared_innovation,
        VarianceForecastError =
          variance_error,
        VarianceSquaredForecastError =
          variance_error^2,
        VarianceAbsoluteForecastError =
          abs(
            variance_error
          ),
        StandardizedInnovation =
          standardized_innovation,
        MeanFitSucceeded =
          TRUE,
        MeanPredictionSucceeded =
          TRUE,
        VarianceFitSucceeded =
          TRUE,
        VariancePredictionSucceeded =
          TRUE,
        ErrorStage =
          NA_character_,
        ErrorMessage =
          NA_character_
      )
  }
  
  bind_rows(
    result_rows
  )
}


# ============================================================
# 7. Expanding-origin combined variance runner
# ============================================================

run_futures_combined_multi_variance_expanding_origin <- function(
    data,
    symbol,
    mean_model_name,
    variance_model_names =
      get_combined_futures_variance_model_names(
        include_garch = TRUE
      ),
    min_training_observations =
      DEFAULT_FUTURES_OOS_MIN_TRAINING_OBSERVATIONS,
    verbose = TRUE
) {
  
  symbol_data <-
    prepare_futures_forecasting_symbol_data(
      data = data,
      symbol = symbol
    )
  
  forecast_indices <-
    get_futures_expanding_origin_indices(
      data = symbol_data,
      min_training_observations =
        min_training_observations
    )
  
  result_rows <-
    vector(
      mode = "list",
      length = length(
        forecast_indices
      )
    )
  
  for (
    iteration_index in
    seq_along(
      forecast_indices
    )
  ) {
    
    if (
      verbose &&
      (
        iteration_index == 1L ||
        iteration_index %% 100L == 0L ||
        iteration_index ==
        length(
          forecast_indices
        )
      )
    ) {
      
      cat(
        "Combined variance OOS:",
        symbol,
        mean_model_name,
        iteration_index,
        "of",
        length(
          forecast_indices
        ),
        "-",
        length(
          variance_model_names
        ),
        "variance models\n"
      )
    }
    
    result_rows[[iteration_index]] <-
      forecast_one_futures_combined_multi_variance_origin(
        symbol_data =
          symbol_data,
        forecast_index =
          forecast_indices[[
            iteration_index
          ]],
        mean_model_name =
          mean_model_name,
        variance_model_names =
          variance_model_names
      )
  }
  
  bind_rows(
    result_rows
  )
}


# ============================================================
# 8. Validation
# ============================================================

stopifnot(
  length(
    get_futures_variance_model_specs()[[
      "ModelName"
    ]]
  ) ==
    16L
)

stopifnot(
  length(
    get_extended_futures_variance_model_names(
      include_garch = TRUE
    )
  ) ==
    7L
)

stopifnot(
  length(
    get_combined_futures_variance_model_names(
      include_garch = TRUE
    )
  ) ==
    23L
)

FUTURES_COMBINED_MULTI_VARIANCE_FORECASTING_VALIDATION <-
  TRUE