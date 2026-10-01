# ============================================================
# futures_models.R
# ============================================================
#
# Purpose:
#
# Reusable modeling functions for the eight competition futures:
#
#   CL
#   GC
#   SR3
#   ZN
#   KE
#   HO
#   ZW
#   NQ
#
# This script is designed for the final competition-aligned
# futures return panel constructed in Notebook 35.
#
# The current final return-data design is:
#
#   - returns calculated from Bloomberg Close prices;
#   - no Friday-to-Sunday weekend return;
#   - valid competition return intervals of 30, 90, and
#     480 minutes;
#   - maximum accepted price staleness of 120 minutes;
#   - generic-futures roll-affected returns excluded;
#   - aligned modeling panel contains all eight futures at
#     each retained timestamp.
#
# This script provides:
#
#   1. Futures model-data validation.
#   2. Competition time and seasonality feature engineering.
#   3. Lagged return features.
#   4. Candidate conditional-mean model specifications.
#   5. Linear conditional-mean model fitting.
#   6. Hurdle conditional-mean model fitting.
#   7. Mean-model prediction.
#   8. Mean residual construction.
#   9. Candidate conditional-variance model specifications.
#  10. Gamma GLM variance-model fitting.
#  11. Conditional variance prediction.
#  12. Standardized innovation construction.
#  13. Basic model summaries and diagnostics.
#  14. Helpers for fitting candidate models by future.
#
# This script DOES NOT:
#
#   - choose the final mean model;
#   - choose the final variance model;
#   - estimate cross-future correlations;
#   - run expanding-origin out-of-sample forecasting;
#   - estimate competitor-futures dependence;
#   - create production CSV files.
#
# Those tasks are handled by later notebooks and scripts.
#
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})


# ============================================================
# 2. Futures symbols
# ============================================================

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


# ============================================================
# 3. Competition day levels
# ============================================================

FUTURES_COMPETITION_DAY_LEVELS <- c(
  "Sunday",
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday"
)


# ============================================================
# 4. Competition return clock levels
# ============================================================

FUTURES_COMPETITION_CLOCK_LEVELS <- c(
  "00:30",
  "08:30",
  "09:00",
  "09:30",
  "10:00",
  "10:30",
  "11:00",
  "11:30",
  "12:00",
  "12:30",
  "13:00",
  "13:30",
  "14:00",
  "14:30",
  "15:00",
  "15:30",
  "16:00",
  "17:30",
  "18:00",
  "18:30",
  "19:00",
  "19:30",
  "20:00",
  "20:30",
  "21:00",
  "21:30",
  "22:00",
  "22:30",
  "23:00",
  "23:30",
  "24:00"
)


# ============================================================
# 5. Competition interval levels
# ============================================================

FUTURES_COMPETITION_INTERVAL_LEVELS <- c(
  "30",
  "90",
  "480"
)


# ============================================================
# 6. Default modeling return column
# ============================================================

DEFAULT_FUTURES_RETURN_COLUMN <- "CandidateLogReturn"


# ============================================================
# 7. Default variance floor
# ============================================================

DEFAULT_FUTURES_VARIANCE_FLOOR <- 1e-12


# ============================================================
# 8. Validate futures symbol
# ============================================================

validate_futures_model_symbol <- function(
    symbol
) {
  
  if (
    length(
      symbol
    ) != 1L
  ) {
    stop(
      "symbol must contain exactly one value.",
      call. = FALSE
    )
  }
  
  if (
    !symbol %in%
    FUTURES_MODEL_SYMBOLS
  ) {
    stop(
      paste0(
        "Unknown futures symbol: ",
        symbol,
        ". Expected one of: ",
        paste(
          FUTURES_MODEL_SYMBOLS,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 9. Validate modeling data
# ============================================================

validate_futures_model_data <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN,
    require_all_symbols = FALSE,
    require_aligned_panel = FALSE
) {
  
  if (
    !is.data.frame(
      data
    )
  ) {
    stop(
      "data must be a data frame.",
      call. = FALSE
    )
  }
  
  required_columns <- c(
    "Symbol",
    "CompetitionTimestampCT",
    "ScheduleDay",
    "CompetitionClockCT",
    "CompetitionMinuteOffset",
    "CompetitionIntervalMinutes",
    return_col
  )
  
  missing_columns <- setdiff(
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
        "Modeling data are missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  invalid_symbols <- setdiff(
    unique(
      data$Symbol
    ),
    FUTURES_MODEL_SYMBOLS
  )
  
  if (
    length(
      invalid_symbols
    ) > 0L
  ) {
    stop(
      paste0(
        "Unexpected futures symbols found: ",
        paste(
          invalid_symbols,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    require_all_symbols
  ) {
    
    missing_symbols <- setdiff(
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
          "The modeling data are missing futures: ",
          paste(
            missing_symbols,
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }
  }
  
  if (
    any(
      is.na(
        data[[return_col]]
      )
    )
  ) {
    stop(
      paste0(
        return_col,
        " contains missing values."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        data[[return_col]]
      )
    )
  ) {
    stop(
      paste0(
        return_col,
        " contains non-finite values."
      ),
      call. = FALSE
    )
  }
  
  valid_intervals <- c(
    30,
    90,
    480
  )
  
  invalid_intervals <- setdiff(
    unique(
      data$CompetitionIntervalMinutes
    ),
    valid_intervals
  )
  
  if (
    length(
      invalid_intervals
    ) > 0L
  ) {
    stop(
      paste0(
        "Unexpected competition interval lengths found: ",
        paste(
          invalid_intervals,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  duplicate_rows <- data |>
    count(
      Symbol,
      CompetitionTimestampCT,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    )
  
  if (
    nrow(
      duplicate_rows
    ) > 0L
  ) {
    stop(
      "Duplicate Symbol-CompetitionTimestampCT rows were found.",
      call. = FALSE
    )
  }
  
  if (
    require_aligned_panel
  ) {
    
    alignment_check <- data |>
      count(
        CompetitionTimestampCT,
        name = "FuturesAvailable"
      )
    
    if (
      any(
        alignment_check$FuturesAvailable !=
        length(
          FUTURES_MODEL_SYMBOLS
        )
      )
    ) {
      stop(
        "The requested aligned panel does not contain all eight futures at every timestamp.",
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 10. Convert competition clock to minutes
# ============================================================

competition_clock_to_minutes <- function(
    clock_value
) {
  
  clock_value <- as.character(
    clock_value
  )
  
  result <- rep(
    NA_real_,
    length(
      clock_value
    )
  )
  
  valid <- !is.na(
    clock_value
  )
  
  if (
    any(
      valid
    )
  ) {
    
    split_values <- strsplit(
      clock_value[
        valid
      ],
      ":",
      fixed = TRUE
    )
    
    parsed <- vapply(
      split_values,
      function(
    parts
      ) {
        
        if (
          length(
            parts
          ) != 2L
        ) {
          return(
            NA_real_
          )
        }
        
        hour_value <- suppressWarnings(
          as.numeric(
            parts[[1]]
          )
        )
        
        minute_value <- suppressWarnings(
          as.numeric(
            parts[[2]]
          )
        )
        
        if (
          is.na(
            hour_value
          ) ||
          is.na(
            minute_value
          )
        ) {
          return(
            NA_real_
          )
        }
        
        hour_value * 60 +
          minute_value
      },
    numeric(1)
    )
    
    result[
      valid
    ] <- parsed
  }
  
  result
}


# ============================================================
# 11. Add core futures modeling features
# ============================================================

add_futures_model_features <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col
  )
  
  result <- data |>
    arrange(
      Symbol,
      CompetitionTimestampCT
    ) |>
    group_by(
      Symbol
    ) |>
    mutate(
      ObservationWithinSymbol =
        row_number(),
      TotalObservationsWithinSymbol =
        n(),
      Trend01 =
        if_else(
          TotalObservationsWithinSymbol > 1L,
          (
            ObservationWithinSymbol - 1
          ) /
            (
              TotalObservationsWithinSymbol - 1
            ),
          0
        ),
      ReturnValue =
        .data[[return_col]],
      LagReturn1 =
        lag(
          ReturnValue,
          n = 1L
        ),
      LagReturn2 =
        lag(
          ReturnValue,
          n = 2L
        ),
      LagAbsoluteReturn1 =
        lag(
          abs(
            ReturnValue
          ),
          n = 1L
        ),
      LagSquaredReturn1 =
        lag(
          ReturnValue^2,
          n = 1L
        ),
      IsZeroReturn =
        ReturnValue == 0,
      PreviousZeroReturn =
        lag(
          IsZeroReturn,
          n = 1L
        ),
      PreviousNonzeroReturn =
        lag(
          !IsZeroReturn,
          n = 1L
        )
    ) |>
    ungroup() |>
    mutate(
      DayFactor =
        factor(
          ScheduleDay,
          levels =
            FUTURES_COMPETITION_DAY_LEVELS
        ),
      ClockFactor =
        factor(
          CompetitionClockCT,
          levels =
            FUTURES_COMPETITION_CLOCK_LEVELS
        ),
      IntervalFactor =
        factor(
          as.character(
            CompetitionIntervalMinutes
          ),
          levels =
            FUTURES_COMPETITION_INTERVAL_LEVELS
        ),
      IsThirtyMinute =
        CompetitionIntervalMinutes == 30,
      IsNinetyMinute =
        CompetitionIntervalMinutes == 90,
      IsOvernight =
        CompetitionIntervalMinutes == 480,
      ClockMinutes =
        competition_clock_to_minutes(
          CompetitionClockCT
        ),
      ClockFraction =
        ClockMinutes /
        1440,
      SinTime =
        sin(
          2 *
            pi *
            ClockFraction
        ),
      CosTime =
        cos(
          2 *
            pi *
            ClockFraction
        ),
      SinTime2 =
        sin(
          4 *
            pi *
            ClockFraction
        ),
      CosTime2 =
        cos(
          4 *
            pi *
            ClockFraction
        ),
      LogIntervalMinutes =
        log(
          CompetitionIntervalMinutes
        ),
      SqrtIntervalMinutes =
        sqrt(
          CompetitionIntervalMinutes
        ),
      IsMonday =
        ScheduleDay == "Monday",
      IsTuesday =
        ScheduleDay == "Tuesday",
      IsWednesday =
        ScheduleDay == "Wednesday",
      IsThursday =
        ScheduleDay == "Thursday",
      IsFriday =
        ScheduleDay == "Friday",
      IsSunday =
        ScheduleDay == "Sunday"
    )
  
  result
}


# ============================================================
# 12. Prepare one future for modeling
# ============================================================

prepare_single_future_model_data <- function(
    data,
    symbol,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_symbol(
    symbol
  )
  
  result <- data |>
    filter(
      Symbol == symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  if (
    nrow(
      result
    ) == 0L
  ) {
    stop(
      paste0(
        "No observations were found for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  add_futures_model_features(
    data = result,
    return_col = return_col
  )
}


# ============================================================
# 13. Build a formula from a response and RHS
# ============================================================

build_futures_formula <- function(
    response,
    rhs
) {
  
  if (
    length(
      response
    ) != 1L ||
    length(
      rhs
    ) != 1L
  ) {
    stop(
      "response and rhs must each contain exactly one string.",
      call. = FALSE
    )
  }
  
  stats::as.formula(
    paste(
      response,
      "~",
      rhs
    )
  )
}


# ============================================================
# 14. Candidate linear mean-model specifications
# ============================================================

get_futures_linear_mean_model_specs <- function() {
  
  tibble(
    ModelName = c(
      "constant",
      "linear_trend",
      "interval",
      "day",
      "harmonic_time",
      "harmonic_time2",
      "day_interval",
      "harmonic_interval",
      "trend_interval",
      "lag1",
      "lag12",
      "lag1_interval",
      "seasonal_compact",
      "seasonal_compact_lag1",
      "clock_factor",
      "clock_interval"
    ),
    RHS = c(
      "1",
      "Trend01",
      "IntervalFactor",
      "DayFactor",
      "SinTime + CosTime",
      "SinTime + CosTime + SinTime2 + CosTime2",
      "DayFactor + IntervalFactor",
      "SinTime + CosTime + IntervalFactor",
      "Trend01 + IntervalFactor",
      "LagReturn1",
      "LagReturn1 + LagReturn2",
      "LagReturn1 + IntervalFactor",
      "DayFactor + IntervalFactor + SinTime + CosTime",
      "DayFactor + IntervalFactor + SinTime + CosTime + LagReturn1",
      "ClockFactor",
      "ClockFactor + IntervalFactor"
    )
  )
}


# ============================================================
# 15. Candidate hurdle mean-model specifications
# ============================================================

get_futures_hurdle_mean_model_specs <- function() {
  
  tibble(
    ModelName = c(
      "hurdle_constant",
      "hurdle_interval",
      "hurdle_seasonal",
      "hurdle_persistence",
      "hurdle_seasonal_persistence"
    ),
    ProbabilityRHS = c(
      "1",
      "IntervalFactor",
      "DayFactor + IntervalFactor + SinTime + CosTime",
      "IntervalFactor + PreviousNonzeroReturn",
      "DayFactor + IntervalFactor + SinTime + CosTime + PreviousNonzeroReturn"
    ),
    ConditionalMeanRHS = c(
      "1",
      "IntervalFactor",
      "DayFactor + IntervalFactor + SinTime + CosTime",
      "IntervalFactor + LagReturn1",
      "DayFactor + IntervalFactor + SinTime + CosTime + LagReturn1"
    )
  )
}


# ============================================================
# 16. Get one linear mean-model specification
# ============================================================

get_futures_linear_mean_model_spec <- function(
    model_name
) {
  
  specs <- get_futures_linear_mean_model_specs()
  
  match_row <- specs |>
    filter(
      ModelName == model_name
    )
  
  if (
    nrow(
      match_row
    ) != 1L
  ) {
    stop(
      paste0(
        "Unknown linear futures mean model: ",
        model_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  match_row
}


# ============================================================
# 17. Get one hurdle mean-model specification
# ============================================================

get_futures_hurdle_mean_model_spec <- function(
    model_name
) {
  
  specs <- get_futures_hurdle_mean_model_specs()
  
  match_row <- specs |>
    filter(
      ModelName == model_name
    )
  
  if (
    nrow(
      match_row
    ) != 1L
  ) {
    stop(
      paste0(
        "Unknown hurdle futures mean model: ",
        model_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  match_row
}


# ============================================================
# 18. Determine variables required by a formula
# ============================================================

get_formula_variables <- function(
    formula_object
) {
  
  all.vars(
    formula_object
  )
}


# ============================================================
# 19. Complete-case data for a formula
# ============================================================

get_complete_formula_data <- function(
    data,
    formula_object
) {
  
  required_variables <- get_formula_variables(
    formula_object
  )
  
  missing_variables <- setdiff(
    required_variables,
    names(
      data
    )
  )
  
  if (
    length(
      missing_variables
    ) > 0L
  ) {
    stop(
      paste0(
        "Data are missing variables required by the model formula: ",
        paste(
          missing_variables,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  complete_index <- stats::complete.cases(
    data[
      required_variables
    ]
  )
  
  data[
    complete_index,
    ,
    drop = FALSE
  ]
}


# ============================================================
# 20. Fit one linear mean model
# ============================================================

fit_futures_linear_mean_model <- function(
    data,
    model_name,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  model_data <- add_futures_model_features(
    data = data,
    return_col = return_col
  )
  
  spec <- get_futures_linear_mean_model_spec(
    model_name
  )
  
  formula_object <- build_futures_formula(
    response = "ReturnValue",
    rhs = spec$RHS[[1]]
  )
  
  estimation_data <- get_complete_formula_data(
    data = model_data,
    formula_object = formula_object
  )
  
  if (
    nrow(
      estimation_data
    ) == 0L
  ) {
    stop(
      paste0(
        "No complete observations remain for mean model ",
        model_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  fit <- stats::lm(
    formula = formula_object,
    data = estimation_data
  )
  
  result <- list(
    ModelType = "linear",
    ModelName = model_name,
    ReturnColumn = return_col,
    Formula = formula_object,
    Fit = fit,
    EstimationData = estimation_data,
    Observations = nrow(
      estimation_data
    )
  )
  
  class(
    result
  ) <- c(
    "futures_mean_model",
    "list"
  )
  
  result
}


# ============================================================
# 21. Fit one hurdle mean model
# ============================================================

fit_futures_hurdle_mean_model <- function(
    data,
    model_name,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  model_data <- add_futures_model_features(
    data = data,
    return_col = return_col
  ) |>
    mutate(
      NonzeroIndicator =
        as.integer(
          ReturnValue != 0
        )
    )
  
  spec <- get_futures_hurdle_mean_model_spec(
    model_name
  )
  
  probability_formula <- build_futures_formula(
    response = "NonzeroIndicator",
    rhs = spec$ProbabilityRHS[[1]]
  )
  
  conditional_mean_formula <- build_futures_formula(
    response = "ReturnValue",
    rhs = spec$ConditionalMeanRHS[[1]]
  )
  
  probability_data <- get_complete_formula_data(
    data = model_data,
    formula_object = probability_formula
  )
  
  conditional_mean_data <- model_data |>
    filter(
      ReturnValue != 0
    )
  
  conditional_mean_data <- get_complete_formula_data(
    data = conditional_mean_data,
    formula_object = conditional_mean_formula
  )
  
  factor_variables <- intersect(
    c(
      "DayFactor",
      "ClockFactor",
      "IntervalFactor"
    ),
    get_formula_variables(
      conditional_mean_formula
    )
  )
  
  for (
    factor_variable in
    factor_variables
  ) {
    
    required_levels <- unique(
      as.character(
        model_data[[factor_variable]]
      )
    )
    
    estimation_levels <- unique(
      as.character(
        conditional_mean_data[[factor_variable]]
      )
    )
    
    missing_levels <- setdiff(
      required_levels,
      estimation_levels
    )
    
    if (
      length(
        missing_levels
      ) > 0L
    ) {
      stop(
        paste0(
          "Hurdle conditional-mean model ",
          model_name,
          " cannot forecast the full observed domain because ",
          factor_variable,
          " is missing estimation level(s): ",
          paste(
            missing_levels,
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }
  }
  
  if (
    nrow(
      probability_data
    ) == 0L
  ) {
    stop(
      paste0(
        "No complete observations remain for the hurdle probability model ",
        model_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    nrow(
      conditional_mean_data
    ) == 0L
  ) {
    stop(
      paste0(
        "No nonzero complete observations remain for the hurdle conditional mean model ",
        model_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  probability_fit <- stats::glm(
    formula = probability_formula,
    data = probability_data,
    family = stats::binomial(
      link = "logit"
    )
  )
  
  conditional_mean_fit <- stats::lm(
    formula = conditional_mean_formula,
    data = conditional_mean_data
  )
  
  result <- list(
    ModelType = "hurdle",
    ModelName = model_name,
    ReturnColumn = return_col,
    ProbabilityFormula = probability_formula,
    ConditionalMeanFormula = conditional_mean_formula,
    ProbabilityFit = probability_fit,
    ConditionalMeanFit = conditional_mean_fit,
    ModelData = model_data,
    ProbabilityObservations = nrow(
      probability_data
    ),
    ConditionalMeanObservations = nrow(
      conditional_mean_data
    )
  )
  
  class(
    result
  ) <- c(
    "futures_mean_model",
    "list"
  )
  
  result
}


# ============================================================
# 22. Fit any candidate mean model
# ============================================================

fit_futures_mean_model <- function(
    data,
    model_name,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  linear_names <-
    get_futures_linear_mean_model_specs()$ModelName
  
  hurdle_names <-
    get_futures_hurdle_mean_model_specs()$ModelName
  
  if (
    model_name %in%
    linear_names
  ) {
    return(
      fit_futures_linear_mean_model(
        data = data,
        model_name = model_name,
        return_col = return_col
      )
    )
  }
  
  if (
    model_name %in%
    hurdle_names
  ) {
    return(
      fit_futures_hurdle_mean_model(
        data = data,
        model_name = model_name,
        return_col = return_col
      )
    )
  }
  
  stop(
    paste0(
      "Unknown futures mean model: ",
      model_name,
      "."
    ),
    call. = FALSE
  )
}


# ============================================================
# 23. Predict from a fitted mean model
# ============================================================

predict_futures_mean_model <- function(
    model,
    new_data
) {
  if (!inherits(
    model,
    "futures_mean_model"
  )) {
    stop(
      "model must be a futures_mean_model object.",
      call. = FALSE
    )
  }
  
  required_feature_columns <-
    c(
      "Trend01",
      "DayFactor",
      "ClockFactor",
      "IntervalFactor",
      "SinTime",
      "CosTime",
      "SinTime2",
      "CosTime2",
      "LagReturn1",
      "LagReturn2",
      "PreviousNonzeroReturn"
    )
  
  features_already_available <-
    all(
      required_feature_columns %in%
        names(
          new_data
        )
    )
  
  if (
    features_already_available
  ) {
    feature_data <-
      new_data
  } else {
    feature_data <-
      add_futures_model_features(
        data =
          new_data,
        return_col =
          model$ReturnColumn
      )
  }
  
  if (
    model$ModelType ==
    "linear"
  ) {
    prediction <-
      stats::predict(
        model$Fit,
        newdata =
          feature_data
      )
    
    return(
      as.numeric(
        prediction
      )
    )
  }
  
  if (
    model$ModelType ==
    "hurdle"
  ) {
    nonzero_probability <-
      stats::predict(
        model$ProbabilityFit,
        newdata =
          feature_data,
        type =
          "response"
      )
    
    conditional_mean <-
      stats::predict(
        model$ConditionalMeanFit,
        newdata =
          feature_data
      )
    
    prediction <-
      as.numeric(
        nonzero_probability
      ) *
      as.numeric(
        conditional_mean
      )
    
    return(
      prediction
    )
  }
  
  stop(
    paste0(
      "Unsupported mean model type: ",
      model$ModelType,
      "."
    ),
    call. = FALSE
  )
}

# ============================================================
# 24. Attach fitted mean and residuals
# ============================================================

attach_futures_mean_residuals <- function(
    data,
    model,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  if (
    !inherits(
      model,
      "futures_mean_model"
    )
  ) {
    stop(
      "model must be a futures_mean_model object.",
      call. = FALSE
    )
  }
  
  feature_data <- add_futures_model_features(
    data = data,
    return_col = return_col
  )
  
  prediction <- predict_futures_mean_model(
    model = model,
    new_data = data
  )
  
  feature_data |>
    mutate(
      ConditionalMean =
        prediction,
      MeanResidual =
        ReturnValue -
        ConditionalMean,
      AbsoluteMeanResidual =
        abs(
          MeanResidual
        ),
      SquaredMeanResidual =
        MeanResidual^2
    )
}


# ============================================================
# 25. Mean-model in-sample statistics
# ============================================================

summarize_futures_mean_model <- function(
    model
) {
  
  if (
    !inherits(
      model,
      "futures_mean_model"
    )
  ) {
    stop(
      "model must be a futures_mean_model object.",
      call. = FALSE
    )
  }
  
  if (
    model$ModelType == "linear"
  ) {
    
    fitted_values <- stats::fitted(
      model$Fit
    )
    
    actual_values <-
      model$EstimationData$ReturnValue
    
    residual_values <-
      actual_values -
      fitted_values
    
    return(
      tibble(
        ModelName =
          model$ModelName,
        ModelType =
          model$ModelType,
        Observations =
          length(
            actual_values
          ),
        MeanActual =
          mean(
            actual_values
          ),
        MeanPrediction =
          mean(
            fitted_values
          ),
        RMSE =
          sqrt(
            mean(
              residual_values^2
            )
          ),
        MAE =
          mean(
            abs(
              residual_values
            )
          ),
        AIC =
          stats::AIC(
            model$Fit
          ),
        BIC =
          stats::BIC(
            model$Fit
          )
      )
    )
  }
  
  if (
    model$ModelType == "hurdle"
  ) {
    
    prediction <- predict_futures_mean_model(
      model = model,
      new_data = model$ModelData
    )
    
    actual_values <-
      model$ModelData$ReturnValue
    
    valid <- is.finite(
      prediction
    ) &
      is.finite(
        actual_values
      )
    
    residual_values <-
      actual_values[
        valid
      ] -
      prediction[
        valid
      ]
    
    combined_aic <-
      stats::AIC(
        model$ProbabilityFit
      ) +
      stats::AIC(
        model$ConditionalMeanFit
      )
    
    combined_bic <-
      stats::BIC(
        model$ProbabilityFit
      ) +
      stats::BIC(
        model$ConditionalMeanFit
      )
    
    return(
      tibble(
        ModelName =
          model$ModelName,
        ModelType =
          model$ModelType,
        Observations =
          sum(
            valid
          ),
        MeanActual =
          mean(
            actual_values[
              valid
            ]
          ),
        MeanPrediction =
          mean(
            prediction[
              valid
            ]
          ),
        RMSE =
          sqrt(
            mean(
              residual_values^2
            )
          ),
        MAE =
          mean(
            abs(
              residual_values
            )
          ),
        AIC =
          combined_aic,
        BIC =
          combined_bic
      )
    )
  }
  
  stop(
    "Unsupported futures mean-model type.",
    call. = FALSE
  )
}


# ============================================================
# 26. Candidate mean-model names
# ============================================================

get_all_futures_mean_model_names <- function() {
  
  c(
    get_futures_linear_mean_model_specs()$ModelName,
    get_futures_hurdle_mean_model_specs()$ModelName
  )
}


# ============================================================
# 27. Fit all mean candidates for one future
# ============================================================

fit_all_futures_mean_candidates <- function(
    data,
    symbol,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN,
    model_names = get_all_futures_mean_model_names()
) {
  
  validate_futures_model_symbol(
    symbol
  )
  
  symbol_data <- data |>
    filter(
      Symbol == symbol
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
        "No observations found for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  result <- vector(
    mode = "list",
    length = length(
      model_names
    )
  )
  
  names(
    result
  ) <- model_names
  
  for (
    model_name in
    model_names
  ) {
    
    result[[model_name]] <- tryCatch(
      fit_futures_mean_model(
        data = symbol_data,
        model_name = model_name,
        return_col = return_col
      ),
      error = function(
    error_object
      ) {
        structure(
          list(
            ModelName = model_name,
            ErrorMessage =
              conditionMessage(
                error_object
              )
          ),
          class =
            "futures_model_error"
        )
      }
    )
  }
  
  result
}


# ============================================================
# 28. Summarize fitted mean-candidate list
# ============================================================

summarize_futures_mean_candidates <- function(
    model_list
) {
  
  summary_rows <- lapply(
    names(
      model_list
    ),
    function(
    model_name
    ) {
      
      current_model <-
        model_list[[model_name]]
      
      if (
        inherits(
          current_model,
          "futures_model_error"
        )
      ) {
        return(
          tibble(
            ModelName =
              current_model$ModelName,
            ModelType =
              NA_character_,
            Observations =
              NA_integer_,
            MeanActual =
              NA_real_,
            MeanPrediction =
              NA_real_,
            RMSE =
              NA_real_,
            MAE =
              NA_real_,
            AIC =
              NA_real_,
            BIC =
              NA_real_,
            FitSucceeded =
              FALSE,
            ErrorMessage =
              current_model$ErrorMessage
          )
        )
      }
      
      summarize_futures_mean_model(
        current_model
      ) |>
        mutate(
          FitSucceeded =
            TRUE,
          ErrorMessage =
            NA_character_
        )
    }
  )
  
  bind_rows(
    summary_rows
  )
}


# ============================================================
# 29. Prepare data for conditional variance modeling
# ============================================================

prepare_futures_variance_data <- function(
    data,
    residual_col = "MeanResidual",
    variance_floor =
      DEFAULT_FUTURES_VARIANCE_FLOOR
) {
  
  if (
    !residual_col %in%
    names(
      data
    )
  ) {
    stop(
      paste0(
        "Residual column not found: ",
        residual_col,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    variance_floor <= 0
  ) {
    stop(
      "variance_floor must be positive.",
      call. = FALSE
    )
  }
  
  result <- data |>
    arrange(
      Symbol,
      CompetitionTimestampCT
    ) |>
    group_by(
      Symbol
    ) |>
    mutate(
      VarianceResidual =
        .data[[residual_col]],
      SquaredResidual =
        VarianceResidual^2,
      SquaredResidualAdjusted =
        pmax(
          SquaredResidual,
          variance_floor
        ),
      LagAbsoluteResidual1 =
        lag(
          abs(
            VarianceResidual
          ),
          n = 1L
        ),
      LagSquaredResidual1 =
        lag(
          SquaredResidual,
          n = 1L
        ),
      LagSquaredResidual2 =
        lag(
          SquaredResidual,
          n = 2L
        )
    ) |>
    ungroup()
  
  result
}


# ============================================================
# 30. Candidate variance-model specifications
# ============================================================

get_futures_variance_model_specs <- function() {
  
  tibble(
    ModelName = c(
      "constant",
      "interval",
      "day",
      "harmonic_time",
      "harmonic_time2",
      "day_interval",
      "harmonic_interval",
      "trend",
      "trend_interval",
      "lag_abs_residual",
      "lag_squared_residual",
      "lag_squared_residual12",
      "seasonal_compact",
      "seasonal_lag_squared",
      "clock_factor",
      "clock_interval"
    ),
    RHS = c(
      "1",
      "IntervalFactor",
      "DayFactor",
      "SinTime + CosTime",
      "SinTime + CosTime + SinTime2 + CosTime2",
      "DayFactor + IntervalFactor",
      "SinTime + CosTime + IntervalFactor",
      "Trend01",
      "Trend01 + IntervalFactor",
      "LagAbsoluteResidual1",
      "LagSquaredResidual1",
      "LagSquaredResidual1 + LagSquaredResidual2",
      "DayFactor + IntervalFactor + SinTime + CosTime",
      "DayFactor + IntervalFactor + SinTime + CosTime + LagSquaredResidual1",
      "ClockFactor",
      "ClockFactor + IntervalFactor"
    )
  )
}


# ============================================================
# 31. Get one variance-model specification
# ============================================================

get_futures_variance_model_spec <- function(
    model_name
) {
  
  specs <- get_futures_variance_model_specs()
  
  match_row <- specs |>
    filter(
      ModelName == model_name
    )
  
  if (
    nrow(
      match_row
    ) != 1L
  ) {
    stop(
      paste0(
        "Unknown futures variance model: ",
        model_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  match_row
}


# ============================================================
# 32. Fit Gamma GLM conditional variance model
# ============================================================

fit_futures_variance_model <- function(
    data,
    model_name,
    residual_col = "MeanResidual",
    variance_floor =
      DEFAULT_FUTURES_VARIANCE_FLOOR
) {
  
  variance_data <- prepare_futures_variance_data(
    data = data,
    residual_col = residual_col,
    variance_floor = variance_floor
  )
  
  spec <- get_futures_variance_model_spec(
    model_name
  )
  
  formula_object <- build_futures_formula(
    response =
      "SquaredResidualAdjusted",
    rhs =
      spec$RHS[[1]]
  )
  
  estimation_data <- get_complete_formula_data(
    data = variance_data,
    formula_object = formula_object
  )
  
  if (
    nrow(
      estimation_data
    ) == 0L
  ) {
    stop(
      paste0(
        "No complete observations remain for variance model ",
        model_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  fit <- stats::glm(
    formula = formula_object,
    data = estimation_data,
    family = stats::Gamma(
      link = "log"
    )
  )
  
  result <- list(
    ModelType =
      "gamma_glm_variance",
    ModelName =
      model_name,
    ResidualColumn =
      residual_col,
    VarianceFloor =
      variance_floor,
    Formula =
      formula_object,
    Fit =
      fit,
    EstimationData =
      estimation_data,
    Observations =
      nrow(
        estimation_data
      )
  )
  
  class(
    result
  ) <- c(
    "futures_variance_model",
    "list"
  )
  
  result
}


# ============================================================
# 33. Predict conditional variance
# ============================================================

predict_futures_variance_model <- function(
    model,
    new_data
) {
  
  if (
    !inherits(
      model,
      "futures_variance_model"
    )
  ) {
    stop(
      "model must be a futures_variance_model object.",
      call. = FALSE
    )
  }
  
  required_model_variables <-
    get_formula_variables(
      model$Formula
    )
  
  required_predictor_columns <-
    setdiff(
      required_model_variables,
      "SquaredResidualAdjusted"
    )
  
  predictors_already_available <-
    all(
      required_predictor_columns %in%
        names(
          new_data
        )
    )
  
  if (
    predictors_already_available
  ) {
    
    variance_data <-
      new_data
    
  } else {
    
    if (
      !model$ResidualColumn %in%
      names(
        new_data
      )
    ) {
      
      missing_predictor_columns <-
        setdiff(
          required_predictor_columns,
          names(
            new_data
          )
        )
      
      stop(
        paste0(
          "Variance prediction data are missing required predictor columns: ",
          paste(
            missing_predictor_columns,
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }
    
    variance_data <-
      prepare_futures_variance_data(
        data =
          new_data,
        residual_col =
          model$ResidualColumn,
        variance_floor =
          model$VarianceFloor
      )
  }
  
  prediction <- stats::predict(
    model$Fit,
    newdata =
      variance_data,
    type =
      "response"
  )
  
  prediction <- pmax(
    as.numeric(
      prediction
    ),
    model$VarianceFloor
  )
  
  prediction
}


# ============================================================
# 34. Attach conditional variance and standardized residual
# ============================================================

attach_futures_variance_predictions <- function(
    data,
    model
) {
  
  conditional_variance <-
    predict_futures_variance_model(
      model = model,
      new_data = data
    )
  
  data |>
    mutate(
      ConditionalVariance =
        conditional_variance,
      ConditionalSD =
        sqrt(
          ConditionalVariance
        ),
      StandardizedResidual =
        MeanResidual /
        ConditionalSD
    )
}


# ============================================================
# 35. Variance-model in-sample statistics
# ============================================================

summarize_futures_variance_model <- function(
    model
) {
  
  if (
    !inherits(
      model,
      "futures_variance_model"
    )
  ) {
    stop(
      "model must be a futures_variance_model object.",
      call. = FALSE
    )
  }
  
  fitted_variance <- stats::fitted(
    model$Fit
  )
  
  actual_squared <-
    model$EstimationData$SquaredResidual
  
  valid <- is.finite(
    fitted_variance
  ) &
    is.finite(
      actual_squared
    )
  
  forecast_error <-
    actual_squared[
      valid
    ] -
    fitted_variance[
      valid
    ]
  
  tibble(
    ModelName =
      model$ModelName,
    ModelType =
      model$ModelType,
    Observations =
      sum(
        valid
      ),
    MeanSquaredResidual =
      mean(
        actual_squared[
          valid
        ]
      ),
    MeanConditionalVariance =
      mean(
        fitted_variance[
          valid
        ]
      ),
    VarianceRMSE =
      sqrt(
        mean(
          forecast_error^2
        )
      ),
    VarianceMAE =
      mean(
        abs(
          forecast_error
        )
      ),
    AIC =
      stats::AIC(
        model$Fit
      ),
    BIC =
      stats::BIC(
        model$Fit
      )
  )
}


# ============================================================
# 36. Fit all variance candidates
# ============================================================

fit_all_futures_variance_candidates <- function(
    data,
    residual_col = "MeanResidual",
    variance_floor =
      DEFAULT_FUTURES_VARIANCE_FLOOR,
    model_names =
      get_futures_variance_model_specs()$ModelName
) {
  
  result <- vector(
    mode = "list",
    length = length(
      model_names
    )
  )
  
  names(
    result
  ) <- model_names
  
  for (
    model_name in
    model_names
  ) {
    
    result[[model_name]] <- tryCatch(
      fit_futures_variance_model(
        data = data,
        model_name = model_name,
        residual_col = residual_col,
        variance_floor = variance_floor
      ),
      error = function(
    error_object
      ) {
        structure(
          list(
            ModelName =
              model_name,
            ErrorMessage =
              conditionMessage(
                error_object
              )
          ),
          class =
            "futures_model_error"
        )
      }
    )
  }
  
  result
}


# ============================================================
# 37. Summarize variance-candidate list
# ============================================================

summarize_futures_variance_candidates <- function(
    model_list
) {
  
  summary_rows <- lapply(
    names(
      model_list
    ),
    function(
    model_name
    ) {
      
      current_model <-
        model_list[[model_name]]
      
      if (
        inherits(
          current_model,
          "futures_model_error"
        )
      ) {
        return(
          tibble(
            ModelName =
              current_model$ModelName,
            ModelType =
              NA_character_,
            Observations =
              NA_integer_,
            MeanSquaredResidual =
              NA_real_,
            MeanConditionalVariance =
              NA_real_,
            VarianceRMSE =
              NA_real_,
            VarianceMAE =
              NA_real_,
            AIC =
              NA_real_,
            BIC =
              NA_real_,
            FitSucceeded =
              FALSE,
            ErrorMessage =
              current_model$ErrorMessage
          )
        )
      }
      
      summarize_futures_variance_model(
        current_model
      ) |>
        mutate(
          FitSucceeded =
            TRUE,
          ErrorMessage =
            NA_character_
        )
    }
  )
  
  bind_rows(
    summary_rows
  )
}


# ============================================================
# 38. Basic return summary by symbol
# ============================================================

summarize_futures_returns_by_symbol <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col
  )
  
  data |>
    group_by(
      Symbol
    ) |>
    summarise(
      Observations =
        n(),
      Mean =
        mean(
          .data[[return_col]]
        ),
      SD =
        sd(
          .data[[return_col]]
        ),
      Median =
        median(
          .data[[return_col]]
        ),
      MeanAbsoluteReturn =
        mean(
          abs(
            .data[[return_col]]
          )
        ),
      ZeroReturnRate =
        mean(
          .data[[return_col]] == 0
        ),
      Minimum =
        min(
          .data[[return_col]]
        ),
      Maximum =
        max(
          .data[[return_col]]
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 39. Return summary by competition day
# ============================================================

summarize_futures_returns_by_day <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col
  )
  
  data |>
    group_by(
      Symbol,
      ScheduleDay
    ) |>
    summarise(
      Observations =
        n(),
      MeanReturn =
        mean(
          .data[[return_col]]
        ),
      SDReturn =
        sd(
          .data[[return_col]]
        ),
      MeanAbsoluteReturn =
        mean(
          abs(
            .data[[return_col]]
          )
        ),
      ZeroReturnRate =
        mean(
          .data[[return_col]] == 0
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 40. Return summary by competition clock
# ============================================================

summarize_futures_returns_by_clock <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col
  )
  
  data |>
    group_by(
      Symbol,
      CompetitionClockCT
    ) |>
    summarise(
      Observations =
        n(),
      MeanReturn =
        mean(
          .data[[return_col]]
        ),
      SDReturn =
        sd(
          .data[[return_col]]
        ),
      MeanAbsoluteReturn =
        mean(
          abs(
            .data[[return_col]]
          )
        ),
      ZeroReturnRate =
        mean(
          .data[[return_col]] == 0
        ),
      .groups =
        "drop"
    ) |>
    mutate(
      ClockMinutes =
        competition_clock_to_minutes(
          CompetitionClockCT
        )
    ) |>
    arrange(
      Symbol,
      ClockMinutes
    )
}


# ============================================================
# 41. Return summary by interval length
# ============================================================

summarize_futures_returns_by_interval <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col
  )
  
  data |>
    group_by(
      Symbol,
      CompetitionIntervalMinutes
    ) |>
    summarise(
      Observations =
        n(),
      MeanReturn =
        mean(
          .data[[return_col]]
        ),
      SDReturn =
        sd(
          .data[[return_col]]
        ),
      MeanAbsoluteReturn =
        mean(
          abs(
            .data[[return_col]]
          )
        ),
      ZeroReturnRate =
        mean(
          .data[[return_col]] == 0
        ),
      .groups =
        "drop"
    ) |>
    arrange(
      Symbol,
      CompetitionIntervalMinutes
    )
}


# ============================================================
# 42. Return summary by day and clock
# ============================================================

summarize_futures_returns_by_day_clock <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col
  )
  
  data |>
    group_by(
      Symbol,
      ScheduleDay,
      CompetitionClockCT
    ) |>
    summarise(
      Observations =
        n(),
      MeanReturn =
        mean(
          .data[[return_col]]
        ),
      SDReturn =
        sd(
          .data[[return_col]]
        ),
      MeanAbsoluteReturn =
        mean(
          abs(
            .data[[return_col]]
          )
        ),
      ZeroReturnRate =
        mean(
          .data[[return_col]] == 0
        ),
      .groups =
        "drop"
    ) |>
    mutate(
      DayFactor =
        factor(
          ScheduleDay,
          levels =
            FUTURES_COMPETITION_DAY_LEVELS
        ),
      ClockMinutes =
        competition_clock_to_minutes(
          CompetitionClockCT
        )
    ) |>
    arrange(
      Symbol,
      DayFactor,
      ClockMinutes
    ) |>
    select(
      -DayFactor
    )
}


# ============================================================
# 43. Residual diagnostics
# ============================================================

summarize_futures_standardized_residuals <- function(
    data,
    standardized_residual_col =
      "StandardizedResidual"
) {
  
  if (
    !standardized_residual_col %in%
    names(
      data
    )
  ) {
    stop(
      paste0(
        "Column not found: ",
        standardized_residual_col,
        "."
      ),
      call. = FALSE
    )
  }
  
  data |>
    group_by(
      Symbol
    ) |>
    summarise(
      Observations =
        sum(
          is.finite(
            .data[[
              standardized_residual_col
            ]]
          )
        ),
      Mean =
        mean(
          .data[[
            standardized_residual_col
          ]],
          na.rm = TRUE
        ),
      SD =
        sd(
          .data[[
            standardized_residual_col
          ]],
          na.rm = TRUE
        ),
      Median =
        median(
          .data[[
            standardized_residual_col
          ]],
          na.rm = TRUE
        ),
      MeanAbsoluteValue =
        mean(
          abs(
            .data[[
              standardized_residual_col
            ]]
          ),
          na.rm = TRUE
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 44. Lag-1 autocorrelation helper
# ============================================================

safe_lag1_correlation <- function(
    x
) {
  
  x <- as.numeric(
    x
  )
  
  if (
    length(
      x
    ) < 3L
  ) {
    return(
      NA_real_
    )
  }
  
  current_values <-
    x[
      2:length(
        x
      )
    ]
  
  lagged_values <-
    x[
      1:(
        length(
          x
        ) - 1L
      )
    ]
  
  valid <- is.finite(
    current_values
  ) &
    is.finite(
      lagged_values
    )
  
  if (
    sum(
      valid
    ) < 3L
  ) {
    return(
      NA_real_
    )
  }
  
  if (
    stats::sd(
      current_values[
        valid
      ]
    ) == 0 ||
    stats::sd(
      lagged_values[
        valid
      ]
    ) == 0
  ) {
    return(
      NA_real_
    )
  }
  
  stats::cor(
    current_values[
      valid
    ],
    lagged_values[
      valid
    ]
  )
}


# ============================================================
# 45. Autocorrelation summary by future
# ============================================================

summarize_futures_autocorrelation <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col
  )
  
  data |>
    arrange(
      Symbol,
      CompetitionTimestampCT
    ) |>
    group_by(
      Symbol
    ) |>
    summarise(
      ReturnLag1Correlation =
        safe_lag1_correlation(
          .data[[return_col]]
        ),
      AbsoluteReturnLag1Correlation =
        safe_lag1_correlation(
          abs(
            .data[[return_col]]
          )
        ),
      SquaredReturnLag1Correlation =
        safe_lag1_correlation(
          .data[[return_col]]^2
        ),
      .groups =
        "drop"
    )
}


# ============================================================
# 46. Check factor-level compatibility
# ============================================================

validate_futures_feature_levels <- function(
    data
) {
  
  unexpected_days <- setdiff(
    unique(
      as.character(
        data$ScheduleDay
      )
    ),
    FUTURES_COMPETITION_DAY_LEVELS
  )
  
  unexpected_clocks <- setdiff(
    unique(
      as.character(
        data$CompetitionClockCT
      )
    ),
    FUTURES_COMPETITION_CLOCK_LEVELS
  )
  
  unexpected_intervals <- setdiff(
    unique(
      as.character(
        data$CompetitionIntervalMinutes
      )
    ),
    FUTURES_COMPETITION_INTERVAL_LEVELS
  )
  
  if (
    length(
      unexpected_days
    ) > 0L
  ) {
    stop(
      paste0(
        "Unexpected ScheduleDay values: ",
        paste(
          unexpected_days,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    length(
      unexpected_clocks
    ) > 0L
  ) {
    stop(
      paste0(
        "Unexpected CompetitionClockCT values: ",
        paste(
          unexpected_clocks,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    length(
      unexpected_intervals
    ) > 0L
  ) {
    stop(
      paste0(
        "Unexpected CompetitionIntervalMinutes values: ",
        paste(
          unexpected_intervals,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 47. Basic futures modeling-data validation bundle
# ============================================================

validate_final_futures_modeling_panel <- function(
    data,
    return_col = DEFAULT_FUTURES_RETURN_COLUMN,
    expected_symbols =
      FUTURES_MODEL_SYMBOLS
) {
  
  validate_futures_model_data(
    data = data,
    return_col = return_col,
    require_all_symbols = TRUE,
    require_aligned_panel = TRUE
  )
  
  validate_futures_feature_levels(
    data
  )
  
  observed_symbols <- sort(
    unique(
      data$Symbol
    )
  )
  
  expected_symbols <- sort(
    expected_symbols
  )
  
  if (
    !identical(
      observed_symbols,
      expected_symbols
    )
  ) {
    stop(
      "Observed futures symbols do not exactly match the expected symbol set.",
      call. = FALSE
    )
  }
  
  if (
    "CandidateReturnUsable" %in%
    names(
      data
    )
  ) {
    if (
      !all(
        data$CandidateReturnUsable
      )
    ) {
      stop(
        "The final modeling panel contains a return marked unusable.",
        call. = FALSE
      )
    }
  }
  
  if (
    "RollAffected" %in%
    names(
      data
    )
  ) {
    if (
      any(
        data$RollAffected
      )
    ) {
      stop(
        "The final modeling panel contains a roll-affected return.",
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}

# ============================================================
# Extended conventional futures model families
# ============================================================
#
# These models are intentionally separate from the original
# futures mean and variance candidate systems.
#
# Existing functions such as:
#   get_all_futures_mean_model_names()
#   get_futures_variance_model_specs()
#
# are NOT modified here.
#
# This preserves the validated Notebook 37, 38, and 40
# candidate universes while adding conventional models for
# later OOS evaluation and Notebook 41 selection.
# ============================================================


# ============================================================
# 1. Generic return extraction
# ============================================================

extract_futures_return_vector <- function(
    data
) {
  
  if (
    "ReturnValue" %in%
    names(
      data
    )
  ) {
    
    return_values <-
      data[["ReturnValue"]]
    
  } else if (
    "CandidateLogReturn" %in%
    names(
      data
    )
  ) {
    
    return_values <-
      data[["CandidateLogReturn"]]
    
  } else {
    
    stop(
      paste0(
        "data must contain ReturnValue or CandidateLogReturn."
      ),
      call. = FALSE
    )
  }
  
  return_values <-
    as.numeric(
      return_values
    )
  
  if (
    length(
      return_values
    ) == 0L
  ) {
    stop(
      "Return vector is empty.",
      call. = FALSE
    )
  }
  
  if (
    !all(
      is.finite(
        return_values
      )
    )
  ) {
    stop(
      "Return vector contains nonfinite values.",
      call. = FALSE
    )
  }
  
  return_values
}


# ============================================================
# 2. Extended mean-model names
# ============================================================

get_extended_futures_mean_model_names <- function() {
  
  c(
    "trimmed_mean_05",
    "trimmed_mean_10",
    "ar1",
    "ar2",
    "ma1",
    "ma2",
    "arma11",
    "arma_aic"
  )
}


# ============================================================
# 3. ARMA order lookup
# ============================================================

get_fixed_futures_arma_order <- function(
    model_name
) {
  
  order_map <-
    list(
      ar1 =
        c(
          1L,
          0L,
          0L
        ),
      ar2 =
        c(
          2L,
          0L,
          0L
        ),
      ma1 =
        c(
          0L,
          0L,
          1L
        ),
      ma2 =
        c(
          0L,
          0L,
          2L
        ),
      arma11 =
        c(
          1L,
          0L,
          1L
        )
    )
  
  if (
    !model_name %in%
    names(
      order_map
    )
  ) {
    stop(
      paste0(
        "Unknown fixed ARMA model: ",
        model_name
      ),
      call. = FALSE
    )
  }
  
  order_map[[model_name]]
}


# ============================================================
# 4. AIC-selected ARMA candidate grid
# ============================================================

get_futures_arma_aic_candidate_grid <- function() {
  
  data.frame(
    P =
      c(
        0L,
        1L,
        2L,
        0L,
        0L,
        1L
      ),
    D =
      rep(
        0L,
        6L
      ),
    Q =
      c(
        0L,
        0L,
        0L,
        1L,
        2L,
        1L
      ),
    stringsAsFactors =
      FALSE
  )
}


# ============================================================
# 5. Fit one ARMA specification
# ============================================================

fit_one_futures_arma_model <- function(
    return_values,
    order
) {
  
  if (
    length(
      order
    ) !=
    3L
  ) {
    stop(
      "ARMA order must have length 3.",
      call. = FALSE
    )
  }
  
  stats::arima(
    x =
      return_values,
    order =
      order,
    include.mean =
      TRUE,
    method =
      "ML"
  )
}


# ============================================================
# 6. Fit AIC-selected ARMA model
# ============================================================

fit_futures_arma_aic_model <- function(
    return_values
) {
  
  candidate_grid <-
    get_futures_arma_aic_candidate_grid()
  
  candidate_fits <-
    vector(
      mode =
        "list",
      length =
        nrow(
          candidate_grid
        )
    )
  
  candidate_aic <-
    rep(
      Inf,
      nrow(
        candidate_grid
      )
    )
  
  candidate_errors <-
    rep(
      NA_character_,
      nrow(
        candidate_grid
      )
    )
  
  for (
    candidate_index in
    seq_len(
      nrow(
        candidate_grid
      )
    )
  ) {
    
    current_order <-
      c(
        candidate_grid[["P"]][[candidate_index]],
        candidate_grid[["D"]][[candidate_index]],
        candidate_grid[["Q"]][[candidate_index]]
      )
    
    current_fit <-
      tryCatch(
        fit_one_futures_arma_model(
          return_values =
            return_values,
          order =
            current_order
        ),
        error =
          function(
    error_condition
          ) {
            error_condition
          }
      )
    
    if (
      inherits(
        current_fit,
        "error"
      )
    ) {
      
      candidate_errors[[candidate_index]] <-
        conditionMessage(
          current_fit
        )
      
      next
    }
    
    current_aic <-
      current_fit[["aic"]]
    
    if (
      !is.finite(
        current_aic
      )
    ) {
      
      candidate_errors[[candidate_index]] <-
        "Nonfinite AIC."
      
      next
    }
    
    candidate_fits[[candidate_index]] <-
      current_fit
    
    candidate_aic[[candidate_index]] <-
      current_aic
  }
  
  if (
    !any(
      is.finite(
        candidate_aic
      )
    )
  ) {
    stop(
      paste0(
        "All ARMA AIC candidates failed. Errors: ",
        paste(
          unique(
            stats::na.omit(
              candidate_errors
            )
          ),
          collapse =
            " | "
        )
      ),
      call. = FALSE
    )
  }
  
  best_index <-
    which.min(
      candidate_aic
    )
  
  list(
    Fit =
      candidate_fits[[best_index]],
    Order =
      c(
        candidate_grid[["P"]][[best_index]],
        candidate_grid[["D"]][[best_index]],
        candidate_grid[["Q"]][[best_index]]
      ),
    AIC =
      candidate_aic[[best_index]],
    CandidateGrid =
      candidate_grid,
    CandidateAIC =
      candidate_aic,
    CandidateErrors =
      candidate_errors
  )
}


# ============================================================
# 7. Fit extended mean model
# ============================================================

fit_extended_futures_mean_model <- function(
    data,
    model_name
) {
  
  valid_model_names <-
    get_extended_futures_mean_model_names()
  
  if (
    !model_name %in%
    valid_model_names
  ) {
    stop(
      paste0(
        "Unknown extended futures mean model: ",
        model_name
      ),
      call. = FALSE
    )
  }
  
  return_values <-
    extract_futures_return_vector(
      data
    )
  
  training_observations <-
    length(
      return_values
    )
  
  if (
    model_name ==
    "trimmed_mean_05"
  ) {
    
    fitted_mean <-
      mean(
        return_values,
        trim =
          0.05
      )
    
    return(
      structure(
        list(
          ModelName =
            model_name,
          ModelType =
            "trimmed_mean",
          Trim =
            0.05,
          Mean =
            fitted_mean,
          TrainingReturns =
            return_values,
          TrainingObservations =
            training_observations
        ),
        class =
          "extended_futures_mean_model"
      )
    )
  }
  
  if (
    model_name ==
    "trimmed_mean_10"
  ) {
    
    fitted_mean <-
      mean(
        return_values,
        trim =
          0.10
      )
    
    return(
      structure(
        list(
          ModelName =
            model_name,
          ModelType =
            "trimmed_mean",
          Trim =
            0.10,
          Mean =
            fitted_mean,
          TrainingReturns =
            return_values,
          TrainingObservations =
            training_observations
        ),
        class =
          "extended_futures_mean_model"
      )
    )
  }
  
  if (
    model_name ==
    "arma_aic"
  ) {
    
    selected_model <-
      fit_futures_arma_aic_model(
        return_values =
          return_values
      )
    
    return(
      structure(
        list(
          ModelName =
            model_name,
          ModelType =
            "arma_aic",
          Fit =
            selected_model[["Fit"]],
          Order =
            selected_model[["Order"]],
          AIC =
            selected_model[["AIC"]],
          CandidateGrid =
            selected_model[["CandidateGrid"]],
          CandidateAIC =
            selected_model[["CandidateAIC"]],
          CandidateErrors =
            selected_model[["CandidateErrors"]],
          TrainingReturns =
            return_values,
          TrainingObservations =
            training_observations
        ),
        class =
          "extended_futures_mean_model"
      )
    )
  }
  
  fixed_order <-
    get_fixed_futures_arma_order(
      model_name
    )
  
  fixed_fit <-
    fit_one_futures_arma_model(
      return_values =
        return_values,
      order =
        fixed_order
    )
  
  structure(
    list(
      ModelName =
        model_name,
      ModelType =
        "fixed_arma",
      Fit =
        fixed_fit,
      Order =
        fixed_order,
      TrainingReturns =
        return_values,
      TrainingObservations =
        training_observations
    ),
    class =
      "extended_futures_mean_model"
  )
}


# ============================================================
# 8. Predict extended mean model
# ============================================================

predict_extended_futures_mean_model <- function(
    model,
    new_data = NULL
) {
  
  if (
    !inherits(
      model,
      "extended_futures_mean_model"
    )
  ) {
    stop(
      "model must be an extended_futures_mean_model.",
      call. = FALSE
    )
  }
  
  if (
    model[["ModelType"]] ==
    "trimmed_mean"
  ) {
    
    prediction <-
      as.numeric(
        model[["Mean"]]
      )
    
  } else {
    
    arma_prediction <-
      stats::predict(
        model[["Fit"]],
        n.ahead =
          1L
      )
    
    prediction <-
      as.numeric(
        arma_prediction[["pred"]][[1]]
      )
  }
  
  if (
    length(
      prediction
    ) !=
    1L ||
    !is.finite(
      prediction
    )
  ) {
    stop(
      "Extended mean prediction is nonfinite.",
      call. = FALSE
    )
  }
  
  prediction
}


# ============================================================
# 9. In-sample fitted means for extended models
# ============================================================

get_extended_futures_mean_fitted_values <- function(
    model
) {
  
  if (
    !inherits(
      model,
      "extended_futures_mean_model"
    )
  ) {
    stop(
      "model must be an extended_futures_mean_model.",
      call. = FALSE
    )
  }
  
  training_returns <-
    model[["TrainingReturns"]]
  
  if (
    model[["ModelType"]] ==
    "trimmed_mean"
  ) {
    
    fitted_values <-
      rep(
        model[["Mean"]],
        length(
          training_returns
        )
      )
    
  } else {
    
    residuals <-
      as.numeric(
        stats::residuals(
          model[["Fit"]]
        )
      )
    
    if (
      length(
        residuals
      ) !=
      length(
        training_returns
      )
    ) {
      stop(
        "ARMA residual length does not match training returns.",
        call. = FALSE
      )
    }
    
    fitted_values <-
      training_returns -
      residuals
  }
  
  as.numeric(
    fitted_values
  )
}


# ============================================================
# 10. Attach residuals from extended mean model
# ============================================================

attach_extended_futures_mean_residuals <- function(
    data,
    model
) {
  
  return_values <-
    extract_futures_return_vector(
      data
    )
  
  fitted_values <-
    get_extended_futures_mean_fitted_values(
      model
    )
  
  if (
    length(
      fitted_values
    ) !=
    length(
      return_values
    )
  ) {
    stop(
      "Extended mean fitted-value length mismatch.",
      call. = FALSE
    )
  }
  
  output_data <-
    data
  
  output_data[["MeanFitted"]] <-
    fitted_values
  
  output_data[["MeanResidual"]] <-
    return_values -
    fitted_values
  
  output_data[["SquaredMeanResidual"]] <-
    output_data[["MeanResidual"]]^2
  
  output_data
}


# ============================================================
# 11. rugarch availability
# ============================================================

futures_rugarch_available <- function() {
  
  requireNamespace(
    "rugarch",
    quietly =
      TRUE
  )
}


# ============================================================
# 12. Extended variance-model names
# ============================================================

get_extended_futures_variance_model_names <- function(
    include_garch = TRUE
) {
  
  model_names <-
    c(
      "rolling_var_20",
      "rolling_var_50",
      "rolling_var_100",
      "ewma_094",
      "ewma_097"
    )
  
  if (
    include_garch &&
    futures_rugarch_available()
  ) {
    
    model_names <-
      c(
        model_names,
        "garch11_normal",
        "garch11_student"
      )
  }
  
  model_names
}


# ============================================================
# 13. Generic residual extraction
# ============================================================

extract_futures_mean_residual_vector <- function(
    data
) {
  
  if (
    !"MeanResidual" %in%
    names(
      data
    )
  ) {
    stop(
      "data must contain MeanResidual.",
      call. = FALSE
    )
  }
  
  residual_values <-
    as.numeric(
      data[["MeanResidual"]]
    )
  
  residual_values <-
    residual_values[
      is.finite(
        residual_values
      )
    ]
  
  if (
    length(
      residual_values
    ) <
    5L
  ) {
    stop(
      "Too few finite mean residuals.",
      call. = FALSE
    )
  }
  
  residual_values
}


# ============================================================
# 14. Rolling-variance window lookup
# ============================================================

get_extended_futures_rolling_variance_window <- function(
    model_name
) {
  
  window_map <-
    c(
      rolling_var_20 =
        20L,
      rolling_var_50 =
        50L,
      rolling_var_100 =
        100L
    )
  
  if (
    !model_name %in%
    names(
      window_map
    )
  ) {
    stop(
      paste0(
        "Unknown rolling variance model: ",
        model_name
      ),
      call. = FALSE
    )
  }
  
  as.integer(
    window_map[[model_name]]
  )
}


# ============================================================
# 15. EWMA lambda lookup
# ============================================================

get_extended_futures_ewma_lambda <- function(
    model_name
) {
  
  lambda_map <-
    c(
      ewma_094 =
        0.94,
      ewma_097 =
        0.97
    )
  
  if (
    !model_name %in%
    names(
      lambda_map
    )
  ) {
    stop(
      paste0(
        "Unknown EWMA model: ",
        model_name
      ),
      call. = FALSE
    )
  }
  
  as.numeric(
    lambda_map[[model_name]]
  )
}


# ============================================================
# 16. One-step EWMA variance forecast
# ============================================================

calculate_futures_ewma_variance <- function(
    residual_values,
    lambda
) {
  
  if (
    !is.numeric(
      lambda
    ) ||
    length(
      lambda
    ) !=
    1L ||
    !is.finite(
      lambda
    ) ||
    lambda <=
    0 ||
    lambda >=
    1
  ) {
    stop(
      "lambda must be strictly between 0 and 1.",
      call. = FALSE
    )
  }
  
  initial_variance <-
    stats::var(
      residual_values
    )
  
  if (
    !is.finite(
      initial_variance
    ) ||
    initial_variance <=
    0
  ) {
    stop(
      "Unable to initialize EWMA variance.",
      call. = FALSE
    )
  }
  
  current_variance <-
    initial_variance
  
  for (
    residual_index in
    seq_along(
      residual_values
    )
  ) {
    
    current_variance <-
      lambda *
      current_variance +
      (
        1 -
          lambda
      ) *
      residual_values[[residual_index]]^2
  }
  
  if (
    !is.finite(
      current_variance
    ) ||
    current_variance <=
    0
  ) {
    stop(
      "EWMA variance forecast is invalid.",
      call. = FALSE
    )
  }
  
  as.numeric(
    current_variance
  )
}


# ============================================================
# 17. Fit extended variance model
# ============================================================

fit_extended_futures_variance_model <- function(
    data,
    model_name
) {
  
  valid_model_names <-
    get_extended_futures_variance_model_names(
      include_garch =
        TRUE
    )
  
  if (
    !model_name %in%
    valid_model_names
  ) {
    
    if (
      model_name %in%
      c(
        "garch11_normal",
        "garch11_student"
      ) &&
      !futures_rugarch_available()
    ) {
      stop(
        paste0(
          "Model ",
          model_name,
          " requires rugarch, but rugarch is not installed."
        ),
        call. = FALSE
      )
    }
    
    stop(
      paste0(
        "Unknown extended futures variance model: ",
        model_name
      ),
      call. = FALSE
    )
  }
  
  residual_values <-
    extract_futures_mean_residual_vector(
      data
    )
  
  if (
    grepl(
      "^rolling_var_",
      model_name
    )
  ) {
    
    rolling_window <-
      get_extended_futures_rolling_variance_window(
        model_name
      )
    
    if (
      length(
        residual_values
      ) <
      rolling_window
    ) {
      stop(
        paste0(
          "Too few residuals for ",
          model_name,
          "."
        ),
        call. = FALSE
      )
    }
    
    rolling_residuals <-
      tail(
        residual_values,
        rolling_window
      )
    
    variance_forecast <-
      stats::var(
        rolling_residuals
      )
    
    if (
      !is.finite(
        variance_forecast
      ) ||
      variance_forecast <=
      0
    ) {
      stop(
        "Rolling variance forecast is invalid.",
        call. = FALSE
      )
    }
    
    return(
      structure(
        list(
          ModelName =
            model_name,
          ModelType =
            "rolling_variance",
          Window =
            rolling_window,
          VarianceForecast =
            as.numeric(
              variance_forecast
            ),
          TrainingObservations =
            length(
              residual_values
            )
        ),
        class =
          "extended_futures_variance_model"
      )
    )
  }
  
  if (
    grepl(
      "^ewma_",
      model_name
    )
  ) {
    
    lambda <-
      get_extended_futures_ewma_lambda(
        model_name
      )
    
    variance_forecast <-
      calculate_futures_ewma_variance(
        residual_values =
          residual_values,
        lambda =
          lambda
      )
    
    return(
      structure(
        list(
          ModelName =
            model_name,
          ModelType =
            "ewma",
          Lambda =
            lambda,
          VarianceForecast =
            variance_forecast,
          TrainingObservations =
            length(
              residual_values
            )
        ),
        class =
          "extended_futures_variance_model"
      )
    )
  }
  
  if (
    model_name %in%
    c(
      "garch11_normal",
      "garch11_student"
    )
  ) {
    
    if (
      !futures_rugarch_available()
    ) {
      stop(
        "rugarch is not installed.",
        call. = FALSE
      )
    }
    
    distribution_model <-
      if (
        model_name ==
        "garch11_normal"
      ) {
        "norm"
      } else {
        "std"
      }
    
    garch_specification <-
      rugarch::ugarchspec(
        variance.model =
          list(
            model =
              "sGARCH",
            garchOrder =
              c(
                1L,
                1L
              )
          ),
        mean.model =
          list(
            armaOrder =
              c(
                0L,
                0L
              ),
            include.mean =
              FALSE
          ),
        distribution.model =
          distribution_model
      )
    
    garch_fit <-
      rugarch::ugarchfit(
        spec =
          garch_specification,
        data =
          residual_values,
        solver =
          "hybrid"
      )
    
    convergence_code <-
      garch_fit@fit$convergence
    
    if (
      length(
        convergence_code
      ) !=
      1L ||
      !is.finite(
        convergence_code
      ) ||
      convergence_code !=
      0L
    ) {
      stop(
        paste0(
          "GARCH fit did not converge for ",
          model_name,
          "."
        ),
        call. = FALSE
      )
    }
    
    garch_forecast <-
      rugarch::ugarchforecast(
        garch_fit,
        n.ahead =
          1L
      )
    
    forecast_sigma <-
      as.numeric(
        rugarch::sigma(
          garch_forecast
        )[[1]]
      )
    
    variance_forecast <-
      forecast_sigma^2
    
    if (
      !is.finite(
        variance_forecast
      ) ||
      variance_forecast <=
      0
    ) {
      stop(
        "GARCH variance forecast is invalid.",
        call. = FALSE
      )
    }
    
    return(
      structure(
        list(
          ModelName =
            model_name,
          ModelType =
            "garch11",
          Distribution =
            distribution_model,
          Fit =
            garch_fit,
          VarianceForecast =
            variance_forecast,
          TrainingObservations =
            length(
              residual_values
            )
        ),
        class =
          "extended_futures_variance_model"
      )
    )
  }
  
  stop(
    paste0(
      "Unhandled extended variance model: ",
      model_name
    ),
    call. = FALSE
  )
}


# ============================================================
# 18. Predict extended variance model
# ============================================================

predict_extended_futures_variance_model <- function(
    model,
    new_data = NULL
) {
  
  if (
    !inherits(
      model,
      "extended_futures_variance_model"
    )
  ) {
    stop(
      "model must be an extended_futures_variance_model.",
      call. = FALSE
    )
  }
  
  variance_forecast <-
    as.numeric(
      model[["VarianceForecast"]]
    )
  
  if (
    length(
      variance_forecast
    ) !=
    1L ||
    !is.finite(
      variance_forecast
    ) ||
    variance_forecast <=
    0
  ) {
    stop(
      "Extended variance prediction is invalid.",
      call. = FALSE
    )
  }
  
  variance_forecast
}


# ============================================================
# 19. Extended-model metadata tables
# ============================================================

get_extended_futures_mean_model_table <- function() {
  
  data.frame(
    ModelName =
      get_extended_futures_mean_model_names(),
    ModelFamily =
      c(
        "trimmed_mean",
        "trimmed_mean",
        "arma",
        "arma",
        "arma",
        "arma",
        "arma",
        "arma_aic"
      ),
    stringsAsFactors =
      FALSE
  )
}


get_extended_futures_variance_model_table <- function() {
  
  model_names <-
    get_extended_futures_variance_model_names(
      include_garch =
        TRUE
    )
  
  model_family <-
    vapply(
      model_names,
      function(
    model_name
      ) {
        
        if (
          grepl(
            "^rolling_var_",
            model_name
          )
        ) {
          return(
            "rolling_variance"
          )
        }
        
        if (
          grepl(
            "^ewma_",
            model_name
          )
        ) {
          return(
            "ewma"
          )
        }
        
        if (
          grepl(
            "^garch11_",
            model_name
          )
        ) {
          return(
            "garch11"
          )
        }
        
        "unknown"
      },
    character(1)
    )
  
  data.frame(
    ModelName =
      model_names,
    ModelFamily =
      model_family,
    stringsAsFactors =
      FALSE
  )
}


# ============================================================
# 20. Extended-model validation flag
# ============================================================

stopifnot(
  identical(
    get_extended_futures_mean_model_names(),
    c(
      "trimmed_mean_05",
      "trimmed_mean_10",
      "ar1",
      "ar2",
      "ma1",
      "ma2",
      "arma11",
      "arma_aic"
    )
  )
)

stopifnot(
  all(
    c(
      "rolling_var_20",
      "rolling_var_50",
      "rolling_var_100",
      "ewma_094",
      "ewma_097"
    ) %in%
      get_extended_futures_variance_model_names(
        include_garch =
          FALSE
      )
  )
)

FUTURES_EXTENDED_MODELS_SCRIPT_VALIDATION <-
  TRUE

# ============================================================
# 48. End of futures_models.R
# ============================================================