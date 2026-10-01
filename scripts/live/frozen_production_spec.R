# ============================================================
# frozen_production_spec.R
# ============================================================
#
# Purpose:
#
#   Canonical machine-readable manifest for the frozen production
#   modeling architecture used by the live trading-competition
#   forecasting system.
#
#   This file contains NO model fitting, NO model selection,
#   NO forecasting, and NO data loading.
#
#   It records:
#
#   1. canonical component orders;
#   2. frozen mean-model selections;
#   3. frozen variance-model selections;
#   4. frozen state transformations;
#   5. frozen innovation marginal families;
#   6. frozen dependence-family architecture;
#   7. frozen arithmetic-return mapping policy;
#   8. the distinction between objects that are frozen and
#      quantities that may update during live production.
#
#   Historical estimation/model selection is research/training.
#   Live forecasting is production/inference.
#
# ============================================================


# ============================================================
# 1. Manifest version
# ============================================================

FROZEN_PRODUCTION_SPEC_VERSION <- "1.0.0"


# ============================================================
# 2. Canonical futures order
# ============================================================

FROZEN_FUTURES_ORDER <- c(
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
# 3. Canonical optimizer future order
# ============================================================

FROZEN_OPTIMIZER_FUTURE_ORDER <- c(
  "Future_1",
  "Future_2",
  "Future_3",
  "Future_4",
  "Future_5",
  "Future_6",
  "Future_7",
  "Future_8"
)


# ============================================================
# 4. Futures-to-optimizer mapping
# ============================================================

FROZEN_FUTURES_OPTIMIZER_MAPPING <- data.frame(
  Future = FROZEN_FUTURES_ORDER,
  OptimizerFuture = FROZEN_OPTIMIZER_FUTURE_ORDER,
  stringsAsFactors = FALSE
)


# ============================================================
# 5. Frozen futures mean-model selections
# ============================================================

FROZEN_FUTURES_MEAN_MODELS <- c(
  CL = "hurdle_seasonal_persistence",
  GC = "linear_trend",
  SR3 = "arma_aic",
  ZN = "ma2",
  KE = "interval",
  HO = "ar2",
  ZW = "linear_trend",
  NQ = "lag1"
)


# ============================================================
# 6. Frozen futures variance-model selections
# ============================================================

FROZEN_FUTURES_VARIANCE_MODELS <- c(
  CL = "harmonic_interval",
  GC = "garch11_normal",
  SR3 = "seasonal_lag_squared",
  ZN = "seasonal_lag_squared",
  KE = "garch11_normal",
  HO = "harmonic_interval",
  ZW = "clock_factor",
  NQ = "clock_factor"
)


# ============================================================
# 7. Canonical final-rank raw-process order
# ============================================================

FROZEN_FINAL_RANK_ORDER <- c(
  "Rank1",
  "Rank2",
  "Rank3",
  "Rank4",
  "Rank15",
  "Rank16"
)


# ============================================================
# 8. Frozen final-rank mean-model selections
# ============================================================

FROZEN_FINAL_RANK_MEAN_MODELS <- c(
  Rank1 = "hurdle_time_duration",
  Rank2 = "linear_time",
  Rank3 = "hurdle_time_duration",
  Rank4 = "linear_time",
  Rank15 = "quadratic_time",
  Rank16 = "linear_time"
)


# ============================================================
# 9. Frozen final-rank variance-model selections
# ============================================================

FROZEN_FINAL_RANK_VARIANCE_MODELS <- c(
  Rank1 = "long_interval",
  Rank2 = "elapsed_linear",
  Rank3 = "elapsed_log",
  Rank4 = "elapsed_linear",
  Rank15 = "elapsed_linear",
  Rank16 = "constant"
)


# ============================================================
# 10. Canonical final-state order
# ============================================================

FROZEN_FINAL_STATE_ORDER <- c(
  "Delta_Final_X1",
  "Delta_Final_D12",
  "Delta_Final_D23",
  "Delta_Final_D34",
  "Delta_Final_D4_15",
  "Delta_Final_D15_16"
)


# ============================================================
# 11. Frozen final-rank state transformation
# ============================================================

FROZEN_FINAL_STATE_TRANSFORM <- matrix(
  c(
    1,  0,  0,  0,  0,  0,
    1, -1,  0,  0,  0,  0,
    0,  1, -1,  0,  0,  0,
    0,  0,  1, -1,  0,  0,
    0,  0,  0,  1, -1,  0,
    0,  0,  0,  0,  1, -1
  ),
  nrow = 6L,
  byrow = TRUE,
  dimnames = list(
    FROZEN_FINAL_STATE_ORDER,
    FROZEN_FINAL_RANK_ORDER
  )
)


# ============================================================
# 12. Canonical daily raw-process order
# ============================================================

FROZEN_DAILY_RAW_ORDER <- c(
  "h1",
  "h2"
)


# ============================================================
# 13. Frozen daily mean-model selections
# ============================================================

FROZEN_DAILY_MEAN_MODELS <- c(
  Rank1 = "hurdle_persistence",
  Rank2 = "hurdle_persistence"
)


# ============================================================
# 14. Frozen daily variance-model selections
# ============================================================

FROZEN_DAILY_VARIANCE_MODELS <- c(
  Rank1 = "constant",
  Rank2 = "quadratic_progress"
)


# ============================================================
# 15. Frozen daily standardized residual correlation
# ============================================================

FROZEN_DAILY_RESIDUAL_CORRELATION <- 0.4811939


# ============================================================
# 16. Canonical daily-state order
# ============================================================

FROZEN_DAILY_STATE_ORDER <- c(
  "Delta_Daily_H1",
  "Delta_Daily_D12"
)


# ============================================================
# 17. Frozen daily-state transformation
# ============================================================

FROZEN_DAILY_STATE_TRANSFORM <- matrix(
  c(
    1,  0,
    1, -1
  ),
  nrow = 2L,
  byrow = TRUE,
  dimnames = list(
    FROZEN_DAILY_STATE_ORDER,
    FROZEN_DAILY_RAW_ORDER
  )
)


# ============================================================
# 18. Canonical full competitor raw-process order
# ============================================================

FROZEN_COMPETITOR_RAW_ORDER <- c(
  "r1",
  "r2",
  "r3",
  "r4",
  "r15",
  "r16",
  "h1",
  "h2"
)


# ============================================================
# 19. Canonical full competitor-state order
# ============================================================

FROZEN_COMPETITOR_STATE_ORDER <- c(
  FROZEN_FINAL_STATE_ORDER,
  FROZEN_DAILY_STATE_ORDER
)


# ============================================================
# 20. Frozen full 8D competitor-state transformation
# ============================================================

FROZEN_COMPETITOR_STATE_TRANSFORM <- matrix(
  0,
  nrow = 8L,
  ncol = 8L,
  dimnames = list(
    FROZEN_COMPETITOR_STATE_ORDER,
    FROZEN_COMPETITOR_RAW_ORDER
  )
)

FROZEN_COMPETITOR_STATE_TRANSFORM[
  1:6,
  1:6
] <- FROZEN_FINAL_STATE_TRANSFORM

FROZEN_COMPETITOR_STATE_TRANSFORM[
  7:8,
  7:8
] <- FROZEN_DAILY_STATE_TRANSFORM


# ============================================================
# 21. Daily-session production policy
# ============================================================

FROZEN_DAILY_ACTIVE_DAYS <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday"
)

FROZEN_DAILY_INACTIVE_DAYS <- c(
  "Sunday",
  "Friday"
)

FROZEN_DAILY_INACTIVE_STATE_POLICY <-
  "exact_zero_mean_variance_and_cross_covariance"


# ============================================================
# 22. Canonical 16D innovation component order
# ============================================================

FROZEN_INNOVATION_COMPONENT_ORDER <- c(
  FROZEN_FUTURES_ORDER,
  FROZEN_FINAL_STATE_ORDER,
  FROZEN_DAILY_STATE_ORDER
)


# ============================================================
# 23. Frozen innovation marginal families
# ============================================================

FROZEN_INNOVATION_FAMILIES <- c(
  CL = "student_t",
  GC = "ged",
  SR3 = "ged",
  ZN = "student_t",
  KE = "ged",
  HO = "skewed_student_t",
  ZW = "ged",
  NQ = "student_t",
  Delta_Final_X1 = "student_t",
  Delta_Final_D12 = "normal",
  Delta_Final_D23 = "ged",
  Delta_Final_D34 = "ged",
  Delta_Final_D4_15 = "ged",
  Delta_Final_D15_16 = "normal",
  Delta_Daily_H1 = "ged",
  Delta_Daily_D12 = "ged"
)


# ============================================================
# 24. Frozen dependence-family architecture
# ============================================================

FROZEN_DEPENDENCE_ARCHITECTURE <- data.frame(
  Block = c(
    "Futures-Futures",
    "FinalState-FinalState",
    "DailyState-DailyState",
    "Futures-FinalState",
    "Futures-DailyState",
    "FinalState-DailyState"
  ),
  Architecture = c(
    "gaussian_copula",
    "student_t_copula",
    "retained_gaussian_dependence",
    "retained_legacy_cross_system_dependence",
    "retained_legacy_cross_system_dependence",
    "retained_legacy_cross_system_dependence"
  ),
  ReopenSelectionLive = rep(
    FALSE,
    6L
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 25. Frozen joint innovation coupling policy
# ============================================================

FROZEN_JOINT_COUPLING_POLICY <- list(
  dimension = 16L,
  driver = "time_varying_latent_gaussian",
  futures_indices = 1:8,
  final_state_indices = 9:14,
  daily_state_indices = 15:16,
  futures_within_block = "gaussian_copula_exact",
  final_state_within_block = "student_t_copula_exact",
  daily_state_within_block = "retained_gaussian_dependence",
  cross_blocks = "retained_legacy_dependence_as_latent_gaussian_targets",
  psd_policy = "shrink_cross_blocks_only",
  sparse_blocks_independent = FALSE
)


# ============================================================
# 26. Frozen competitor-futures dependence architecture
# ============================================================

FROZEN_COMPETITOR_FUTURES_DEPENDENCE_POLICY <- list(
  structural_component = "holdings",
  residual_component = "historical_residual_dependence",
  total_log_cross_covariance = "holdings_plus_residual",
  holdings_formula = "B_raw_t_times_V_log_t",
  residualization_required = TRUE,
  arithmetic_conversion_required = TRUE,
  joint_psd_policy = "shrink_cross_covariance_only",
  alter_competitor_marginal_covariance = FALSE,
  alter_futures_marginal_covariance = FALSE
)


# ============================================================
# 27. Frozen futures return-unit conventions
# ============================================================

FROZEN_FUTURES_RETURN_UNITS <- list(
  model_estimation = "log_returns",
  conditional_mean_forecast = "log_returns",
  conditional_variance_forecast = "log_return_variance",
  optimizer_mu = "arithmetic_returns",
  optimizer_sigma = "arithmetic_return_covariance",
  simulator_account_returns = "arithmetic_returns"
)


# ============================================================
# 28. Frozen competitor-unit conventions
# ============================================================

FROZEN_COMPETITOR_UNITS <- list(
  final_rank_raw_processes = "log_returns",
  daily_raw_processes = "dollar_pnl",
  final_state = "log_balance_state_increments",
  daily_state = "dollar_pnl_state_increments"
)


# ============================================================
# 29. Frozen arithmetic-return mapping policy
# ============================================================

FROZEN_ARITHMETIC_MAPPING_POLICY <- list(
  mapping_family = "monotone_standardized_soft_lower_floor",
  direct_exponentiation_of_non_gaussian_innovations = FALSE,
  standardized_output_mean = 0,
  standardized_output_variance = 1,
  arithmetic_return_strictly_greater_than_minus_one = TRUE,
  preserves_monotone_order = TRUE,
  preserves_copula_rank_dependence = TRUE,
  old_off_diagonal_sigma_role = "diagnostic_benchmark_only"
)


# ============================================================
# 30. Frozen live-update policy
# ============================================================

FROZEN_LIVE_UPDATE_POLICY <- data.frame(
  Object = c(
    "candidate_model_universe",
    "mean_model_selection",
    "variance_model_selection",
    "feature_definitions",
    "return_unit_definitions",
    "state_transformations",
    "innovation_marginal_family",
    "dependence_family_selection",
    "dependence_architecture",
    "arithmetic_mapping_method",
    "optimizer_schema",
    "selected_mean_model_coefficients",
    "selected_variance_model_coefficients",
    "arma_internal_state",
    "garch_internal_state",
    "lagged_observations",
    "conditional_mean_forecasts",
    "conditional_variance_forecasts",
    "current_competitor_state",
    "remaining_horizon"
  ),
  LiveAction = c(
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "frozen",
    "may_refit_selected_model",
    "may_refit_selected_model",
    "update",
    "update",
    "update",
    "recompute",
    "recompute",
    "recompute",
    "recompute"
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 31. Canonical optimizer production files
# ============================================================

FROZEN_OPTIMIZER_INPUT_FILES <- c(
  "mu.csv",
  "Sigma.csv",
  "state_mean_competition.csv",
  "state_cov_competition.csv",
  "state_futures_cov_competition.csv",
  "margin.csv",
  "current_state.csv",
  "innovation_marginals.csv",
  "innovation_latent_correlation.csv",
  "innovation_arithmetic_mapping.csv",
  "innovation_system_metadata.csv",
  "innovation_reference_quantiles.csv"
)


# ============================================================
# 32. Canonical manifest object
# ============================================================

FROZEN_PRODUCTION_SPEC <- list(
  Version =
    FROZEN_PRODUCTION_SPEC_VERSION,
  
  FuturesOrder =
    FROZEN_FUTURES_ORDER,
  
  OptimizerFutureOrder =
    FROZEN_OPTIMIZER_FUTURE_ORDER,
  
  FuturesOptimizerMapping =
    FROZEN_FUTURES_OPTIMIZER_MAPPING,
  
  FuturesMeanModels =
    FROZEN_FUTURES_MEAN_MODELS,
  
  FuturesVarianceModels =
    FROZEN_FUTURES_VARIANCE_MODELS,
  
  FinalRankOrder =
    FROZEN_FINAL_RANK_ORDER,
  
  FinalRankMeanModels =
    FROZEN_FINAL_RANK_MEAN_MODELS,
  
  FinalRankVarianceModels =
    FROZEN_FINAL_RANK_VARIANCE_MODELS,
  
  FinalStateOrder =
    FROZEN_FINAL_STATE_ORDER,
  
  FinalStateTransform =
    FROZEN_FINAL_STATE_TRANSFORM,
  
  DailyRawOrder =
    FROZEN_DAILY_RAW_ORDER,
  
  DailyMeanModels =
    FROZEN_DAILY_MEAN_MODELS,
  
  DailyVarianceModels =
    FROZEN_DAILY_VARIANCE_MODELS,
  
  DailyResidualCorrelation =
    FROZEN_DAILY_RESIDUAL_CORRELATION,
  
  DailyStateOrder =
    FROZEN_DAILY_STATE_ORDER,
  
  DailyStateTransform =
    FROZEN_DAILY_STATE_TRANSFORM,
  
  CompetitorRawOrder =
    FROZEN_COMPETITOR_RAW_ORDER,
  
  CompetitorStateOrder =
    FROZEN_COMPETITOR_STATE_ORDER,
  
  CompetitorStateTransform =
    FROZEN_COMPETITOR_STATE_TRANSFORM,
  
  DailyActiveDays =
    FROZEN_DAILY_ACTIVE_DAYS,
  
  DailyInactiveDays =
    FROZEN_DAILY_INACTIVE_DAYS,
  
  DailyInactiveStatePolicy =
    FROZEN_DAILY_INACTIVE_STATE_POLICY,
  
  InnovationComponentOrder =
    FROZEN_INNOVATION_COMPONENT_ORDER,
  
  InnovationFamilies =
    FROZEN_INNOVATION_FAMILIES,
  
  DependenceArchitecture =
    FROZEN_DEPENDENCE_ARCHITECTURE,
  
  JointCouplingPolicy =
    FROZEN_JOINT_COUPLING_POLICY,
  
  CompetitorFuturesDependencePolicy =
    FROZEN_COMPETITOR_FUTURES_DEPENDENCE_POLICY,
  
  FuturesReturnUnits =
    FROZEN_FUTURES_RETURN_UNITS,
  
  CompetitorUnits =
    FROZEN_COMPETITOR_UNITS,
  
  ArithmeticMappingPolicy =
    FROZEN_ARITHMETIC_MAPPING_POLICY,
  
  LiveUpdatePolicy =
    FROZEN_LIVE_UPDATE_POLICY,
  
  OptimizerInputFiles =
    FROZEN_OPTIMIZER_INPUT_FILES
)


# ============================================================
# 33. Internal manifest validation
# ============================================================

stopifnot(
  length(
    FROZEN_PRODUCTION_SPEC[["FuturesOrder"]]
  ) == 8L
)

stopifnot(
  identical(
    names(
      FROZEN_PRODUCTION_SPEC[["FuturesMeanModels"]]
    ),
    FROZEN_PRODUCTION_SPEC[["FuturesOrder"]]
  )
)

stopifnot(
  identical(
    names(
      FROZEN_PRODUCTION_SPEC[["FuturesVarianceModels"]]
    ),
    FROZEN_PRODUCTION_SPEC[["FuturesOrder"]]
  )
)

stopifnot(
  length(
    FROZEN_PRODUCTION_SPEC[["FinalRankOrder"]]
  ) == 6L
)

stopifnot(
  identical(
    names(
      FROZEN_PRODUCTION_SPEC[["FinalRankMeanModels"]]
    ),
    FROZEN_PRODUCTION_SPEC[["FinalRankOrder"]]
  )
)

stopifnot(
  identical(
    names(
      FROZEN_PRODUCTION_SPEC[["FinalRankVarianceModels"]]
    ),
    FROZEN_PRODUCTION_SPEC[["FinalRankOrder"]]
  )
)

stopifnot(
  length(
    FROZEN_PRODUCTION_SPEC[["CompetitorStateOrder"]]
  ) == 8L
)

stopifnot(
  identical(
    dim(
      FROZEN_PRODUCTION_SPEC[["CompetitorStateTransform"]]
    ),
    c(
      8L,
      8L
    )
  )
)

stopifnot(
  length(
    FROZEN_PRODUCTION_SPEC[["InnovationComponentOrder"]]
  ) == 16L
)

stopifnot(
  identical(
    names(
      FROZEN_PRODUCTION_SPEC[["InnovationFamilies"]]
    ),
    FROZEN_PRODUCTION_SPEC[["InnovationComponentOrder"]]
  )
)

stopifnot(
  isFALSE(
    FROZEN_PRODUCTION_SPEC[["JointCouplingPolicy"]][["sparse_blocks_independent"]]
  )
)

stopifnot(
  identical(
    FROZEN_PRODUCTION_SPEC[["ArithmeticMappingPolicy"]][["mapping_family"]],
    "monotone_standardized_soft_lower_floor"
  )
)

cat(
  paste0(
    "Frozen production specification loaded successfully.\n",
    "Version: ",
    FROZEN_PRODUCTION_SPEC_VERSION,
    "\n",
    "Futures: ",
    length(
      FROZEN_FUTURES_ORDER
    ),
    "\n",
    "Competitor state dimensions: ",
    length(
      FROZEN_COMPETITOR_STATE_ORDER
    ),
    "\n",
    "Innovation dimensions: ",
    length(
      FROZEN_INNOVATION_COMPONENT_ORDER
    ),
    "\n"
  )
)