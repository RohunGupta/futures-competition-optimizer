# ============================================================
# innovation_dependence_utils.R
# ============================================================
# Reusable dependence-model utilities for innovation copulas.
#
# Scope:
# - selected-marginal CDF transforms to uniforms;
# - uniform transforms to Gaussian or Student-t scores;
# - Gaussian copula likelihood, fitting, simulation;
# - Student-t copula likelihood, fitting, df estimation, simulation;
# - AIC/BIC;
# - empirical and model-implied tail co-occurrence diagnostics;
# - correlation / PSD validation and explicit regularization helpers;
# - arbitrary block extraction and blockwise fitting helpers;
# - transformation of simulated uniforms back through selected marginals;
# - deterministic self-tests.
#
# This file intentionally does NOT select a dependence model.
# Notebook 49.0 fits candidate dependence models.
# Notebook 49.1 calibrates them.
# Notebook 49.2 selects the final dependence specification.
#
# This file is base-R only.
# ============================================================

INNOVATION_DEPENDENCE_UTILS_VERSION <- "1.0.0"

# ============================================================
# 1. Scalar and Matrix Validation Helpers
# ============================================================

assert_scalar_numeric <- function(
    x,
    name,
    finite = TRUE,
    lower = NULL,
    upper = NULL,
    inclusive_lower = TRUE,
    inclusive_upper = TRUE
) {
  if (
    !is.numeric(x) ||
    length(x) != 1L ||
    is.na(x)
  ) {
    stop(
      paste0(
        name,
        " must be a non-missing numeric scalar."
      ),
      call. = FALSE
    )
  }
  
  if (
    finite &&
    !is.finite(x)
  ) {
    stop(
      paste0(
        name,
        " must be finite."
      ),
      call. = FALSE
    )
  }
  
  if (
    !is.null(
      lower
    )
  ) {
    if (
      inclusive_lower
    ) {
      if (
        x <
        lower
      ) {
        stop(
          paste0(
            name,
            " must be >= ",
            lower,
            "."
          ),
          call. = FALSE
        )
      }
    } else {
      if (
        x <=
        lower
      ) {
        stop(
          paste0(
            name,
            " must be > ",
            lower,
            "."
          ),
          call. = FALSE
        )
      }
    }
  }
  
  if (
    !is.null(
      upper
    )
  ) {
    if (
      inclusive_upper
    ) {
      if (
        x >
        upper
      ) {
        stop(
          paste0(
            name,
            " must be <= ",
            upper,
            "."
          ),
          call. = FALSE
        )
      }
    } else {
      if (
        x >=
        upper
      ) {
        stop(
          paste0(
            name,
            " must be < ",
            upper,
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

assert_probability <- function(
    x,
    name = "probability",
    open_interval = TRUE
) {
  assert_scalar_numeric(
    x,
    name
  )
  
  if (
    open_interval
  ) {
    if (
      !(
        x >
        0 &&
        x <
        1
      )
    ) {
      stop(
        paste0(
          name,
          " must lie strictly between 0 and 1."
        ),
        call. = FALSE
      )
    }
  } else {
    if (
      !(
        x >=
        0 &&
        x <=
        1
      )
    ) {
      stop(
        paste0(
          name,
          " must lie between 0 and 1."
        ),
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}

assert_numeric_matrix <- function(
    x,
    name = "x",
    min_rows = 1L,
    min_cols = 1L,
    allow_na = FALSE
) {
  if (
    !is.matrix(
      x
    )
  ) {
    stop(
      paste0(
        name,
        " must be a matrix."
      ),
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(
      x
    )
  ) {
    stop(
      paste0(
        name,
        " must be numeric."
      ),
      call. = FALSE
    )
  }
  
  if (
    nrow(
      x
    ) <
    min_rows
  ) {
    stop(
      paste0(
        name,
        " must have at least ",
        min_rows,
        " rows."
      ),
      call. = FALSE
    )
  }
  
  if (
    ncol(
      x
    ) <
    min_cols
  ) {
    stop(
      paste0(
        name,
        " must have at least ",
        min_cols,
        " columns."
      ),
      call. = FALSE
    )
  }
  
  if (
    !allow_na &&
    any(
      !is.finite(
        x
      )
    )
  ) {
    stop(
      paste0(
        name,
        " must contain only finite values."
      ),
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}

as_numeric_matrix <- function(
    x,
    name = "x"
) {
  if (
    is.data.frame(
      x
    )
  ) {
    x <-
      as.matrix(
        x
      )
  }
  
  if (
    !is.matrix(
      x
    )
  ) {
    stop(
      paste0(
        name,
        " must be coercible to a numeric matrix."
      ),
      call. = FALSE
    )
  }
  
  storage.mode(
    x
  ) <-
    "double"
  
  x
}

# ============================================================
# 2. Probability Clipping and Uniform Validation
# ============================================================

clip_probabilities <- function(
    u,
    eps = 1e-10
) {
  assert_scalar_numeric(
    eps,
    "eps",
    lower = 0,
    upper = 0.5,
    inclusive_lower = FALSE,
    inclusive_upper = FALSE
  )
  
  u <-
    as.numeric(
      u
    )
  
  if (
    any(
      !is.finite(
        u
      )
    )
  ) {
    stop(
      "u must contain only finite values before clipping.",
      call. = FALSE
    )
  }
  
  pmin(
    pmax(
      u,
      eps
    ),
    1 -
      eps
  )
}

clip_uniform_matrix <- function(
    U,
    eps = 1e-10
) {
  U <-
    as_numeric_matrix(
      U,
      "U"
    )
  
  assert_numeric_matrix(
    U,
    "U"
  )
  
  clipped <-
    matrix(
      clip_probabilities(
        as.numeric(
          U
        ),
        eps =
          eps
      ),
      nrow =
        nrow(
          U
        ),
      ncol =
        ncol(
          U
        ),
      dimnames =
        dimnames(
          U
        )
    )
  
  clipped
}

validate_uniform_matrix <- function(
    U,
    strict = TRUE,
    allow_na = FALSE
) {
  U <-
    as_numeric_matrix(
      U,
      "U"
    )
  
  assert_numeric_matrix(
    U,
    "U",
    allow_na =
      allow_na
  )
  
  values <-
    as.numeric(
      U
    )
  
  if (
    allow_na
  ) {
    values <-
      values[
        is.finite(
          values
        )
      ]
  }
  
  if (
    length(
      values
    ) ==
    0L
  ) {
    stop(
      "U contains no finite values.",
      call. = FALSE
    )
  }
  
  if (
    strict
  ) {
    valid <-
      all(
        values >
          0 &
          values <
          1
      )
  } else {
    valid <-
      all(
        values >=
          0 &
          values <=
          1
      )
  }
  
  if (
    !valid
  ) {
    if (
      strict
    ) {
      stop(
        "All finite U values must lie strictly between 0 and 1.",
        call. = FALSE
      )
    } else {
      stop(
        "All finite U values must lie between 0 and 1.",
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}

# ============================================================
# 3. Correlation-Matrix Validation
# ============================================================

correlation_matrix_diagnostics <- function(
    R,
    symmetry_tolerance = 1e-10,
    diagonal_tolerance = 1e-10,
    eigen_tolerance = 1e-10
) {
  R <-
    as_numeric_matrix(
      R,
      "R"
    )
  
  assert_numeric_matrix(
    R,
    "R"
  )
  
  if (
    nrow(
      R
    ) !=
    ncol(
      R
    )
  ) {
    stop(
      "R must be square.",
      call. = FALSE
    )
  }
  
  symmetric_error <-
    max(
      abs(
        R -
          t(
            R
          )
      )
    )
  
  diagonal_error <-
    max(
      abs(
        diag(
          R
        ) -
          1
      )
    )
  
  eigenvalues <-
    eigen(
      (
        R +
          t(
            R
          )
      ) /
        2,
      symmetric =
        TRUE,
      only.values =
        TRUE
    )[["values"]]
  
  minimum_eigenvalue <-
    min(
      eigenvalues
    )
  
  maximum_eigenvalue <-
    max(
      eigenvalues
    )
  
  positive_definite <-
    minimum_eigenvalue >
    eigen_tolerance
  
  positive_semidefinite <-
    minimum_eigenvalue >=
    -eigen_tolerance
  
  condition_number <-
    if (
      positive_definite
    ) {
      maximum_eigenvalue /
        minimum_eigenvalue
    } else {
      Inf
    }
  
  off_diagonal_values <-
    R[
      row(
        R
      ) !=
        col(
          R
        )
    ]
  
  off_diagonal_in_range <-
    all(
      abs(
        off_diagonal_values
      ) <
        1 +
        1e-10
    )
  
  list(
    Dimension =
      nrow(
        R
      ),
    Symmetric_Error =
      symmetric_error,
    Diagonal_Error =
      diagonal_error,
    Minimum_Eigenvalue =
      minimum_eigenvalue,
    Maximum_Eigenvalue =
      maximum_eigenvalue,
    Positive_Definite =
      positive_definite,
    Positive_Semidefinite =
      positive_semidefinite,
    Condition_Number =
      condition_number,
    OffDiagonal_In_Range =
      off_diagonal_in_range,
    Valid_Correlation =
      symmetric_error <=
      symmetry_tolerance &&
      diagonal_error <=
      diagonal_tolerance &&
      positive_definite &&
      off_diagonal_in_range
  )
}

validate_correlation_matrix <- function(
    R,
    require_positive_definite = TRUE,
    symmetry_tolerance = 1e-10,
    diagonal_tolerance = 1e-10,
    eigen_tolerance = 1e-10
) {
  diagnostics <-
    correlation_matrix_diagnostics(
      R =
        R,
      symmetry_tolerance =
        symmetry_tolerance,
      diagonal_tolerance =
        diagonal_tolerance,
      eigen_tolerance =
        eigen_tolerance
    )
  
  if (
    diagnostics[["Symmetric_Error"]] >
    symmetry_tolerance
  ) {
    stop(
      "Correlation matrix is not symmetric within tolerance.",
      call. = FALSE
    )
  }
  
  if (
    diagnostics[["Diagonal_Error"]] >
    diagonal_tolerance
  ) {
    stop(
      "Correlation matrix diagonal is not equal to 1 within tolerance.",
      call. = FALSE
    )
  }
  
  if (
    !diagnostics[["OffDiagonal_In_Range"]]
  ) {
    stop(
      "Correlation matrix contains invalid off-diagonal values.",
      call. = FALSE
    )
  }
  
  if (
    require_positive_definite &&
    !diagnostics[["Positive_Definite"]]
  ) {
    stop(
      "Correlation matrix must be positive definite.",
      call. = FALSE
    )
  }
  
  if (
    !require_positive_definite &&
    !diagnostics[["Positive_Semidefinite"]]
  ) {
    stop(
      "Correlation matrix must be positive semidefinite.",
      call. = FALSE
    )
  }
  
  invisible(
    diagnostics
  )
}

regularize_correlation_matrix <- function(
    R,
    minimum_eigenvalue = 1e-8
) {
  assert_scalar_numeric(
    minimum_eigenvalue,
    "minimum_eigenvalue",
    lower = 0,
    inclusive_lower = FALSE
  )
  
  R <-
    as_numeric_matrix(
      R,
      "R"
    )
  
  if (
    nrow(
      R
    ) !=
    ncol(
      R
    )
  ) {
    stop(
      "R must be square.",
      call. = FALSE
    )
  }
  
  R <-
    (
      R +
        t(
          R
        )
    ) /
    2
  
  eigen_decomposition <-
    eigen(
      R,
      symmetric =
        TRUE
    )
  
  adjusted_values <-
    pmax(
      eigen_decomposition[["values"]],
      minimum_eigenvalue
    )
  
  adjusted <-
    eigen_decomposition[["vectors"]] %*%
    diag(
      adjusted_values,
      nrow =
        length(
          adjusted_values
        )
    ) %*%
    t(
      eigen_decomposition[["vectors"]]
    )
  
  scale_vector <-
    sqrt(
      diag(
        adjusted
      )
    )
  
  adjusted <-
    adjusted /
    outer(
      scale_vector,
      scale_vector
    )
  
  adjusted <-
    (
      adjusted +
        t(
          adjusted
        )
    ) /
    2
  
  diag(
    adjusted
  ) <-
    1
  
  validate_correlation_matrix(
    adjusted,
    require_positive_definite =
      TRUE
  )
  
  adjusted
}

# ============================================================
# 4. Hyperspherical Correlation Parameterization
# ============================================================

correlation_parameter_count <- function(
    dimension
) {
  if (
    !is.numeric(
      dimension
    ) ||
    length(
      dimension
    ) !=
    1L ||
    is.na(
      dimension
    ) ||
    dimension <
    1 ||
    dimension !=
    as.integer(
      dimension
    )
  ) {
    stop(
      "dimension must be a positive integer.",
      call. = FALSE
    )
  }
  
  dimension <-
    as.integer(
      dimension
    )
  
  dimension *
    (
      dimension -
        1L
    ) /
    2L
}

correlation_from_unconstrained <- function(
    theta,
    dimension
) {
  if (
    !is.numeric(
      theta
    ) ||
    any(
      !is.finite(
        theta
      )
    )
  ) {
    stop(
      "theta must be a finite numeric vector.",
      call. = FALSE
    )
  }
  
  expected_length <-
    correlation_parameter_count(
      dimension
    )
  
  if (
    length(
      theta
    ) !=
    expected_length
  ) {
    stop(
      paste0(
        "theta length must equal ",
        expected_length,
        " for dimension ",
        dimension,
        "."
      ),
      call. = FALSE
    )
  }
  
  dimension <-
    as.integer(
      dimension
    )
  
  if (
    dimension ==
    1L
  ) {
    return(
      matrix(
        1,
        nrow = 1L,
        ncol = 1L
      )
    )
  }
  
  partials <-
    tanh(
      theta
    )
  
  L <-
    matrix(
      0,
      nrow =
        dimension,
      ncol =
        dimension
    )
  
  L[
    1L,
    1L
  ] <-
    1
  
  counter <-
    1L
  
  for (
    i in
    2L:dimension
  ) {
    remaining_scale <-
      1
    
    for (
      j in
      seq_len(
        i -
        1L
      )
    ) {
      current_partial <-
        partials[counter]
      
      L[
        i,
        j
      ] <-
        current_partial *
        remaining_scale
      
      remaining_scale <-
        remaining_scale *
        sqrt(
          max(
            1 -
              current_partial^2,
            .Machine[["double.eps"]]
          )
        )
      
      counter <-
        counter +
        1L
    }
    
    L[
      i,
      i
    ] <-
      remaining_scale
  }
  
  R <-
    L %*%
    t(
      L
    )
  
  R <-
    (
      R +
        t(
          R
        )
    ) /
    2
  
  diag(
    R
  ) <-
    1
  
  validate_correlation_matrix(
    R,
    require_positive_definite =
      TRUE,
    eigen_tolerance =
      1e-12
  )
  
  R
}

unconstrained_from_correlation <- function(
    R,
    eps = 1e-10
) {
  validate_correlation_matrix(
    R,
    require_positive_definite =
      TRUE
  )
  
  assert_scalar_numeric(
    eps,
    "eps",
    lower = 0,
    upper = 0.5,
    inclusive_lower = FALSE,
    inclusive_upper = FALSE
  )
  
  dimension <-
    nrow(
      R
    )
  
  if (
    dimension ==
    1L
  ) {
    return(
      numeric(
        0L
      )
    )
  }
  
  L <-
    t(
      chol(
        R
      )
    )
  
  partials <-
    numeric(
      correlation_parameter_count(
        dimension
      )
    )
  
  counter <-
    1L
  
  for (
    i in
    2L:dimension
  ) {
    remaining_scale <-
      1
    
    for (
      j in
      seq_len(
        i -
        1L
      )
    ) {
      current_partial <-
        L[
          i,
          j
        ] /
        remaining_scale
      
      current_partial <-
        min(
          max(
            current_partial,
            -1 +
              eps
          ),
          1 -
            eps
        )
      
      partials[counter] <-
        current_partial
      
      remaining_scale <-
        remaining_scale *
        sqrt(
          max(
            1 -
              current_partial^2,
            .Machine[["double.eps"]]
          )
        )
      
      counter <-
        counter +
        1L
    }
  }
  
  atanh(
    partials
  )
}

# ============================================================
# 5. Selected-Marginal Transform Helpers
# ============================================================

resolve_distribution_cdf_function <- function(
    cdf_function = NULL
) {
  if (
    !is.null(
      cdf_function
    )
  ) {
    if (
      !is.function(
    cdf_function
      )
    ) {
      stop(
        "cdf_function must be a function when supplied.",
        call. = FALSE
      )
    }
    
    return(
      cdf_function
    )
  }
  
  if (
    !exists(
      "distribution_cdf",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "distribution_cdf is not available. Source innovation_distribution_utils.R or supply cdf_function.",
      call. = FALSE
    )
  }
  
  get(
    "distribution_cdf",
    mode = "function",
    inherits = TRUE
  )
}

resolve_distribution_quantile_function <- function(
    quantile_function = NULL
) {
  if (
    !is.null(
      quantile_function
    )
  ) {
    if (
      !is.function(
    quantile_function
      )
    ) {
      stop(
        "quantile_function must be a function when supplied.",
        call. = FALSE
      )
    }
    
    return(
      quantile_function
    )
  }
  
  if (
    !exists(
      "distribution_quantile",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "distribution_quantile is not available. Source innovation_distribution_utils.R or supply quantile_function.",
      call. = FALSE
    )
  }
  
  get(
    "distribution_quantile",
    mode = "function",
    inherits = TRUE
  )
}

validate_marginal_specification <- function(
    component_names,
    families,
    parameters
) {
  if (
    !is.character(
      component_names
    ) ||
    length(
      component_names
    ) ==
    0L ||
    anyNA(
      component_names
    ) ||
    any(
      component_names ==
      ""
    )
  ) {
    stop(
      "component_names must be a non-empty character vector without missing or blank values.",
      call. = FALSE
    )
  }
  
  if (
    anyDuplicated(
      component_names
    )
  ) {
    stop(
      "component_names must be unique.",
      call. = FALSE
    )
  }
  
  if (
    !is.character(
      families
    ) ||
    length(
      families
    ) !=
    length(
      component_names
    )
  ) {
    stop(
      "families must be a character vector aligned with component_names.",
      call. = FALSE
    )
  }
  
  if (
    !is.list(
      parameters
    ) ||
    length(
      parameters
    ) !=
    length(
      component_names
    )
  ) {
    stop(
      "parameters must be a list aligned with component_names.",
      call. = FALSE
    )
  }
  
  if (
    !is.null(
      names(
        families
      )
    )
  ) {
    if (
      !identical(
        names(
          families
        ),
        component_names
      )
    ) {
      stop(
        "Named families must have names exactly matching component_names in the same order.",
        call. = FALSE
      )
    }
  }
  
  if (
    !is.null(
      names(
        parameters
      )
    )
  ) {
    if (
      !identical(
        names(
          parameters
        ),
        component_names
      )
    ) {
      stop(
        "Named parameters must have names exactly matching component_names in the same order.",
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}

transform_marginals_to_uniforms <- function(
    X,
    families,
    parameters,
    component_names = colnames(X),
    cdf_function = NULL,
    eps = 1e-10,
    allow_na = FALSE
) {
  X <-
    as_numeric_matrix(
      X,
      "X"
    )
  
  assert_numeric_matrix(
    X,
    "X",
    allow_na =
      allow_na
  )
  
  if (
    is.null(
      component_names
    )
  ) {
    component_names <-
      paste0(
        "V",
        seq_len(
          ncol(
            X
          )
        )
      )
  }
  
  if (
    length(
      component_names
    ) !=
    ncol(
      X
    )
  ) {
    stop(
      "component_names length must equal ncol(X).",
      call. = FALSE
    )
  }
  
  validate_marginal_specification(
    component_names,
    families,
    parameters
  )
  
  cdf_fun <-
    resolve_distribution_cdf_function(
      cdf_function
    )
  
  U <-
    matrix(
      NA_real_,
      nrow =
        nrow(
          X
        ),
      ncol =
        ncol(
          X
        ),
      dimnames =
        list(
          rownames(
            X
          ),
          component_names
        )
    )
  
  for (
    j in
    seq_len(
      ncol(
        X
      )
    )
  ) {
    current_x <-
      X[
        ,
        j
      ]
    
    valid_rows <-
      is.finite(
        current_x
      )
    
    if (
      !allow_na &&
      !all(
        valid_rows
      )
    ) {
      stop(
        paste0(
          "Component ",
          component_names[j],
          " contains non-finite values."
        ),
        call. = FALSE
      )
    }
    
    if (
      any(
        valid_rows
      )
    ) {
      current_u <-
        cdf_fun(
          q =
            current_x[valid_rows],
          family =
            families[j],
          parameters =
            parameters[[j]]
        )
      
      current_u <-
        as.numeric(
          current_u
        )
      
      if (
        length(
          current_u
        ) !=
        sum(
          valid_rows
        ) ||
        any(
          !is.finite(
            current_u
          )
        )
      ) {
        stop(
          paste0(
            "Marginal CDF produced invalid values for component ",
            component_names[j],
            "."
          ),
          call. = FALSE
        )
      }
      
      U[
        valid_rows,
        j
      ] <-
        clip_probabilities(
          current_u,
          eps =
            eps
        )
    }
  }
  
  U
}

transform_uniforms_to_marginals <- function(
    U,
    families,
    parameters,
    component_names = colnames(U),
    quantile_function = NULL,
    eps = 1e-10,
    allow_na = FALSE
) {
  U <-
    as_numeric_matrix(
      U,
      "U"
    )
  
  validate_uniform_matrix(
    U,
    strict = FALSE,
    allow_na =
      allow_na
  )
  
  if (
    is.null(
      component_names
    )
  ) {
    component_names <-
      paste0(
        "V",
        seq_len(
          ncol(
            U
          )
        )
      )
  }
  
  if (
    length(
      component_names
    ) !=
    ncol(
      U
    )
  ) {
    stop(
      "component_names length must equal ncol(U).",
      call. = FALSE
    )
  }
  
  validate_marginal_specification(
    component_names,
    families,
    parameters
  )
  
  quantile_fun <-
    resolve_distribution_quantile_function(
      quantile_function
    )
  
  X <-
    matrix(
      NA_real_,
      nrow =
        nrow(
          U
        ),
      ncol =
        ncol(
          U
        ),
      dimnames =
        list(
          rownames(
            U
          ),
          component_names
        )
    )
  
  for (
    j in
    seq_len(
      ncol(
        U
      )
    )
  ) {
    current_u <-
      U[
        ,
        j
      ]
    
    valid_rows <-
      is.finite(
        current_u
      )
    
    if (
      !allow_na &&
      !all(
        valid_rows
      )
    ) {
      stop(
        paste0(
          "Component ",
          component_names[j],
          " contains non-finite uniform values."
        ),
        call. = FALSE
      )
    }
    
    if (
      any(
        valid_rows
      )
    ) {
      current_u_clipped <-
        clip_probabilities(
          current_u[valid_rows],
          eps =
            eps
        )
      
      current_x <-
        quantile_fun(
          p =
            current_u_clipped,
          family =
            families[j],
          parameters =
            parameters[[j]]
        )
      
      current_x <-
        as.numeric(
          current_x
        )
      
      if (
        length(
          current_x
        ) !=
        sum(
          valid_rows
        ) ||
        any(
          !is.finite(
            current_x
          )
        )
      ) {
        stop(
          paste0(
            "Marginal quantile produced invalid values for component ",
            component_names[j],
            "."
          ),
          call. = FALSE
        )
      }
      
      X[
        valid_rows,
        j
      ] <-
        current_x
    }
  }
  
  X
}

# ============================================================
# 6. Uniform-to-Score Transforms
# ============================================================

uniforms_to_gaussian_scores <- function(
    U,
    eps = 1e-10
) {
  U <-
    clip_uniform_matrix(
      U,
      eps =
        eps
    )
  
  Z <-
    qnorm(
      U
    )
  
  dimnames(
    Z
  ) <-
    dimnames(
      U
    )
  
  Z
}

uniforms_to_t_scores <- function(
    U,
    df,
    eps = 1e-10
) {
  assert_scalar_numeric(
    df,
    "df",
    lower = 2,
    inclusive_lower = FALSE
  )
  
  U <-
    clip_uniform_matrix(
      U,
      eps =
        eps
    )
  
  Z <-
    qt(
      U,
      df =
        df
    )
  
  dimnames(
    Z
  ) <-
    dimnames(
      U
    )
  
  Z
}

gaussian_scores_to_uniforms <- function(
    Z,
    eps = 1e-10
) {
  Z <-
    as_numeric_matrix(
      Z,
      "Z"
    )
  
  assert_numeric_matrix(
    Z,
    "Z"
  )
  
  U <-
    pnorm(
      Z
    )
  
  U <-
    matrix(
      clip_probabilities(
        as.numeric(
          U
        ),
        eps =
          eps
      ),
      nrow =
        nrow(
          Z
        ),
      ncol =
        ncol(
          Z
        ),
      dimnames =
        dimnames(
          Z
        )
    )
  
  U
}

t_scores_to_uniforms <- function(
    T_scores,
    df,
    eps = 1e-10
) {
  assert_scalar_numeric(
    df,
    "df",
    lower = 2,
    inclusive_lower = FALSE
  )
  
  T_scores <-
    as_numeric_matrix(
      T_scores,
      "T_scores"
    )
  
  assert_numeric_matrix(
    T_scores,
    "T_scores"
  )
  
  U <-
    pt(
      T_scores,
      df =
        df
    )
  
  U <-
    matrix(
      clip_probabilities(
        as.numeric(
          U
        ),
        eps =
          eps
      ),
      nrow =
        nrow(
          T_scores
        ),
      ncol =
        ncol(
          T_scores
        ),
      dimnames =
        dimnames(
          T_scores
        )
    )
  
  U
}

# ============================================================
# 7. Multivariate Normal and Student-t Log Densities
# ============================================================

multivariate_normal_log_density <- function(
    X,
    correlation
) {
  X <-
    as_numeric_matrix(
      X,
      "X"
    )
  
  assert_numeric_matrix(
    X,
    "X"
  )
  
  validate_correlation_matrix(
    correlation,
    require_positive_definite =
      TRUE
  )
  
  dimension <-
    ncol(
      X
    )
  
  if (
    nrow(
      correlation
    ) !=
    dimension
  ) {
    stop(
      "correlation dimension must equal ncol(X).",
      call. = FALSE
    )
  }
  
  chol_R <-
    chol(
      correlation
    )
  
  log_det_R <-
    2 *
    sum(
      log(
        diag(
          chol_R
        )
      )
    )
  
  inverse_R <-
    chol2inv(
      chol_R
    )
  
  quadratic <-
    rowSums(
      (
        X %*%
          inverse_R
      ) *
        X
    )
  
  -0.5 *
    (
      dimension *
        log(
          2 *
            pi
        ) +
        log_det_R +
        quadratic
    )
}

multivariate_t_log_density <- function(
    X,
    correlation,
    df
) {
  X <-
    as_numeric_matrix(
      X,
      "X"
    )
  
  assert_numeric_matrix(
    X,
    "X"
  )
  
  validate_correlation_matrix(
    correlation,
    require_positive_definite =
      TRUE
  )
  
  assert_scalar_numeric(
    df,
    "df",
    lower = 2,
    inclusive_lower = FALSE
  )
  
  dimension <-
    ncol(
      X
    )
  
  if (
    nrow(
      correlation
    ) !=
    dimension
  ) {
    stop(
      "correlation dimension must equal ncol(X).",
      call. = FALSE
    )
  }
  
  chol_R <-
    chol(
      correlation
    )
  
  log_det_R <-
    2 *
    sum(
      log(
        diag(
          chol_R
        )
      )
    )
  
  inverse_R <-
    chol2inv(
      chol_R
    )
  
  quadratic <-
    rowSums(
      (
        X %*%
          inverse_R
      ) *
        X
    )
  
  constant <-
    lgamma(
      (
        df +
          dimension
      ) /
        2
    ) -
    lgamma(
      df /
        2
    ) -
    0.5 *
    dimension *
    log(
      df *
        pi
    ) -
    0.5 *
    log_det_R
  
  constant -
    0.5 *
    (
      df +
        dimension
    ) *
    log1p(
      quadratic /
        df
    )
}

# ============================================================
# 8. Gaussian Copula Likelihood
# ============================================================

gaussian_copula_log_density <- function(
    U,
    correlation,
    eps = 1e-10
) {
  U <-
    clip_uniform_matrix(
      U,
      eps =
        eps
    )
  
  validate_correlation_matrix(
    correlation,
    require_positive_definite =
      TRUE
  )
  
  if (
    ncol(
      U
    ) !=
    nrow(
      correlation
    )
  ) {
    stop(
      "correlation dimension must equal ncol(U).",
      call. = FALSE
    )
  }
  
  Z <-
    uniforms_to_gaussian_scores(
      U,
      eps =
        eps
    )
  
  joint_log_density <-
    multivariate_normal_log_density(
      Z,
      correlation
    )
  
  marginal_log_density <-
    rowSums(
      dnorm(
        Z,
        log =
          TRUE
      )
    )
  
  joint_log_density -
    marginal_log_density
}

gaussian_copula_log_likelihood <- function(
    U,
    correlation,
    eps = 1e-10
) {
  sum(
    gaussian_copula_log_density(
      U,
      correlation,
      eps =
        eps
    )
  )
}

# ============================================================
# 9. Student-t Copula Likelihood
# ============================================================

t_copula_log_density <- function(
    U,
    correlation,
    df,
    eps = 1e-10
) {
  U <-
    clip_uniform_matrix(
      U,
      eps =
        eps
    )
  
  validate_correlation_matrix(
    correlation,
    require_positive_definite =
      TRUE
  )
  
  assert_scalar_numeric(
    df,
    "df",
    lower = 2,
    inclusive_lower = FALSE
  )
  
  if (
    ncol(
      U
    ) !=
    nrow(
      correlation
    )
  ) {
    stop(
      "correlation dimension must equal ncol(U).",
      call. = FALSE
    )
  }
  
  T_scores <-
    uniforms_to_t_scores(
      U,
      df =
        df,
      eps =
        eps
    )
  
  joint_log_density <-
    multivariate_t_log_density(
      T_scores,
      correlation,
      df =
        df
    )
  
  marginal_log_density <-
    rowSums(
      dt(
        T_scores,
        df =
          df,
        log =
          TRUE
      )
    )
  
  joint_log_density -
    marginal_log_density
}

t_copula_log_likelihood <- function(
    U,
    correlation,
    df,
    eps = 1e-10
) {
  sum(
    t_copula_log_density(
      U,
      correlation,
      df =
        df,
      eps =
        eps
    )
  )
}

# ============================================================
# 10. Information Criteria
# ============================================================

copula_information_criteria <- function(
    log_likelihood,
    parameter_count,
    sample_size
) {
  assert_scalar_numeric(
    log_likelihood,
    "log_likelihood"
  )
  
  if (
    !is.numeric(
      parameter_count
    ) ||
    length(
      parameter_count
    ) !=
    1L ||
    is.na(
      parameter_count
    ) ||
    parameter_count <
    0 ||
    parameter_count !=
    as.integer(
      parameter_count
    )
  ) {
    stop(
      "parameter_count must be a non-negative integer.",
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(
      sample_size
    ) ||
    length(
      sample_size
    ) !=
    1L ||
    is.na(
      sample_size
    ) ||
    sample_size <
    1 ||
    sample_size !=
    as.integer(
      sample_size
    )
  ) {
    stop(
      "sample_size must be a positive integer.",
      call. = FALSE
    )
  }
  
  parameter_count <-
    as.integer(
      parameter_count
    )
  
  sample_size <-
    as.integer(
      sample_size
    )
  
  list(
    LogLikelihood =
      log_likelihood,
    ParameterCount =
      parameter_count,
    SampleSize =
      sample_size,
    AIC =
      -2 *
      log_likelihood +
      2 *
      parameter_count,
    BIC =
      -2 *
      log_likelihood +
      log(
        sample_size
      ) *
      parameter_count
  )
}

# ============================================================
# 11. Complete-Case and Block Helpers
# ============================================================

complete_case_uniform_block <- function(
    U,
    columns = NULL,
    minimum_rows = 2L,
    eps = 1e-10
) {
  U <-
    as_numeric_matrix(
      U,
      "U"
    )
  
  if (
    !is.null(
      columns
    )
  ) {
    if (
      is.character(
        columns
      )
    ) {
      if (
        is.null(
          colnames(
            U
          )
        )
      ) {
        stop(
          "Character columns require colnames(U).",
          call. = FALSE
        )
      }
      
      missing_columns <-
        setdiff(
          columns,
          colnames(
            U
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
            "Unknown columns: ",
            paste(
              missing_columns,
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      }
      
      U <-
        U[
          ,
          columns,
          drop = FALSE
        ]
    } else {
      U <-
        U[
          ,
          columns,
          drop = FALSE
        ]
    }
  }
  
  complete_rows <-
    complete.cases(
      U
    ) &
    apply(
      U,
      1L,
      function(
    row
      ) {
        all(
          is.finite(
            row
          )
        )
      }
    )
  
  block <-
    U[
      complete_rows,
      ,
      drop = FALSE
    ]
  
  if (
    nrow(
      block
    ) <
    minimum_rows
  ) {
    stop(
      paste0(
        "Complete-case block has only ",
        nrow(
          block
        ),
        " rows; minimum required is ",
        minimum_rows,
        "."
      ),
      call. = FALSE
    )
  }
  
  block <-
    clip_uniform_matrix(
      block,
      eps =
        eps
    )
  
  list(
    U =
      block,
    CompleteRowIndex =
      which(
        complete_rows
      ),
    SampleSize =
      nrow(
        block
      ),
    Dimension =
      ncol(
        block
      ),
    Components =
      colnames(
        block
      )
  )
}

extract_named_block <- function(
    U,
    block_components,
    minimum_rows = 2L,
    eps = 1e-10
) {
  complete_case_uniform_block(
    U,
    columns =
      block_components,
    minimum_rows =
      minimum_rows,
    eps =
      eps
  )
}

build_block_list <- function(
    U,
    block_definitions,
    minimum_rows = 2L,
    eps = 1e-10
) {
  if (
    !is.list(
      block_definitions
    ) ||
    length(
      block_definitions
    ) ==
    0L
  ) {
    stop(
      "block_definitions must be a non-empty named list of component vectors.",
      call. = FALSE
    )
  }
  
  if (
    is.null(
      names(
        block_definitions
      )
    ) ||
    any(
      names(
        block_definitions
      ) ==
      ""
    )
  ) {
    stop(
      "block_definitions must be named.",
      call. = FALSE
    )
  }
  
  output <-
    vector(
      "list",
      length(
        block_definitions
      )
    )
  
  names(
    output
  ) <-
    names(
      block_definitions
    )
  
  for (
    i in
    seq_along(
      block_definitions
    )
  ) {
    output[[i]] <-
      extract_named_block(
        U =
          U,
        block_components =
          block_definitions[[i]],
        minimum_rows =
          minimum_rows,
        eps =
          eps
      )
  }
  
  output
}

# ============================================================
# 12. Gaussian Copula Fitting
# ============================================================

safe_sample_correlation <- function(
    X,
    minimum_eigenvalue = 1e-8
) {
  X <-
    as_numeric_matrix(
      X,
      "X"
    )
  
  assert_numeric_matrix(
    X,
    "X",
    min_rows =
      2L
  )
  
  if (
    ncol(
      X
    ) ==
    1L
  ) {
    return(
      matrix(
        1,
        nrow = 1L,
        ncol = 1L,
        dimnames =
          list(
            colnames(
              X
            ),
            colnames(
              X
            )
          )
      )
    )
  }
  
  R <-
    suppressWarnings(
      cor(
        X
      )
    )
  
  if (
    any(
      !is.finite(
        R
      )
    )
  ) {
    stop(
      "Sample correlation contains non-finite values.",
      call. = FALSE
    )
  }
  
  dimnames(
    R
  ) <-
    list(
      colnames(
        X
      ),
      colnames(
        X
      )
    )
  
  diagnostics <-
    correlation_matrix_diagnostics(
      R
    )
  
  if (
    !diagnostics[["Positive_Definite"]]
  ) {
    R <-
      regularize_correlation_matrix(
        R,
        minimum_eigenvalue =
          minimum_eigenvalue
      )
    
    dimnames(
      R
    ) <-
      list(
        colnames(
          X
        ),
        colnames(
          X
        )
      )
  }
  
  R
}

fit_gaussian_copula <- function(
    U,
    method = c(
      "mle",
      "empirical"
    ),
    eps = 1e-10,
    control = list(
      maxit = 2000L,
      reltol = 1e-9
    ),
    minimum_eigenvalue = 1e-8
) {
  method <-
    match.arg(
      method
    )
  
  block <-
    complete_case_uniform_block(
      U,
      minimum_rows =
        2L,
      eps =
        eps
    )
  
  U_complete <-
    block[["U"]]
  
  dimension <-
    ncol(
      U_complete
    )
  
  sample_size <-
    nrow(
      U_complete
    )
  
  if (
    dimension ==
    1L
  ) {
    correlation <-
      matrix(
        1,
        nrow = 1L,
        ncol = 1L,
        dimnames =
          list(
            colnames(
              U_complete
            ),
            colnames(
              U_complete
            )
          )
      )
    
    log_likelihood <-
      0
    
    information <-
      copula_information_criteria(
        log_likelihood,
        0L,
        sample_size
      )
    
    return(
      list(
        Family =
          "gaussian",
        Method =
          "degenerate_dimension_1",
        Correlation =
          correlation,
        DF =
          NA_real_,
        LogLikelihood =
          information[["LogLikelihood"]],
        ParameterCount =
          information[["ParameterCount"]],
        AIC =
          information[["AIC"]],
        BIC =
          information[["BIC"]],
        SampleSize =
          sample_size,
        Dimension =
          dimension,
        Converged =
          TRUE,
        OptimConvergenceCode =
          0L,
        OptimMessage =
          "Dimension 1 copula is degenerate.",
        OptimValue =
          0,
        InitialCorrelation =
          correlation,
        CompleteRowIndex =
          block[["CompleteRowIndex"]]
      )
    )
  }
  
  Z <-
    uniforms_to_gaussian_scores(
      U_complete,
      eps =
        eps
    )
  
  initial_correlation <-
    safe_sample_correlation(
      Z,
      minimum_eigenvalue =
        minimum_eigenvalue
    )
  
  if (
    method ==
    "empirical"
  ) {
    log_likelihood <-
      gaussian_copula_log_likelihood(
        U_complete,
        initial_correlation,
        eps =
          eps
      )
    
    parameter_count <-
      correlation_parameter_count(
        dimension
      )
    
    information <-
      copula_information_criteria(
        log_likelihood,
        parameter_count,
        sample_size
      )
    
    return(
      list(
        Family =
          "gaussian",
        Method =
          "empirical",
        Correlation =
          initial_correlation,
        DF =
          NA_real_,
        LogLikelihood =
          information[["LogLikelihood"]],
        ParameterCount =
          information[["ParameterCount"]],
        AIC =
          information[["AIC"]],
        BIC =
          information[["BIC"]],
        SampleSize =
          sample_size,
        Dimension =
          dimension,
        Converged =
          TRUE,
        OptimConvergenceCode =
          0L,
        OptimMessage =
          "Empirical Gaussian-score correlation used.",
        OptimValue =
          -log_likelihood,
        InitialCorrelation =
          initial_correlation,
        CompleteRowIndex =
          block[["CompleteRowIndex"]]
      )
    )
  }
  
  initial_theta <-
    unconstrained_from_correlation(
      initial_correlation
    )
  
  objective <- function(
    theta
  ) {
    current_correlation <-
      tryCatch(
        correlation_from_unconstrained(
          theta,
          dimension
        ),
        error = function(
    e
        ) {
          NULL
        }
      )
    
    if (
      is.null(
        current_correlation
      )
    ) {
      return(
        1e100
      )
    }
    
    current_log_likelihood <-
      tryCatch(
        gaussian_copula_log_likelihood(
          U_complete,
          current_correlation,
          eps =
            eps
        ),
        error = function(
    e
        ) {
          NA_real_
        }
      )
    
    if (
      !is.finite(
        current_log_likelihood
      )
    ) {
      return(
        1e100
      )
    }
    
    -current_log_likelihood
  }
  
  optimization <-
    optim(
      par =
        initial_theta,
      fn =
        objective,
      method =
        "BFGS",
      control =
        control,
      hessian =
        FALSE
    )
  
  fitted_correlation <-
    correlation_from_unconstrained(
      optimization[["par"]],
      dimension
    )
  
  dimnames(
    fitted_correlation
  ) <-
    list(
      colnames(
        U_complete
      ),
      colnames(
        U_complete
      )
    )
  
  log_likelihood <-
    -optimization[["value"]]
  
  parameter_count <-
    correlation_parameter_count(
      dimension
    )
  
  information <-
    copula_information_criteria(
      log_likelihood,
      parameter_count,
      sample_size
    )
  
  list(
    Family =
      "gaussian",
    Method =
      "mle",
    Correlation =
      fitted_correlation,
    DF =
      NA_real_,
    LogLikelihood =
      information[["LogLikelihood"]],
    ParameterCount =
      information[["ParameterCount"]],
    AIC =
      information[["AIC"]],
    BIC =
      information[["BIC"]],
    SampleSize =
      sample_size,
    Dimension =
      dimension,
    Converged =
      identical(
        optimization[["convergence"]],
        0L
      ) &&
      is.finite(
        log_likelihood
      ),
    OptimConvergenceCode =
      as.integer(
        optimization[["convergence"]]
      ),
    OptimMessage =
      if (
        is.null(
          optimization[["message"]]
        )
      ) {
        NA_character_
      } else {
        as.character(
          optimization[["message"]]
        )
      },
    OptimValue =
      optimization[["value"]],
    InitialCorrelation =
      initial_correlation,
    CompleteRowIndex =
      block[["CompleteRowIndex"]]
  )
}

# ============================================================
# 13. Student-t Copula Fitting
# ============================================================

fit_t_copula <- function(
    U,
    df_start = 8,
    df_lower = 2.05,
    df_upper = 200,
    fit_correlation = TRUE,
    initial_correlation = NULL,
    eps = 1e-10,
    control = list(
      maxit = 3000L,
      reltol = 1e-9
    ),
    minimum_eigenvalue = 1e-8
) {
  assert_scalar_numeric(
    df_start,
    "df_start",
    lower = 2,
    inclusive_lower = FALSE
  )
  
  assert_scalar_numeric(
    df_lower,
    "df_lower",
    lower = 2,
    inclusive_lower = FALSE
  )
  
  assert_scalar_numeric(
    df_upper,
    "df_upper",
    lower =
      df_lower,
    inclusive_lower =
      FALSE
  )
  
  if (
    df_start <=
    df_lower ||
    df_start >=
    df_upper
  ) {
    stop(
      "df_start must lie strictly between df_lower and df_upper.",
      call. = FALSE
    )
  }
  
  block <-
    complete_case_uniform_block(
      U,
      minimum_rows =
        2L,
      eps =
        eps
    )
  
  U_complete <-
    block[["U"]]
  
  dimension <-
    ncol(
      U_complete
    )
  
  sample_size <-
    nrow(
      U_complete
    )
  
  if (
    dimension ==
    1L
  ) {
    correlation <-
      matrix(
        1,
        nrow = 1L,
        ncol = 1L,
        dimnames =
          list(
            colnames(
              U_complete
            ),
            colnames(
              U_complete
            )
          )
      )
    
    log_likelihood <-
      0
    
    information <-
      copula_information_criteria(
        log_likelihood,
        0L,
        sample_size
      )
    
    return(
      list(
        Family =
          "student_t",
        Method =
          "degenerate_dimension_1",
        Correlation =
          correlation,
        DF =
          Inf,
        LogLikelihood =
          information[["LogLikelihood"]],
        ParameterCount =
          information[["ParameterCount"]],
        AIC =
          information[["AIC"]],
        BIC =
          information[["BIC"]],
        SampleSize =
          sample_size,
        Dimension =
          dimension,
        Converged =
          TRUE,
        OptimConvergenceCode =
          0L,
        OptimMessage =
          "Dimension 1 copula is degenerate.",
        OptimValue =
          0,
        InitialCorrelation =
          correlation,
        CompleteRowIndex =
          block[["CompleteRowIndex"]]
      )
    )
  }
  
  if (
    is.null(
      initial_correlation
    )
  ) {
    initial_scores <-
      uniforms_to_t_scores(
        U_complete,
        df =
          df_start,
        eps =
          eps
      )
    
    initial_correlation <-
      safe_sample_correlation(
        initial_scores,
        minimum_eigenvalue =
          minimum_eigenvalue
      )
  } else {
    validate_correlation_matrix(
      initial_correlation,
      require_positive_definite =
        TRUE
    )
    
    if (
      nrow(
        initial_correlation
      ) !=
      dimension
    ) {
      stop(
        "initial_correlation dimension must equal ncol(U).",
        call. = FALSE
      )
    }
  }
  
  dimnames(
    initial_correlation
  ) <-
    list(
      colnames(
        U_complete
      ),
      colnames(
        U_complete
      )
    )
  
  initial_theta <-
    unconstrained_from_correlation(
      initial_correlation
    )
  
  encode_df <- function(
    df
  ) {
    log(
      (
        df -
          df_lower
      ) /
        (
          df_upper -
            df
        )
    )
  }
  
  decode_df <- function(
    eta
  ) {
    df_lower +
      (
        df_upper -
          df_lower
      ) *
      plogis(
        eta
      )
  }
  
  initial_eta <-
    encode_df(
      df_start
    )
  
  if (
    fit_correlation
  ) {
    initial_parameter <-
      c(
        initial_theta,
        initial_eta
      )
    
    objective <- function(
    parameter
    ) {
      theta <-
        parameter[
          seq_len(
            length(
              initial_theta
            )
          )
        ]
      
      eta <-
        parameter[
          length(
            parameter
          )
        ]
      
      current_df <-
        decode_df(
          eta
        )
      
      current_correlation <-
        tryCatch(
          correlation_from_unconstrained(
            theta,
            dimension
          ),
          error = function(
    e
          ) {
            NULL
          }
        )
      
      if (
        is.null(
          current_correlation
        )
      ) {
        return(
          1e100
        )
      }
      
      current_log_likelihood <-
        tryCatch(
          t_copula_log_likelihood(
            U_complete,
            current_correlation,
            df =
              current_df,
            eps =
              eps
          ),
          error = function(
    e
          ) {
            NA_real_
          }
        )
      
      if (
        !is.finite(
          current_log_likelihood
        )
      ) {
        return(
          1e100
        )
      }
      
      -current_log_likelihood
    }
    
    optimization <-
      optim(
        par =
          initial_parameter,
        fn =
          objective,
        method =
          "BFGS",
        control =
          control,
        hessian =
          FALSE
      )
    
    fitted_theta <-
      optimization[["par"]][
        seq_len(
          length(
            initial_theta
          )
        )
      ]
    
    fitted_df <-
      decode_df(
        optimization[["par"]][
          length(
            optimization[["par"]]
          )
        ]
      )
    
    fitted_correlation <-
      correlation_from_unconstrained(
        fitted_theta,
        dimension
      )
    
    parameter_count <-
      correlation_parameter_count(
        dimension
      ) +
      1L
  } else {
    objective <- function(
    eta
    ) {
      current_df <-
        decode_df(
          eta
        )
      
      current_log_likelihood <-
        tryCatch(
          t_copula_log_likelihood(
            U_complete,
            initial_correlation,
            df =
              current_df,
            eps =
              eps
          ),
          error = function(
    e
          ) {
            NA_real_
          }
        )
      
      if (
        !is.finite(
          current_log_likelihood
        )
      ) {
        return(
          1e100
        )
      }
      
      -current_log_likelihood
    }
    
    optimization <-
      optim(
        par =
          initial_eta,
        fn =
          objective,
        method =
          "BFGS",
        control =
          control,
        hessian =
          FALSE
      )
    
    fitted_df <-
      decode_df(
        optimization[["par"]][1L]
      )
    
    fitted_correlation <-
      initial_correlation
    
    parameter_count <-
      1L
  }
  
  dimnames(
    fitted_correlation
  ) <-
    list(
      colnames(
        U_complete
      ),
      colnames(
        U_complete
      )
    )
  
  log_likelihood <-
    -optimization[["value"]]
  
  information <-
    copula_information_criteria(
      log_likelihood,
      parameter_count,
      sample_size
    )
  
  list(
    Family =
      "student_t",
    Method =
      if (
        fit_correlation
      ) {
        "joint_mle"
      } else {
        "df_only_mle"
      },
    Correlation =
      fitted_correlation,
    DF =
      fitted_df,
    LogLikelihood =
      information[["LogLikelihood"]],
    ParameterCount =
      information[["ParameterCount"]],
    AIC =
      information[["AIC"]],
    BIC =
      information[["BIC"]],
    SampleSize =
      sample_size,
    Dimension =
      dimension,
    Converged =
      identical(
        optimization[["convergence"]],
        0L
      ) &&
      is.finite(
        log_likelihood
      ) &&
      is.finite(
        fitted_df
      ),
    OptimConvergenceCode =
      as.integer(
        optimization[["convergence"]]
      ),
    OptimMessage =
      if (
        is.null(
          optimization[["message"]]
        )
      ) {
        NA_character_
      } else {
        as.character(
          optimization[["message"]]
        )
      },
    OptimValue =
      optimization[["value"]],
    InitialCorrelation =
      initial_correlation,
    CompleteRowIndex =
      block[["CompleteRowIndex"]],
    DFLower =
      df_lower,
    DFUpper =
      df_upper
  )
}

# ============================================================
# 14. Copula Candidate Wrapper
# ============================================================

fit_dependence_candidates <- function(
    U,
    gaussian_method = "mle",
    t_df_start = 8,
    t_df_lower = 2.05,
    t_df_upper = 200,
    t_fit_correlation = TRUE,
    eps = 1e-10,
    gaussian_control = list(
      maxit = 2000L,
      reltol = 1e-9
    ),
    t_control = list(
      maxit = 3000L,
      reltol = 1e-9
    )
) {
  gaussian_fit <-
    fit_gaussian_copula(
      U =
        U,
      method =
        gaussian_method,
      eps =
        eps,
      control =
        gaussian_control
    )
  
  t_fit <-
    fit_t_copula(
      U =
        U,
      df_start =
        t_df_start,
      df_lower =
        t_df_lower,
      df_upper =
        t_df_upper,
      fit_correlation =
        t_fit_correlation,
      initial_correlation =
        gaussian_fit[["Correlation"]],
      eps =
        eps,
      control =
        t_control
    )
  
  list(
    Gaussian =
      gaussian_fit,
    StudentT =
      t_fit
  )
}

copula_fit_summary <- function(
    fit
) {
  required_names <-
    c(
      "Family",
      "Method",
      "LogLikelihood",
      "ParameterCount",
      "AIC",
      "BIC",
      "SampleSize",
      "Dimension",
      "Converged"
    )
  
  if (
    !is.list(
      fit
    ) ||
    !all(
      required_names %in%
      names(
        fit
      )
    )
  ) {
    stop(
      "fit is not a recognized copula fit object.",
      call. = FALSE
    )
  }
  
  data.frame(
    Family =
      as.character(
        fit[["Family"]]
      ),
    Method =
      as.character(
        fit[["Method"]]
      ),
    SampleSize =
      as.integer(
        fit[["SampleSize"]]
      ),
    Dimension =
      as.integer(
        fit[["Dimension"]]
      ),
    ParameterCount =
      as.integer(
        fit[["ParameterCount"]]
      ),
    LogLikelihood =
      as.numeric(
        fit[["LogLikelihood"]]
      ),
    AIC =
      as.numeric(
        fit[["AIC"]]
      ),
    BIC =
      as.numeric(
        fit[["BIC"]]
      ),
    DF =
      if (
        is.null(
          fit[["DF"]]
        )
      ) {
        NA_real_
      } else {
        as.numeric(
          fit[["DF"]]
        )
      },
    Converged =
      isTRUE(
        fit[["Converged"]]
      ),
    stringsAsFactors =
      FALSE
  )
}

# ============================================================
# 15. Copula Simulation
# ============================================================

simulate_gaussian_copula <- function(
    n,
    correlation,
    eps = 1e-10,
    seed = NULL
) {
  if (
    !is.numeric(
      n
    ) ||
    length(
      n
    ) !=
    1L ||
    is.na(
      n
    ) ||
    n <
    1 ||
    n !=
    as.integer(
      n
    )
  ) {
    stop(
      "n must be a positive integer.",
      call. = FALSE
    )
  }
  
  n <-
    as.integer(
      n
    )
  
  validate_correlation_matrix(
    correlation,
    require_positive_definite =
      TRUE
  )
  
  if (
    !is.null(
      seed
    )
  ) {
    set.seed(
      seed
    )
  }
  
  dimension <-
    nrow(
      correlation
    )
  
  independent_normals <-
    matrix(
      rnorm(
        n *
          dimension
      ),
      nrow =
        n,
      ncol =
        dimension
    )
  
  correlated_normals <-
    independent_normals %*%
    chol(
      correlation
    )
  
  U <-
    pnorm(
      correlated_normals
    )
  
  U <-
    matrix(
      clip_probabilities(
        as.numeric(
          U
        ),
        eps =
          eps
      ),
      nrow =
        n,
      ncol =
        dimension
    )
  
  colnames(
    U
  ) <-
    colnames(
      correlation
    )
  
  U
}

simulate_t_copula <- function(
    n,
    correlation,
    df,
    eps = 1e-10,
    seed = NULL
) {
  if (
    !is.numeric(
      n
    ) ||
    length(
      n
    ) !=
    1L ||
    is.na(
      n
    ) ||
    n <
    1 ||
    n !=
    as.integer(
      n
    )
  ) {
    stop(
      "n must be a positive integer.",
      call. = FALSE
    )
  }
  
  n <-
    as.integer(
      n
    )
  
  validate_correlation_matrix(
    correlation,
    require_positive_definite =
      TRUE
  )
  
  assert_scalar_numeric(
    df,
    "df",
    lower = 2,
    inclusive_lower = FALSE
  )
  
  if (
    !is.null(
      seed
    )
  ) {
    set.seed(
      seed
    )
  }
  
  dimension <-
    nrow(
      correlation
    )
  
  independent_normals <-
    matrix(
      rnorm(
        n *
          dimension
      ),
      nrow =
        n,
      ncol =
        dimension
    )
  
  correlated_normals <-
    independent_normals %*%
    chol(
      correlation
    )
  
  chi_square <-
    rchisq(
      n,
      df =
        df
    )
  
  scaling <-
    sqrt(
      chi_square /
        df
    )
  
  t_scores <-
    correlated_normals /
    scaling
  
  U <-
    pt(
      t_scores,
      df =
        df
    )
  
  U <-
    matrix(
      clip_probabilities(
        as.numeric(
          U
        ),
        eps =
          eps
      ),
      nrow =
        n,
      ncol =
        dimension
    )
  
  colnames(
    U
  ) <-
    colnames(
      correlation
    )
  
  U
}

simulate_copula_from_fit <- function(
    fit,
    n,
    eps = 1e-10,
    seed = NULL
) {
  if (
    !is.list(
      fit
    ) ||
    is.null(
      fit[["Family"]]
    ) ||
    is.null(
      fit[["Correlation"]]
    )
  ) {
    stop(
      "fit must contain Family and Correlation.",
      call. = FALSE
    )
  }
  
  if (
    fit[["Family"]] ==
    "gaussian"
  ) {
    return(
      simulate_gaussian_copula(
        n =
          n,
        correlation =
          fit[["Correlation"]],
        eps =
          eps,
        seed =
          seed
      )
    )
  }
  
  if (
    fit[["Family"]] ==
    "student_t"
  ) {
    return(
      simulate_t_copula(
        n =
          n,
        correlation =
          fit[["Correlation"]],
        df =
          fit[["DF"]],
        eps =
          eps,
        seed =
          seed
      )
    )
  }
  
  stop(
    paste0(
      "Unsupported copula family: ",
      fit[["Family"]]
    ),
    call. = FALSE
  )
}

# ============================================================
# 16. Pair Index Helpers
# ============================================================

pair_index_table <- function(
    component_names
) {
  if (
    !is.character(
      component_names
    ) ||
    length(
      component_names
    ) <
    2L
  ) {
    stop(
      "component_names must contain at least two names.",
      call. = FALSE
    )
  }
  
  if (
    anyDuplicated(
      component_names
    )
  ) {
    stop(
      "component_names must be unique.",
      call. = FALSE
    )
  }
  
  pairs <-
    utils::combn(
      seq_along(
        component_names
      ),
      2L
    )
  
  data.frame(
    Index_1 =
      pairs[
        1L,
      ],
    Index_2 =
      pairs[
        2L,
      ],
    Component_1 =
      component_names[
        pairs[
          1L,
        ]
      ],
    Component_2 =
      component_names[
        pairs[
          2L,
        ]
      ],
    stringsAsFactors =
      FALSE
  )
}

# ============================================================
# 17. Tail Co-occurrence Diagnostics
# ============================================================

pairwise_tail_cooccurrence <- function(
    U,
    tail_probabilities = c(
      0.01,
      0.025,
      0.05,
      0.10
    ),
    include_cross_tails = TRUE
) {
  U <-
    as_numeric_matrix(
      U,
      "U"
    )
  
  validate_uniform_matrix(
    U,
    strict = FALSE,
    allow_na = TRUE
  )
  
  if (
    is.null(
      colnames(
        U
      )
    )
  ) {
    colnames(
      U
    ) <-
      paste0(
        "V",
        seq_len(
          ncol(
            U
          )
        )
      )
  }
  
  if (
    ncol(
      U
    ) <
    2L
  ) {
    stop(
      "U must contain at least two components.",
      call. = FALSE
    )
  }
  
  if (
    !is.numeric(
      tail_probabilities
    ) ||
    length(
      tail_probabilities
    ) ==
    0L ||
    any(
      !is.finite(
        tail_probabilities
      )
    ) ||
    any(
      tail_probabilities <=
      0 |
      tail_probabilities >=
      0.5
    )
  ) {
    stop(
      "tail_probabilities must be finite values strictly between 0 and 0.5.",
      call. = FALSE
    )
  }
  
  pair_table <-
    pair_index_table(
      colnames(
        U
      )
    )
  
  output <-
    vector(
      "list",
      nrow(
        pair_table
      ) *
        length(
          tail_probabilities
        )
    )
  
  counter <-
    1L
  
  for (
    pair_row in
    seq_len(
      nrow(
        pair_table
      )
    )
  ) {
    i <-
      pair_table[["Index_1"]][pair_row]
    
    j <-
      pair_table[["Index_2"]][pair_row]
    
    complete_rows <-
      is.finite(
        U[
          ,
          i
        ]
      ) &
      is.finite(
        U[
          ,
          j
        ]
      )
    
    u1 <-
      U[
        complete_rows,
        i
      ]
    
    u2 <-
      U[
        complete_rows,
        j
      ]
    
    n_pair <-
      length(
        u1
      )
    
    for (
      alpha in
      tail_probabilities
    ) {
      if (
        n_pair ==
        0L
      ) {
        lower_lower <-
          NA_real_
        
        upper_upper <-
          NA_real_
        
        lower_upper <-
          NA_real_
        
        upper_lower <-
          NA_real_
      } else {
        lower_lower <-
          mean(
            u1 <=
              alpha &
              u2 <=
              alpha
          )
        
        upper_upper <-
          mean(
            u1 >=
              1 -
              alpha &
              u2 >=
              1 -
              alpha
          )
        
        lower_upper <-
          mean(
            u1 <=
              alpha &
              u2 >=
              1 -
              alpha
          )
        
        upper_lower <-
          mean(
            u1 >=
              1 -
              alpha &
              u2 <=
              alpha
          )
      }
      
      output[[counter]] <-
        data.frame(
          Component_1 =
            pair_table[["Component_1"]][pair_row],
          Component_2 =
            pair_table[["Component_2"]][pair_row],
          Tail_Probability =
            alpha,
          Pair_N =
            n_pair,
          Lower_Lower =
            lower_lower,
          Upper_Upper =
            upper_upper,
          Lower_Upper =
            if (
              include_cross_tails
            ) {
              lower_upper
            } else {
              NA_real_
            },
          Upper_Lower =
            if (
              include_cross_tails
            ) {
              upper_lower
            } else {
              NA_real_
            },
          stringsAsFactors =
            FALSE
        )
      
      counter <-
        counter +
        1L
    }
  }
  
  do.call(
    rbind,
    output
  )
}

model_implied_tail_cooccurrence <- function(
    fit,
    simulation_n = 100000L,
    tail_probabilities = c(
      0.01,
      0.025,
      0.05,
      0.10
    ),
    include_cross_tails = TRUE,
    seed = 12345L,
    eps = 1e-10
) {
  simulated_U <-
    simulate_copula_from_fit(
      fit =
        fit,
      n =
        simulation_n,
      eps =
        eps,
      seed =
        seed
    )
  
  pairwise_tail_cooccurrence(
    simulated_U,
    tail_probabilities =
      tail_probabilities,
    include_cross_tails =
      include_cross_tails
  )
}

tail_cooccurrence_calibration <- function(
    observed_U,
    fit,
    simulation_n = 100000L,
    tail_probabilities = c(
      0.01,
      0.025,
      0.05,
      0.10
    ),
    include_cross_tails = TRUE,
    seed = 12345L,
    eps = 1e-10
) {
  observed <-
    pairwise_tail_cooccurrence(
      U =
        observed_U,
      tail_probabilities =
        tail_probabilities,
      include_cross_tails =
        include_cross_tails
    )
  
  model <-
    model_implied_tail_cooccurrence(
      fit =
        fit,
      simulation_n =
        simulation_n,
      tail_probabilities =
        tail_probabilities,
      include_cross_tails =
        include_cross_tails,
      seed =
        seed,
      eps =
        eps
    )
  
  merge_columns <-
    c(
      "Component_1",
      "Component_2",
      "Tail_Probability"
    )
  
  merged <-
    merge(
      observed,
      model,
      by =
        merge_columns,
      suffixes = c(
        "_Observed",
        "_Model"
      ),
      all = FALSE,
      sort = FALSE
    )
  
  merged[["Lower_Lower_Error"]] <-
    merged[["Lower_Lower_Model"]] -
    merged[["Lower_Lower_Observed"]]
  
  merged[["Upper_Upper_Error"]] <-
    merged[["Upper_Upper_Model"]] -
    merged[["Upper_Upper_Observed"]]
  
  merged[["Lower_Upper_Error"]] <-
    merged[["Lower_Upper_Model"]] -
    merged[["Lower_Upper_Observed"]]
  
  merged[["Upper_Lower_Error"]] <-
    merged[["Upper_Lower_Model"]] -
    merged[["Upper_Lower_Observed"]]
  
  merged[["Lower_Lower_Absolute_Error"]] <-
    abs(
      merged[["Lower_Lower_Error"]]
    )
  
  merged[["Upper_Upper_Absolute_Error"]] <-
    abs(
      merged[["Upper_Upper_Error"]]
    )
  
  merged[["Lower_Upper_Absolute_Error"]] <-
    abs(
      merged[["Lower_Upper_Error"]]
    )
  
  merged[["Upper_Lower_Absolute_Error"]] <-
    abs(
      merged[["Upper_Lower_Error"]]
    )
  
  merged
}

tail_calibration_summary <- function(
    calibration_table
) {
  required_columns <-
    c(
      "Lower_Lower_Absolute_Error",
      "Upper_Upper_Absolute_Error"
    )
  
  if (
    !is.data.frame(
      calibration_table
    ) ||
    !all(
      required_columns %in%
      names(
        calibration_table
      )
    )
  ) {
    stop(
      "calibration_table is missing required tail calibration columns.",
      call. = FALSE
    )
  }
  
  same_tail_errors <-
    c(
      calibration_table[["Lower_Lower_Absolute_Error"]],
      calibration_table[["Upper_Upper_Absolute_Error"]]
    )
  
  cross_tail_errors <-
    c(
      calibration_table[["Lower_Upper_Absolute_Error"]],
      calibration_table[["Upper_Lower_Absolute_Error"]]
    )
  
  list(
    SameTail_MAE =
      mean(
        same_tail_errors,
        na.rm = TRUE
      ),
    CrossTail_MAE =
      if (
        all(
          is.na(
            cross_tail_errors
          )
        )
      ) {
        NA_real_
      } else {
        mean(
          cross_tail_errors,
          na.rm = TRUE
        )
      },
    Maximum_SameTail_Absolute_Error =
      max(
        same_tail_errors,
        na.rm = TRUE
      ),
    Maximum_CrossTail_Absolute_Error =
      if (
        all(
          is.na(
            cross_tail_errors
          )
        )
      ) {
        NA_real_
      } else {
        max(
          cross_tail_errors,
          na.rm = TRUE
        )
      }
  )
}

# ============================================================
# 18. Rank Dependence Diagnostics
# ============================================================

pairwise_rank_dependence <- function(
    U
) {
  U <-
    as_numeric_matrix(
      U,
      "U"
    )
  
  validate_uniform_matrix(
    U,
    strict = FALSE,
    allow_na = TRUE
  )
  
  if (
    is.null(
      colnames(
        U
      )
    )
  ) {
    colnames(
      U
    ) <-
      paste0(
        "V",
        seq_len(
          ncol(
            U
          )
        )
      )
  }
  
  pair_table <-
    pair_index_table(
      colnames(
        U
      )
    )
  
  output <-
    vector(
      "list",
      nrow(
        pair_table
      )
    )
  
  for (
    pair_row in
    seq_len(
      nrow(
        pair_table
      )
    )
  ) {
    i <-
      pair_table[["Index_1"]][pair_row]
    
    j <-
      pair_table[["Index_2"]][pair_row]
    
    complete_rows <-
      is.finite(
        U[
          ,
          i
        ]
      ) &
      is.finite(
        U[
          ,
          j
        ]
      )
    
    u1 <-
      U[
        complete_rows,
        i
      ]
    
    u2 <-
      U[
        complete_rows,
        j
      ]
    
    if (
      length(
        u1
      ) <
      2L
    ) {
      spearman <-
        NA_real_
      
      kendall <-
        NA_real_
    } else {
      spearman <-
        suppressWarnings(
          cor(
            u1,
            u2,
            method = "spearman"
          )
        )
      
      kendall <-
        suppressWarnings(
          cor(
            u1,
            u2,
            method = "kendall"
          )
        )
    }
    
    output[[pair_row]] <-
      data.frame(
        Component_1 =
          pair_table[["Component_1"]][pair_row],
        Component_2 =
          pair_table[["Component_2"]][pair_row],
        Pair_N =
          length(
            u1
          ),
        Spearman_Rho =
          spearman,
        Kendall_Tau =
          kendall,
        stringsAsFactors =
          FALSE
      )
  }
  
  do.call(
    rbind,
    output
  )
}

# ============================================================
# 19. Blockwise Fitting Helpers
# ============================================================

fit_dependence_by_blocks <- function(
    U,
    block_definitions,
    minimum_rows = 2L,
    gaussian_method = "mle",
    t_df_start = 8,
    t_df_lower = 2.05,
    t_df_upper = 200,
    t_fit_correlation = TRUE,
    eps = 1e-10,
    gaussian_control = list(
      maxit = 2000L,
      reltol = 1e-9
    ),
    t_control = list(
      maxit = 3000L,
      reltol = 1e-9
    )
) {
  blocks <-
    build_block_list(
      U,
      block_definitions,
      minimum_rows =
        minimum_rows,
      eps =
        eps
    )
  
  output <-
    vector(
      "list",
      length(
        blocks
      )
    )
  
  names(
    output
  ) <-
    names(
      blocks
    )
  
  for (
    i in
    seq_along(
      blocks
    )
  ) {
    current_block <-
      blocks[[i]]
    
    output[[i]] <-
      list(
        Block =
          current_block,
        Fits =
          fit_dependence_candidates(
            U =
              current_block[["U"]],
            gaussian_method =
              gaussian_method,
            t_df_start =
              t_df_start,
            t_df_lower =
              t_df_lower,
            t_df_upper =
              t_df_upper,
            t_fit_correlation =
              t_fit_correlation,
            eps =
              eps,
            gaussian_control =
              gaussian_control,
            t_control =
              t_control
          )
      )
  }
  
  output
}

block_fit_summary <- function(
    block_fit_results
) {
  if (
    !is.list(
      block_fit_results
    ) ||
    length(
      block_fit_results
    ) ==
    0L
  ) {
    stop(
      "block_fit_results must be a non-empty list.",
      call. = FALSE
    )
  }
  
  rows <-
    list()
  
  counter <-
    1L
  
  for (
    block_name in
    names(
      block_fit_results
    )
  ) {
    current_fits <-
      block_fit_results[[block_name]][["Fits"]]
    
    for (
      fit_name in
      names(
        current_fits
      )
    ) {
      current_summary <-
        copula_fit_summary(
          current_fits[[fit_name]]
        )
      
      current_summary[["Block"]] <-
        block_name
      
      rows[[counter]] <-
        current_summary
      
      counter <-
        counter +
        1L
    }
  }
  
  result <-
    do.call(
      rbind,
      rows
    )
  
  result <-
    result[
      ,
      c(
        "Block",
        setdiff(
          names(
            result
          ),
          "Block"
        )
      ),
      drop = FALSE
    ]
  
  rownames(
    result
  ) <-
    NULL
  
  result
}

# ============================================================
# 20. Simulation Back Through Selected Marginals
# ============================================================

simulate_innovations_from_copula_fit <- function(
    fit,
    n,
    families,
    parameters,
    component_names = names(families),
    quantile_function = NULL,
    eps = 1e-10,
    seed = NULL
) {
  simulated_U <-
    simulate_copula_from_fit(
      fit =
        fit,
      n =
        n,
      eps =
        eps,
      seed =
        seed
    )
  
  if (
    is.null(
      component_names
    )
  ) {
    if (
      !is.null(
        colnames(
          simulated_U
        )
      )
    ) {
      component_names <-
        colnames(
          simulated_U
        )
    } else {
      component_names <-
        paste0(
          "V",
          seq_len(
            ncol(
              simulated_U
            )
          )
        )
    }
  }
  
  if (
    length(
      component_names
    ) !=
    ncol(
      simulated_U
    )
  ) {
    stop(
      "component_names length must match fitted copula dimension.",
      call. = FALSE
    )
  }
  
  colnames(
    simulated_U
  ) <-
    component_names
  
  simulated_X <-
    transform_uniforms_to_marginals(
      U =
        simulated_U,
      families =
        families,
      parameters =
        parameters,
      component_names =
        component_names,
      quantile_function =
        quantile_function,
      eps =
        eps,
      allow_na =
        FALSE
    )
  
  list(
    Uniforms =
      simulated_U,
    Innovations =
      simulated_X
  )
}

# ============================================================
# 21. Generic Dependence Fit Validation
# ============================================================

validate_copula_fit <- function(
    fit,
    require_converged = TRUE
) {
  required_names <-
    c(
      "Family",
      "Correlation",
      "LogLikelihood",
      "ParameterCount",
      "AIC",
      "BIC",
      "SampleSize",
      "Dimension",
      "Converged"
    )
  
  if (
    !is.list(
      fit
    ) ||
    !all(
      required_names %in%
      names(
        fit
      )
    )
  ) {
    stop(
      "fit does not contain the required copula-fit fields.",
      call. = FALSE
    )
  }
  
  validate_correlation_matrix(
    fit[["Correlation"]],
    require_positive_definite =
      TRUE
  )
  
  if (
    nrow(
      fit[["Correlation"]]
    ) !=
    fit[["Dimension"]]
  ) {
    stop(
      "fit correlation dimension does not match fit$Dimension.",
      call. = FALSE
    )
  }
  
  if (
    !is.finite(
      fit[["LogLikelihood"]]
    ) ||
    !is.finite(
      fit[["AIC"]]
    ) ||
    !is.finite(
      fit[["BIC"]]
    )
  ) {
    stop(
      "fit contains non-finite likelihood or information criteria.",
      call. = FALSE
    )
  }
  
  if (
    fit[["Family"]] ==
    "student_t"
  ) {
    if (
      is.null(
        fit[["DF"]]
      ) ||
      !is.finite(
        fit[["DF"]]
      ) ||
      fit[["DF"]] <=
      2
    ) {
      stop(
        "Student-t copula fit must contain finite DF > 2.",
        call. = FALSE
      )
    }
  }
  
  if (
    require_converged &&
    !isTRUE(
      fit[["Converged"]]
    )
  ) {
    stop(
      "Copula fit did not converge.",
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}

# ============================================================
# 22. Self-Test
# ============================================================

innovation_dependence_utils_self_test <- function(
    verbose = TRUE
) {
  set.seed(
    12345L
  )
  
  test_R <-
    matrix(
      c(
        1.0, 0.45, -0.20,
        0.45, 1.0, 0.30,
        -0.20, 0.30, 1.0
      ),
      nrow =
        3L,
      byrow =
        TRUE
    )
  
  colnames(
    test_R
  ) <-
    c(
      "A",
      "B",
      "C"
    )
  
  rownames(
    test_R
  ) <-
    c(
      "A",
      "B",
      "C"
    )
  
  validate_correlation_matrix(
    test_R
  )
  
  theta <-
    unconstrained_from_correlation(
      test_R
    )
  
  reconstructed_R <-
    correlation_from_unconstrained(
      theta,
      3L
    )
  
  dimnames(
    reconstructed_R
  ) <-
    dimnames(
      test_R
    )
  
  stopifnot(
    max(
      abs(
        test_R -
          reconstructed_R
      )
    ) <
      1e-8
  )
  
  U_gaussian <-
    simulate_gaussian_copula(
      n =
        400L,
      correlation =
        test_R,
      seed =
        101L
    )
  
  colnames(
    U_gaussian
  ) <-
    c(
      "A",
      "B",
      "C"
    )
  
  validate_uniform_matrix(
    U_gaussian
  )
  
  gaussian_log_likelihood <-
    gaussian_copula_log_likelihood(
      U_gaussian,
      test_R
    )
  
  stopifnot(
    is.finite(
      gaussian_log_likelihood
    )
  )
  
  gaussian_fit <-
    fit_gaussian_copula(
      U =
        U_gaussian,
      method =
        "empirical"
    )
  
  validate_copula_fit(
    gaussian_fit
  )
  
  stopifnot(
    gaussian_fit[["Dimension"]] ==
      3L
  )
  
  stopifnot(
    gaussian_fit[["SampleSize"]] ==
      400L
  )
  
  U_t <-
    simulate_t_copula(
      n =
        400L,
      correlation =
        test_R,
      df =
        6,
      seed =
        202L
    )
  
  colnames(
    U_t
  ) <-
    c(
      "A",
      "B",
      "C"
    )
  
  validate_uniform_matrix(
    U_t
  )
  
  t_log_likelihood <-
    t_copula_log_likelihood(
      U_t,
      test_R,
      df =
        6
    )
  
  stopifnot(
    is.finite(
      t_log_likelihood
    )
  )
  
  t_fit_df_only <-
    fit_t_copula(
      U =
        U_t,
      df_start =
        8,
      fit_correlation =
        FALSE,
      initial_correlation =
        test_R,
      control =
        list(
          maxit = 1000L,
          reltol = 1e-8
        )
    )
  
  validate_copula_fit(
    t_fit_df_only
  )
  
  stopifnot(
    t_fit_df_only[["DF"]] >
      2
  )
  
  tail_table <-
    pairwise_tail_cooccurrence(
      U =
        U_t,
      tail_probabilities =
        c(
          0.05,
          0.10
        )
    )
  
  stopifnot(
    nrow(
      tail_table
    ) ==
      3L *
      2L
  )
  
  model_tail_table <-
    model_implied_tail_cooccurrence(
      fit =
        t_fit_df_only,
      simulation_n =
        2000L,
      tail_probabilities =
        c(
          0.05,
          0.10
        ),
      seed =
        303L
    )
  
  stopifnot(
    nrow(
      model_tail_table
    ) ==
      3L *
      2L
  )
  
  calibration_table <-
    tail_cooccurrence_calibration(
      observed_U =
        U_t,
      fit =
        t_fit_df_only,
      simulation_n =
        2000L,
      tail_probabilities =
        c(
          0.05,
          0.10
        ),
      seed =
        404L
    )
  
  stopifnot(
    nrow(
      calibration_table
    ) ==
      3L *
      2L
  )
  
  calibration_summary <-
    tail_calibration_summary(
      calibration_table
    )
  
  stopifnot(
    is.finite(
      calibration_summary[["SameTail_MAE"]]
    )
  )
  
  rank_dependence <-
    pairwise_rank_dependence(
      U_t
    )
  
  stopifnot(
    nrow(
      rank_dependence
    ) ==
      3L
  )
  
  block_definitions <-
    list(
      AB =
        c(
          "A",
          "B"
        ),
      BC =
        c(
          "B",
          "C"
        )
    )
  
  blocks <-
    build_block_list(
      U_t,
      block_definitions
    )
  
  stopifnot(
    length(
      blocks
    ) ==
      2L
  )
  
  stopifnot(
    blocks[["AB"]][["Dimension"]] ==
      2L
  )
  
  stopifnot(
    blocks[["BC"]][["Dimension"]] ==
      2L
  )
  
  if (
    verbose
  ) {
    cat(
      "innovation_dependence_utils.R self-test passed: TRUE\n"
    )
  }
  
  invisible(
    TRUE
  )
}

# ============================================================
# 23. Optional Immediate Self-Test
# ============================================================

if (
  identical(
    Sys.getenv(
      "RUN_INNOVATION_DEPENDENCE_UTILS_SELF_TEST"
    ),
    "1"
  )
) {
  innovation_dependence_utils_self_test(
    verbose =
      TRUE
  )
}