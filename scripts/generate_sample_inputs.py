"""
Generate deterministic, fully synthetic sample inputs for the public
futures competition optimizer.

These files are demonstration data only. They contain no Bloomberg data,
competition participant data, account data, or fitted private artifacts.
"""

from pathlib import Path
import numpy as np
import pandas as pd


# ============================================================
# CONFIGURATION
# ============================================================

T = 147
K = 8
N_STATE = 8
N_COMPONENTS = 16

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "examples" / "sample_inputs"
OUT.mkdir(parents=True, exist_ok=True)

FUTURES = [f"Future_{i}" for i in range(1, 9)]

# Canonical competition-state representation used by the optimizer.
STATES = [
    "Delta_Final_X1",
    "Delta_Final_D12",
    "Delta_Final_D23",
    "Delta_Final_D34",
    "Delta_Final_D4_15",
    "Delta_Final_D15_16",
    "Delta_Daily_H1",
    "Delta_Daily_D12",
]

COMPONENTS = [
    "CL",
    "GC",
    "SR3",
    "ZN",
    "KE",
    "HO",
    "ZW",
    "NQ",
    "Delta_Final_X1",
    "Delta_Final_D12",
    "Delta_Final_D23",
    "Delta_Final_D34",
    "Delta_Final_D4_15",
    "Delta_Final_D15_16",
    "Delta_Daily_H1",
    "Delta_Daily_D12",
]

FUTURES_PRODUCTION_ORDER = [
    "CL",
    "GC",
    "SR3",
    "ZN",
    "KE",
    "HO",
    "ZW",
    "NQ",
]

MAPPING_FAMILY = "Monotone_Standardized_Soft_Lower_Floor"
RETURN_FLOOR = -1.0
RETURN_FLOOR_EPSILON = 1e-6


# ============================================================
# 1. FUTURES EXPECTED RETURNS
# mu.csv
# ============================================================

times = np.arange(T)

mu = pd.DataFrame({"t_original": times})

for j, future in enumerate(FUTURES):
    # Small deterministic synthetic signal.
    mu[future] = (
        0.00010 * np.sin((times + 3 * j) / 11.0)
        + 0.000025 * (j - 3.5)
    )

mu.to_csv(OUT / "mu.csv", index=False)


# ============================================================
# 2. FUTURES COVARIANCE
# Sigma.csv
#
# Diagonal synthetic covariance matrix at every t.
# ============================================================

future_sd = np.array(
    [0.010, 0.008, 0.002, 0.004, 0.012, 0.011, 0.010, 0.013],
    dtype=float,
)

sigma_rows = []

for t in times:
    for i, f1 in enumerate(FUTURES):
        for j, f2 in enumerate(FUTURES):
            covariance = (
                future_sd[i] ** 2
                if i == j
                else 0.0
            )

            sigma_rows.append(
                {
                    "t_original": t,
                    "future_1": f1,
                    "future_2": f2,
                    "covariance": covariance,
                }
            )

pd.DataFrame(sigma_rows).to_csv(
    OUT / "Sigma.csv",
    index=False,
)


# ============================================================
# 3. COMPETITION STATE MEAN
# state_mean_competition.csv
# ============================================================

state_mean = pd.DataFrame(
    {"t_original": times}
)

for j, state in enumerate(STATES):
    state_mean[state] = (
        0.00005
        * np.sin(
            (times + 2 * j) / 13.0
        )
    )

state_mean.to_csv(
    OUT / "state_mean_competition.csv",
    index=False,
)


# ============================================================
# 4. COMPETITION STATE COVARIANCE
# state_cov_competition.csv
# ============================================================

state_sd = np.array(
    [
        0.008,
        0.006,
        0.006,
        0.006,
        0.007,
        0.007,
        150.0,
        100.0,
    ],
    dtype=float,
)

state_cov_rows = []

for t in times:
    for i, s1 in enumerate(STATES):
        for j, s2 in enumerate(STATES):
            covariance = (
                state_sd[i] ** 2
                if i == j
                else 0.0
            )

            state_cov_rows.append(
                {
                    "t_original": t,
                    "state_1": s1,
                    "state_2": s2,
                    "covariance": covariance,
                }
            )

pd.DataFrame(state_cov_rows).to_csv(
    OUT / "state_cov_competition.csv",
    index=False,
)


# ============================================================
# 5. STATE / FUTURES CROSS-COVARIANCE
# state_futures_cov_competition.csv
#
# Zero cross-covariance is deliberately used for the public
# synthetic example. It is valid and keeps the demo transparent.
# ============================================================

cross_rows = []

for t in times:
    for state in STATES:
        for future in FUTURES:
            cross_rows.append(
                {
                    "t_original": t,
                    "state": state,
                    "future": future,
                    "covariance": 0.0,
                }
            )

pd.DataFrame(cross_rows).to_csv(
    OUT / "state_futures_cov_competition.csv",
    index=False,
)


# ============================================================
# 6. INNOVATION MARGINALS
# innovation_marginals.csv
#
# Public demo uses normal standardized marginals.
# ============================================================

marginal_rows = []

for i, component in enumerate(COMPONENTS, start=1):
    marginal_rows.append(
        {
            "component": component,
            "component_index": i,
            "distribution": "normal",
            "df": np.nan,
            "shape": np.nan,
            "skew": np.nan,
        }
    )

pd.DataFrame(marginal_rows).to_csv(
    OUT / "innovation_marginals.csv",
    index=False,
)


# ============================================================
# 7. 16D LATENT CORRELATION
# innovation_latent_correlation.csv
#
# Identity correlation at every competition time.
# ============================================================

latent_rows = []

for t in times:
    for i, c1 in enumerate(COMPONENTS, start=1):
        for j, c2 in enumerate(COMPONENTS, start=1):
            latent_rows.append(
                {
                    "t_original": t,
                    "component_1_index": i,
                    "component_2_index": j,
                    "component_1": c1,
                    "component_2": c2,
                    "correlation": 1.0 if i == j else 0.0,
                }
            )

pd.DataFrame(latent_rows).to_csv(
    OUT / "innovation_latent_correlation.csv",
    index=False,
)


# ============================================================
# 8. ARITHMETIC RETURN MAPPING
# innovation_arithmetic_mapping.csv
# ============================================================

mapping_rows = []

for i, future in enumerate(
    FUTURES_PRODUCTION_ORDER,
    start=1,
):
    mapping_rows.append(
        {
            "future": future,
            "future_index": i,
            "mapping_family": MAPPING_FAMILY,
            "floor_parameter": 1.0,
            "raw_mean": 0.0,
            "raw_sd": 1.0,
            "return_floor": RETURN_FLOOR,
            "return_floor_epsilon": RETURN_FLOOR_EPSILON,
        }
    )

pd.DataFrame(mapping_rows).to_csv(
    OUT / "innovation_arithmetic_mapping.csv",
    index=False,
)


# ============================================================
# 9. INNOVATION SYSTEM METADATA
# innovation_system_metadata.csv
# ============================================================

metadata = {
    "interface_version":
        "1.0.0",

    "system_type":
        "practice_heterogeneous_distributional",

    "serialization_step":
        "7A",

    "practice_mode":
        "heterogeneous_distributional",

    "production_time_count":
        str(T),

    "future_count":
        "8",

    "final_state_count":
        "6",

    "daily_state_count":
        "2",

    "component_count":
        "16",

    "final_state_copula":
        "student_t",

    "final_state_student_t_df":
        "6.0",

    "futures_dependence":
        "Gaussian_Copula",

    "final_state_dependence":
        "StudentT_Copula",

    "daily_state_dependence":
        "Existing_Gaussian_Dependence",

    "cross_system_dependence":
        "Existing_Conservative_PostShrinkage_Latent_Gaussian_Coupling",

    "canonical_component_order":
        "|".join(COMPONENTS),

    "futures_order":
        "|".join(FUTURES_PRODUCTION_ORDER),

    "arithmetic_mapping_family":
        MAPPING_FAMILY,

    "return_floor":
        str(RETURN_FLOOR),

    "return_floor_epsilon":
        str(RETURN_FLOOR_EPSILON),
}

# Additional metadata required by the optimizer's production-interface loader.
# These describe interface compatibility only; all public example inputs
# remain deterministic synthetic demonstration data.
metadata.update({
    "arithmetic_mapping_version": "50.25",
    "arithmetic_return_mapping_final": "TRUE",
    "old_off_diagonal_arithmetic_covariance_policy": "Benchmark_Only",
    "marginal_system_version": "50.0",
    "dependence_system_version": "50.1",
    "joint_coupling_version": "50.15",
    "marginal_production_parameters_final": "TRUE",
    "dependence_production_parameters_final": "TRUE",
    "joint_16d_coupling_final": "TRUE",
    "python_monte_carlo_modified": "FALSE",
})

pd.DataFrame(
    {
        "key": list(metadata.keys()),
        "value": list(metadata.values()),
    }
).to_csv(
    OUT / "innovation_system_metadata.csv",
    index=False,
)


# ============================================================
# 10. MARGIN FORECASTS
# margin.csv
#
# Synthetic positive dollar margin requirements.
# ============================================================

synthetic_margin = np.array(
    [
        750.0,   # Future_1
        1100.0,  # Future_2
        500.0,   # Future_3
        1800.0,  # Future_4
        2200.0,  # Future_5
        2600.0,  # Future_6
        1700.0,  # Future_7
        2400.0,  # Future_8
    ],
    dtype=float,
)

margin = pd.DataFrame(
    {"t_original": times}
)

for j, future in enumerate(FUTURES):
    margin[future] = synthetic_margin[j]

margin.to_csv(
    OUT / "margin.csv",
    index=False,
)


# ============================================================
# 11. CURRENT STATE
# current_state.csv
#
# One synthetic row. All carried positions are flat, matching
# the public notebook's zero actual-product starting positions.
# ============================================================

current_state = {
    "our_balance": 25000.0,
    "our_rank": 100,
    "balance_1st": 32000.0,
    "balance_2nd": 30500.0,
    "balance_3rd": 29500.0,
    "balance_4th": 28500.0,
    "balance_15th": 26000.0,
    "balance_16th": 25750.0,
    "our_daily_pnl": 0.0,
    "our_daily_rank": 100,
    "daily_1st_pnl": 1500.0,
    "daily_2nd_pnl": 1200.0,
}

for future in FUTURES:
    current_state[f"weight_{future}"] = 0.0

# Positive synthetic contract notionals.
synthetic_notional = [
    70000.0,
    250000.0,
    2500.0,
    110000.0,
    50000.0,
    42000.0,
    35000.0,
    220000.0,
]

for future, notional in zip(
    FUTURES,
    synthetic_notional,
):
    current_state[
        f"notional_{future}"
    ] = notional

pd.DataFrame(
    [current_state]
).to_csv(
    OUT / "current_state.csv",
    index=False,
)


# ============================================================
# FINAL VALIDATION
# ============================================================

expected_files = [
    "mu.csv",
    "Sigma.csv",
    "state_mean_competition.csv",
    "state_cov_competition.csv",
    "state_futures_cov_competition.csv",
    "innovation_marginals.csv",
    "innovation_latent_correlation.csv",
    "innovation_arithmetic_mapping.csv",
    "innovation_system_metadata.csv",
    "margin.csv",
    "current_state.csv",
]

missing = [
    name
    for name in expected_files
    if not (OUT / name).exists()
]

if missing:
    raise RuntimeError(
        f"Failed to generate: {missing}"
    )

print()
print("Synthetic sample-input generation complete.")
print(f"Output directory: {OUT}")
print()
print("Generated files:")

for name in expected_files:
    path = OUT / name
    rows = len(pd.read_csv(path))
    print(f"  {name:<42} {rows:>6} rows")

print()
print(
    "All values are deterministic synthetic demonstration data."
)
