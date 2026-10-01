# Futures Competition Portfolio Optimizer

An end-to-end research and portfolio-construction system built for a futures trading competition. The project combines an R forecasting/dependence pipeline with a Python optimizer that simulates both futures returns and competition state, maximizes expected prize value, and converts continuous portfolio targets into executable futures positions subject to implementation constraints.

The public repository contains the modeling and optimization source code plus a deterministic synthetic example. Real competition/account data, Bloomberg exports, and fitted production artifacts are intentionally excluded.

## What the system does

The production workflow has two main layers:

1. **Forecasting and dependence modeling (R)**
   - parses and aligns futures data to the competition clock;
   - builds conditional mean and variance forecasts for eight futures;
   - models competitor/final-rank and daily-prize state processes;
   - estimates futures, competition-state, and cross-system dependence;
   - constructs the 11 files consumed by the optimizer.

2. **Portfolio optimization and implementation (Python)**
   - loads time-varying futures and competition-state forecasts;
   - simulates joint futures/competition paths using the supplied marginal and dependence system;
   - evaluates final-leaderboard and daily-prize expected value;
   - solves for a continuous futures allocation under a gross-exposure constraint;
   - enumerates executable contract combinations around the continuous target;
   - applies margin and transition constraints;
   - selects an executable portfolio and produces explicit trade instructions.

The frozen eight-underlying production universe is:

`CL`, `GC`, `SR3`, `ZN`, `KE`, `HO`, `ZW`, `NQ`.

Where appropriate, the implementation layer can use alternate contract sizes (for example micro or smaller contracts) to approximate the desired underlying exposure.

## Architecture

```text
Historical / live market data + competition state
                    |
                    v
          R forecasting pipeline
                    |
        +-----------+-----------+
        |                       |
        v                       v
 Futures forecasts       Competition-state forecasts
        |                       |
        +-----------+-----------+
                    |
                    v
        Dependence / innovation system
                    |
                    v
          11 optimizer input files
                    |
                    v
          Python joint simulation
                    |
                    v
       Continuous prize-EV optimizer
                    |
                    v
       Discrete executable search
                    |
                    v
     Holdings / exposures / trade list
```

## Repository structure

```text
futures-competition-optimizer/
├── optimizer/
│   └── futures_competition_optimizer.ipynb
├── scripts/
│   ├── futures_data.R
│   ├── futures_models.R
│   ├── futures_forecasting.R
│   ├── competitor_data.R
│   ├── competitor_models.R
│   ├── competitor_forecasting.R
│   ├── daily_competitor_data.R
│   ├── daily_competitor_models.R
│   ├── daily_competitor_forecasting.R
│   ├── competitor_futures_dependence.R
│   ├── innovation_dependence_utils.R
│   ├── joint_competition_forecasting.R
│   ├── generate_sample_inputs.py
│   └── live/
├── examples/
│   ├── sample_inputs/
│   └── sample_output/
├── requirements.txt
├── R-packages.txt
└── .gitignore
```

## Optimizer inputs

The live R pipeline produces the following interface:

```text
mu.csv
Sigma.csv
state_mean_competition.csv
state_cov_competition.csv
state_futures_cov_competition.csv
innovation_marginals.csv
innovation_latent_correlation.csv
innovation_arithmetic_mapping.csv
innovation_system_metadata.csv
margin.csv
current_state.csv
```

The public `examples/sample_inputs/` directory contains deterministic synthetic versions of all 11 files so the optimizer interface can be inspected and demonstrated without exposing private or licensed data.

## Synthetic end-to-end example

Generate/re-generate the public sample inputs from the repository root:

```bash
python scripts/generate_sample_inputs.py
```

The optimizer notebook is configured to read from:

```text
examples/sample_inputs/
```

Open:

```text
optimizer/futures_competition_optimizer.ipynb
```

and run the optimizer cells. For a lightweight demonstration, reduce the Monte Carlo and retained-candidate controls from the production/research settings before execution.

A committed example run is available in `examples/sample_output/`:

```text
sample_trades.csv
sample_holdings.csv
sample_exposures.csv
sample_summary.csv
```

For the included synthetic state, the example continuous target concentrated on synthetic GC exposure. The discrete implementation stage enumerated executable contract combinations and selected a combination of GC, MGC, and 1OZ contracts that closely matched the target while satisfying the example margin/gross-exposure constraints. These results are illustrative only and are not trading recommendations.

## Python setup

Python dependencies are listed in `requirements.txt`.

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

Core optimizer dependencies are NumPy, Pandas, and SciPy. `yfinance` is used by the optional Yahoo-based live-data updater.

## R setup

Required R packages are listed in `R-packages.txt`. They include the tidyverse/data-manipulation stack plus `forecast` and `rugarch` for time-series and volatility modeling.

One way to install them is:

```r
install.packages(readLines("R-packages.txt"))
```

## Public demo vs. production pipeline

The repository is designed to show the full modeling architecture without redistributing restricted inputs.

**Included publicly**
- forecasting/modeling source code;
- optimizer source code;
- live-pipeline orchestration code;
- deterministic synthetic optimizer inputs;
- synthetic optimizer outputs.

**Not included**
- Bloomberg source/export data;
- real competition participant, leaderboard, or account-state files;
- fitted production `.rds` model objects;
- generated production outputs and caches.

As a result, the synthetic optimizer demonstration is reproducible from the public files, while the full live forecasting pipeline is **not turnkey** without the private/licensed data and fitted production artifacts used during the competition.

## Modeling notes

The source includes:

- competition-aligned futures return construction and roll handling;
- candidate conditional-mean and conditional-variance models;
- expanding-origin out-of-sample forecasting infrastructure;
- competitor final-rank and daily-prize state modeling;
- standardized innovation and marginal-distribution handling;
- Gaussian and Student-t dependence components;
- joint futures/competition-state covariance and dependence construction;
- arithmetic-return mapping;
- Monte Carlo prize evaluation;
- continuous gross-exposure-constrained portfolio optimization;
- discrete contract enumeration, margin checks, and executable trade generation.

The production clock contains 147 competition intervals. The optimizer's competition-state representation contains eight futures components and eight competition-state components in the joint innovation/dependence interface.

## Data and licensing

No Bloomberg data is distributed in this repository. Bloomberg-related scripts are included only to document the data-processing and modeling workflow developed around the licensed source data. The synthetic files under `examples/` were generated specifically for this public repository and do not contain Bloomberg observations or real competition/account information.

Yahoo Finance support is provided as an alternate live-data acquisition path in the pipeline source; users are responsible for complying with the terms of any data provider they use.

## Disclaimer

This repository is a research/educational project created for a simulated futures trading competition. The included synthetic results are not investment advice or a recommendation to trade any instrument.
