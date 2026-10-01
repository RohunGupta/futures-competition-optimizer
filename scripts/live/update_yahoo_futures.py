# ============================================================
# scripts/live/update_yahoo_futures.py
# ============================================================
#
# STAGES 1-2: ACQUISITION + YAHOO-ONLY PERSISTENCE
#
# Purpose:
#
#   Test the Yahoo futures acquisition layer without modifying
#   any production file.
#
#   For each optimizer future:
#
#     1. Read the latest stored production timestamp.
#     2. Query Yahoo's current generic underlyingSymbol.
#     3. Download Yahoo generic and dated-contract 15m data.
#     4. Convert Yahoo data to 30m endpoint observations.
#     5. Verify that the dated contract matches the generic.
#     6. Construct proposed price observations in memory.
#     7. Detect whether a gap exists from production history.
#     8. Print diagnostics.
#
# IMPORTANT:
#
#   This script DOES NOT write any file.
#
#   In particular, it does NOT modify:
#
#     data/raw/*
#     data/live_inputs/futures_history.csv
#     data/live_inputs/futures_roll_history.csv
#
#   Stage 1 is intentionally read-only.
#
# ============================================================


from pathlib import Path
from datetime import datetime, timedelta
from zoneinfo import ZoneInfo
import re

import numpy as np
import pandas as pd
import yfinance as yf


# ============================================================
# 1. Project paths
# ============================================================

PROJECT_ROOT = Path(__file__).resolve().parents[2]

PRODUCTION_HISTORY_PATH = (
    PROJECT_ROOT
    / "data"
    / "live_inputs"
    / "futures_history.csv"
)

PRODUCTION_ROLL_PATH = (
    PROJECT_ROOT
    / "data"
    / "live_inputs"
    / "futures_roll_history.csv"
)

# ============================================================
# Stage 2 Yahoo-only persistent archives
# ============================================================
#
# These files are intentionally isolated from:
#
#   data/raw/
#   data/live_inputs/
#
# Stage 2 never writes to the production Bloomberg pipeline.
#
# ============================================================

YAHOO_DATA_DIR = (
    PROJECT_ROOT
    / "data"
    / "yahoo"
)

YAHOO_HISTORY_PATH = (
    YAHOO_DATA_DIR
    / "yahoo_futures_history.csv"
)

YAHOO_ROLL_PATH = (
    YAHOO_DATA_DIR
    / "yahoo_futures_roll_history.csv"
)

YAHOO_STATE_PATH = (
    YAHOO_DATA_DIR
    / "yahoo_updater_state.csv"
)

YAHOO_IDENTITY_PATH = (
    YAHOO_DATA_DIR
    / "yahoo_contract_identity.csv"
)

# ============================================================
# Stage 4 shadow canonical adapter paths
# ============================================================

YAHOO_SHADOW_HISTORY_PATH = (
    YAHOO_DATA_DIR
    / "yahoo_shadow_futures_history.csv"
)

YAHOO_SHADOW_IDENTITY_PATH = (
    YAHOO_DATA_DIR
    / "yahoo_shadow_contract_identity.csv"
)

# ============================================================
# 2. Time zones
# ============================================================

CT_TZ = ZoneInfo("America/Chicago")
ET_TZ = ZoneInfo("America/New_York")


# ============================================================
# 3. Canonical futures
# ============================================================

SYMBOLS = [
    "CL",
    "GC",
    "SR3",
    "ZN",
    "KE",
    "HO",
    "ZW",
    "NQ",
]


GENERIC_TICKERS = {
    "CL": "CL=F",
    "GC": "GC=F",
    "SR3": "SR3=F",
    "ZN": "ZN=F",
    "KE": "KE=F",
    "HO": "HO=F",
    "ZW": "ZW=F",
    "NQ": "NQ=F",
}


# ============================================================
# 4. Yahoo dated-contract validation
# ============================================================
#
# Yahoo currently reports dated contracts using roots such as:
#
#   CLX26.NYM
#   GCZ26.CMX
#   SR3Z26.CME
#   ZNZ26.CBT
#   KEZ26.CBT
#   HOX26.NYM
#   ZWZ26.CBT
#   NQZ26.CME
#
# We validate the returned ticker rather than blindly trusting
# arbitrary metadata.
#
# ============================================================

DATED_TICKER_PATTERNS = {
    "CL": r"^CL[A-Z][0-9]{2}\.NYM$",
    "GC": r"^GC[A-Z][0-9]{2}\.CMX$",
    "SR3": r"^SR3[A-Z][0-9]{2}\.CME$",
    "ZN": r"^ZN[A-Z][0-9]{2}\.CBT$",
    "KE": r"^KE[A-Z][0-9]{2}\.CBT$",
    "HO": r"^HO[A-Z][0-9]{2}\.NYM$",
    "ZW": r"^ZW[A-Z][0-9]{2}\.CBT$",
    "NQ": r"^NQ[A-Z][0-9]{2}\.CME$",
}


# ============================================================
# 5. Diagnostic controls
# ============================================================

DOWNLOAD_LOOKBACK_DAYS = 7

PRICE_MATCH_ABS_TOL = 1e-8
PRICE_MATCH_REL_TOL = 1e-10

MIN_MATCHED_BARS = 1

# Stage 1 deliberately does not attempt historical gap
# reconstruction. It only detects and reports the gap.
ENABLE_GAP_RECONSTRUCTION = True


# Stage 2 enables persistent Yahoo-only archives.
ENABLE_YAHOO_ARCHIVE_WRITES = True

# Yahoo may revise previously published intraday bars.
# For the exact same Symbol/YahooDatedTicker/EndTimestampUTC
# key, the newest Yahoo observation is authoritative.
#
# Contract identity remains part of the key, so this does NOT
# silently reconcile a different dated contract at the same
# symbol/timestamp.
ARCHIVE_PRICE_ABS_TOL = 1e-8
ARCHIVE_PRICE_REL_TOL = 1e-10


# ============================================================
# Stage 3 controls
# ============================================================

# Stage 3 reconstructs the dated contract used by Yahoo's
# generic futures series at every available 30m timestamp.

ENABLE_TIMESTAMP_IDENTITY_RECONSTRUCTION = True

# Stage 4 builds shadow canonical inputs only.
# It NEVER writes data/live_inputs/.
ENABLE_STAGE4_SHADOW_ADAPTER = True

# Candidate generation extends around the download window.
# This is intentionally broader than the seven-day price
# window so nearby prior/current/next contracts are considered.
CANDIDATE_MONTHS_BEFORE = 4
CANDIDATE_MONTHS_AFTER = 8

# Futures month codes.
MONTH_CODES = {
    1: "F",
    2: "G",
    3: "H",
    4: "J",
    5: "K",
    6: "M",
    7: "N",
    8: "Q",
    9: "U",
    10: "V",
    11: "X",
    12: "Z",
}

# Plausible listed month cycles for the eight Yahoo contracts.
CONTRACT_MONTHS = {
    "CL": set(range(1, 13)),
    "GC": {2, 4, 6, 8, 10, 12},
    "SR3": {3, 6, 9, 12},
    "ZN": {3, 6, 9, 12},
    "KE": {3, 5, 7, 9, 12},
    "HO": set(range(1, 13)),
    "ZW": {3, 5, 7, 9, 12},
    "NQ": {3, 6, 9, 12},
}

YAHOO_CONTRACT_SUFFIX = {
    "CL": ".NYM",
    "GC": ".CMX",
    "SR3": ".CME",
    "ZN": ".CBT",
    "KE": ".CBT",
    "HO": ".NYM",
    "ZW": ".CBT",
    "NQ": ".CME",
}


# ============================================================
# 6. Utility functions
# ============================================================

def section(title):
    print()
    print("=" * 80)
    print(title)
    print("=" * 80)


def normalize_timestamp_series(series):
    return pd.to_datetime(
        series,
        utc=True,
        errors="coerce",
    )


def safe_float(x):
    try:
        value = float(x)
    except Exception:
        return np.nan

    if not np.isfinite(value):
        return np.nan

    return value


# ============================================================
# 7. Read production futures history
# ============================================================

def read_production_history(path):

    if not path.exists():
        raise FileNotFoundError(
            "Production futures history does not exist:\n"
            f"{path}"
        )

    df = pd.read_csv(path)

    required = {
        "Symbol",
        "EndTimestampET",
        "EndTimestampCT",
        "Close",
    }

    missing = required.difference(df.columns)

    if missing:
        raise ValueError(
            "Production futures history is missing required "
            "columns: "
            + ", ".join(sorted(missing))
        )

    df = df.copy()

    df["Symbol"] = (
        df["Symbol"]
        .astype(str)
        .str.strip()
        .str.upper()
    )

    df["EndTimestampET_UTC"] = (
        normalize_timestamp_series(
            df["EndTimestampET"]
        )
    )

    df["EndTimestampCT_UTC"] = (
        normalize_timestamp_series(
            df["EndTimestampCT"]
        )
    )

    df["Close"] = pd.to_numeric(
        df["Close"],
        errors="coerce",
    )

    bad_symbols = sorted(
        set(df["Symbol"].dropna())
        .difference(SYMBOLS)
    )

    if bad_symbols:
        raise ValueError(
            "Unexpected symbols in production futures history: "
            + ", ".join(bad_symbols)
        )

    bad_rows = df[
        df["EndTimestampET_UTC"].isna()
        | df["EndTimestampCT_UTC"].isna()
        | df["Close"].isna()
        | ~np.isfinite(df["Close"])
        | (df["Close"] <= 0)
    ]

    if not bad_rows.empty:
        raise ValueError(
            "Production futures history contains invalid "
            "timestamp or Close observations."
        )

    duplicates = (
        df.groupby(
            ["Symbol", "EndTimestampET_UTC"]
        )
        .size()
    )

    duplicates = duplicates[
        duplicates > 1
    ]

    if not duplicates.empty:
        raise ValueError(
            "Production futures history contains duplicate "
            "Symbol/EndTimestampET observations."
        )

    missing_symbols = [
        symbol
        for symbol in SYMBOLS
        if symbol not in set(df["Symbol"])
    ]

    if missing_symbols:
        raise ValueError(
            "Production futures history is missing symbols: "
            + ", ".join(missing_symbols)
        )

    return df


# ============================================================
# 8. Read production roll history
# ============================================================

def read_production_roll_history(path):

    if not path.exists():
        raise FileNotFoundError(
            "Production futures roll history does not exist:\n"
            f"{path}"
        )

    df = pd.read_csv(path)

    required = {
        "Symbol",
        "Date",
        "UnderlyingContract",
    }

    missing = required.difference(df.columns)

    if missing:
        raise ValueError(
            "Production futures roll history is missing "
            "required columns: "
            + ", ".join(sorted(missing))
        )

    df = df.copy()

    df["Symbol"] = (
        df["Symbol"]
        .astype(str)
        .str.strip()
        .str.upper()
    )

    df["DateParsed"] = pd.to_datetime(
        df["Date"],
        errors="coerce",
    )

    if df["DateParsed"].isna().any():
        raise ValueError(
            "Production roll history contains invalid dates."
        )

    if (
        df["UnderlyingContract"]
        .isna()
        .any()
    ):
        raise ValueError(
            "Production roll history contains missing "
            "contract identities."
        )

    duplicates = (
        df.groupby(
            ["Symbol", "DateParsed"]
        )
        .size()
    )

    duplicates = duplicates[
        duplicates > 1
    ]

    if not duplicates.empty:
        raise ValueError(
            "Production roll history contains duplicate "
            "Symbol/Date observations."
        )

    return df


# ============================================================
# 9. Latest production state
# ============================================================

def build_latest_production_state(
    history,
    roll_history,
):

    rows = []

    for symbol in SYMBOLS:

        hist_sub = (
            history[
                history["Symbol"] == symbol
            ]
            .sort_values(
                "EndTimestampCT_UTC"
            )
        )

        roll_sub = (
            roll_history[
                roll_history["Symbol"] == symbol
            ]
            .sort_values(
                "DateParsed"
            )
        )

        latest_hist = hist_sub.iloc[-1]

        latest_roll = (
            roll_sub.iloc[-1]
            if not roll_sub.empty
            else None
        )

        rows.append(
            {
                "Symbol": symbol,
                "ProductionLastTimestampUTC": (
                    latest_hist[
                        "EndTimestampCT_UTC"
                    ]
                ),
                "ProductionLastClose": (
                    float(
                        latest_hist["Close"]
                    )
                ),
                "ProductionLastRollDate": (
                    latest_roll["DateParsed"]
                    if latest_roll is not None
                    else pd.NaT
                ),
                "ProductionLastContract": (
                    str(
                        latest_roll[
                            "UnderlyingContract"
                        ]
                    )
                    if latest_roll is not None
                    else None
                ),
            }
        )

    return pd.DataFrame(rows)


# ============================================================
# 10. Query Yahoo current underlying contract
# ============================================================

def get_current_underlying_ticker(
    symbol,
    generic_ticker,
):

    ticker = yf.Ticker(
        generic_ticker
    )

    metadata_sources = []

    # First try get_info().
    try:
        info = ticker.get_info()

        if isinstance(info, dict):
            metadata_sources.append(info)

    except Exception as exc:
        print(
            f"{symbol}: get_info() warning: {exc}"
        )

    # Then try .info if needed.
    try:
        info2 = ticker.info

        if isinstance(info2, dict):
            metadata_sources.append(info2)

    except Exception as exc:
        print(
            f"{symbol}: info warning: {exc}"
        )

    underlying = None

    for metadata in metadata_sources:

        candidate = metadata.get(
            "underlyingSymbol"
        )

        if candidate:
            underlying = str(
                candidate
            ).strip()
            break

    if not underlying:
        raise RuntimeError(
            f"{symbol}: Yahoo did not provide "
            "underlyingSymbol."
        )

    pattern = DATED_TICKER_PATTERNS[
        symbol
    ]

    if re.fullmatch(
        pattern,
        underlying,
    ) is None:
        print(
            f"{symbol}: Yahoo underlyingSymbol "
            f"'{underlying}' is a root identity, "
            "not a dated contract."
        )
        return None

    return underlying


# ============================================================
# 11. Download Yahoo 15-minute Close data
# ============================================================

def download_yahoo_15m(
    ticker,
    start_date,
    end_date,
):

    try:
        raw = yf.download(
            ticker,
            start=start_date,
            end=end_date,
            interval="15m",
            auto_adjust=False,
            progress=False,
            prepost=False,
            threads=False,
        )

    except Exception as exc:
        raise RuntimeError(
            f"Yahoo download failed for {ticker}: {exc}"
        ) from exc

    if raw is None or raw.empty:
        raise RuntimeError(
            f"Yahoo returned no 15m data for {ticker}."
        )

    if isinstance(
        raw.columns,
        pd.MultiIndex,
    ):

        if (
            "Close"
            not in raw.columns.get_level_values(0)
        ):
            raise RuntimeError(
                f"{ticker}: Yahoo data has no Close column."
            )

        close = raw["Close"]

        if isinstance(
            close,
            pd.DataFrame,
        ):
            close = close.iloc[:, 0]

    else:

        if "Close" not in raw.columns:
            raise RuntimeError(
                f"{ticker}: Yahoo data has no Close column."
            )

        close = raw["Close"]

    close = pd.to_numeric(
        close,
        errors="coerce",
    ).dropna()

    if close.empty:
        raise RuntimeError(
            f"{ticker}: no usable Yahoo Close values."
        )

    if (
        (~np.isfinite(close.to_numpy()))
        .any()
        or (close <= 0).any()
    ):
        raise RuntimeError(
            f"{ticker}: Yahoo returned invalid Close values."
        )

    index = close.index

    if index.tz is None:
        index = index.tz_localize(
            "UTC"
        )

    index_utc = index.tz_convert(
        "UTC"
    )

    out = pd.DataFrame(
        {
            "TimestampUTC": index_utc,
            "Close": close.to_numpy(
                dtype=float
            ),
        }
    )

    out = (
        out
        .drop_duplicates(
            subset=["TimestampUTC"],
            keep="last",
        )
        .sort_values(
            "TimestampUTC"
        )
        .reset_index(drop=True)
    )

    return out

# ============================================================
# 11A. Optional Yahoo download for Stage 3 candidates
# ============================================================

def try_download_yahoo_15m(
    ticker,
    start_date,
    end_date,
):

    try:
        return download_yahoo_15m(
            ticker=ticker,
            start_date=start_date,
            end_date=end_date,
        )

    except Exception as exc:

        print(
            "Candidate unavailable: "
            f"{ticker} "
            f"({exc})"
        )

        return None

# ============================================================
# 12. Convert Yahoo 15m to diagnostic 30m endpoints
# ============================================================
#
# This intentionally uses the same convention validated in the
# Yahoo diagnostics:
#
#   - take the last available 15m Close in each 30m bucket;
#   - label the result by the END of that 30m interval.
#
# Stage 1 will compare these endpoints against the existing
# production timestamp grid before this convention is allowed
# into production.
#
# ============================================================

def yahoo_15m_to_30m(
    data_15m,
):

    temp = data_15m.copy()

    temp["TimestampCT"] = (
        temp["TimestampUTC"]
        .dt.tz_convert(CT_TZ)
    )

    temp = (
        temp[
            ["TimestampCT", "Close"]
        ]
        .set_index(
            "TimestampCT"
        )
    )

    out = (
        temp["Close"]
        .resample("30min")
        .last()
        .dropna()
        .to_frame()
        .reset_index()
    )

    out["EndTimestampCT"] = (
        out["TimestampCT"]
        + pd.Timedelta(
            minutes=30
        )
    )

    out = out.drop(
        columns=["TimestampCT"]
    )

    out["EndTimestampET"] = (
        out["EndTimestampCT"]
        .dt.tz_convert(ET_TZ)
    )

    out["EndTimestampUTC"] = (
        out["EndTimestampCT"]
        .dt.tz_convert("UTC")
    )

    return (
        out[
            [
                "EndTimestampUTC",
                "EndTimestampET",
                "EndTimestampCT",
                "Close",
            ]
        ]
        .sort_values(
            "EndTimestampUTC"
        )
        .reset_index(drop=True)
    )


# ============================================================
# 13. Compare generic and dated contract
# ============================================================

def compare_generic_and_dated(
    generic_30m,
    dated_30m,
):

    merged = generic_30m.merge(
        dated_30m,
        on="EndTimestampUTC",
        how="inner",
        suffixes=(
            "_Generic",
            "_Dated",
        ),
    )

    if merged.empty:
        raise RuntimeError(
            "Generic and dated contract have no "
            "overlapping 30m observations."
        )

    merged["AbsPriceDiff"] = (
        merged["Close_Generic"]
        - merged["Close_Dated"]
    ).abs()

    scale = np.maximum(
        1.0,
        np.maximum(
            np.abs(
                merged[
                    "Close_Generic"
                ]
            ),
            np.abs(
                merged[
                    "Close_Dated"
                ]
            ),
        ),
    )

    merged["RelPriceDiff"] = (
        merged["AbsPriceDiff"]
        / scale
    )

    merged["ExactWithinTolerance"] = (
        (
            merged["AbsPriceDiff"]
            <= PRICE_MATCH_ABS_TOL
        )
        | (
            merged["RelPriceDiff"]
            <= PRICE_MATCH_REL_TOL
        )
    )

    return merged


# ============================================================
# 14. Construct proposed Yahoo observations
# ============================================================

def construct_proposed_observations(
    symbol,
    generic_ticker,
    dated_ticker,
    dated_30m,
    last_production_timestamp_utc,
):

    proposed = dated_30m.copy()

    proposed = proposed[
        proposed["EndTimestampUTC"]
        > last_production_timestamp_utc
    ].copy()

    proposed.insert(
        0,
        "Symbol",
        symbol,
    )

    proposed["YahooGenericTicker"] = (
        generic_ticker
    )

    proposed["YahooDatedTicker"] = (
        dated_ticker
    )

    proposed["Source"] = "Yahoo"

    proposed = (
        proposed[
            [
                "Symbol",
                "YahooGenericTicker",
                "YahooDatedTicker",
                "Source",
                "EndTimestampET",
                "EndTimestampCT",
                "EndTimestampUTC",
                "Close",
            ]
        ]
        .sort_values(
            "EndTimestampUTC"
        )
        .reset_index(drop=True)
    )

    return proposed


# ============================================================
# 15. Detect gap information
# ============================================================

def diagnose_gap(
    last_production_timestamp_utc,
    proposed,
    latest_yahoo_timestamp_utc,
):

    if (
        latest_yahoo_timestamp_utc
        <= last_production_timestamp_utc
    ):
        return {
            "HasNewYahooData": False,
            "FirstProposedTimestampUTC": pd.NaT,
            "LatestYahooTimestampUTC": (
                latest_yahoo_timestamp_utc
            ),
            "ElapsedMinutesToFirstProposed": np.nan,
            "PotentialGap": False,
        }

    if proposed.empty:
        return {
            "HasNewYahooData": True,
            "FirstProposedTimestampUTC": pd.NaT,
            "LatestYahooTimestampUTC": (
                latest_yahoo_timestamp_utc
            ),
            "ElapsedMinutesToFirstProposed": np.nan,
            "PotentialGap": True,
        }

    first_timestamp = (
        proposed[
            "EndTimestampUTC"
        ].iloc[0]
    )

    elapsed_minutes = (
        first_timestamp
        - last_production_timestamp_utc
    ).total_seconds() / 60.0

    # This is intentionally only a diagnostic flag.
    #
    # Futures market closures mean elapsed wall-clock time
    # greater than 30 minutes does NOT by itself prove a
    # missing tradable interval. Later stages will use the
    # competition/market clock for exact gap recovery.
    potential_gap = (
        elapsed_minutes > 30.0
    )

    return {
        "HasNewYahooData": True,
        "FirstProposedTimestampUTC": (
            first_timestamp
        ),
        "LatestYahooTimestampUTC": (
            latest_yahoo_timestamp_utc
        ),
        "ElapsedMinutesToFirstProposed": (
            elapsed_minutes
        ),
        "PotentialGap": potential_gap,
    }


# ============================================================
# 16. Stage 2 archive schema
# ============================================================

YAHOO_HISTORY_COLUMNS = [
    "Symbol",
    "YahooGenericTicker",
    "YahooDatedTicker",
    "EndTimestampUTC",
    "EndTimestampET",
    "EndTimestampCT",
    "Close",
]

YAHOO_ROLL_COLUMNS = [
    "Symbol",
    "ObservedDateCT",
    "YahooGenericTicker",
    "YahooDatedTicker",
    "ObservedAtUTC",
]

YAHOO_STATE_COLUMNS = [
    "Symbol",
    "YahooGenericTicker",
    "CurrentYahooDatedTicker",
    "LastArchivedTimestampUTC",
    "LastArchivedClose",
    "LastSuccessfulRunUTC",
]


YAHOO_IDENTITY_COLUMNS = [
    "Symbol",
    "YahooGenericTicker",
    "EndTimestampUTC",
    "EndTimestampET",
    "EndTimestampCT",
    "GenericClose",
    "YahooDatedTicker",
    "DatedClose",
    "AbsPriceDiff",
    "RelPriceDiff",
    "ExactMatchCount",
]

# ============================================================
# Stage 4 schemas
# ============================================================

STAGE4_HISTORY_COLUMNS = [
    "Symbol",
    "BloombergSecurity",
    "SourceType",
    "SourceFile",
    "SourceRow",
    "RawDateMarker",
    "RawDate",
    "RawTimeInterval",
    "StartTime",
    "EndTime",
    "StartTimestampET",
    "EndTimestampET",
    "StartTimestampCT",
    "EndTimestampCT",
    "StartDateCT",
    "StartTimeCT",
    "EndDateCT",
    "EndTimeCT",
    "NominalIntervalMinutes",
    "ActualIntervalMinutes",
    "Close",
    "NetChg",
    "Open",
    "High",
    "Low",
    "TickCount",
    "Volume",
]

STAGE4_IDENTITY_COLUMNS = [
    "Symbol",
    "EndTimestampUTC",
    "EndTimestampET",
    "EndTimestampCT",
    "YahooGenericTicker",
    "YahooDatedTicker",
]


# ============================================================
# 17. Stage 2 safety helpers
# ============================================================

def assert_yahoo_only_write_path(path):

    resolved = path.resolve()
    yahoo_root = YAHOO_DATA_DIR.resolve()

    try:
        resolved.relative_to(
            yahoo_root
        )
    except ValueError as exc:
        raise RuntimeError(
            "Stage 2 attempted to write outside "
            "data/yahoo:\n"
            f"{resolved}"
        ) from exc


def ensure_yahoo_directory():

    assert_yahoo_only_write_path(
        YAHOO_DATA_DIR
    )

    YAHOO_DATA_DIR.mkdir(
        parents=True,
        exist_ok=True,
    )


def price_values_match(
    a,
    b,
):

    a = float(a)
    b = float(b)

    abs_diff = abs(a - b)

    scale = max(
        1.0,
        abs(a),
        abs(b),
    )

    rel_diff = (
        abs_diff / scale
    )

    return (
        abs_diff
        <= ARCHIVE_PRICE_ABS_TOL
        or rel_diff
        <= ARCHIVE_PRICE_REL_TOL
    )


# ============================================================
# 18. Normalize Stage 2 history rows
# ============================================================

def normalize_yahoo_history_rows(data):

    out = data.copy()

    required = set(
        YAHOO_HISTORY_COLUMNS
    )

    missing = required.difference(
        out.columns
    )

    if missing:
        raise ValueError(
            "Yahoo history rows are missing "
            "required columns: "
            + ", ".join(
                sorted(missing)
            )
        )

    out = out[
        YAHOO_HISTORY_COLUMNS
    ].copy()

    out["Symbol"] = (
        out["Symbol"]
        .astype(str)
        .str.strip()
        .str.upper()
    )

    out["YahooGenericTicker"] = (
        out["YahooGenericTicker"]
        .astype(str)
        .str.strip()
    )

    out["YahooDatedTicker"] = (
        out["YahooDatedTicker"]
        .astype(str)
        .str.strip()
    )

    for column in [
        "EndTimestampUTC",
        "EndTimestampET",
        "EndTimestampCT",
    ]:
        out[column] = pd.to_datetime(
            out[column],
            utc=True,
            errors="coerce",
        )

    out["Close"] = pd.to_numeric(
        out["Close"],
        errors="coerce",
    )

    invalid = (
        ~out["Symbol"].isin(SYMBOLS)
        | out["EndTimestampUTC"].isna()
        | out["EndTimestampET"].isna()
        | out["EndTimestampCT"].isna()
        | out["Close"].isna()
        | ~np.isfinite(out["Close"])
        | (out["Close"] <= 0)
        | (
            out["YahooDatedTicker"]
            .str.len()
            == 0
        )
    )

    if invalid.any():
        raise ValueError(
            "Yahoo history contains invalid rows."
        )

    return out


# ============================================================
# 19. Read existing Yahoo history archive
# ============================================================

def read_yahoo_history_archive():

    if not YAHOO_HISTORY_PATH.exists():
        return pd.DataFrame(
            columns=YAHOO_HISTORY_COLUMNS
        )

    existing = pd.read_csv(
        YAHOO_HISTORY_PATH
    )

    return normalize_yahoo_history_rows(
        existing
    )


# ============================================================
# 20. Merge Yahoo history append-only
# ============================================================
#
# Archive key:
#
#   Symbol + YahooDatedTicker + EndTimestampUTC
#
# This deliberately preserves dated-contract observations even
# if Yahoo's generic contract later rolls to another contract.
#
# Yahoo may revise previously published bars. For an exact
# Symbol/YahooDatedTicker/EndTimestampUTC overlap, the newest
# Yahoo value is authoritative.
#
# The dated contract remains part of the archive key. A
# different contract identity therefore does not overwrite an
# existing contract observation.
#
# ============================================================

def merge_yahoo_history_archive(
    existing,
    incoming,
):

    existing = normalize_yahoo_history_rows(
        existing
    )

    incoming = normalize_yahoo_history_rows(
        incoming
    )

    key_columns = [
        "Symbol",
        "YahooDatedTicker",
        "EndTimestampUTC",
    ]

    existing_duplicates = (
        existing.groupby(
            key_columns
        )
        .size()
    )

    if (
        existing_duplicates > 1
    ).any():
        raise RuntimeError(
            "Existing Yahoo history archive "
            "contains duplicate keys."
        )

    incoming_duplicates = (
        incoming.groupby(
            key_columns
        )
        .size()
    )

    if (
        incoming_duplicates > 1
    ).any():
        raise RuntimeError(
            "Incoming Yahoo history contains "
            "duplicate keys."
        )

    existing_lookup = {
        (
            row.Symbol,
            row.YahooDatedTicker,
            row.EndTimestampUTC,
        ): row
        for row in existing.itertuples(
            index=False
        )
    }

    revision_keys = set()
    new_rows = []
    exact_overlap_count = 0
    price_revision_count = 0

    for row in incoming.itertuples(
        index=False
    ):

        key = (
            row.Symbol,
            row.YahooDatedTicker,
            row.EndTimestampUTC,
        )

        old = existing_lookup.get(
            key
        )

        if old is None:

            new_rows.append(
                {
                    column: getattr(
                        row,
                        column,
                    )
                    for column in (
                        YAHOO_HISTORY_COLUMNS
                    )
                }
            )

            continue

        if price_values_match(
            old.Close,
            row.Close,
        ):
            exact_overlap_count += 1
            continue

        # Same exact Symbol/contract/timestamp, but Yahoo now
        # reports a different price. Treat the newest Yahoo
        # observation as an authoritative vendor revision.
        revision_keys.add(
            key
        )

        new_rows.append(
            {
                column: getattr(
                    row,
                    column,
                )
                for column in (
                    YAHOO_HISTORY_COLUMNS
                )
            }
        )

        price_revision_count += 1
        continue

    if revision_keys:

        keep_mask = []

        for row in existing.itertuples(
            index=False
        ):

            key = (
                row.Symbol,
                row.YahooDatedTicker,
                row.EndTimestampUTC,
            )

            keep_mask.append(
                key not in revision_keys
            )

        existing_kept = existing.loc[
            keep_mask
        ].copy()

    else:

        existing_kept = (
            existing.copy()
        )

    if new_rows:

        additions = pd.DataFrame(
            new_rows,
            columns=YAHOO_HISTORY_COLUMNS,
        )

        merged = pd.concat(
            [
                existing_kept,
                additions,
            ],
            ignore_index=True,
        )

    else:

        merged = existing_kept

    merged = (
        merged[
            YAHOO_HISTORY_COLUMNS
        ]
        .sort_values(
            [
                "EndTimestampUTC",
                "Symbol",
                "YahooDatedTicker",
            ]
        )
        .reset_index(drop=True)
    )

    final_duplicates = (
        merged.groupby(
            key_columns
        )
        .size()
    )

    if (
        final_duplicates > 1
    ).any():
        raise RuntimeError(
            "Merged Yahoo history contains "
            "duplicate keys."
        )

    return (
        merged,
        {
            "ExistingRows": len(
                existing
            ),
            "IncomingRows": len(
                incoming
            ),
            "ExactOverlaps": (
                exact_overlap_count
            ),
            "PriceRevisions": (
                price_revision_count
            ),
            "NewRows": (
                len(new_rows)
                - price_revision_count
            ),
            "FinalRows": len(
                merged
            ),
        },
    )


# ============================================================
# 21. Build Stage 2 incoming history
# ============================================================
#
# Stage 2 archives the full dated-contract window downloaded
# during this run, not merely observations newer than the
# Bloomberg production history.
#
# That is intentional. The Yahoo archive must preserve dated
# contract data independently of Bloomberg.
#
# ============================================================

def build_incoming_yahoo_history(
    symbol,
    generic_ticker,
    dated_ticker,
    dated_30m,
):

    incoming = dated_30m.copy()

    incoming.insert(
        0,
        "Symbol",
        symbol,
    )

    incoming.insert(
        1,
        "YahooGenericTicker",
        generic_ticker,
    )

    incoming.insert(
        2,
        "YahooDatedTicker",
        dated_ticker,
    )

    incoming = incoming.rename(
        columns={
            "EndTimestampUTC":
                "EndTimestampUTC",
            "EndTimestampET":
                "EndTimestampET",
            "EndTimestampCT":
                "EndTimestampCT",
        }
    )

    return normalize_yahoo_history_rows(
        incoming
    )


# ============================================================
# 22. Read and merge Yahoo roll archive
# ============================================================

def read_yahoo_roll_archive():

    if not YAHOO_ROLL_PATH.exists():
        return pd.DataFrame(
            columns=YAHOO_ROLL_COLUMNS
        )

    out = pd.read_csv(
        YAHOO_ROLL_PATH
    )

    missing = set(
        YAHOO_ROLL_COLUMNS
    ).difference(
        out.columns
    )

    if missing:
        raise ValueError(
            "Yahoo roll archive is missing "
            "columns: "
            + ", ".join(
                sorted(missing)
            )
        )

    out = out[
        YAHOO_ROLL_COLUMNS
    ].copy()

    out["Symbol"] = (
        out["Symbol"]
        .astype(str)
        .str.strip()
        .str.upper()
    )

    out["ObservedDateCT"] = (
        pd.to_datetime(
            out["ObservedDateCT"],
            errors="coerce",
        )
        .dt.date
    )

    out["ObservedAtUTC"] = (
        pd.to_datetime(
            out["ObservedAtUTC"],
            utc=True,
            errors="coerce",
        )
    )

    if (
        out["ObservedDateCT"].isna().any()
        or out["ObservedAtUTC"].isna().any()
    ):
        raise ValueError(
            "Yahoo roll archive contains "
            "invalid dates/timestamps."
        )

    return out


def merge_yahoo_roll_archive(
    existing,
    incoming,
):

    key_columns = [
        "Symbol",
        "ObservedDateCT",
    ]

    if existing.empty:
        combined = incoming.copy()

    else:

        overlap = existing.merge(
            incoming,
            on=key_columns,
            how="inner",
            suffixes=(
                "_Stored",
                "_Incoming",
            ),
        )

        conflicts = overlap[
            overlap[
                "YahooDatedTicker_Stored"
            ]
            != overlap[
                "YahooDatedTicker_Incoming"
            ]
        ]

        if not conflicts.empty:
            raise RuntimeError(
                "Yahoo roll archive contains "
                "a conflicting same-day "
                "contract identity:\n"
                + conflicts.to_string(
                    index=False
                )
            )

        existing_keys = set(
            zip(
                existing["Symbol"],
                existing[
                    "ObservedDateCT"
                ],
            )
        )

        additions = incoming[
            [
                (
                    row.Symbol,
                    row.ObservedDateCT,
                )
                not in existing_keys
                for row in incoming.itertuples(
                    index=False
                )
            ]
        ]

        combined = pd.concat(
            [
                existing,
                additions,
            ],
            ignore_index=True,
        )

    combined = (
        combined[
            YAHOO_ROLL_COLUMNS
        ]
        .sort_values(
            [
                "ObservedDateCT",
                "Symbol",
            ]
        )
        .drop_duplicates(
            subset=key_columns,
            keep="first",
        )
        .reset_index(drop=True)
    )

    return combined

# ============================================================
# 22A. Latest archived Yahoo dated contract
# ============================================================

def get_latest_archived_dated_ticker(
    symbol,
    yahoo_roll_archive,
):

    if yahoo_roll_archive.empty:
        return None

    sub = yahoo_roll_archive[
        yahoo_roll_archive[
            "Symbol"
        ] == symbol
    ].copy()

    if sub.empty:
        return None

    sub = sub.sort_values(
        [
            "ObservedDateCT",
            "ObservedAtUTC",
        ]
    )

    candidate = str(
        sub.iloc[-1][
            "YahooDatedTicker"
        ]
    ).strip()

    if re.fullmatch(
        DATED_TICKER_PATTERNS[symbol],
        candidate,
    ) is None:
        return None

    return candidate

# ============================================================
# 23. Build today's Yahoo roll observations
# ============================================================

def build_current_roll_observations(
    results,
    observed_at_utc,
):

    observed_date_ct = (
        observed_at_utc
        .tz_convert(CT_TZ)
        .date()
    )

    rows = []

    for row in results.itertuples(
        index=False
    ):

        rows.append(
            {
                "Symbol": row.Symbol,
                "ObservedDateCT": (
                    observed_date_ct
                ),
                "YahooGenericTicker": (
                    row.GenericTicker
                ),
                "YahooDatedTicker": (
                    row.CurrentYahooDatedTicker
                ),
                "ObservedAtUTC": (
                    observed_at_utc
                ),
            }
        )

    return pd.DataFrame(
        rows,
        columns=YAHOO_ROLL_COLUMNS,
    )


# ============================================================
# 24. Build Yahoo updater state
# ============================================================

def build_yahoo_state(
    history,
    results,
    run_timestamp_utc,
):

    rows = []

    for result in results.itertuples(
        index=False
    ):

        sub = history[
            history["Symbol"]
            == result.Symbol
        ].copy()

        if sub.empty:
            raise RuntimeError(
                f"{result.Symbol}: no archived "
                "Yahoo history after merge."
            )

        latest = (
            sub.sort_values(
                "EndTimestampUTC"
            )
            .iloc[-1]
        )

        rows.append(
            {
                "Symbol": (
                    result.Symbol
                ),
                "YahooGenericTicker": (
                    result.GenericTicker
                ),
                "CurrentYahooDatedTicker": (
                    result.CurrentYahooDatedTicker
                ),
                "LastArchivedTimestampUTC": (
                    latest[
                        "EndTimestampUTC"
                    ]
                ),
                "LastArchivedClose": (
                    float(
                        latest["Close"]
                    )
                ),
                "LastSuccessfulRunUTC": (
                    run_timestamp_utc
                ),
            }
        )

    return pd.DataFrame(
        rows,
        columns=YAHOO_STATE_COLUMNS,
    )


# ============================================================
# 25. Validate final Yahoo archives
# ============================================================

def validate_yahoo_archives(
    history,
    roll_history,
    state,
):

    history_symbols = set(
        history["Symbol"]
    )

    missing_history_symbols = (
        set(SYMBOLS)
        .difference(
            history_symbols
        )
    )

    if missing_history_symbols:
        raise RuntimeError(
            "Yahoo history archive is missing "
            "symbols: "
            + ", ".join(
                sorted(
                    missing_history_symbols
                )
            )
        )

    roll_symbols = set(
        roll_history["Symbol"]
    )

    missing_roll_symbols = (
        set(SYMBOLS)
        .difference(
            roll_symbols
        )
    )

    if missing_roll_symbols:
        raise RuntimeError(
            "Yahoo roll archive is missing "
            "symbols: "
            + ", ".join(
                sorted(
                    missing_roll_symbols
                )
            )
        )

    if set(
        state["Symbol"]
    ) != set(SYMBOLS):
        raise RuntimeError(
            "Yahoo updater state does not "
            "contain exactly all eight symbols."
        )

    history_duplicates = (
        history.groupby(
            [
                "Symbol",
                "YahooDatedTicker",
                "EndTimestampUTC",
            ]
        )
        .size()
    )

    if (
        history_duplicates > 1
    ).any():
        raise RuntimeError(
            "Yahoo history archive contains "
            "duplicate keys after validation."
        )

    roll_duplicates = (
        roll_history.groupby(
            [
                "Symbol",
                "ObservedDateCT",
            ]
        )
        .size()
    )

    if (
        roll_duplicates > 1
    ).any():
        raise RuntimeError(
            "Yahoo roll archive contains "
            "duplicate Symbol/date rows."
        )

    if len(state) != len(SYMBOLS):
        raise RuntimeError(
            "Yahoo updater state must contain "
            "exactly eight rows."
        )

    return True


# ============================================================
# 26. Atomic CSV writer
# ============================================================

def write_csv_atomically(
    data,
    path,
):

    assert_yahoo_only_write_path(
        path
    )

    temporary_path = path.with_suffix(
        path.suffix + ".tmp"
    )

    assert_yahoo_only_write_path(
        temporary_path
    )

    data.to_csv(
        temporary_path,
        index=False,
    )

    temporary_path.replace(
        path
    )


# ============================================================
# 27. Write Stage 2 Yahoo archives
# ============================================================

def write_yahoo_archives(
    history,
    roll_history,
    state,
):

    if not ENABLE_YAHOO_ARCHIVE_WRITES:
        print(
            "Yahoo archive writes are disabled."
        )
        return

    ensure_yahoo_directory()

    write_csv_atomically(
        history,
        YAHOO_HISTORY_PATH,
    )

    write_csv_atomically(
        roll_history,
        YAHOO_ROLL_PATH,
    )

    write_csv_atomically(
        state,
        YAHOO_STATE_PATH,
    )

# ============================================================
# 28. Stage 3 candidate-contract generation
# ============================================================

def add_months(
    year,
    month,
    offset,
):

    total = (
        year * 12
        + (month - 1)
        + offset
    )

    new_year = total // 12
    new_month = total % 12 + 1

    return (
        new_year,
        new_month,
    )


def build_yahoo_dated_ticker(
    symbol,
    year,
    month,
):

    if month not in CONTRACT_MONTHS[symbol]:
        return None

    month_code = MONTH_CODES[month]

    year_code = str(year)[-2:]

    suffix = YAHOO_CONTRACT_SUFFIX[
        symbol
    ]

    return (
        f"{symbol}"
        f"{month_code}"
        f"{year_code}"
        f"{suffix}"
    )


def generate_candidate_tickers(
    symbol,
    start_date,
    end_date,
    current_ticker,
    archived_history,
):

    candidates = set()

    # Include Yahoo's current underlying when metadata supplied
    # a valid dated contract. If metadata supplied only a root
    # symbol, current_ticker is None and candidate generation
    # proceeds from the calendar/archive candidates instead.

    if (
        current_ticker is not None
        and re.fullmatch(
            DATED_TICKER_PATTERNS[symbol],
            str(current_ticker),
        )
        is not None
    ):
        candidates.add(
            current_ticker
        )

    # Include every dated contract we have already archived
    # for this symbol.
    if not archived_history.empty:

        archived = (
            archived_history[
                archived_history["Symbol"]
                == symbol
            ][
                "YahooDatedTicker"
            ]
            .dropna()
            .astype(str)
            .str.strip()
        )

        candidates.update(
            ticker
            for ticker in archived
            if ticker
        )

    anchor_year = start_date.year
    anchor_month = start_date.month

    total_offsets = range(
        -CANDIDATE_MONTHS_BEFORE,
        CANDIDATE_MONTHS_AFTER + 1,
    )

    for offset in total_offsets:

        year, month = add_months(
            anchor_year,
            anchor_month,
            offset,
        )

        ticker = (
            build_yahoo_dated_ticker(
                symbol=symbol,
                year=year,
                month=month,
            )
        )

        if ticker is not None:
            candidates.add(
                ticker
            )

    # Also anchor candidate generation around the end of the
    # requested window in case it spans a calendar boundary.
    end_anchor = (
        end_date
        - timedelta(days=1)
    )

    for offset in total_offsets:

        year, month = add_months(
            end_anchor.year,
            end_anchor.month,
            offset,
        )

        ticker = (
            build_yahoo_dated_ticker(
                symbol=symbol,
                year=year,
                month=month,
            )
        )

        if ticker is not None:
            candidates.add(
                ticker
            )

    pattern = re.compile(
        DATED_TICKER_PATTERNS[
            symbol
        ]
    )

    candidates = sorted(
        ticker
        for ticker in candidates
        if pattern.fullmatch(ticker)
    )

    return candidates


# ============================================================
# 29. Download Stage 3 candidate contracts
# ============================================================

def download_candidate_contracts(
    symbol,
    candidate_tickers,
    start_date,
    end_date,
):

    downloaded = {}

    for ticker in candidate_tickers:

        data_15m = (
            try_download_yahoo_15m(
                ticker=ticker,
                start_date=start_date,
                end_date=end_date,
            )
        )

        if data_15m is None:
            continue

        data_30m = (
            yahoo_15m_to_30m(
                data_15m
            )
        )

        if data_30m.empty:
            continue

        downloaded[ticker] = (
            data_30m
        )

    if not downloaded:
        raise RuntimeError(
            f"{symbol}: none of the generated "
            "dated-contract candidates returned "
            "usable Yahoo data."
        )

    return downloaded

# ============================================================
# 30. Add archived contract observations to candidates
# ============================================================

def add_archived_candidate_data(
    symbol,
    candidate_data,
    archived_history,
):

    if archived_history.empty:
        return candidate_data

    symbol_archive = (
        archived_history[
            archived_history["Symbol"]
            == symbol
        ]
        .copy()
    )

    if symbol_archive.empty:
        return candidate_data

    for ticker, ticker_rows in (
        symbol_archive.groupby(
            "YahooDatedTicker"
        )
    ):

        archived = (
            ticker_rows[
                [
                    "EndTimestampUTC",
                    "EndTimestampET",
                    "EndTimestampCT",
                    "Close",
                ]
            ]
            .copy()
        )

        archived = (
            archived
            .sort_values(
                "EndTimestampUTC"
            )
            .drop_duplicates(
                subset=[
                    "EndTimestampUTC"
                ],
                keep="last",
            )
            .reset_index(drop=True)
        )

        if ticker not in candidate_data:

            candidate_data[
                ticker
            ] = archived

            continue

        downloaded = (
            candidate_data[
                ticker
            ]
            .copy()
        )

        combined = pd.concat(
            [
                archived.assign(
                    _SourcePriority=0
                ),
                downloaded.assign(
                    _SourcePriority=1
                ),
            ],
            ignore_index=True,
        )

        # If Yahoo still supplies the observation, prefer the
        # newly downloaded value. The Stage 2 archive merge
        # separately enforces its historical immutability rules.
        combined = (
            combined
            .sort_values(
                [
                    "EndTimestampUTC",
                    "_SourcePriority",
                ]
            )
            .drop_duplicates(
                subset=[
                    "EndTimestampUTC"
                ],
                keep="last",
            )
            .drop(
                columns=[
                    "_SourcePriority"
                ]
            )
            .sort_values(
                "EndTimestampUTC"
            )
            .reset_index(drop=True)
        )

        candidate_data[
            ticker
        ] = combined

    return candidate_data


# ============================================================
# 31. Reconstruct generic contract identity bar by bar
# ============================================================

def reconstruct_generic_contract_identity(
    symbol,
    generic_ticker,
    generic_30m,
    candidate_data,
):

    rows = []

    candidate_lookup = {}

    for ticker, data in candidate_data.items():

        candidate_lookup[
            ticker
        ] = {
            timestamp: float(close)
            for timestamp, close in zip(
                data["EndTimestampUTC"],
                data["Close"],
            )
        }

    for generic_row in generic_30m.itertuples(
        index=False
    ):

        timestamp = (
            generic_row.EndTimestampUTC
        )

        generic_close = float(
            generic_row.Close
        )

        exact_matches = []

        for ticker, lookup in (
            candidate_lookup.items()
        ):

            if timestamp not in lookup:
                continue

            dated_close = float(
                lookup[timestamp]
            )

            abs_diff = abs(
                generic_close
                - dated_close
            )

            scale = max(
                1.0,
                abs(generic_close),
                abs(dated_close),
            )

            rel_diff = (
                abs_diff / scale
            )

            exact = (
                abs_diff
                <= PRICE_MATCH_ABS_TOL
                or rel_diff
                <= PRICE_MATCH_REL_TOL
            )

            if exact:

                exact_matches.append(
                    {
                        "Ticker": ticker,
                        "DatedClose": (
                            dated_close
                        ),
                        "AbsPriceDiff": (
                            abs_diff
                        ),
                        "RelPriceDiff": (
                            rel_diff
                        ),
                    }
                )

        if len(exact_matches) == 0:

            rows.append(
                {
                    "Symbol": symbol,
                    "YahooGenericTicker": (
                        generic_ticker
                    ),
                    "EndTimestampUTC": (
                        timestamp
                    ),
                    "EndTimestampET": (
                        generic_row
                        .EndTimestampET
                    ),
                    "EndTimestampCT": (
                        generic_row
                        .EndTimestampCT
                    ),
                    "GenericClose": (
                        generic_close
                    ),
                    "YahooDatedTicker": (
                        None
                    ),
                    "DatedClose": (
                        np.nan
                    ),
                    "AbsPriceDiff": (
                        np.nan
                    ),
                    "RelPriceDiff": (
                        np.nan
                    ),
                    "ExactMatchCount": 0,
                }
            )

            continue

        if len(exact_matches) > 1:

            # Do not silently choose among contracts that have
            # the same exact price. Preserve the ambiguity so
            # Stage 3 can fail loudly.
            rows.append(
                {
                    "Symbol": symbol,
                    "YahooGenericTicker": (
                        generic_ticker
                    ),
                    "EndTimestampUTC": (
                        timestamp
                    ),
                    "EndTimestampET": (
                        generic_row
                        .EndTimestampET
                    ),
                    "EndTimestampCT": (
                        generic_row
                        .EndTimestampCT
                    ),
                    "GenericClose": (
                        generic_close
                    ),
                    "YahooDatedTicker": (
                        "|".join(
                            sorted(
                                match["Ticker"]
                                for match
                                in exact_matches
                            )
                        )
                    ),
                    "DatedClose": (
                        np.nan
                    ),
                    "AbsPriceDiff": (
                        0.0
                    ),
                    "RelPriceDiff": (
                        0.0
                    ),
                    "ExactMatchCount": (
                        len(
                            exact_matches
                        )
                    ),
                }
            )

            continue

        match = exact_matches[0]

        rows.append(
            {
                "Symbol": symbol,
                "YahooGenericTicker": (
                    generic_ticker
                ),
                "EndTimestampUTC": (
                    timestamp
                ),
                "EndTimestampET": (
                    generic_row
                    .EndTimestampET
                ),
                "EndTimestampCT": (
                    generic_row
                    .EndTimestampCT
                ),
                "GenericClose": (
                    generic_close
                ),
                "YahooDatedTicker": (
                    match["Ticker"]
                ),
                "DatedClose": (
                    match[
                        "DatedClose"
                    ]
                ),
                "AbsPriceDiff": (
                    match[
                        "AbsPriceDiff"
                    ]
                ),
                "RelPriceDiff": (
                    match[
                        "RelPriceDiff"
                    ]
                ),
                "ExactMatchCount": 1,
            }
        )

    return pd.DataFrame(
        rows,
        columns=YAHOO_IDENTITY_COLUMNS,
    )


# ============================================================
# 32. Validate Stage 3 reconstruction
# ============================================================

def validate_reconstructed_identity(
    reconstructed,
):

    unresolved = (
        reconstructed[
            reconstructed[
                "ExactMatchCount"
            ]
            == 0
        ]
        .copy()
    )

    ambiguous = (
        reconstructed[
            reconstructed[
                "ExactMatchCount"
            ]
            > 1
        ]
        .copy()
    )

    resolved = (
        reconstructed[
            reconstructed[
                "ExactMatchCount"
            ]
            == 1
        ]
        .copy()
    )

    if not resolved.empty:

        if (
            resolved[
                "YahooDatedTicker"
            ].isna().any()
            or resolved[
                "DatedClose"
            ].isna().any()
        ):
            raise RuntimeError(
                "Resolved Stage 3 rows contain "
                "missing dated-contract data."
            )

    return (
        resolved,
        unresolved,
        ambiguous,
    )


# ============================================================
# 33. Detect reconstructed contract switches
# ============================================================

def build_identity_switch_table(
    reconstructed,
):

    resolved = (
        reconstructed[
            reconstructed[
                "ExactMatchCount"
            ]
            == 1
        ]
        .sort_values(
            [
                "Symbol",
                "EndTimestampUTC",
            ]
        )
        .copy()
    )

    resolved[
        "PreviousYahooDatedTicker"
    ] = (
        resolved
        .groupby(
            "Symbol"
        )[
            "YahooDatedTicker"
        ]
        .shift(1)
    )

    switches = (
        resolved[
            resolved[
                "PreviousYahooDatedTicker"
            ].notna()
            & (
                resolved[
                    "YahooDatedTicker"
                ]
                != resolved[
                    "PreviousYahooDatedTicker"
                ]
            )
        ]
        [
            [
                "Symbol",
                "EndTimestampUTC",
                "EndTimestampCT",
                "PreviousYahooDatedTicker",
                "YahooDatedTicker",
            ]
        ]
        .reset_index(drop=True)
    )

    return switches


# ============================================================
# 34. Read Stage 3 identity archive
# ============================================================

def read_yahoo_identity_archive():

    if not YAHOO_IDENTITY_PATH.exists():

        return pd.DataFrame(
            columns=YAHOO_IDENTITY_COLUMNS
        )

    existing = pd.read_csv(
        YAHOO_IDENTITY_PATH
    )

    missing = set(
        YAHOO_IDENTITY_COLUMNS
    ).difference(
        existing.columns
    )

    if missing:
        raise RuntimeError(
            "Yahoo identity archive is "
            "missing columns: "
            + ", ".join(
                sorted(missing)
            )
        )

    existing = existing[
        YAHOO_IDENTITY_COLUMNS
    ].copy()

    for column in [
        "EndTimestampUTC",
        "EndTimestampET",
        "EndTimestampCT",
    ]:

        existing[column] = (
            pd.to_datetime(
                existing[column],
                utc=True,
                errors="coerce",
            )
        )

    if (
        existing[
            "EndTimestampUTC"
        ].isna().any()
    ):
        raise RuntimeError(
            "Yahoo identity archive contains "
            "invalid timestamps."
        )

    existing[
        "ExactMatchCount"
    ] = pd.to_numeric(
        existing[
            "ExactMatchCount"
        ],
        errors="coerce",
    )

    return existing


# ============================================================
# 35. Merge Stage 3 identity archive
# ============================================================

def merge_yahoo_identity_archive(
    existing,
    incoming,
):

    # Only uniquely resolved observations are allowed into the
    # persistent identity archive.
    incoming = (
        incoming[
            incoming[
                "ExactMatchCount"
            ]
            == 1
        ]
        .copy()
    )

    key_columns = [
        "Symbol",
        "EndTimestampUTC",
    ]

    if existing.empty:

        merged = incoming.copy()

    else:

        overlap = existing.merge(
            incoming,
            on=key_columns,
            how="inner",
            suffixes=(
                "_Stored",
                "_Incoming",
            ),
        )

        contract_conflicts = (
            overlap[
                overlap[
                    "YahooDatedTicker_Stored"
                ]
                != overlap[
                    "YahooDatedTicker_Incoming"
                ]
            ]
        )

        if not contract_conflicts.empty:

            raise RuntimeError(
                "Stage 3 reconstructed a "
                "different dated contract for "
                "an already archived generic "
                "timestamp:\n"
                + contract_conflicts[
                    [
                        "Symbol",
                        "EndTimestampUTC",
                        "YahooDatedTicker_Stored",
                        "YahooDatedTicker_Incoming",
                    ]
                ]
                .head(20)
                .to_string(
                    index=False
                )
            )

        existing_keys = set(
            zip(
                existing["Symbol"],
                existing[
                    "EndTimestampUTC"
                ],
            )
        )

        additions = (
            incoming[
                [
                    (
                        row.Symbol,
                        row.EndTimestampUTC,
                    )
                    not in existing_keys
                    for row
                    in incoming.itertuples(
                        index=False
                    )
                ]
            ]
            .copy()
        )

        merged = pd.concat(
            [
                existing,
                additions,
            ],
            ignore_index=True,
        )

    duplicates = (
        merged.groupby(
            key_columns
        )
        .size()
    )

    if (
        duplicates > 1
    ).any():

        raise RuntimeError(
            "Yahoo identity archive contains "
            "duplicate Symbol/timestamp keys."
        )

    merged = (
        merged[
            YAHOO_IDENTITY_COLUMNS
        ]
        .sort_values(
            [
                "EndTimestampUTC",
                "Symbol",
            ]
        )
        .reset_index(drop=True)
    )

    return merged


# ============================================================
# 36. Write Stage 3 identity archive
# ============================================================

def write_yahoo_identity_archive(
    identity,
):

    write_csv_atomically(
        identity,
        YAHOO_IDENTITY_PATH,
    )

# ============================================================
# 37. Stage 4 contract-name helpers
# ============================================================

def yahoo_ticker_to_canonical_contract(
    symbol,
    yahoo_ticker,
):

    pattern = re.compile(
        DATED_TICKER_PATTERNS[
            symbol
        ]
    )

    if not pattern.fullmatch(
        yahoo_ticker
    ):
        raise ValueError(
            f"{symbol}: invalid Yahoo dated "
            f"ticker {yahoo_ticker!r}."
        )

    root_with_contract = (
        yahoo_ticker
        .split(".")[0]
    )

    if symbol == "SR3":

        contract_suffix = (
            root_with_contract[
                len("SR3"):
            ]
        )

        return (
            "SFR"
            + contract_suffix
        )

    if symbol == "ZN":

        contract_suffix = (
            root_with_contract[
                len("ZN"):
            ]
        )

        return (
            "TY"
            + contract_suffix
        )

    if symbol == "ZW":

        contract_suffix = (
            root_with_contract[
                len("ZW"):
            ]
        )

        return (
            "W"
            + contract_suffix
        )

    return root_with_contract


# ============================================================
# 38. Stage 4 timestamp serialization
# ============================================================

def format_stage4_timestamp(
    timestamp,
    timezone,
):

    timestamp = pd.Timestamp(
        timestamp
    )

    if timestamp.tzinfo is None:

        timestamp = (
            timestamp
            .tz_localize("UTC")
        )

    timestamp = (
        timestamp
        .tz_convert(timezone)
    )

    return timestamp.strftime(
        "%Y-%m-%d %H:%M:%S%z"
    )


# ============================================================
# 39. Build Stage 4 Yahoo shadow identity
# ============================================================

def build_stage4_shadow_identity(
    identity_archive,
):

    resolved = (
        identity_archive[
            identity_archive[
                "ExactMatchCount"
            ]
            == 1
        ]
        .copy()
    )

    if resolved.empty:
        raise RuntimeError(
            "Stage 4 cannot build an identity "
            "adapter from an empty identity archive."
        )

    duplicates = (
        resolved.groupby(
            [
                "Symbol",
                "EndTimestampUTC",
            ]
        )
        .size()
    )

    if (
        duplicates > 1
    ).any():
        raise RuntimeError(
            "Stage 4 identity source contains "
            "duplicate Symbol/timestamp keys."
        )

    out = resolved[
        [
            "Symbol",
            "EndTimestampUTC",
            "EndTimestampET",
            "EndTimestampCT",
            "YahooGenericTicker",
            "YahooDatedTicker",
        ]
    ].copy()

    out["EndTimestampUTC"] = (
        pd.to_datetime(
            out["EndTimestampUTC"],
            utc=True,
            errors="coerce",
        )
    )

    out["EndTimestampET"] = (
        out["EndTimestampUTC"]
        .dt.tz_convert(ET_TZ)
    )

    out["EndTimestampCT"] = (
        out["EndTimestampUTC"]
        .dt.tz_convert(CT_TZ)
    )

    if (
        out[
            [
                "EndTimestampUTC",
                "EndTimestampET",
                "EndTimestampCT",
            ]
        ]
        .isna()
        .any()
        .any()
    ):
        raise RuntimeError(
            "Stage 4 identity adapter contains "
            "invalid timestamps."
        )

    out = (
        out[
            STAGE4_IDENTITY_COLUMNS
        ]
        .sort_values(
            [
                "Symbol",
                "EndTimestampUTC",
            ]
        )
        .reset_index(drop=True)
    )

    return out


# ============================================================
# 40. Select Yahoo price actually used by generic
# ============================================================

def build_stage4_selected_yahoo_prices(
    yahoo_history,
    shadow_identity,
):

    history = (
        yahoo_history.copy()
    )

    identity = (
        shadow_identity.copy()
    )

    history[
        "EndTimestampUTC"
    ] = pd.to_datetime(
        history[
            "EndTimestampUTC"
        ],
        utc=True,
        errors="coerce",
    )

    identity[
        "EndTimestampUTC"
    ] = pd.to_datetime(
        identity[
            "EndTimestampUTC"
        ],
        utc=True,
        errors="coerce",
    )

    selected = identity.merge(
        history[
            [
                "Symbol",
                "YahooDatedTicker",
                "EndTimestampUTC",
                "Close",
            ]
        ],
        on=[
            "Symbol",
            "YahooDatedTicker",
            "EndTimestampUTC",
        ],
        how="left",
        validate="one_to_one",
    )

    missing_prices = (
        selected[
            "Close"
        ]
        .isna()
    )

    if missing_prices.any():

        print(
            selected.loc[
                missing_prices,
                [
                    "Symbol",
                    "YahooDatedTicker",
                    "EndTimestampUTC",
                ],
            ]
            .head(20)
            .to_string(
                index=False
            )
        )

        raise RuntimeError(
            "Stage 4 identity rows are missing "
            "their corresponding archived "
            "dated-contract prices."
        )

    if (
        ~np.isfinite(
            selected["Close"]
        )
    ).any() or (
        selected["Close"] <= 0
    ).any():

        raise RuntimeError(
            "Stage 4 selected Yahoo prices "
            "contain invalid Close values."
        )

    return selected


# ============================================================
# 41. Build Yahoo rows in canonical 27-column shape
# ============================================================

def build_stage4_yahoo_canonical_rows(
    selected_prices,
):

    rows = []

    for row_number, row in enumerate(
        selected_prices.itertuples(
            index=False
        ),
        start=1,
    ):

        end_utc = pd.Timestamp(
            row.EndTimestampUTC
        )

        if end_utc.tzinfo is None:
            end_utc = (
                end_utc
                .tz_localize("UTC")
            )

        end_et = (
            end_utc
            .tz_convert(ET_TZ)
        )

        end_ct = (
            end_utc
            .tz_convert(CT_TZ)
        )

        start_et = (
            end_et
            - pd.Timedelta(
                minutes=30
            )
        )

        start_ct = (
            end_ct
            - pd.Timedelta(
                minutes=30
            )
        )

        canonical_contract = (
            yahoo_ticker_to_canonical_contract(
                symbol=row.Symbol,
                yahoo_ticker=(
                    row.YahooDatedTicker
                ),
            )
        )

        rows.append(
            {
                "Symbol": row.Symbol,

                # Keep the existing canonical column name so
                # downstream schema remains unchanged. The
                # value explicitly identifies Yahoo provenance.
                "BloombergSecurity": (
                    row.YahooGenericTicker
                ),

                "SourceType": (
                    "YahooFinance"
                ),

                "SourceFile": (
                    "yahoo_shadow_adapter"
                ),

                "SourceRow": (
                    row_number
                ),

                "RawDateMarker": (
                    end_ct.strftime(
                        "%Y-%m-%d"
                    )
                ),

                # IMPORTANT:
                # For Yahoo, RawDate is the CT calendar date of
                # the actual timestamp. Stage 4 does not pretend
                # this is Bloomberg's trade/session date.
                "RawDate": (
                    end_ct.date()
                ),

                "RawTimeInterval": (
                    start_ct.strftime(
                        "%H:%M"
                    )
                    + " - "
                    + end_ct.strftime(
                        "%H:%M"
                    )
                ),

                "StartTime": (
                    start_et.strftime(
                        "%H:%M"
                    )
                ),

                "EndTime": (
                    end_et.strftime(
                        "%H:%M"
                    )
                ),

                "StartTimestampET": (
                    format_stage4_timestamp(
                        start_et,
                        ET_TZ,
                    )
                ),

                "EndTimestampET": (
                    format_stage4_timestamp(
                        end_et,
                        ET_TZ,
                    )
                ),

                "StartTimestampCT": (
                    format_stage4_timestamp(
                        start_ct,
                        CT_TZ,
                    )
                ),

                "EndTimestampCT": (
                    format_stage4_timestamp(
                        end_ct,
                        CT_TZ,
                    )
                ),

                "StartDateCT": (
                    start_ct.date()
                ),

                "StartTimeCT": (
                    start_ct.strftime(
                        "%H:%M"
                    )
                ),

                "EndDateCT": (
                    end_ct.date()
                ),

                "EndTimeCT": (
                    end_ct.strftime(
                        "%H:%M"
                    )
                ),

                "NominalIntervalMinutes": 30,

                "ActualIntervalMinutes": 30,

                "Close": float(
                    row.Close
                ),

                # Yahoo Stage 1-4 only requires Close for the
                # downstream return builder. Do not fabricate
                # Bloomberg-style OHLC/volume fields.
                "NetChg": np.nan,
                "Open": np.nan,
                "High": np.nan,
                "Low": np.nan,
                "TickCount": np.nan,
                "Volume": np.nan,

                # Local helper; removed before canonical output.
                "_YahooDatedTicker": (
                    row.YahooDatedTicker
                ),

                "_CanonicalContract": (
                    canonical_contract
                ),
            }
        )

    return pd.DataFrame(
        rows
    )


# ============================================================
# 42. Validate Stage 4 Yahoo canonical rows
# ============================================================

def validate_stage4_yahoo_rows(
    yahoo_rows,
):

    if yahoo_rows.empty:
        raise RuntimeError(
            "Stage 4 produced zero Yahoo "
            "canonical rows."
        )

    missing_symbols = (
        set(SYMBOLS)
        .difference(
            set(
                yahoo_rows[
                    "Symbol"
                ]
            )
        )
    )

    if missing_symbols:
        raise RuntimeError(
            "Stage 4 Yahoo rows are missing "
            "symbols: "
            + ", ".join(
                sorted(
                    missing_symbols
                )
            )
        )

    duplicate_count = int(
        yahoo_rows.duplicated(
            subset=[
                "Symbol",
                "EndTimestampCT",
            ]
        ).sum()
    )

    if duplicate_count != 0:
        raise RuntimeError(
            "Stage 4 Yahoo rows contain "
            "duplicate Symbol/timestamp keys."
        )

    close_values = pd.to_numeric(
        yahoo_rows["Close"],
        errors="coerce",
    )

    if (
        close_values.isna().any()
        or (
            ~np.isfinite(
                close_values
            )
        ).any()
        or (
            close_values <= 0
        ).any()
    ):
        raise RuntimeError(
            "Stage 4 Yahoo rows contain "
            "invalid Close values."
        )


# ============================================================
# 43. Build hybrid shadow canonical history
# ============================================================

def build_stage4_shadow_history(
    production_history,
    yahoo_rows,
):

    production = (
        production_history.copy()
    )

    yahoo = (
        yahoo_rows.copy()
    )

    # Stage 4 is a transition adapter:
    #
    #   historical portion -> frozen Bloomberg canonical rows
    #   new portion        -> validated Yahoo rows
    #
    # Yahoo is allowed to replace Bloomberg only at timestamps
    # strictly AFTER the latest stored production timestamp for
    # each symbol. Existing production history remains untouched.

    production_end = (
        production.assign(
            _EndUTC=pd.to_datetime(
                production[
                    "EndTimestampCT"
                ],
                utc=True,
                errors="coerce",
            )
        )
        .groupby(
            "Symbol"
        )["_EndUTC"]
        .max()
        .to_dict()
    )

    yahoo[
        "_EndUTC"
    ] = pd.to_datetime(
        yahoo[
            "EndTimestampCT"
        ],
        utc=True,
        errors="coerce",
    )

    keep_yahoo = []

    for symbol, end_utc in zip(
        yahoo["Symbol"],
        yahoo["_EndUTC"],
    ):

        cutoff = production_end.get(
            symbol
        )

        keep_yahoo.append(
            cutoff is None
            or end_utc > cutoff
        )

    yahoo_new = (
        yahoo.loc[
            keep_yahoo
        ]
        .copy()
    )

    yahoo_new = yahoo_new.drop(
        columns=[
            "_EndUTC",
            "_YahooDatedTicker",
            "_CanonicalContract",
        ],
        errors="ignore",
    )

    yahoo_new = yahoo_new[
        STAGE4_HISTORY_COLUMNS
    ].copy()

    production_canonical = (
        production[
            STAGE4_HISTORY_COLUMNS
        ]
        .copy()
    )

    shadow = pd.concat(
        [
            production_canonical,
            yahoo_new,
        ],
        ignore_index=True,
    )

    shadow[
        "_EndUTC"
    ] = pd.to_datetime(
        shadow[
            "EndTimestampCT"
        ],
        utc=True,
        errors="coerce",
    )

    duplicates = (
        shadow.groupby(
            [
                "Symbol",
                "_EndUTC",
            ]
        )
        .size()
    )

    if (
        duplicates > 1
    ).any():
        raise RuntimeError(
            "Stage 4 hybrid shadow history "
            "contains duplicate Symbol/"
            "timestamp keys."
        )

    shadow = (
        shadow
        .sort_values(
            [
                "Symbol",
                "_EndUTC",
            ]
        )
        .drop(
            columns=[
                "_EndUTC"
            ]
        )
        .reset_index(drop=True)
    )

    return (
        shadow,
        yahoo_new,
    )


# ============================================================
# 44. Validate Stage 4 transition boundary
# ============================================================

def validate_stage4_transition(
    production_history,
    shadow_history,
    yahoo_new,
):

    if len(
        shadow_history
    ) != (
        len(production_history)
        + len(yahoo_new)
    ):
        raise RuntimeError(
            "Stage 4 shadow-history row-count "
            "identity failed."
        )

    original = (
        production_history[
            STAGE4_HISTORY_COLUMNS
        ]
        .copy()
    )

    shadow_prefix = (
        shadow_history[
            shadow_history[
                "SourceType"
            ]
            != "YahooFinance"
        ]
        [
            STAGE4_HISTORY_COLUMNS
        ]
        .copy()
    )

    # The hybrid shadow history is deliberately sorted by
    # Symbol/timestamp, so compare the historical production
    # rows after applying the same deterministic ordering to
    # both sides.
    #
    # This remains an exact row-preservation test; only row
    # ordering is normalized before comparison.

    original["_CompareTimestamp"] = (
        pd.to_datetime(
            original[
                "EndTimestampCT"
            ],
            utc=True,
            errors="coerce",
        )
    )

    shadow_prefix[
        "_CompareTimestamp"
    ] = pd.to_datetime(
        shadow_prefix[
            "EndTimestampCT"
        ],
        utc=True,
        errors="coerce",
    )

    if (
        original[
            "_CompareTimestamp"
        ].isna().any()
        or shadow_prefix[
            "_CompareTimestamp"
        ].isna().any()
    ):
        raise RuntimeError(
            "Stage 4 historical-preservation "
            "validation encountered invalid "
            "timestamps."
        )

    original = (
        original
        .sort_values(
            [
                "Symbol",
                "_CompareTimestamp",
            ]
        )
        .drop(
            columns=[
                "_CompareTimestamp"
            ]
        )
        .reset_index(drop=True)
    )

    shadow_prefix = (
        shadow_prefix
        .sort_values(
            [
                "Symbol",
                "_CompareTimestamp",
            ]
        )
        .drop(
            columns=[
                "_CompareTimestamp"
            ]
        )
        .reset_index(drop=True)
    )

    # Compare historical values exactly while allowing pandas
    # dtype promotion caused solely by concatenating the
    # historical rows with Yahoo rows.
    #
    # Example:
    #   production value: 123 stored as int
    #   shadow value:     123.0 after concat with NaN
    #
    # Those represent the same historical value and must not
    # trigger a false preservation failure.

    try:

        pd.testing.assert_frame_equal(
            original,
            shadow_prefix,
            check_dtype=False,
            check_exact=True,
            check_names=True,
            check_like=False,
        )

    except AssertionError as exc:

        raise RuntimeError(
            "Stage 4 genuinely changed at least "
            "one historical production value.\n"
            f"{exc}"
        ) from exc


# ============================================================
# 45. Write Stage 4 shadow outputs
# ============================================================

def write_stage4_shadow_outputs(
    shadow_history,
    shadow_identity,
):

    assert_yahoo_only_write_path(
        YAHOO_SHADOW_HISTORY_PATH
    )

    assert_yahoo_only_write_path(
        YAHOO_SHADOW_IDENTITY_PATH
    )

    write_csv_atomically(
        shadow_history,
        YAHOO_SHADOW_HISTORY_PATH,
    )

    write_csv_atomically(
        shadow_identity,
        YAHOO_SHADOW_IDENTITY_PATH,
    )

# ============================================================
# 46. Main Stage 1-4 run
# ============================================================

def main():

    section(
        "YAHOO FUTURES UPDATER - STAGES 1-4"
    )

    print(
        "MODE: ACQUISITION + ARCHIVING + RECONSTRUCTION + SHADOW ADAPTER"
    )

    print(
        "NO PRODUCTION FILE WILL BE MODIFIED."
    )

    print(
        f"\nProject root:\n{PROJECT_ROOT}"
    )

    print(
        "\nProduction history:\n"
        f"{PRODUCTION_HISTORY_PATH}"
    )

    print(
        "\nProduction roll history:\n"
        f"{PRODUCTION_ROLL_PATH}"
    )


    # --------------------------------------------------------
    # Read existing production state
    # --------------------------------------------------------

    section(
        "READ EXISTING PRODUCTION STATE"
    )

    production_history = (
        read_production_history(
            PRODUCTION_HISTORY_PATH
        )
    )

    production_roll_history = (
        read_production_roll_history(
            PRODUCTION_ROLL_PATH
        )
    )

    latest_state = (
        build_latest_production_state(
            production_history,
            production_roll_history,
        )
    )

    existing_yahoo_roll = (
        read_yahoo_roll_archive()
    )

    print(
        latest_state.to_string(
            index=False
        )
    )


    # Read the existing Yahoo-only archive before acquisition
    # so price-based contract identification can use previously
    # archived dated-contract observations when metadata does
    # not provide a valid dated ticker.

    existing_yahoo_history = (
        read_yahoo_history_archive()
    )


    # --------------------------------------------------------
    # Define Yahoo download window
    # --------------------------------------------------------

    now_ct = datetime.now(
        CT_TZ
    )

    download_start_date = (
        now_ct.date()
        - timedelta(
            days=DOWNLOAD_LOOKBACK_DAYS
        )
    )

    # yfinance end is exclusive, so use tomorrow.
    download_end_date = (
        now_ct.date()
        + timedelta(
            days=1
        )
    )

    section(
        "YAHOO DOWNLOAD WINDOW"
    )

    print(
        f"Current time CT: {now_ct}"
    )

    print(
        f"Start date:      {download_start_date}"
    )

    print(
        f"End date:        {download_end_date} "
        "(exclusive)"
    )


    # --------------------------------------------------------
    # Process all eight futures
    # --------------------------------------------------------

    result_rows = []
    all_proposed = []
    all_yahoo_archive_rows = []
    
    # Stage 3 keeps the generic 30m series from the acquisition
    # pass so it does not need to redownload the generic ticker.
    stage3_generic_data = {}
    
    # Current Yahoo metadata identity for each symbol.
    stage3_current_tickers = {}

    for symbol in SYMBOLS:

        section(
            f"{symbol} - YAHOO ACQUISITION"
        )

        generic_ticker = (
            GENERIC_TICKERS[
                symbol
            ]
        )

        state_row = (
            latest_state[
                latest_state["Symbol"]
                == symbol
            ]
            .iloc[0]
        )

        last_production_utc = (
            state_row[
                "ProductionLastTimestampUTC"
            ]
        )

        print(
            "Production last timestamp CT: "
            f"{last_production_utc.tz_convert(CT_TZ)}"
        )

        print(
            "Production last Close:        "
            f"{state_row['ProductionLastClose']}"
        )

        print(
            "Production last roll:         "
            f"{state_row['ProductionLastContract']}"
        )

        print(
            f"Yahoo generic ticker:         "
            f"{generic_ticker}"
        )


        # ----------------------------------------------------
        # Query Yahoo metadata, then download generic data
        # ----------------------------------------------------

        metadata_dated_ticker = (
            get_current_underlying_ticker(
                symbol,
                generic_ticker,
            )
        )

        generic_15m = (
            download_yahoo_15m(
                generic_ticker,
                download_start_date,
                download_end_date,
            )
        )

        generic_30m = (
            yahoo_15m_to_30m(
                generic_15m
            )
        )


        # ----------------------------------------------------
        # Resolve current dated contract
        # ----------------------------------------------------
        #
        # Preferred path:
        #   Yahoo metadata supplies a valid dated ticker.
        #
        # Fallback path:
        #   Yahoo supplies only a root such as CL.NYM, so
        #   identify the current dated contract by requiring a
        #   unique price match against the latest generic bar.
        #
        # ----------------------------------------------------

        if metadata_dated_ticker is not None:

            dated_ticker = (
                metadata_dated_ticker
            )

            dated_15m = (
                download_yahoo_15m(
                    dated_ticker,
                    download_start_date,
                    download_end_date,
                )
            )

            dated_30m = (
                yahoo_15m_to_30m(
                    dated_15m
                )
            )

            print(
                "Yahoo current underlying:     "
                f"{dated_ticker} "
                "(dated metadata)"
            )

        else:

            dated_ticker = (
                get_latest_archived_dated_ticker(
                    symbol=symbol,
                    yahoo_roll_archive=(
                        existing_yahoo_roll
                    ),
                )
            )

            if dated_ticker is None:
                raise RuntimeError(
                    f"{symbol}: Yahoo returned only "
                    "a root underlyingSymbol and no "
                    "previously validated dated Yahoo "
                    "contract is available in the "
                    "roll archive."
                )

            # Yahoo's generic ticker is the live price
            # stream. The archived dated ticker supplies
            # the currently validated contract identity.
            dated_30m = (
                generic_30m.copy()
            )

            print(
                "Yahoo current underlying:     "
                f"{dated_ticker} "
                "(archived identity; generic prices)"
            )

        stage3_current_tickers[
            symbol
        ] = dated_ticker

        stage3_generic_data[
            symbol
        ] = generic_30m.copy()

        print(
            "Yahoo generic 30m bars:       "
            f"{len(generic_30m)}"
        )

        print(
            "Yahoo dated 30m bars:         "
            f"{len(dated_30m)}"
        )


        # ----------------------------------------------------
        # Validate generic vs current dated contract
        # ----------------------------------------------------

        comparison = (
            compare_generic_and_dated(
                generic_30m,
                dated_30m,
            )
        )

        matched_count = len(
            comparison
        )

        exact_count = int(
            comparison[
                "ExactWithinTolerance"
            ].sum()
        )

        exact_pct = (
            exact_count
            / matched_count
            if matched_count > 0
            else np.nan
        )

        median_abs_diff = float(
            comparison[
                "AbsPriceDiff"
            ].median()
        )

        max_abs_diff = float(
            comparison[
                "AbsPriceDiff"
            ].max()
        )

        latest_comparison = (
            comparison
            .sort_values(
                "EndTimestampUTC"
            )
            .iloc[-1]
        )

        latest_match = bool(
            latest_comparison[
                "ExactWithinTolerance"
            ]
        )

        print(
            "Overlapping generic/dated:    "
            f"{matched_count}"
        )

        print(
            "Exact within tolerance:       "
            f"{exact_count}"
        )

        print(
            "Exact match percentage:       "
            f"{exact_pct:.6f}"
        )

        print(
            "Median absolute difference:   "
            f"{median_abs_diff:.12g}"
        )

        print(
            "Maximum absolute difference:  "
            f"{max_abs_diff:.12g}"
        )

        print(
            "Latest generic/dated match:   "
            f"{latest_match}"
        )

        if (
            matched_count
            < MIN_MATCHED_BARS
        ):
            raise RuntimeError(
                f"{symbol}: insufficient "
                "generic/dated overlap."
            )

        if not latest_match:
            raise RuntimeError(
                f"{symbol}: current Yahoo "
                "underlyingSymbol does not match "
                "the latest generic observation."
            )


        # ----------------------------------------------------
        # Construct proposed new observations
        # ----------------------------------------------------

        proposed = (
            construct_proposed_observations(
                symbol=symbol,
                generic_ticker=generic_ticker,
                dated_ticker=dated_ticker,
                dated_30m=generic_30m,
                last_production_timestamp_utc=(
                    last_production_utc
                ),
            )
        )

        if not proposed.empty:
            all_proposed.append(
                proposed
            )

        latest_yahoo_utc = (
            dated_30m[
                "EndTimestampUTC"
            ].max()
        )

        gap_info = (
            diagnose_gap(
                last_production_timestamp_utc=(
                    last_production_utc
                ),
                proposed=proposed,
                latest_yahoo_timestamp_utc=(
                    latest_yahoo_utc
                ),
            )
        )

        print(
            "Proposed new observations:    "
            f"{len(proposed)}"
        )

        print(
            "Latest Yahoo timestamp CT:    "
            f"{latest_yahoo_utc.tz_convert(CT_TZ)}"
        )

        if (
            pd.notna(
                gap_info[
                    "FirstProposedTimestampUTC"
                ]
            )
        ):
            print(
                "First proposed timestamp CT: "
                f"{gap_info['FirstProposedTimestampUTC'].tz_convert(CT_TZ)}"
            )

            print(
                "Elapsed minutes from stored "
                "endpoint: "
                f"{gap_info['ElapsedMinutesToFirstProposed']:.1f}"
            )

        print(
            "Potential gap detected:       "
            f"{gap_info['PotentialGap']}"
        )

        if (
            gap_info[
                "PotentialGap"
            ]
            and not ENABLE_GAP_RECONSTRUCTION
        ):
            print(
                "Stage 1 action: REPORT ONLY. "
                "Gap reconstruction is disabled."
            )


        # ----------------------------------------------------
        # Save in-memory summary
        # ----------------------------------------------------

        result_rows.append(
            {
                "Symbol": symbol,
                "GenericTicker": (
                    generic_ticker
                ),
                "CurrentYahooDatedTicker": (
                    dated_ticker
                ),
                "ProductionLastTimestampCT": (
                    last_production_utc
                    .tz_convert(CT_TZ)
                ),
                "LatestYahooTimestampCT": (
                    latest_yahoo_utc
                    .tz_convert(CT_TZ)
                ),
                "Generic30mBars": (
                    len(generic_30m)
                ),
                "Dated30mBars": (
                    len(dated_30m)
                ),
                "MatchedBars": (
                    matched_count
                ),
                "ExactMatchPct": (
                    exact_pct
                ),
                "MedianAbsPriceDiff": (
                    median_abs_diff
                ),
                "MaxAbsPriceDiff": (
                    max_abs_diff
                ),
                "LatestMatch": (
                    latest_match
                ),
                "ProposedNewObservations": (
                    len(proposed)
                ),
                "PotentialGap": (
                    gap_info[
                        "PotentialGap"
                    ]
                ),
            }
        )


    # --------------------------------------------------------
    # Combined summary
    # --------------------------------------------------------

    results = pd.DataFrame(
        result_rows
    )

    section(
        "STAGE 1 SUMMARY"
    )

    print(
        results.to_string(
            index=False
        )
    )


    # --------------------------------------------------------
    # Proposed observation structural validation
    # --------------------------------------------------------

    section(
        "PROPOSED OBSERVATION VALIDATION"
    )

    if all_proposed:

        proposed_all = pd.concat(
            all_proposed,
            ignore_index=True,
        )

        duplicates = (
            proposed_all
            .groupby(
                [
                    "Symbol",
                    "EndTimestampUTC",
                ]
            )
            .size()
        )

        duplicates = duplicates[
            duplicates > 1
        ]

        if not duplicates.empty:
            raise RuntimeError(
                "Proposed Yahoo observations "
                "contain duplicate "
                "Symbol/timestamp rows."
            )

        if (
            proposed_all[
                "EndTimestampET"
            ].isna().any()
            or proposed_all[
                "EndTimestampCT"
            ].isna().any()
        ):
            raise RuntimeError(
                "Proposed Yahoo observations "
                "contain missing timestamps."
            )

        if (
            proposed_all[
                "Close"
            ].isna().any()
            or (
                ~np.isfinite(
                    proposed_all[
                        "Close"
                    ]
                )
            ).any()
            or (
                proposed_all[
                    "Close"
                ] <= 0
            ).any()
        ):
            raise RuntimeError(
                "Proposed Yahoo observations "
                "contain invalid Close values."
            )

        print(
            "Proposed rows:               "
            f"{len(proposed_all)}"
        )

        print(
            "Duplicate Symbol/timestamps: 0"
        )

        print(
            "Missing timestamps:          0"
        )

        print(
            "Invalid Close values:        0"
        )

        coverage = (
            proposed_all
            .groupby(
                "Symbol",
                as_index=False,
            )
            .agg(
                ProposedRows=(
                    "EndTimestampUTC",
                    "size",
                ),
                FirstProposedCT=(
                    "EndTimestampCT",
                    "min",
                ),
                LastProposedCT=(
                    "EndTimestampCT",
                    "max",
                ),
                DatedTicker=(
                    "YahooDatedTicker",
                    "first",
                ),
            )
        )

        print(
            "\nProposed coverage:\n"
        )

        print(
            coverage.to_string(
                index=False
            )
        )

    else:

        proposed_all = pd.DataFrame()

        print(
            "No observations are newer than "
            "the existing production history."
        )

    # --------------------------------------------------------
    # Stage 2 persistent Yahoo archives
    # --------------------------------------------------------

    section(
        "STAGE 2 PERSISTENT YAHOO ARCHIVES"
    )

    # Stage 1 acquires the Yahoo generic price stream only.
    #
    # Do NOT label the full generic window as one dated
    # contract and merge it into the dated-contract archive.
    # Timestamp-level dated identity is established in Stage 3.
    #
    # Until Stage 3 completes, preserve the existing validated
    # dated-contract archive unchanged.

    merged_yahoo_history = (
        existing_yahoo_history.copy()
    )

    history_merge_stats = {
        "ExistingRows": len(
            existing_yahoo_history
        ),
        "IncomingRows": 0,
        "ExactOverlaps": 0,
        "PriceRevisions": 0,
        "NewRows": 0,
        "FinalRows": len(
            existing_yahoo_history
        ),
    }

    run_timestamp_utc = pd.Timestamp.now(
        tz="UTC"
    )

    existing_yahoo_roll = (
        read_yahoo_roll_archive()
    )

    current_roll_observations = (
        build_current_roll_observations(
            results=results,
            observed_at_utc=run_timestamp_utc,
        )
    )

    merged_yahoo_roll = (
        merge_yahoo_roll_archive(
            existing=existing_yahoo_roll,
            incoming=current_roll_observations,
        )
    )

    yahoo_state = (
        build_yahoo_state(
            history=merged_yahoo_history,
            results=results,
            run_timestamp_utc=run_timestamp_utc,
        )
    )

    validate_yahoo_archives(
        history=merged_yahoo_history,
        roll_history=merged_yahoo_roll,
        state=yahoo_state,
    )

    print(
        "Existing Yahoo history rows: "
        f"{history_merge_stats['ExistingRows']}"
    )

    print(
        "Incoming Yahoo history rows: "
        f"{history_merge_stats['IncomingRows']}"
    )

    print(
        "Exact overlapping rows:      "
        f"{history_merge_stats['ExactOverlaps']}"
    )

    print(
        "Price revisions:             "
        f"{history_merge_stats['PriceRevisions']}"
    )

    print(
        "Genuinely new history rows:  "
        f"{history_merge_stats['NewRows']}"
    )

    print(
        "Final Yahoo history rows:    "
        f"{history_merge_stats['FinalRows']}"
    )

    print(
        "Final Yahoo roll rows:       "
        f"{len(merged_yahoo_roll)}"
    )

    print(
        "Final Yahoo state rows:      "
        f"{len(yahoo_state)}"
    )

    print()
    print(
        "Stage 2 archive write deferred until "
        "Stage 3 timestamp-level identity "
        "reconstruction passes."
    )

    # --------------------------------------------------------
    # Stage 3 timestamp-level contract reconstruction
    # --------------------------------------------------------

    section(
        "STAGE 3 TIMESTAMP-LEVEL CONTRACT RECONSTRUCTION"
    )

    existing_identity = (
        read_yahoo_identity_archive()
    )

    stage3_reconstructed = []
    stage3_switch_tables = []
    stage3_candidate_archive_rows = []

    for symbol in SYMBOLS:

        print()
        print(
            "-" * 80
        )
        print(
            f"{symbol} - STAGE 3"
        )
        print(
            "-" * 80
        )

        generic_ticker = (
            GENERIC_TICKERS[
                symbol
            ]
        )

        current_ticker = (
            stage3_current_tickers[
                symbol
            ]
        )

        candidate_tickers = (
            generate_candidate_tickers(
                symbol=symbol,
                start_date=(
                    download_start_date
                ),
                end_date=(
                    download_end_date
                ),
                current_ticker=(
                    current_ticker
                ),
                archived_history=(
                    merged_yahoo_history
                ),
            )
        )

        print(
            "Generated candidates: "
            f"{len(candidate_tickers)}"
        )

        print(
            "Candidate tickers:     "
            + ", ".join(
                candidate_tickers
            )
        )

        candidate_data = (
            download_candidate_contracts(
                symbol=symbol,
                candidate_tickers=(
                    candidate_tickers
                ),
                start_date=(
                    download_start_date
                ),
                end_date=(
                    download_end_date
                ),
            )
        )

        candidate_data = (
            add_archived_candidate_data(
                symbol=symbol,
                candidate_data=(
                    candidate_data
                ),
                archived_history=(
                    merged_yahoo_history
                ),
            )
        )

        print(
            "Available candidates: "
            f"{len(candidate_data)}"
        )

        print(
            "Available tickers:     "
            + ", ".join(
                sorted(
                    candidate_data.keys()
                )
            )
        )

        reconstructed = (
            reconstruct_generic_contract_identity(
                symbol=symbol,
                generic_ticker=(
                    generic_ticker
                ),
                generic_30m=(
                    stage3_generic_data[
                        symbol
                    ]
                ),
                candidate_data=(
                    candidate_data
                ),
            )
        )

        (
            resolved,
            unresolved,
            ambiguous,
        ) = validate_reconstructed_identity(
            reconstructed
        )

        switches = (
            build_identity_switch_table(
                reconstructed
            )
        )

        print(
            "Generic bars:          "
            f"{len(reconstructed)}"
        )

        print(
            "Uniquely resolved:     "
            f"{len(resolved)}"
        )

        print(
            "Unresolved:            "
            f"{len(unresolved)}"
        )

        print(
            "Ambiguous:             "
            f"{len(ambiguous)}"
        )

        print(
            "Detected switches:     "
            f"{len(switches)}"
        )

        if not switches.empty:

            print(
                "\nDetected switches:\n"
            )

            print(
                switches.to_string(
                    index=False
                )
            )

        if not unresolved.empty:

            print(
                "\nFirst unresolved rows:\n"
            )

            print(
                unresolved[
                    [
                        "Symbol",
                        "EndTimestampCT",
                        "GenericClose",
                    ]
                ]
                .head(20)
                .to_string(
                    index=False
                )
            )

        if not ambiguous.empty:

            print(
                "\nFirst ambiguous rows:\n"
            )

            print(
                ambiguous[
                    [
                        "Symbol",
                        "EndTimestampCT",
                        "GenericClose",
                        "YahooDatedTicker",
                        "ExactMatchCount",
                    ]
                ]
                .head(20)
                .to_string(
                    index=False
                )
            )

        stage3_reconstructed.append(
            reconstructed
        )

        # Persist dated-contract observations only for contracts
        # that Yahoo's generic series actually used during this
        # reconstruction window.
        #
        # Candidate contracts that were downloaded merely to
        # test possible identities are not retained.

        used_tickers = sorted(
            resolved[
                "YahooDatedTicker"
            ]
            .dropna()
            .unique()
        )

        print(
            "Contracts actually used: "
            + ", ".join(
                used_tickers
            )
        )

        for used_ticker in used_tickers:

            if (
                used_ticker
                not in candidate_data
            ):
                raise RuntimeError(
                    f"{symbol}: reconstructed "
                    f"{used_ticker}, but its "
                    "dated-contract data are "
                    "not available."
                )

            used_data = (
                candidate_data[
                    used_ticker
                ]
            )

            used_archive_rows = (
                build_incoming_yahoo_history(
                    symbol=symbol,
                    generic_ticker=(
                        generic_ticker
                    ),
                    dated_ticker=(
                        used_ticker
                    ),
                    dated_30m=(
                        used_data
                    ),
                )
            )

            stage3_candidate_archive_rows.append(
                used_archive_rows
            )

        if not switches.empty:

            stage3_switch_tables.append(
                switches
            )

    reconstructed_all = pd.concat(
        stage3_reconstructed,
        ignore_index=True,
    )

    unresolved_mask = (
        reconstructed_all[
            "ExactMatchCount"
        ]
        == 0
    )

    # Yahoo's generic ticker can publish the newest 30m bar
    # slightly before the corresponding dated-contract ticker.
    #
    # Permit only the latest observation for a symbol to remain
    # temporarily unresolved. Any unresolved interior/historical
    # observation remains a hard failure.

    latest_timestamp_by_symbol = (
        reconstructed_all
        .groupby(
            "Symbol"
        )[
            "EndTimestampUTC"
        ]
        .transform(
            "max"
        )
    )

    terminal_unresolved_mask = (
        unresolved_mask
        & (
            reconstructed_all[
                "EndTimestampUTC"
            ]
            == latest_timestamp_by_symbol
        )
    )

    nonterminal_unresolved_mask = (
        unresolved_mask
        & ~terminal_unresolved_mask
    )

    total_unresolved = int(
        unresolved_mask.sum()
    )

    total_terminal_unresolved = int(
        terminal_unresolved_mask.sum()
    )

    total_nonterminal_unresolved = int(
        nonterminal_unresolved_mask.sum()
    )

    total_ambiguous = int(
        (
            reconstructed_all[
                "ExactMatchCount"
            ]
            > 1
        ).sum()
    )

    total_resolved = int(
        (
            reconstructed_all[
                "ExactMatchCount"
            ]
            == 1
        ).sum()
    )

    print()
    print(
        "Stage 3 total generic bars: "
        f"{len(reconstructed_all)}"
    )

    print(
        "Stage 3 uniquely resolved:   "
        f"{total_resolved}"
    )

    print(
        "Stage 3 unresolved:          "
        f"{total_unresolved}"
    )

    print(
        "  Terminal/deferred:         "
        f"{total_terminal_unresolved}"
    )

    print(
        "  Non-terminal/fatal:        "
        f"{total_nonterminal_unresolved}"
    )

    print(
        "Stage 3 ambiguous:           "
        f"{total_ambiguous}"
    )

    # --------------------------------------------------------
    # Stage 3 acceptance gate
    # --------------------------------------------------------
    #
    # Do not write a partial identity archive. We first require
    # the entire currently requested generic window to be
    # uniquely reconstructed.
    #
    # --------------------------------------------------------

    if total_nonterminal_unresolved > 0:

        raise RuntimeError(
            "STAGE 3 FAILED: at least one "
            "non-terminal Yahoo generic "
            "observation could not be matched "
            "to any available dated contract. "
            "No Stage 3 identity archive was "
            "written."
        )

    if total_terminal_unresolved > 0:

        print()
        print(
            "Stage 3 note: "
            f"{total_terminal_unresolved} latest "
            "generic observation(s) are not yet "
            "available from the corresponding "
            "dated Yahoo contract and will be "
            "deferred until a later run."
        )

    if total_ambiguous > 0:

        raise RuntimeError(
            "STAGE 3 FAILED: at least one Yahoo "
            "generic observation matched more "
            "than one dated contract exactly. "
            "No Stage 3 identity archive was "
            "written."
        )

    # --------------------------------------------------------
    # Add all successfully available candidate-contract data
    # to the Stage 2 dated-contract archive.
    # --------------------------------------------------------

    if stage3_candidate_archive_rows:

        stage3_candidate_history = (
            pd.concat(
                stage3_candidate_archive_rows,
                ignore_index=True,
            )
        )

        (
            merged_yahoo_history,
            stage3_history_merge_stats,
        ) = merge_yahoo_history_archive(
            existing=(
                merged_yahoo_history
            ),
            incoming=(
                stage3_candidate_history
            ),
        )

        print()
        print(
            "Stage 3 candidate-history "
            "archive merge:"
        )

        print(
            "  Exact overlaps:     "
            f"{stage3_history_merge_stats['ExactOverlaps']}"
        )

        print(
            "  Price revisions:    "
            f"{stage3_history_merge_stats['PriceRevisions']}"
        )

        print(
            "  New rows:           "
            f"{stage3_history_merge_stats['NewRows']}"
        )

        print(
            "  Final rows:         "
            f"{stage3_history_merge_stats['FinalRows']}"
        )

    # --------------------------------------------------------
    # Merge timestamp-level generic identity archive
    # --------------------------------------------------------

    reconstructed_for_archive = (
        reconstructed_all.loc[
            reconstructed_all[
                "ExactMatchCount"
            ]
            == 1
        ]
        .copy()
    )

    merged_identity = (
        merge_yahoo_identity_archive(
            existing=existing_identity,
            incoming=reconstructed_for_archive,
        )
    )

    # Validate all eight symbols.
    identity_symbols = set(
        merged_identity[
            "Symbol"
        ]
    )

    missing_identity_symbols = (
        set(SYMBOLS)
        .difference(
            identity_symbols
        )
    )

    if missing_identity_symbols:

        raise RuntimeError(
            "Stage 3 identity archive is "
            "missing symbols: "
            + ", ".join(
                sorted(
                    missing_identity_symbols
                )
            )
        )

    # --------------------------------------------------------
    # Persist only after complete Stage 3 validation
    # --------------------------------------------------------

    # Stage 3 has now established timestamp-level identity.
    # Only now is it safe to persist the Yahoo archives.

    yahoo_state = (
        build_yahoo_state(
            history=merged_yahoo_history,
            results=results,
            run_timestamp_utc=run_timestamp_utc,
        )
    )

    validate_yahoo_archives(
        history=merged_yahoo_history,
        roll_history=merged_yahoo_roll,
        state=yahoo_state,
    )

    write_yahoo_archives(
        history=merged_yahoo_history,
        roll_history=merged_yahoo_roll,
        state=yahoo_state,
    )

    write_yahoo_identity_archive(
        merged_identity
    )

    print()
    print(
        "Yahoo timestamp-level identity archive:"
    )

    print(
        YAHOO_IDENTITY_PATH
    )

    print()
    print(
        "Final identity rows: "
        f"{len(merged_identity)}"
    )

    if stage3_switch_tables:

        all_switches = pd.concat(
            stage3_switch_tables,
            ignore_index=True,
        )

        print()
        print(
            "All reconstructed Yahoo switches:"
        )

        print()

        print(
            all_switches.to_string(
                index=False
            )
        )

    else:

        all_switches = pd.DataFrame()

        print()
        print(
            "No reconstructed contract switches "
            "occurred in the current window."
        )

    print()
    print(
        "STAGE 3 TIMESTAMP-LEVEL "
        "RECONSTRUCTION PASSED"
    )

    # --------------------------------------------------------
    # Stage 4 shadow canonical adapter
    # --------------------------------------------------------

    section(
        "STAGE 4 SHADOW CANONICAL ADAPTER"
    )

    if not ENABLE_STAGE4_SHADOW_ADAPTER:

        print(
            "Stage 4 shadow adapter disabled."
        )

    else:

        # Use the fully validated/persisted Stage 3 identity
        # state rather than an intermediate reconstruction.
        persisted_identity = (
            read_yahoo_identity_archive()
        )

        shadow_identity = (
            build_stage4_shadow_identity(
                persisted_identity
            )
        )

        selected_yahoo_prices = (
            build_stage4_selected_yahoo_prices(
                yahoo_history=(
                    merged_yahoo_history
                ),
                shadow_identity=(
                    shadow_identity
                ),
            )
        )

        yahoo_canonical_rows = (
            build_stage4_yahoo_canonical_rows(
                selected_yahoo_prices
            )
        )

        validate_stage4_yahoo_rows(
            yahoo_canonical_rows
        )

        (
            shadow_history,
            stage4_new_yahoo_rows,
        ) = build_stage4_shadow_history(
            production_history=(
                production_history
            ),
            yahoo_rows=(
                yahoo_canonical_rows
            ),
        )

        validate_stage4_transition(
            production_history=(
                production_history
            ),
            shadow_history=(
                shadow_history
            ),
            yahoo_new=(
                stage4_new_yahoo_rows
            ),
        )

        write_stage4_shadow_outputs(
            shadow_history=(
                shadow_history
            ),
            shadow_identity=(
                shadow_identity
            ),
        )

        print(
            "Production history rows:       "
            f"{len(production_history)}"
        )

        print(
            "Yahoo canonical rows built:   "
            f"{len(yahoo_canonical_rows)}"
        )

        print(
            "New Yahoo rows after cutoff:  "
            f"{len(stage4_new_yahoo_rows)}"
        )

        print(
            "Final shadow history rows:    "
            f"{len(shadow_history)}"
        )

        print(
            "Timestamp identity rows:      "
            f"{len(shadow_identity)}"
        )

        print()
        print(
            "Yahoo rows appended by symbol:"
        )
        print()

        if stage4_new_yahoo_rows.empty:

            print(
                "No Yahoo rows occur after "
                "the current production cutoff."
            )

        else:

            stage4_coverage = (
                stage4_new_yahoo_rows
                .assign(
                    _Timestamp=pd.to_datetime(
                        stage4_new_yahoo_rows[
                            "EndTimestampCT"
                        ],
                        utc=True,
                        errors="coerce",
                    )
                )
                .groupby(
                    "Symbol"
                )
                .agg(
                    Rows=(
                        "Symbol",
                        "size",
                    ),
                    FirstTimestamp=(
                        "_Timestamp",
                        "min",
                    ),
                    LastTimestamp=(
                        "_Timestamp",
                        "max",
                    ),
                )
                .reset_index()
            )

            print(
                stage4_coverage.to_string(
                    index=False
                )
            )

        print()
        print(
            "Shadow canonical history:"
        )
        print(
            YAHOO_SHADOW_HISTORY_PATH
        )

        print()
        print(
            "Shadow timestamp identity:"
        )
        print(
            YAHOO_SHADOW_IDENTITY_PATH
        )

        print()
        print(
            "STAGE 4 SHADOW CANONICAL "
            "ADAPTER PASSED"
        )

    # --------------------------------------------------------
    # Final safety assertions
    # --------------------------------------------------------

    section(
        "STAGE 1 SAFETY CHECK"
    )

    print(
        "Production files were READ ONLY."
    )

    print(
        "Yahoo-only archive CSVs may "
        "have been written under data/yahoo/."
    )

    print(
        "No file under data/raw/ was written."
    )

    print(
        "No file under data/live_inputs/ "
        "was written."
    )

    print(
        "No production state was modified."
    )

    print(
        "Yahoo timestamp-level contract "
        "reconstruction was performed."
    )

    print(
        "No reconstructed Yahoo data was "
        "written into production inputs."
    )


    # --------------------------------------------------------
    # Final status
    # --------------------------------------------------------

    all_latest_match = bool(
        results[
            "LatestMatch"
        ].all()
    )

    section(
        "STAGE 1 RESULT"
    )

    if all_latest_match:

        print(
            "STAGE 1 ACQUISITION TEST PASSED"
        )

        print()
        print(
            "All eight Yahoo generic series "
            "passed acquisition validation, "
            "and Stage 3 independently "
            "reconstructed their timestamp-level "
            "dated contract identities."
        )

        print()
        print(
            "The proposed observations passed "
            "basic structural validation."
        )

        print()
        print(
            "No production file was modified."
        )

        print()
        print(
            "Stage 2 Yahoo archives were "
            "validated and persisted."
        )

    else:

        print(
            "STAGE 1 ACQUISITION TEST FAILED"
        )

        raise RuntimeError(
            "At least one current Yahoo dated "
            "contract failed the latest generic "
            "price match."
        )


if __name__ == "__main__":
    main()
