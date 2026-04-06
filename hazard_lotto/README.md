# hazard_lotto

Survival analysis of German Lotto 6/49 numbers using Kaplan-Meier curves.

## Overview

This project analyses historical German lottery (Lotto 6/49) draw data to
explore how long individual numbers "survive" between consecutive draws.

### Key steps

1. **Read lottery data** – Historical draws are loaded from an Excel file
   (`data/lottery.xlsx`) with columns `Date, N1, N2, N3, N4, N5, N6`.  
   When no file is present a synthetic dataset is generated for demonstration.
2. **Build structured dataframe** – One row per draw; first column is the draw
   date, columns 2-7 are the six drawn numbers.
3. **Time since last draw** – For each number 1–49 the script finds the most
   recent draw in which it appeared.
4. **Kaplan-Meier survival curves** – For each number the gap (in number of
   draws) between consecutive appearances is treated as a survival time with
   "event = drawn again".  KM curves are estimated with the `survival` package
   and visualised with `survminer`.

## Project structure

```
hazard_lotto/
├── R/
│   └── analysis.R    # Main analysis script
├── data/             # Place lottery.xlsx here (gitignored raw data)
├── renv/             # renv environment (auto-managed)
├── renv.lock         # Locked package versions
├── .Rprofile         # renv activation
└── hazard_lotto.Rproj
```

## Usage

1. Open `hazard_lotto.Rproj` in RStudio.
2. Run `renv::restore()` to install all dependencies.
3. Optionally place a `data/lottery.xlsx` file (columns: `Date, N1, N2, N3, N4, N5, N6`).
4. Source or run `R/analysis.R`.

## Dependencies

| Package    | Purpose                          |
|------------|----------------------------------|
| here       | Project-root-relative paths      |
| dplyr      | Data wrangling                   |
| tidyr      | Reshaping data                   |
| lubridate  | Date handling                    |
| ggplot2    | Plotting                         |
| openxlsx   | Reading `.xlsx` files            |
| survival   | Kaplan-Meier estimation          |
| survminer  | KM curve visualisation           |
