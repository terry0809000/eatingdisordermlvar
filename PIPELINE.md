# EMA network analysis pipeline

Run all commands from the repository root. The existing analysis code is preserved; this documentation update did not execute the models.

## 1. Prepare R and packages

Install R with Rscript available in your terminal, then:

```sh
Rscript scripts/install_packages.R
```

Dependencies: dplyr, tidyr, readr, tibble, lubridate, lme4, qgraph, corpcor, purrr. The installer uses currently available package versions; it is not a package lock.

## 2. Supply authorized EMA data

Place `momentplusemamlvar.csv` in `data/raw/`, or pass a different CSV path.
Required columns: id, timelog, neg_affect, pos_affect, dietary_sum, comp_yn, body_satis, mal_ex, binge_yn, fear_wtgain, exercise_yn, day, beep.

See the README for node definitions. No participant data are distributed.

## 3. Run

```sh
Rscript scripts/run_mlvar_analysis.R data/raw/momentplusemamlvar.csv outputs
```

The first argument is the input CSV; the second is the output directory.
Without arguments, the script uses those same defaults.

## 4. Understand the sequence

1. Prepare variables and participant/day/survey ordering.
2. Construct lagged predictors from the immediately previous survey of the same participant on the same day.
3. Select target observations where `exercise_yn == 1`. Do not filter before constructing lags.
4. Fit the seven-node base model and eight-node additive model including `mal_ex`.
5. Export directed temporal networks and undirected residual contemporaneous networks.
6. Export edge, centrality, shared-node strength and dataset-summary outputs.

The original script is `scripts/mlvarfinaladjusted2.R`; the portable entry point is `scripts/run_mlvar_analysis.R`.

## 5. Keep the run interpretable

Retain the command used, input version, script revision, settings and R session/package versions with your private run record. Identical output across environments has not been established by this documentation work. Access to the same authorized input is necessary for reproduction.

Review node scaling and observation inclusion settings in the script before a new analysis. Temporal associations and contemporaneous partial correlations are not causal effects.

## 6. Outputs and sharing

Four network PNGs, edge CSVs, centrality tables, adjacency/layout files and reporting notes are written to the selected output directory. See README for the principal filenames.

Keep participant-level inputs, processed datasets and model outputs in approved storage. Share aggregate results only when authorized.
