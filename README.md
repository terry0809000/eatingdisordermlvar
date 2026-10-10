<p align="center"><img src="assets/project-cover.png" width="100%" alt="Conceptual scientific artwork for Eating Disorder Dynamics" /></p>

# Eating disorder dynamics, one moment at a time

**Affect · Body image · Eating-related symptoms · Maladaptive exercise**

A difficult day is rarely explained by a single feeling. This project follows the connections between feelings, thoughts and behaviours across everyday moments, asking what changes when maladaptive exercise enters the network.

This repository contains an R workflow for an exercise-restricted ecological momentary assessment (EMA) network analysis of eating-disorder-related affective, cognitive, and behavioural variables. The analysis compares a base network with an additive network that includes maladaptive exercise (`mal_ex`) among observations where exercise was endorsed at the current survey.

## Project aim

The analysis asks whether maladaptive exercise adds meaningful contemporaneous or temporal connectivity to an exercise-restricted network of eating-disorder symptoms and affective states. The key design choice is that target rows are restricted to observations where `exercise_yn == 1` at time `t`, while lagged predictors are taken from the immediately previous survey within the same participant and same day, rather than from the previous exercise-only observation.

## Data

The expected input file is:

```text
data/raw/momentplusemamlvar.csv
```

Expected columns:

```text
id, timelog, neg_affect, pos_affect, dietary_sum, comp_yn, body_satis,
mal_ex, binge_yn, fear_wtgain, exercise_yn, day, beep
```

Raw EMA data are not included in this repository. Place the CSV file in `data/raw/` before running the analysis.

## Network design

### Base model nodes

- `binge_yn`
- `comp_yn`
- `dietary_sum`
- `pos_affect`
- `neg_affect`
- `body_satis`
- `fear_wtgain`

### Additive model nodes

The additive model includes all base nodes plus:

- `mal_ex`

## Temporal rule

Lagged predictors are defined using the immediately previous survey within the same participant and same day according to the `beep` variable. This means the lagged observation is the real previous EMA survey, not the previous exercise-only row.

## Contemporaneous rule

Contemporaneous networks are estimated only on target rows where `exercise_yn == 1` at time `t`.

## Follow the pipeline

`EMA surveys → order within participant and day → construct genuine previous-survey lags → select exercise-target rows → fit base and additive models → export networks, edges and centrality`

See [PIPELINE.md](PIPELINE.md) for the execution map and reproducibility notes.

## Installation

Install the required R packages:

```bash
Rscript scripts/install_packages.R
```

## Running the analysis

From the repository root:

```bash
Rscript scripts/run_mlvar_analysis.R data/raw/momentplusemamlvar.csv outputs
```

If the data are saved at the default location, this shorter command is sufficient:

```bash
Rscript scripts/run_mlvar_analysis.R
```

The original hard-coded script is preserved as `scripts/mlvarfinaladjusted2.R`. The portable command-line version is `scripts/run_mlvar_analysis.R`.

## Outputs

The analysis writes publication-oriented outputs to `outputs/`, including network plots, edge tables, centrality tables, adjacency matrices, layout files, dataset summaries, maladaptive-exercise diagnostics, and short reporting paragraphs.

Important generated outputs include:

```text
base_contemporaneous.png
base_temporal.png
additive_contemporaneous.png
additive_temporal.png
all_centrality.csv
edges_con_base.csv
edges_tem_base.csv
edges_con_additive.csv
edges_tem_additive.csv
strength_compare_shared_nodes.csv
dataset_paragraph.txt
centrality_methods_note.txt
```

## Methodological notes

- Nodes are optionally scaled before lag construction.
- Participants with fewer than two exercise-target observations are excluded from the target-row analysis.
- Temporal networks are directed and estimated using mixed-effects models with participant-level random intercepts.
- Contemporaneous networks are undirected and estimated using partial correlations among residuals after conditioning on lagged predictors.
- For contemporaneous networks, strength is the sum of absolute partial-correlation edge weights.
- For temporal networks, in-strength, out-strength, and total strength are calculated from absolute directed edge weights.

## Privacy and data governance

Do not commit raw EMA data, processed individual-level datasets, participant identifiers, or generated model outputs unless approved for public sharing. The `.gitignore` file excludes common data and output files by default.

## Reading the networks with care

The estimated edges describe associations within this selected EMA sample. They do not establish causal effects, diagnose an individual, or show that changing a node will improve health. Conditioning on exercise-target observations also limits which daily moments the networks describe.

The cover is conceptual artwork about lived experience and temporal connections; it is not an empirical network or an anatomical result.
