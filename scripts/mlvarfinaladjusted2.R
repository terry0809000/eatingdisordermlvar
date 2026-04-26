############################################################
# Custom network comparison for momentplusemamlvar.csv
#
# DATASET FORMAT USED HERE
# Columns:
#   id, timelog, neg_affect, pos_affect, dietary_sum,
#   comp_yn, body_satis, mal_ex, binge_yn, fear_wtgain,
#   exercise_yn, day, beep
#
# DESIGN
# - Target rows at time t: only observations where exercise_yn == 1
# - Base model nodes:
#     binge_yn, comp_yn, dietary_sum, pos_affect,
#     neg_affect, body_satis, fear_wtgain
# - Additive model nodes:
#     base nodes + mal_ex
#
# TEMPORAL RULE
# - Lagged predictors come from the immediately previous survey
#   within the same person and same day, using the existing beep variable
# - This means t-1 is the real next previous survey, NOT the previous
#   exercise-only row
#
# CONTEMPORANEOUS RULE
# - Estimated only on target rows where exercise_yn == 1 at time t
#
# OUTPUTS
# - Publication-level network plots with white node text
# - Abbreviated node labels
# - Edge tables
# - Centrality tables
# - Strength comparison
# - Dataset summary tables
# - Auto-generated dataset description paragraph
# - Auto-generated methods note on centrality metrics
############################################################

# ==========================================================
# 0) Packages
# ==========================================================
required_pkgs <- c(
  "dplyr", "tidyr", "readr", "tibble", "lubridate",
  "lme4", "qgraph", "corpcor", "purrr"
)

to_install <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(lubridate)
library(lme4)
library(qgraph)
library(corpcor)
library(purrr)

# ==========================================================
# 1) User settings
# ==========================================================
data_path <- "C:/Users/Terry Yu/Documents/momentplusemamlvar.csv"
out_dir   <- "C:/Users/Terry Yu/Desktop/RA Intern/ED Data Analysis/Week 12"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

idvar       <- "id"
dayvar_raw  <- "day"
beepvar_raw <- "beep"
exercisevar <- "exercise_yn"

SCALE_NODES <- TRUE
min_finite_required <- 50
min_rows_for_model  <- 30

# ==========================================================
# 2) Helper functions
# ==========================================================
safe_scale <- function(x) {
  if (all(is.na(x))) return(x)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x)
  as.numeric(scale(x))
}

round_numeric_df <- function(df, digits = 3) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

adjacency_to_edge_table <- function(A, network_name, directed = FALSE, drop_zero = TRUE) {
  nodes <- rownames(A)
  if (is.null(nodes)) nodes <- colnames(A)
  
  df <- expand.grid(
    from = nodes,
    to   = nodes,
    stringsAsFactors = FALSE
  ) %>%
    mutate(weight = as.vector(A))
  
  if (!directed) {
    df <- df %>%
      filter(from != to) %>%
      rowwise() %>%
      mutate(pair_id = paste(sort(c(from, to)), collapse = " | ")) %>%
      ungroup() %>%
      group_by(pair_id) %>%
      slice(1) %>%
      ungroup() %>%
      select(-pair_id)
  } else {
    df <- df %>% filter(from != to)
  }
  
  if (drop_zero) {
    df <- df %>% filter(weight != 0)
  }
  
  df %>%
    mutate(
      abs_weight = abs(weight),
      network = network_name
    ) %>%
    select(network, from, to, weight, abs_weight) %>%
    arrange(desc(abs_weight))
}

make_centrality_table <- function(A, network_name, directed = FALSE) {
  A2 <- A
  diag(A2) <- 0
  
  node_names <- colnames(A2)
  if (is.null(node_names)) node_names <- rownames(A2)
  
  cent <- qgraph::centrality(A2)
  
  if (directed) {
    out_strength <- rowSums(abs(A2), na.rm = TRUE)
    in_strength  <- colSums(abs(A2), na.rm = TRUE)
    total_strength <- out_strength + in_strength
    
    tibble(
      network = network_name,
      node = node_names,
      InStrength = in_strength,
      OutStrength = out_strength,
      TotalStrength = total_strength,
      Betweenness = cent$Betweenness,
      Closeness = cent$Closeness
    )
  } else {
    undirected_strength <- rowSums(abs(A2), na.rm = TRUE)
    
    tibble(
      network = network_name,
      node = node_names,
      Strength = undirected_strength,
      Betweenness = cent$Betweenness,
      Closeness = cent$Closeness
    )
  }
}

plot_network_png <- function(
    A,
    file,
    title_text,
    labels,
    colors,
    directed = FALSE,
    layout_matrix = NULL
) {
  if (directed) {
    png_width  <- 2600
    png_height <- 1900
    node_size  <- 18
    text_size  <- 1.42
    edge_size  <- 16
    arrow_size <- 5.5
    repulse    <- 1.22
  } else {
    png_width  <- 2400
    png_height <- 1800
    node_size  <- 20
    text_size  <- 1.60
    edge_size  <- 18
    arrow_size <- 0
    repulse    <- 1.12
  }
  
  png(file, width = png_width, height = png_height, res = 300)
  
  qgraph(
    A,
    layout = if (is.null(layout_matrix)) "spring" else layout_matrix,
    labels = labels,
    color = colors,
    directed = directed,
    title = title_text,
    label.color = "white",
    label.cex = text_size,
    label.scale.equal = TRUE,
    vsize = node_size,
    border.width = 2.2,
    esize = edge_size,
    asize = arrow_size,
    repulsion = repulse,
    mar = c(5, 5, 7, 5),
    legend = FALSE,
    details = TRUE
  )
  
  dev.off()
}

get_qgraph_layout <- function(
    A,
    labels,
    colors,
    directed = FALSE
) {
  if (directed) {
    node_size  <- 18
    text_size  <- 1.42
    edge_size  <- 16
    arrow_size <- 5.5
    repulse    <- 1.22
  } else {
    node_size  <- 20
    text_size  <- 1.60
    edge_size  <- 18
    arrow_size <- 0
    repulse    <- 1.12
  }
  
  qg <- qgraph(
    A,
    layout = "spring",
    labels = labels,
    color = colors,
    directed = directed,
    label.color = "white",
    label.cex = text_size,
    label.scale.equal = TRUE,
    vsize = node_size,
    border.width = 2.2,
    esize = edge_size,
    asize = arrow_size,
    repulsion = repulse,
    legend = FALSE,
    details = TRUE,
    DoNotPlot = TRUE
  )
  
  qg$layout
}

make_temporal_dataset <- function(df, idvar, dayvar, beepvar, target_nodes, exercise_var) {
  df <- df %>%
    arrange(.data[[idvar]], .data[[dayvar]], .data[[beepvar]]) %>%
    group_by(.data[[idvar]], .data[[dayvar]]) %>%
    mutate(prev_row_exists = row_number() > 1)
  
  for (v in target_nodes) {
    lag_name <- paste0("lag_", v)
    df[[lag_name]] <- df %>%
      group_by(.data[[idvar]], .data[[dayvar]]) %>%
      arrange(.data[[beepvar]], .by_group = TRUE) %>%
      mutate(tmp = lag(.data[[v]], 1)) %>%
      pull(tmp)
  }
  
  df %>%
    ungroup() %>%
    mutate(target_exercise_row = .data[[exercise_var]] == 1)
}

fit_temporal_network <- function(df, nodes, idvar, min_rows = 30) {
  lag_nodes <- paste0("lag_", nodes)
  
  A <- matrix(
    0,
    nrow = length(nodes),
    ncol = length(nodes),
    dimnames = list(nodes, nodes)
  )
  
  model_summaries <- list()
  
  for (target in nodes) {
    needed <- c(idvar, target, lag_nodes)
    
    subdat <- df %>%
      select(all_of(needed)) %>%
      filter(if_all(all_of(c(target, lag_nodes)), ~ is.finite(.x)))
    
    if (nrow(subdat) < min_rows) {
      message("Skipping temporal model for ", target, " (<", min_rows, " complete rows).")
      next
    }
    
    rhs <- paste(lag_nodes, collapse = " + ")
    form <- as.formula(paste0(target, " ~ ", rhs, " + (1 | ", idvar, ")"))
    
    fit <- tryCatch(
      lmer(form, data = subdat, REML = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      message("Temporal model failed for ", target)
      next
    }
    
    cf <- fixef(fit)
    
    for (pred in nodes) {
      lag_pred <- paste0("lag_", pred)
      if (lag_pred %in% names(cf)) {
        A[pred, target] <- unname(cf[lag_pred])
      }
    }
    
    model_summaries[[target]] <- summary(fit)
  }
  
  list(A = A, fits = model_summaries)
}

fit_contemporaneous_network <- function(df_target, nodes) {
  lag_nodes <- paste0("lag_", nodes)
  
  resid_df <- matrix(NA_real_, nrow = nrow(df_target), ncol = length(nodes))
  colnames(resid_df) <- nodes
  
  for (target in nodes) {
    needed <- c(target, lag_nodes)
    subdat <- df_target[, needed, drop = FALSE]
    
    cc <- complete.cases(subdat)
    if (sum(cc) < 10) next
    
    form <- as.formula(paste0(target, " ~ ", paste(lag_nodes, collapse = " + ")))
    fit <- tryCatch(
      lm(form, data = subdat[cc, , drop = FALSE]),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    r <- rep(NA_real_, nrow(df_target))
    r[cc] <- resid(fit)
    resid_df[, target] <- r
  }
  
  resid_df <- as.data.frame(resid_df)
  resid_df <- resid_df %>% filter(if_all(everything(), ~ is.finite(.x)))
  
  if (nrow(resid_df) < 10) {
    stop("Too few complete residual rows for contemporaneous network.")
  }
  
  S <- cor(resid_df, use = "pairwise.complete.obs")
  P <- corpcor::cor2pcor(S)
  diag(P) <- 0
  P <- (P + t(P)) / 2
  
  rownames(P) <- nodes
  colnames(P) <- nodes
  
  list(A = P, residuals = resid_df)
}

strength_from_adj <- function(A) {
  A2 <- A
  diag(A2) <- 0
  s <- rowSums(abs(A2), na.rm = TRUE)
  if (is.null(names(s))) names(s) <- rownames(A2)
  s
}

# ==========================================================
# 3) Load data
# ==========================================================
dat <- read_csv(data_path, show_col_types = FALSE)

message("Loaded data: ", nrow(dat), " rows x ", ncol(dat), " columns")
print(names(dat))

required_vars <- c(
  "id", "timelog", "neg_affect", "pos_affect", "dietary_sum",
  "comp_yn", "body_satis", "mal_ex", "binge_yn", "fear_wtgain",
  "exercise_yn", "day", "beep"
)

missing_required <- setdiff(required_vars, names(dat))
if (length(missing_required) > 0) {
  stop("Missing required columns: ", paste(missing_required, collapse = ", "))
}

# ==========================================================
# 4) Basic cleaning for this exact dataset
# ==========================================================
dat[[idvar]] <- as.factor(dat[[idvar]])

all_model_vars <- c(
  "neg_affect", "pos_affect", "dietary_sum", "comp_yn",
  "body_satis", "mal_ex", "binge_yn", "fear_wtgain", "exercise_yn"
)

dat <- dat %>%
  mutate(across(all_of(all_model_vars), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(
    day_index = suppressWarnings(as.numeric(as.Date(day))),
    beep_within_day = suppressWarnings(as.numeric(beep))
  )

if (any(!is.finite(dat$day_index))) {
  dat <- dat %>%
    mutate(day_index = suppressWarnings(as.numeric(as.Date(day, format = "%Y/%m/%d"))))
}

dat <- dat %>%
  filter(is.finite(day_index), is.finite(beep_within_day)) %>%
  arrange(.data[[idvar]], day_index, beep_within_day)

if (nrow(dat) == 0) stop("No valid rows remain after day/beep cleaning.")

dat <- dat %>%
  group_by(.data[[idvar]]) %>%
  mutate(day_index = dense_rank(day_index)) %>%
  ungroup()

# ==========================================================
# 5) Define node sets
# ==========================================================
vars_base <- c(
  "binge_yn",
  "comp_yn",
  "dietary_sum",
  "pos_affect",
  "neg_affect",
  "body_satis",
  "fear_wtgain"
)

vars_additive <- c(
  "binge_yn",
  "comp_yn",
  "dietary_sum",
  "pos_affect",
  "neg_affect",
  "body_satis",
  "fear_wtgain",
  "mal_ex"
)

# ==========================================================
# 6) QC
# ==========================================================
qc_nodes <- tibble(
  var = vars_additive,
  n_finite = sapply(vars_additive, function(v) sum(is.finite(dat[[v]]))),
  variance = sapply(vars_additive, function(v) var(dat[[v]], na.rm = TRUE))
) %>%
  mutate(
    zero_var = is.na(variance) | variance == 0
  )

message("QC table:")
print(qc_nodes)

vars_base_use <- qc_nodes %>%
  filter(var %in% vars_base, n_finite >= min_finite_required, !zero_var) %>%
  pull(var)

vars_additive_use <- qc_nodes %>%
  filter(var %in% vars_additive, n_finite >= min_finite_required, !zero_var) %>%
  pull(var)

message("Usable base nodes: ", paste(vars_base_use, collapse = ", "))
message("Usable additive nodes: ", paste(vars_additive_use, collapse = ", "))

if (length(vars_base_use) < 3) stop("Too few usable nodes in base model.")
if (length(vars_additive_use) < 3) stop("Too few usable nodes in additive model.")

# ==========================================================
# 7) Optional scaling on full sequence before lagging
# ==========================================================
dat_base_full <- dat
dat_add_full  <- dat

if (SCALE_NODES) {
  dat_base_full <- dat_base_full %>%
    mutate(across(all_of(vars_base_use), safe_scale))
  
  dat_add_full <- dat_add_full %>%
    mutate(across(all_of(vars_additive_use), safe_scale))
}

# ==========================================================
# 8) Create lagged predictors from full real-time sequence
# ==========================================================
dat_base_lagged <- make_temporal_dataset(
  df = dat_base_full,
  idvar = idvar,
  dayvar = "day_index",
  beepvar = "beep_within_day",
  target_nodes = vars_base_use,
  exercise_var = exercisevar
)

dat_add_lagged <- make_temporal_dataset(
  df = dat_add_full,
  idvar = idvar,
  dayvar = "day_index",
  beepvar = "beep_within_day",
  target_nodes = vars_additive_use,
  exercise_var = exercisevar
)

# ==========================================================
# 9) Restrict target rows to exercise_yn == 1 at time t
# ==========================================================
dat_base_target <- dat_base_lagged %>%
  filter(target_exercise_row == TRUE, prev_row_exists == TRUE)

dat_add_target <- dat_add_lagged %>%
  filter(target_exercise_row == TRUE, prev_row_exists == TRUE)

message("Rows in base target dataset: ", nrow(dat_base_target))
message("Rows in additive target dataset: ", nrow(dat_add_target))

if (nrow(dat_base_target) == 0) stop("No target rows remain for base model.")
if (nrow(dat_add_target) == 0) stop("No target rows remain for additive model.")

# ==========================================================
# 10) Keep participants with >= 2 target rows
# ==========================================================
dat_base_target <- dat_base_target %>%
  group_by(.data[[idvar]]) %>%
  filter(n() >= 2) %>%
  ungroup()

dat_add_target <- dat_add_target %>%
  group_by(.data[[idvar]]) %>%
  filter(n() >= 2) %>%
  ungroup()

message("Rows after participant filter - base: ", nrow(dat_base_target))
message("Rows after participant filter - additive: ", nrow(dat_add_target))

# ==========================================================
# 10b) Dataset summary for reporting
# ==========================================================
dataset_summary <- tibble(
  sample = c("Base target rows", "Additive target rows"),
  n_rows = c(nrow(dat_base_target), nrow(dat_add_target)),
  n_participants = c(
    dplyr::n_distinct(dat_base_target[[idvar]]),
    dplyr::n_distinct(dat_add_target[[idvar]])
  )
)

message("Dataset summary:")
print(dataset_summary)

exercise_row_summary <- tibble(
  n_total_rows = nrow(dat),
  n_exercise_yn_1_rows = sum(dat[[exercisevar]] == 1, na.rm = TRUE),
  prop_exercise_yn_1 = mean(dat[[exercisevar]] == 1, na.rm = TRUE)
) %>%
  round_numeric_df(3)

message("Exercise row summary from full dataset:")
print(exercise_row_summary)

if ("mal_ex" %in% names(dat_add_target)) {
  mal_ex_summary <- tibble(
    variable = "mal_ex",
    n_finite = sum(is.finite(dat_add_target$mal_ex)),
    mean = mean(dat_add_target$mal_ex, na.rm = TRUE),
    sd = sd(dat_add_target$mal_ex, na.rm = TRUE),
    min = min(dat_add_target$mal_ex, na.rm = TRUE),
    max = max(dat_add_target$mal_ex, na.rm = TRUE)
  ) %>%
    round_numeric_df(3)
  
  message("mal_ex summary in exercise-target rows:")
  print(mal_ex_summary)
  
  mal_ex_binary_summary <- tibble(
    n_0 = sum(dat_add_target$mal_ex == 0, na.rm = TRUE),
    n_1 = sum(dat_add_target$mal_ex == 1, na.rm = TRUE),
    prop_1 = mean(dat_add_target$mal_ex == 1, na.rm = TRUE)
  ) %>%
    round_numeric_df(3)
  
  message("mal_ex binary summary in exercise-target rows:")
  print(mal_ex_binary_summary)
} else {
  mal_ex_summary <- tibble(
    variable = "mal_ex",
    n_finite = NA_real_,
    mean = NA_real_,
    sd = NA_real_,
    min = NA_real_,
    max = NA_real_
  )
  
  mal_ex_binary_summary <- tibble(
    n_0 = NA_real_,
    n_1 = NA_real_,
    prop_1 = NA_real_
  )
}

report_paragraph <- paste0(
  "The analysis was based on repeated ecological momentary assessment observations structured at the survey level, ",
  "with lagged predictors defined using the immediately preceding survey within the same participant and day. ",
  "For the exercise-restricted analysis, only target observations at time t where exercise_yn = 1 were retained, ",
  "while lagged values at t-1 were drawn from the full within-day sequence rather than from exercise-only observations. ",
  "In the full dataset, there were ", exercise_row_summary$n_exercise_yn_1_rows,
  " observations where exercise_yn = 1. After restricting to rows with a valid previous within-day survey and excluding participants ",
  "with fewer than two target observations, the base model included ", dataset_summary$n_rows[dataset_summary$sample == "Base target rows"],
  " observations from ", dataset_summary$n_participants[dataset_summary$sample == "Base target rows"],
  " participants, and the additive model included ", dataset_summary$n_rows[dataset_summary$sample == "Additive target rows"],
  " observations from ", dataset_summary$n_participants[dataset_summary$sample == "Additive target rows"],
  " participants. Within the exercise-target dataset, mal_ex had mean ", round(mal_ex_summary$mean, 3),
  ", standard deviation ", round(mal_ex_summary$sd, 3),
  ", minimum ", round(mal_ex_summary$min, 3),
  ", and maximum ", round(mal_ex_summary$max, 3), "."
)

cat("\n\n--- DATASET PARAGRAPH ---\n")
cat(report_paragraph)
cat("\n\n")

centrality_methods_note <- paste0(
  "For contemporaneous networks, which were undirected, node centrality was summarised using strength, ",
  "defined as the sum of the absolute partial-correlation edge weights connected to each node. ",
  "For temporal networks, which were directed, centrality was summarised using in-strength and out-strength, ",
  "representing the sums of the absolute incoming and outgoing edge weights, respectively; ",
  "total strength was defined as their sum."
)

cat("\n\n--- CENTRALITY METHODS NOTE ---\n")
cat(centrality_methods_note)
cat("\n\n")

# ==========================================================
# 11) Fit temporal networks
# ==========================================================
message("Fitting temporal BASE network...")
tem_base <- fit_temporal_network(
  df = dat_base_target,
  nodes = vars_base_use,
  idvar = idvar,
  min_rows = min_rows_for_model
)

message("Fitting temporal ADDITIVE network...")
tem_add <- fit_temporal_network(
  df = dat_add_target,
  nodes = vars_additive_use,
  idvar = idvar,
  min_rows = min_rows_for_model
)

A_tem_base <- tem_base$A
A_tem_add  <- tem_add$A

# ==========================================================
# 12) Fit contemporaneous networks
# ==========================================================
message("Fitting contemporaneous BASE network...")
con_base <- fit_contemporaneous_network(
  df_target = dat_base_target,
  nodes = vars_base_use
)

message("Fitting contemporaneous ADDITIVE network...")
con_add <- fit_contemporaneous_network(
  df_target = dat_add_target,
  nodes = vars_additive_use
)

A_con_base <- con_base$A
A_con_add  <- con_add$A

# ==========================================================
# 13) Labels and colours
# ==========================================================
pretty_node_labels <- c(
  binge_yn    = "Binge\nEating",
  comp_yn     = "Comp.\nBeh.",
  dietary_sum = "Dietary\nRestr.",
  pos_affect  = "Pos.\nAffect",
  neg_affect  = "Neg.\nAffect",
  body_satis  = "Body\nSat.",
  fear_wtgain = "Fear\nWt. Gain",
  mal_ex      = "Mal.\nEx"
)

purple_nodes <- c("binge_yn", "dietary_sum", "comp_yn", "mal_ex")
blue_nodes   <- c("pos_affect", "neg_affect", "body_satis", "fear_wtgain")

make_pretty_labels <- function(A) {
  nodes <- colnames(A)
  labs <- pretty_node_labels[nodes]
  labs[is.na(labs)] <- nodes[is.na(labs)]
  unname(labs)
}

make_node_colors <- function(A) {
  nodes <- colnames(A)
  ifelse(
    nodes %in% purple_nodes, "#6A3D9A",
    ifelse(nodes %in% blue_nodes, "#1F78B4", "#BDBDBD")
  )
}

lab_con_base <- make_pretty_labels(A_con_base)
lab_tem_base <- make_pretty_labels(A_tem_base)
lab_con_add  <- make_pretty_labels(A_con_add)
lab_tem_add  <- make_pretty_labels(A_tem_add)

col_con_base <- make_node_colors(A_con_base)
col_tem_base <- make_node_colors(A_tem_base)
col_con_add  <- make_node_colors(A_con_add)
col_tem_add  <- make_node_colors(A_tem_add)

# ==========================================================
# 14) Compute reproducible layouts
# ==========================================================
layout_con_base <- get_qgraph_layout(
  A = A_con_base,
  labels = lab_con_base,
  colors = col_con_base,
  directed = FALSE
)

layout_tem_base <- get_qgraph_layout(
  A = A_tem_base,
  labels = lab_tem_base,
  colors = col_tem_base,
  directed = TRUE
)

layout_con_add <- get_qgraph_layout(
  A = A_con_add,
  labels = lab_con_add,
  colors = col_con_add,
  directed = FALSE
)

layout_tem_add <- get_qgraph_layout(
  A = A_tem_add,
  labels = lab_tem_add,
  colors = col_tem_add,
  directed = TRUE
)

# ==========================================================
# 15) Plot and save networks
# ==========================================================
plot_network_png(
  A = A_con_base,
  file = file.path(out_dir, "base_contemporaneous.png"),
  title_text = "Base contemporaneous network\n(target rows: exercise_yn = 1 at time t)",
  labels = lab_con_base,
  colors = col_con_base,
  directed = FALSE,
  layout_matrix = layout_con_base
)

plot_network_png(
  A = A_tem_base,
  file = file.path(out_dir, "base_temporal.png"),
  title_text = "Base temporal network\n(t-1 = immediately previous survey within day)",
  labels = lab_tem_base,
  colors = col_tem_base,
  directed = TRUE,
  layout_matrix = layout_tem_base
)

plot_network_png(
  A = A_con_add,
  file = file.path(out_dir, "additive_contemporaneous.png"),
  title_text = "Additive contemporaneous network (+ mal_ex)\n(target rows: exercise_yn = 1 at time t)",
  labels = lab_con_add,
  colors = col_con_add,
  directed = FALSE,
  layout_matrix = layout_con_add
)

plot_network_png(
  A = A_tem_add,
  file = file.path(out_dir, "additive_temporal.png"),
  title_text = "Additive temporal network (+ mal_ex)\n(t-1 = immediately previous survey within day)",
  labels = lab_tem_add,
  colors = col_tem_add,
  directed = TRUE,
  layout_matrix = layout_tem_add
)

# ==========================================================
# 16) Centrality tables
# ==========================================================
cent_con_base <- make_centrality_table(A_con_base, "Base_Contemporaneous", directed = FALSE)
cent_tem_base <- make_centrality_table(A_tem_base, "Base_Temporal", directed = TRUE)

cent_con_add  <- make_centrality_table(A_con_add, "Additive_Contemporaneous", directed = FALSE)
cent_tem_add  <- make_centrality_table(A_tem_add, "Additive_Temporal", directed = TRUE)

all_centrality <- bind_rows(
  round_numeric_df(cent_con_base, 3),
  round_numeric_df(cent_tem_base, 3),
  round_numeric_df(cent_con_add, 3),
  round_numeric_df(cent_tem_add, 3)
)

message("All centrality:")
print(all_centrality)

# ==========================================================
# 17) Edge tables
# ==========================================================
edges_con_base <- adjacency_to_edge_table(A_con_base, "Base_Contemporaneous", directed = FALSE) %>% round_numeric_df(3)
edges_tem_base <- adjacency_to_edge_table(A_tem_base, "Base_Temporal", directed = TRUE) %>% round_numeric_df(3)

edges_con_add  <- adjacency_to_edge_table(A_con_add, "Additive_Contemporaneous", directed = FALSE) %>% round_numeric_df(3)
edges_tem_add  <- adjacency_to_edge_table(A_tem_add, "Additive_Temporal", directed = TRUE) %>% round_numeric_df(3)

message("Top base contemporaneous edges:")
print(head(edges_con_base, 20))

message("Top additive contemporaneous edges:")
print(head(edges_con_add, 20))

message("Top base temporal edges:")
print(head(edges_tem_base, 20))

message("Top additive temporal edges:")
print(head(edges_tem_add, 20))

# ==========================================================
# 18) Shared-node strength comparison
# ==========================================================
str_base <- strength_from_adj(A_con_base)
str_add  <- strength_from_adj(A_con_add)

shared_nodes <- intersect(names(str_base), names(str_add))

strength_compare <- tibble(
  node = shared_nodes,
  strength_base = unname(str_base[shared_nodes]),
  strength_additive = unname(str_add[shared_nodes]),
  delta = strength_additive - strength_base
) %>%
  arrange(desc(abs(delta))) %>%
  round_numeric_df(3)

message("Shared-node strength comparison:")
print(strength_compare)

# ==========================================================
# 19) mal_ex diagnostics
# ==========================================================
if ("mal_ex" %in% names(dat_add_target)) {
  message("Distribution of mal_ex in exercise-target rows:")
  print(summary(dat_add_target$mal_ex))
  print(table(dat_add_target$mal_ex, useNA = "ifany"))
  
  mal_by_id <- dat_add_target %>%
    group_by(.data[[idvar]]) %>%
    summarise(
      n_target_rows = n(),
      mal_ex_mean = mean(mal_ex, na.rm = TRUE),
      mal_ex_sd   = sd(mal_ex, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mal_ex_mean)
  
  print(head(mal_by_id, 20))
} else {
  mal_by_id <- tibble(
    !!idvar := NA,
    n_target_rows = NA_real_,
    mal_ex_mean = NA_real_,
    mal_ex_sd = NA_real_
  )
}

# ==========================================================
# 20) Save outputs
# ==========================================================
write_csv(qc_nodes, file.path(out_dir, "qc_nodes.csv"))

write_csv(dataset_summary, file.path(out_dir, "dataset_summary_target_rows.csv"))
write_csv(exercise_row_summary, file.path(out_dir, "exercise_row_summary_full_dataset.csv"))
write_csv(mal_ex_summary, file.path(out_dir, "mal_ex_summary_exercise_target_rows.csv"))
write_csv(mal_ex_binary_summary, file.path(out_dir, "mal_ex_binary_summary_exercise_target_rows.csv"))
write_csv(mal_by_id, file.path(out_dir, "mal_ex_by_participant.csv"))

writeLines(report_paragraph, file.path(out_dir, "dataset_paragraph.txt"))
writeLines(centrality_methods_note, file.path(out_dir, "centrality_methods_note.txt"))

write_csv(round_numeric_df(cent_con_base, 3), file.path(out_dir, "cent_con_base.csv"))
write_csv(round_numeric_df(cent_tem_base, 3), file.path(out_dir, "cent_tem_base.csv"))
write_csv(round_numeric_df(cent_con_add, 3),  file.path(out_dir, "cent_con_additive.csv"))
write_csv(round_numeric_df(cent_tem_add, 3),  file.path(out_dir, "cent_tem_additive.csv"))
write_csv(all_centrality, file.path(out_dir, "all_centrality.csv"))

write_csv(edges_con_base, file.path(out_dir, "edges_con_base.csv"))
write_csv(edges_tem_base, file.path(out_dir, "edges_tem_base.csv"))
write_csv(edges_con_add,  file.path(out_dir, "edges_con_additive.csv"))
write_csv(edges_tem_add,  file.path(out_dir, "edges_tem_additive.csv"))

write_csv(strength_compare, file.path(out_dir, "strength_compare_shared_nodes.csv"))

write_csv(
  A_con_base %>% as.data.frame() %>% rownames_to_column("from"),
  file.path(out_dir, "adjacency_con_base.csv")
)

write_csv(
  A_tem_base %>% as.data.frame() %>% rownames_to_column("from"),
  file.path(out_dir, "adjacency_tem_base.csv")
)

write_csv(
  A_con_add %>% as.data.frame() %>% rownames_to_column("from"),
  file.path(out_dir, "adjacency_con_additive.csv")
)

write_csv(
  A_tem_add %>% as.data.frame() %>% rownames_to_column("from"),
  file.path(out_dir, "adjacency_tem_additive.csv")
)

write_csv(
  as.data.frame(layout_con_base) %>% rownames_to_column("node"),
  file.path(out_dir, "layout_con_base.csv")
)

write_csv(
  as.data.frame(layout_tem_base) %>% rownames_to_column("node"),
  file.path(out_dir, "layout_tem_base.csv")
)

write_csv(
  as.data.frame(layout_con_add) %>% rownames_to_column("node"),
  file.path(out_dir, "layout_con_additive.csv")
)

write_csv(
  as.data.frame(layout_tem_add) %>% rownames_to_column("node"),
  file.path(out_dir, "layout_tem_additive.csv")
)

message("All outputs saved to: ", out_dir)

############################################################
# End
############################################################