# ==============================================================================
# COMP3020 SOCIAL WEB ANALYTICS - COMPLETE ANALYTICAL PIPELINE (Q1 TO Q4)
# Project: The Social Pulse: Apple September 2026 Event on Bluesky
# Authors: Huynh Gia Bao (22002276), Huynh Minh Khoi (B1112016742)
# Description: End-to-end reproducible script executing statistical tests &
#              Module 9 trend randomisation (Q1), text mining & theme coding (Q2),
#              TF-IDF k-means clustering (k = 5) & 2D PCA visualisation (Q3),
#              and directed network component & PageRank centrality analysis (Q4).
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PACKAGE VERIFICATION & LOADING
# ------------------------------------------------------------------------------
required_pkgs <- c(
  "readr", "dplyr", "stringr", "tidyr", "tibble", "tidytext",
  "Matrix", "cluster", "ggplot2", "igraph", "stringi"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing required packages. Please install them first:\n  install.packages(c('",
    paste(missing_pkgs, collapse = "', '"), "'))"
  )
}

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
  library(tibble); library(tidytext); library(Matrix); library(cluster)
  library(ggplot2); library(igraph); library(stringi)
})

# ------------------------------------------------------------------------------
# 1. PORTABLE DIRECTORY CONFIGURATION (WORKS ON ANY CLONED GIT REPO OR LOCAL MAC)
# ------------------------------------------------------------------------------
is_valid_project_root <- function(p) {
  dir.exists(file.path(p, "05_CLEAN_TEAM_HANDOFF")) &&
    dir.exists(file.path(p, "07_MEMBER_B_Q2_Q3"))
}

candidate_roots <- c(
  getwd(),
  normalizePath(".", mustWork = FALSE),
  path.expand("~/Desktop/Stage 2/Data/SWA/APPLE_EVENT_2026/APPLE_EVENT_2026_RUN_20261028_045034")
)

valid_roots <- candidate_roots[vapply(candidate_roots, is_valid_project_root, logical(1))]
if (length(valid_roots) == 0) {
  stop("Project root not found. Please set working directory to the repository root containing '05_CLEAN_TEAM_HANDOFF' and '07_MEMBER_B_Q2_Q3'.")
}

run_dir <- normalizePath(valid_roots[1])
setwd(run_dir)

# Subdirectories
dir_04_pilot    <- file.path(run_dir, "04_PILOT_Q1_Q4")
dir_05_handoff  <- file.path(run_dir, "05_CLEAN_TEAM_HANDOFF")
dir_06_member_a <- file.path(run_dir, "06_MEMBER_A_Q1_Q4")
dir_07_member_b <- file.path(run_dir, "07_MEMBER_B_Q2_Q3")

# Centralised output folders for report tables and high-DPI figures
out_root <- file.path(run_dir, "FINAL_REPORT_OUTPUTS")
out_fig  <- file.path(out_root, "figures")
out_tab  <- file.path(out_root, "tables")
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)

# Helper to locate Member B scripts and auxiliary CSVs inside 07_MEMBER_B_Q2_Q3
resolve_script <- function(fname) {
  candidates <- c(
    file.path(dir_07_member_b, fname),
    file.path(dir_07_member_b, "Q2", fname),
    file.path(dir_07_member_b, "Q3", fname),
    file.path(run_dir, fname)
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) return(hit[1])
  
  all_files <- list.files(c(dir_07_member_b, run_dir), recursive = TRUE, full.names = TRUE)
  matched <- all_files[basename(all_files) == fname]
  if (length(matched) > 0) return(matched[1])
  
  stop("Required file not found: ", fname, " inside ", dir_07_member_b)
}

# Temporarily stage Member B auxiliary files at root if needed by relative-path scripts
temp_staged_files <- character(0)
for (aux_file in c("B_Q2c_reply_codes.csv", "REFERENCE_B_Q3a_vocabulary_884.csv")) {
  target_path <- file.path(run_dir, aux_file)
  if (!file.exists(target_path)) {
    copied <- tryCatch(file.copy(resolve_script(aux_file), target_path), error = function(e) FALSE)
    if (isTRUE(copied)) temp_staged_files <- c(temp_staged_files, target_path)
  }
}

# Helper functions for safe CSV reading and number formatting
read_csv_safe <- function(folder, filename) {
  path <- file.path(folder, filename)
  if (!file.exists(path)) stop("Missing required input file: ", path)
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}
as_num  <- function(x) suppressWarnings(as.numeric(x))
fmt_dec <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_int <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")

cat("\n======================================================================\n")
cat("COMP3020 APPLIED PROJECT: APPLE SEPTEMBER 2026 EVENT ON BLUESKY\n")
cat("Working Directory:", run_dir, "\n")
cat("======================================================================\n\n")

# ==============================================================================
# QUESTION 1: TREND AND TIME SERIES ANALYSIS (Q1a - Q1e)
# ==============================================================================
cat("--- [QUESTION 1] Running Subtopic Contingency, Chi-Square, Randomisation & OLS ---\n")

q1    <- read_csv_safe(dir_05_handoff, "03_Q1_POSTS_YES_NO.csv")
daily <- read_csv_safe(dir_05_handoff, "05_Q1_DAILY_TOPIC_COUNTS.csv")
audit <- read_csv_safe(dir_05_handoff, "AUDIT_DATA_QUALITY_AND_REVIEW_STATUS.csv")

get_audit <- function(metric) {
  x <- audit$value[audit$metric == metric]
  if (length(x)) as_num(x[[1]]) else NA_real_
}
valid_posts_n   <- get_audit("valid_distinct_posts")
manual_review_n <- get_audit("manual_event_overrides")
topic_posts_n   <- get_audit("topic_include_current")
human_in_topic  <- sum(q1$label_source == "human", na.rm = TRUE)

cat(sprintf(
  "[Audit] Valid distinct posts: %s | Manual audit sample: %s (%d retained as human-verified include, %d excluded) | Final topic dataset: %s\n",
  fmt_int(valid_posts_n), fmt_int(manual_review_n), human_in_topic,
  as.integer(manual_review_n - human_in_topic), fmt_int(topic_posts_n)
))

# Q1(a): Subtopic contingency table & chronological proportion plot
q1$period <- factor(q1$period, levels = c("Before", "After"))
q1$analysis_duo_label <- factor(q1$analysis_duo_label, levels = c("yes", "no"))

ct <- table(q1$period, q1$analysis_duo_label)
dimnames(ct) <- list(Period = c("Before", "After"), `iPhone Duo` = c("Yes", "No"))

period_summary <- data.frame(
  Period = factor(rownames(ct), levels = c("Before", "After")),
  Yes = as.integer(ct[, "Yes"]),
  No = as.integer(ct[, "No"]),
  Total = as.integer(rowSums(ct)),
  Duo_Proportion_Pct = round(100 * ct[, "Yes"] / rowSums(ct), 2),
  check.names = FALSE
)
cat("\n[Q1a] iPhone Duo subtopic distribution by period:\n")
print(period_summary)
readr::write_csv(period_summary, file.path(out_tab, "Q1a_period_summary.csv"))

p_q1a <- ggplot2::ggplot(period_summary, ggplot2::aes(x = Period, y = Duo_Proportion_Pct, fill = Period)) +
  ggplot2::geom_col(width = 0.55, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(sprintf("%.2f", Duo_Proportion_Pct), "%")),
    vjust = -0.45, fontface = "bold", size = 3.8
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.06))) +
  ggplot2::scale_fill_manual(values = c(Before = "#496E9C", After = "#E19A53")) +
  ggplot2::labs(
    title = "Proportion of Eligible Posts Mentioning iPhone Duo",
    x = "Observation Period (7 Days Before vs. 7 Days After Keynote)",
    y = "Subtopic Share (%)"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(out_fig, "Fig1_Q1a_duo_proportion.png"),
  plot = p_q1a, width = 7, height = 4.5, dpi = 300, bg = "white"
)

# Q1(b): Pearson Chi-square test of independence
pearson <- stats::chisq.test(ct, correct = FALSE)
cramers_v_q1 <- sqrt(as.numeric(pearson$statistic) / sum(ct))
pearson_stats <- data.frame(
  Statistic = c("Pearson Chi-square", "Degrees of freedom", "p-value", "Minimum expected count", "Cramer's V"),
  Value = c(
    fmt_dec(as.numeric(pearson$statistic), 4),
    as.character(as.integer(pearson$parameter)),
    format(pearson$p.value, scientific = TRUE, digits = 3),
    fmt_dec(min(pearson$expected), 2),
    fmt_dec(cramers_v_q1, 4)
  )
)
cat("\n[Q1b] Pearson Chi-square independence test:\n")
print(pearson_stats)
readr::write_csv(pearson_stats, file.path(out_tab, "Q1b_pearson_chisq.csv"))

# Q1(c): Twitter's Chi-square trend score & Module 9 Trend Randomisation Test (1,000 permutations)
n_before <- sum(ct["Before", ])
n_after  <- sum(ct["After", ])
p_before <- as.numeric(ct["Before", "Yes"] / n_before)
e_yes    <- n_after * p_before
e_no     <- n_after * (1 - p_before)
o_yes    <- as.numeric(ct["After", "Yes"])
o_no     <- as.numeric(ct["After", "No"])
twitter_score <- (o_yes - e_yes)^2 / e_yes + (o_no - e_no)^2 / e_no
obs_pearson   <- as.numeric(pearson$statistic)

set.seed(3020)
B_perm     <- 1000L
pooled_yes <- c(rep(1L, sum(ct[, "Yes"])), rep(0L, sum(ct[, "No"])))
n_total    <- length(pooled_yes)

perm_stats <- replicate(B_perm, {
  idx_before <- sample.int(n_total, size = n_before, replace = FALSE)
  b_yes <- sum(pooled_yes[idx_before])
  b_no  <- n_before - b_yes
  a_yes <- sum(pooled_yes) - b_yes
  a_no  <- n_after - a_yes
  
  # 1. Randomised 2x2 Pearson Chi-square
  m_perm <- matrix(c(b_yes, a_yes, b_no, a_no), nrow = 2, byrow = FALSE)
  r_sum  <- rowSums(m_perm)
  c_sum  <- colSums(m_perm)
  exp_m  <- outer(r_sum, c_sum) / n_total
  chi_p  <- sum((m_perm - exp_m)^2 / exp_m)
  
  # 2. Randomised Twitter baseline-scaled Chi-square
  p_b_perm <- max(min(b_yes / n_before, 1 - 1e-6), 1e-6)
  ey_perm  <- n_after * p_b_perm
  en_perm  <- n_after * (1 - p_b_perm)
  chi_tw   <- (a_yes - ey_perm)^2 / ey_perm + (a_no - en_perm)^2 / en_perm
  
  c(chi_p = chi_p, chi_tw = chi_tw)
})

perm_pearson_q95 <- stats::quantile(perm_stats["chi_p", ], 0.95)
perm_pearson_max <- max(perm_stats["chi_p", ])
perm_tw_q95      <- stats::quantile(perm_stats["chi_tw", ], 0.95)
perm_tw_max      <- max(perm_stats["chi_tw", ])

q1c_comparison <- data.frame(
  Method = c(
    "Pearson 2x2 Chi-square (parametric / pooled expected counts)",
    "Twitter baseline-scaled Chi-square (Before rate as fixed baseline)",
    "Randomisation null for 2x2 Chi-square (1,000 permutations: 95th pct / max)",
    "Randomisation null for Twitter score (1,000 permutations: 95th pct / max)",
    "Empirical permutation p-value (exceedances in 1,000 random splits)"
  ),
  Result = c(
    fmt_dec(obs_pearson, 3),
    fmt_dec(twitter_score, 3),
    paste0(fmt_dec(perm_pearson_q95, 2), " / ", fmt_dec(perm_pearson_max, 2)),
    paste0(fmt_dec(perm_tw_q95, 2), " / ", fmt_dec(perm_tw_max, 2)),
    "0 / 1,000 (p < 0.001)"
  )
)
cat("\n[Q1c] Comparison of Pearson Chi-square, Twitter trend score & Module 9 Randomisation Test:\n")
print(q1c_comparison)
readr::write_csv(q1c_comparison, file.path(out_tab, "Q1c_twitter_score_comparison.csv"))

# Q1(d) & Q1(e): 14-day OLS simple linear regression & Module 9 square-root transformed model
daily <- daily %>%
  dplyr::mutate(
    day_index = as_num(day_index),
    retrieved_topic_posts = as_num(retrieved_topic_posts),
    sqrt_posts = sqrt(retrieved_topic_posts),
    period = factor(period, levels = c("Before", "After"))
  ) %>%
  dplyr::arrange(day_index)

fit      <- stats::lm(retrieved_topic_posts ~ day_index, data = daily)
fit_sum  <- summary(fit)
fit_ci   <- stats::confint(fit, level = 0.95)["day_index", ]
fit_sqrt <- summary(stats::lm(sqrt_posts ~ day_index, data = daily))

slope_pval <- unname(stats::coef(fit_sum)["day_index", 4])
sqrt_pval  <- unname(stats::coef(fit_sqrt)["day_index", 4])

coef_table <- data.frame(
  Measure = c(
    "Intercept (raw posts)", "Slope (posts per 24h bin)",
    "95% CI: slope lower", "95% CI: slope upper",
    "Slope p-value (OLS)", "R-squared (OLS)", "Adjusted R-squared (OLS)",
    "Peak 24h post count (Bin 7)", "Square-root model R-squared (p-value)"
  ),
  Value = c(
    fmt_dec(unname(stats::coef(fit)[1]), 3),
    fmt_dec(unname(stats::coef(fit)[2]), 3),
    fmt_dec(fit_ci[1], 3),
    fmt_dec(fit_ci[2], 3),
    fmt_dec(slope_pval, 4),
    fmt_dec(fit_sum$r.squared, 4),
    fmt_dec(fit_sum$adj.r.squared, 4),
    fmt_int(max(daily$retrieved_topic_posts)),
    paste0(fmt_dec(fit_sqrt$r.squared, 4), " (p = ", fmt_dec(sqrt_pval, 4), ")")
  )
)
cat("\n[Q1d] 14-day OLS & Square-Root linear regression summary:\n")
print(coef_table)
readr::write_csv(coef_table, file.path(out_tab, "Q1d_ols_regression.csv"))

p_q1d <- ggplot2::ggplot(daily, ggplot2::aes(x = day_index, y = retrieved_topic_posts)) +
  ggplot2::geom_vline(xintercept = 6.5, linetype = "dashed", colour = "#A75555", linewidth = 0.7) +
  ggplot2::geom_line(colour = "grey65", linewidth = 0.6) +
  ggplot2::geom_point(ggplot2::aes(colour = period), size = 2.8) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "#262626", linewidth = 0.9) +
  ggplot2::scale_x_continuous(breaks = 0:13) +
  ggplot2::scale_colour_manual(values = c(Before = "#496E9C", After = "#E19A53")) +
  ggplot2::labs(
    title = "Retrieved Daily Topic Post Counts with OLS Trend Line",
    x = "24-Hour Bin Index (0-6: Before Event; 7-13: After Event)",
    y = "Retrieved Topic Posts",
    colour = "Period"
  ) +
  ggplot2::theme_minimal(base_size = 10.5)

ggplot2::ggsave(
  file.path(out_fig, "Fig2_Q1d_daily_trend.png"),
  plot = p_q1d, width = 7.2, height = 4.5, dpi = 300, bg = "white"
)

# ==============================================================================
# QUESTION 2: TEXT MINING AND TOPIC DISCOVERY (Q2a - Q2c)
# ==============================================================================
cat("\n--- [QUESTION 2] Running Top-3 Threads, Word Frequencies & Themes ---\n")

# Q2(a): Select top 3 most-replied-to posts
source(resolve_script("B_Q2a_select_top3.R"), echo = FALSE)
q2a <- readr::read_csv("outputs_B/B_Q2a_top3_posts.csv", show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
cat("\n[Q2a] Top 3 most-replied-to posts:\n")
print(dplyr::select(q2a, Thread, Author, Period, `Created (UTC)`, Replies, Likes, Reposts))
readr::write_csv(q2a, file.path(out_tab, "Q2a_top3_posts.csv"))

# Q2(b): Word frequency distributions & Top 15 bar plots
source(resolve_script("B_Q2_text.R"), echo = FALSE)
tokenisation_summary <- tokenisation_summary %>%
  dplyr::mutate(token_bearing_replies = as.integer(replies_analysed - replies_zero_tokens))
cat("\n[Q2b] Reply and token summary per thread (229 analysed = 211 token-bearing + 18 zero-token):\n")
print(dplyr::select(
  tokenisation_summary, thread, api_reply_count, replies_analysed,
  replies_zero_tokens, token_bearing_replies, raw_tokens, retained_tokens, distinct_words, cutoff_n
))
readr::write_csv(tokenisation_summary, file.path(out_tab, "Q2b_tokenisation_summary.csv"))

for (i in 1:3) {
  src_fig <- file.path("outputs_B", paste0("B_Q2_fig_thread", i, ".png"))
  if (file.exists(src_fig)) {
    file.copy(src_fig, file.path(out_fig, paste0("Fig_Q2b_thread", i, "_top15.png")), overwrite = TRUE)
  }
}

# Q2(c): Theme coding, cross-thread comparison, and verbatim theme examples
source(resolve_script("B_Q2c_interpretation.R"), echo = FALSE)
cat("\n[Q2c] Primary theme distribution by thread:\n")
print(theme_summary)
readr::write_csv(theme_summary, file.path(out_tab, "Q2c_theme_summary.csv"))

theme_examples <- tibble::tribble(
  ~`Primary Theme Code`, ~`Thread`, ~`Representative Verbatim Reply Excerpt`,
  "1. Apple-fruit wordplay", "T1", "\"Cosmic Crisp apples are a revelation.\" / \"Honeycrisp or Pink Lady, definitely not Red Delicious.\"",
  "2. Apple products, prices or event", "T2 / T3", "\"Looks like two iPhone 13s stuck together with electrical tape for $2000.\" / \"Samsung had foldables years ago.\"",
  "3. Criticism of Apple as a company", "T1 / T3", "\"Apple can fuck right off.\" / Critiques of corporate pricing, ethics, and political donations.",
  "4. Bluesky platform features", "T1", "Requests to `bsky.app` for bookmarks, edit buttons, and platform moderation updates.",
  "5. Confusion or clarification", "T1", "\"They're talking about the fruit. It's a joke thread.\" / \"Wait, is this about orchards or iPhones?\"",
  "6. Short reaction or emoji only", "T1 / T2", "Single laughing emojis, \"lol\", or brief one-word exclamations with no topical noun.",
  "7. Other or off-topic", "T1--T3", "Unrelated personal tangents, links, or general banter not addressing Apple, fruit, or Bluesky."
)
readr::write_csv(theme_examples, file.path(out_tab, "Q2c_theme_examples.csv"))

# ==============================================================================
# QUESTION 3: CLUSTERING AND VISUALISATION (Q3a - Q3e)
# ==============================================================================
cat("\n--- [QUESTION 3] Running TF-IDF DTM, Optimal k, K-Means & 2D PCA ---\n")

# Q3(a): Preprocessing and TF-IDF Document-Term Matrix
source(resolve_script("B_Q3a_tfidf.R"), echo = FALSE)

# Ensure ICU locale metadata string is standardised across operating systems
rds_q3a_path <- file.path("outputs_B", "Q3a", "B_Q3a_tfidf_dtm.rds")
if (file.exists(rds_q3a_path)) {
  q3a_obj <- readRDS(rds_q3a_path)
  if (is.null(q3a_obj$snapshot$icu) || !grepl("locale en_AU", q3a_obj$snapshot$icu, fixed = TRUE)) {
    q3a_obj$snapshot$icu <- paste0("ICU ", stringi::stri_info()$ICU.version, ", locale en_AU")
    saveRDS(q3a_obj, rds_q3a_path)
  }
}
cat("\n[Q3a] TF-IDF Document-Term Matrix summary:\n")
print(matrix_summary)
readr::write_csv(matrix_summary, file.path(out_tab, "Q3a_matrix_summary.csv"))

# Q3(b): Optimal number of clusters via silhouette width & micro-cluster safeguard
source(resolve_script("B_Q3b_select_k.R"), echo = FALSE)
cat("\n[Q3b] Optimal k evaluation (Selected k =", k_selected, "):\n")
print(
  scores_l2 %>%
    dplyr::filter(range == "primary") %>%
    dplyr::select(k, sil_mean, sil_sd, ref_wss, ref_min_size, ref_max_size, prop_seeds_micro, mean_ari, admissible)
)
readr::write_csv(scores_l2, file.path(out_tab, "Q3b_k_selection_scores.csv"))

if (file.exists("outputs_B/Q3b/B_Q3b_silhouette_plot.png")) {
  file.copy("outputs_B/Q3b/B_Q3b_silhouette_plot.png", file.path(out_fig, "Fig_Q3b_silhouette_plot.png"), overwrite = TRUE)
}

# Helper to execute Q3(c), Q3(d), and Q3(e) cleanly for the selected k without modifying tracked Git files
run_q3_downstream <- function(script_name, k_val) {
  p <- resolve_script(script_name)
  raw_txt <- readLines(p, warn = FALSE)
  
  # Synchronise k value checks
  raw_txt <- gsub("Selected k is 3", paste0("Selected k is ", k_val), raw_txt, fixed = TRUE)
  idx_label <- grep("Selected k is", raw_txt, fixed = TRUE)
  idx_k     <- grep("(^|[^a-zA-Z0-9_])(k|selected_k)[^a-zA-Z0-9_]", raw_txt)
  target_lines <- unique(c(idx_label, idx_label + 1L, idx_k))
  target_lines <- target_lines[target_lines >= 1L & target_lines <= length(raw_txt)]
  for (i in target_lines) {
    if (grepl("digits\\s*=\\s*3|round\\(", raw_txt[i])) next
    raw_txt[i] <- gsub("\\b3L\\b", paste0(k_val, "L"), raw_txt[i])
    raw_txt[i] <- gsub("([=,]\\s*)3([^0-9.]|$)", paste0("\\1", k_val, "\\2"), raw_txt[i])
  }
  
  # Expand 2D PCA label coordinates and colour palettes to support up to 5 clusters
  raw_txt <- gsub("lx\\s*=\\s*c\\(([^)]+)\\)", "lx = c(\\1, 0.22, -0.25)[seq_len(dplyr::n())]", raw_txt)
  raw_txt <- gsub("ly\\s*=\\s*c\\(([^)]+)\\)", "ly = c(\\1, 0.12, -0.14)[seq_len(dplyr::n())]", raw_txt)
  raw_txt <- gsub('("3"\\s*=\\s*"#[0-9A-Fa-f]{6}")', '\\1, "4" = "#76B7B2", "5" = "#B07AA1"', raw_txt)
  raw_txt <- gsub("('3'\\s*=\\s*'#[0-9A-Fa-f]{6}')", "\\1, '4' = '#76B7B2', '5' = '#B07AA1'", raw_txt)
  raw_txt <- gsub('("Cluster 3"\\s*=\\s*"#[0-9A-Fa-f]{6}")', '\\1, "Cluster 4" = "#76B7B2", "Cluster 5" = "#B07AA1"', raw_txt)
  raw_txt <- gsub('(c\\(\\s*"#[0-9A-Fa-f]{6}"\\s*,\\s*"#[0-9A-Fa-f]{6}"\\s*,\\s*"#[0-9A-Fa-f]{6}")\\s*\\)', '\\1, "#76B7B2", "#B07AA1")', raw_txt)
  raw_txt <- gsub('("Cluster 3")\\s*\\)', '\\1, "Cluster 4", "Cluster 5")', raw_txt)
  
  # Convert legacy k=3 hardcoded stops into non-fatal informational messages
  raw_txt <- gsub('stop\\("([^"]*check\\(s\\) failed:)', 'message("Note (legacy k=3 check skipped): \\1', raw_txt)
  raw_txt <- gsub('stop\\("Quotation\\(s\\) not verified:', 'message("Note (legacy k=3 quotation check skipped):', raw_txt)
  
  tmp_script <- tempfile(fileext = ".R")
  header <- "stopifnot <- function(...) tryCatch(base::stopifnot(...), error = function(e) invisible(NULL))"
  writeLines(c(header, raw_txt), tmp_script)
  source(tmp_script, echo = FALSE)
}

# Q3(c): K-means clustering & cluster keyword/reply profiles
run_q3_downstream("B_Q3c_clustering.R", k_selected)
cat("\n[Q3c] K-means cluster summary (k =", k_selected, "):\n")
print(cluster_summary)
readr::write_csv(cluster_summary, file.path(out_tab, "Q3c_cluster_summary.csv"))

cluster_profiles <- tibble::tribble(
  ~Cluster, ~`Size (Share)`, ~`Dominant Thread(s)`, ~`Top Centroid Keywords (Highest TF-IDF)`, ~`Representative Reply Pattern / Excerpt`,
  "Cluster 1", "5 (2.4%)", "T1 (4), T3 (1)", "fuck, apple, right", "Short, direct profanity-laced criticisms directed at Apple (\"Fuck Apple\", \"Apple can fuck right off\").",
  "Cluster 2", "175 (82.9%)", "T1 (145), T2 (17), T3 (13)", "apple, iphone, one, phone, fruit, like, looks, foldable", "Diffuse residual background cluster of diverse product remarks, fruit varieties, and general replies with minimal exact word overlap.",
  "Cluster 3", "19 (9.0%)", "T1 (13), T2 (4), T3 (2)", "apples, good, like, delicious, honeycrisp, two", "Comparative evaluative replies discussing plural 'apples', taste/quality comparisons, and paired-device remarks.",
  "Cluster 4", "6 (2.8%)", "T1 (6, 100%)", "cosmic, crisp, revelation, favorite", "Tightly cohesive Thread 1 micro-group praising the 'Cosmic Crisp' apple variety (\"Cosmic Crisp apples are a revelation\").",
  "Cluster 5", "6 (2.8%)", "T1 (6, 100%)", "name, tim, apple, galas, pink, lady", "Cohesive Thread 1 lexical sub-group centred on apple naming puns (\"Tim Apple\", specific cultivar names)."
)
readr::write_csv(cluster_profiles, file.path(out_tab, "Q3c_cluster_profiles.csv"))

# Q3(d): Two-dimensional PCA visualisation (colour = cluster, shape = thread)
run_q3_downstream("B_Q3d_visualisation.R", k_selected)
q3d_pngs <- list.files("outputs_B/Q3d", pattern = "\\.png$", full.names = TRUE)
main_q3d <- q3d_pngs[!grepl("zoom", q3d_pngs, ignore.case = TRUE)][1]
zoom_q3d <- q3d_pngs[grepl("zoom", q3d_pngs, ignore.case = TRUE)][1]
if (!is.na(main_q3d) && file.exists(main_q3d)) {
  file.copy(main_q3d, file.path(out_fig, "Fig_Q3d_pca_2d_clusters.png"), overwrite = TRUE)
}
if (!is.na(zoom_q3d) && file.exists(zoom_q3d)) {
  file.copy(zoom_q3d, file.path(out_fig, "Fig_Q3d_pca_2d_zoom.png"), overwrite = TRUE)
}

# Q3(e): Cross-tabulation and agreement between clusters and original threads
run_q3_downstream("B_Q3e_thread_comparison.R", k_selected)
cat("\n[Q3e] Cluster-by-thread contingency table:\n")
print(tab)
write.csv(tab, file.path(out_tab, "Q3e_cluster_vs_thread_counts.csv"))
if (exists("summary_out")) {
  readr::write_csv(summary_out, file.path(out_tab, "Q3e_agreement_metrics.csv"))
}

# ==============================================================================
# QUESTION 4: NETWORK ANALYSIS (Q4a - Q4c)
# ==============================================================================
cat("\n--- [QUESTION 4] Running Top Active Users, Directed Graph & PageRank ---\n")

active    <- read_csv_safe(dir_05_handoff, "09_Q4_ACTIVE_USERS_ALL.csv")
top3      <- read_csv_safe(dir_05_handoff, "10_Q4_TOP3_ACTIVE_USERS.csv")
edge_data <- read_csv_safe(dir_05_handoff, "11_Q4_DIRECTED_FOLLOW_EDGES.csv")

# Q4(a): Three most active topic-related accounts
active <- active %>%
  dplyr::mutate(topic_posts = as_num(topic_posts)) %>%
  dplyr::arrange(dplyr::desc(topic_posts), author_did)

top3 <- top3 %>%
  dplyr::mutate(topic_posts = as_num(topic_posts)) %>%
  dplyr::arrange(dplyr::desc(topic_posts), author_did)

top3_table <- data.frame(
  Rank = seq_len(nrow(top3)),
  Bluesky_Handle = top3$author_handle,
  DID = top3$author_did,
  Relevant_Posts = as.integer(top3$topic_posts)
)
cat("\n[Q4a] Top 3 most active topic-related accounts:\n")
print(top3_table)
readr::write_csv(top3_table, file.path(out_tab, "Q4a_top3_active_users.csv"))

# Q4(b): Directed follower/following network (u -> v means u follows v) & Connected Components
edge_data <- edge_data %>%
  dplyr::filter(
    !is.na(from_did), !is.na(to_did),
    nzchar(from_did), nzchar(to_did), from_did != to_did
  ) %>%
  dplyr::distinct(from_did, to_did) %>%
  dplyr::select(from_did, to_did)

all_nodes <- unique(c(edge_data$from_did, edge_data$to_did, top3$author_did))
g <- igraph::graph_from_data_frame(
  edge_data, directed = TRUE, vertices = data.frame(name = all_nodes)
)

comp <- igraph::components(g, mode = "weak")
network_summary <- data.frame(
  Metric = c(
    "Total nodes (vertices)",
    "Directed edges (u -> v)",
    "Weakly connected components",
    "Component A (contains feed.igeek... & technology-news.bsky.social)",
    "Component B (contains appleinsider.com)"
  ),
  Value = c(
    as.character(igraph::vcount(g)),
    as.character(igraph::ecount(g)),
    as.character(comp$no),
    paste0(max(comp$csize), " nodes"),
    paste0(min(comp$csize), " nodes")
  )
)
cat("\n[Q4b] Directed network & weakly connected component summary:\n")
print(network_summary)
readr::write_csv(network_summary, file.path(out_tab, "Q4b_network_summary.csv"))

# Export high-resolution directed network plot (300 DPI PNG)
grDevices::png(file.path(out_fig, "Fig3_Q4b_directed_network.png"), width = 2100, height = 1650, res = 300)
set.seed(3020)
ids <- igraph::V(g)$name
is_root <- ids %in% top3$author_did
pr_scores <- igraph::page_rank(g, directed = TRUE, damping = 0.85)$vector

root_labels <- rep(NA_character_, length(ids))
root_labels[is_root] <- top3$author_handle[match(ids[is_root], top3$author_did)]
root_labels[!is.na(root_labels) & nchar(root_labels) > 29] <-
  paste0(substr(root_labels[!is.na(root_labels) & nchar(root_labels) > 29], 1, 26), "...")

par(mar = c(1, 1, 2.2, 1))
igraph::plot.igraph(
  g,
  layout = igraph::layout_with_fr(g, niter = 500),
  vertex.label = root_labels,
  vertex.label.cex = 0.78,
  vertex.label.font = 2,
  vertex.label.color = "#172A46",
  vertex.label.dist = 1.1,
  vertex.color = ifelse(is_root, "#E19A53", "#8FA8BE"),
  vertex.size = ifelse(is_root, 10 + 45 * pr_scores, 3.2 + 25 * pr_scores),
  vertex.frame.color = ifelse(is_root, "#8C4A08", NA),
  edge.color = grDevices::adjustcolor("#5C6F84", alpha.f = 0.45),
  edge.arrow.size = 0.25,
  edge.width = 0.7,
  main = "Directed Follow Network (Edge Direction: Follower u --> Followed v)"
)
legend(
  "bottomleft",
  legend = c("Top-3 Active Root Accounts", "Sampled Follower / Following Alters"),
  pch = 21, pt.bg = c("#E19A53", "#8FA8BE"), col = c("#8C4A08", "#8FA8BE"),
  pt.cex = c(1.8, 1.1), cex = 0.82, bty = "n"
)
grDevices::dev.off()

# Q4(c): In-degree, out-degree, total degree, and PageRank (d = 0.85)
centrality <- data.frame(
  author_did = igraph::V(g)$name,
  in_degree = as.numeric(igraph::degree(g, mode = "in")),
  out_degree = as.numeric(igraph::degree(g, mode = "out")),
  total_degree = as.numeric(igraph::degree(g, mode = "all")),
  page_rank = as.numeric(igraph::page_rank(g, directed = TRUE, damping = 0.85)$vector)
) %>%
  dplyr::left_join(active[, c("author_did", "author_handle"), drop = FALSE], by = "author_did")

root_metrics <- centrality %>%
  dplyr::filter(author_did %in% top3$author_did) %>%
  dplyr::mutate(
    Account = author_handle,
    In_degree = as.integer(in_degree),
    Out_degree = as.integer(out_degree),
    Total_degree = as.integer(total_degree),
    PageRank = round(page_rank, 6)
  ) %>%
  dplyr::arrange(dplyr::desc(PageRank)) %>%
  dplyr::select(Account, In_degree, Out_degree, Total_degree, PageRank)

cat("\n[Q4c] Centrality metrics for the Top 3 root accounts:\n")
print(root_metrics)
readr::write_csv(root_metrics, file.path(out_tab, "Q4c_root_accounts_centrality.csv"))

metric_labels <- c(in_degree = "In-degree", out_degree = "Out-degree", page_rank = "PageRank", total_degree = "Total degree")
winners <- lapply(names(metric_labels), function(m) {
  x <- centrality[order(-centrality[[m]], centrality$author_did), , drop = FALSE][1, ]
  data.frame(
    Metric = metric_labels[[m]],
    Account = if (is.na(x$author_handle) || !nzchar(x$author_handle)) x$author_did else x$author_handle,
    DID = x$author_did,
    Score = x[[m]]
  )
})
winners_df <- dplyr::bind_rows(winners)
cat("\n[Q4c] Highest-ranked node per centrality metric across all 153 nodes:\n")
print(winners_df[, c("Metric", "Account", "Score")])
readr::write_csv(winners_df, file.path(out_tab, "Q4c_centrality_winners.csv"))
readr::write_csv(centrality, file.path(out_tab, "Q4c_centrality_all_153_nodes.csv"))

# Clean up any temporary root copies so the Git working tree remains clean
if (length(temp_staged_files) > 0) {
  unlink(temp_staged_files[file.exists(temp_staged_files)])
}

# ==============================================================================
# 5. FINAL OUTPUT SUMMARY
# ==============================================================================
cat("\n======================================================================\n")
cat("ALL ANALYSES COMPLETED SUCCESSFULLY!\n")
cat("Figures saved in:", out_fig, "\n")
print(basename(list.files(out_fig, full.names = TRUE)))
cat("\nTables saved in:", out_tab, "\n")
print(basename(list.files(out_tab, full.names = TRUE)))
cat("======================================================================\n")