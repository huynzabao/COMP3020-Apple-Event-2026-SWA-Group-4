
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "readr", "tibble", "Matrix", "cluster", "ggplot2")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
  library(Matrix); library(cluster); library(ggplot2)
})

# ---- 1. Paths and settings ----------------------------------------------------
f_q3a_rds  <- file.path("outputs_B", "Q3a", "B_Q3a_tfidf_dtm.rds")
f_q3a_sum  <- file.path("outputs_B", "Q3a", "B_Q3a_matrix_summary.csv")
f_docs_csv <- file.path("05_CLEAN_TEAM_HANDOFF",
                        "08_Q3_UNIQUE_REPLY_DOCUMENTS.csv")
output_dir <- file.path("outputs_B", "Q3b")
if (!file.exists(f_q3a_rds)) {
  stop("Q3(a) output not found: ", f_q3a_rds,
       "\nRun B_Q3a_tfidf.R first (working directory: ", getwd(), ").")
}
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Every Q3(a) output file is checksummed before and after this script.
q3a_files  <- list.files(file.path("outputs_B", "Q3a"), full.names = TRUE)
md5_before <- tools::md5sum(q3a_files)

k_primary  <- 2:10      # pre-specified candidate range
k_extended <- 11:20     # checked because no interior optimum appears in 2-10
k_all      <- c(k_primary, k_extended)
seeds      <- 1:20      # independent repetitions of the whole k-means fit
nstart     <- 50        # random starts per fit (best WSS kept by kmeans)
iter_max   <- 100
micro_size <- 5         # a cluster smaller than this is a "micro-cluster"
max_prop_micro <- 0.20  # max share of seeds allowed to produce one

# Rounding for export: parse the formatted string so the stored double is the
# one nearest to the printed decimal (plain round() can leave artefacts such
# as 0.0018600000000000001 in CSV files).
rnd <- function(x, digits = 0) as.numeric(sprintf(paste0("%.", digits, "f"), x))
# readr 2.1.x can print such doubles with spurious digits; write them as text.
write_out <- function(df, path) {
  as_txt <- function(x) vapply(x, function(v) if (is.na(v)) NA_character_ else
    format(v, digits = 15, scientific = FALSE), character(1))
  write_csv(mutate(df, across(where(is.double), as_txt)), path)
}

# ---- 2. Load and verify the Q3(a) matrix -------------------------------------
q3a <- readRDS(f_q3a_rds)
stopifnot(all(c("dtm_tfidf", "dtm_counts", "clustering_input", "doc_metadata",
                "vocabulary", "settings", "snapshot") %in% names(q3a)))
dtm  <- q3a$dtm_tfidf
Xs   <- q3a$clustering_input
meta <- q3a$doc_metadata
elig_meta <- meta %>% filter(eligible_for_clustering) %>%
  arrange(cluster_input_row)

# Optional: confirm the RDS was built from the current 08 file
docs_md5_now <- if (file.exists(f_docs_csv)) unname(tools::md5sum(f_docs_csv)) else
  NA_character_
# Optional: the RDS must come from the same Q3(a) run as the Q3(a) summary
# CSV (and hence the Q3(a) PDF). A mismatch means files from two different
# runs or machines have been mixed.
q3a_sum_vocab <- if (file.exists(f_q3a_sum)) {
  q3a_sum_tab <- read_csv(f_q3a_sum, col_types = cols(.default = col_character()))
  as.integer(q3a_sum_tab$value[q3a_sum_tab$metric == "Vocabulary size (columns)"])
} else NA_integer_
docs_md5_rds <- unname(q3a$snapshot$input_md5[grepl("08_Q3",
                                                    names(q3a$snapshot$input_md5))])

input_checks <- tibble::tribble(
  ~check, ~value,
  "1. Documents in full reply population (matrix rows)", nrow(dtm),
  "1b. Documents in metadata", nrow(meta),
  "2. Vocabulary columns", ncol(dtm),
  "2b. Vocabulary rows in vocabulary table", nrow(q3a$vocabulary),
  "3. Clustering-input rows (eligible)", nrow(Xs),
  "4. Zero-feature documents in metadata", sum(!meta$eligible_for_clustering),
  "4b. All-zero rows in full matrix", sum(rowSums(dtm != 0) == 0),
  "5. Input rows matching eligible reply_uri in order (1 = yes)",
    as.integer(identical(rownames(Xs), elig_meta$reply_uri)),
  "6. Documents without thread_label", sum(is.na(meta$thread_label)),
  "7. Non-finite or negative TF-IDF values",
    sum(!is.finite(dtm@x) | dtm@x < 0),
  "8. Duplicate document identifiers", sum(duplicated(rownames(dtm))),
  "9. RDS 08-file MD5 equals current 08 file (1 = yes, NA = file absent)",
    if (is.na(docs_md5_now)) NA_integer_ else
      as.integer(identical(docs_md5_rds, docs_md5_now)),
  "9b. RDS vocabulary equals Q3a summary CSV (1 = yes, NA = CSV absent)",
    if (is.na(q3a_sum_vocab)) NA_integer_ else
      as.integer(ncol(dtm) == q3a_sum_vocab),
  "9c. Q3a RDS built with fixed ICU tokeniser locale en_AU (1 = yes)",
    as.integer(!is.null(q3a$snapshot$icu) &&
                 grepl("locale en_AU", q3a$snapshot$icu, fixed = TRUE)),
  "9d. Tokenisation regression: 'mu.social' is one term (1 = yes)",
    as.integer("mu.social" %in% colnames(dtm) &&
                 !any(c("mu", "social") %in% colnames(dtm))),
  "9e. Input columns identical to Q3a vocabulary, same order (1 = yes)",
    as.integer(identical(colnames(Xs), q3a$vocabulary$term) &&
                 identical(colnames(Xs), colnames(dtm))),
  "10. Clustering input equals eligible rows of full matrix (1 = yes)",
    as.integer(identical(Xs, dtm[meta$eligible_for_clustering, , drop = FALSE]))
)
print(input_checks, n = Inf)
iv <- setNames(input_checks$value, input_checks$check)
stopifnot(
  iv[1] == iv[2], iv[3] == iv[4],
  iv[5] + iv[6] == iv[1], iv[6] == iv[7],
  iv[8] == 1, iv[9] == 0, iv[10] == 0, iv[11] == 0, iv[17] == 1,
  is.na(iv[12]) || iv[12] == 1,
  is.na(iv[13]) || iv[13] == 1,
  iv[14] == 1, iv[15] == 1, iv[16] == 1,
  all(rowSums(Xs != 0) > 0)               # no zero vector can reach k-means
)

# ---- 3. Representations ---------------------------------------------------------
# K-means in stats requires a dense matrix; 211 x V doubles is small.
X_raw <- as.matrix(Xs)
dense_mb <- rnd(as.numeric(object.size(X_raw)) / 2^20, 2)

# L2 normalisation of the eligible rows only: x / ||x||_2. Zero vectors were
# excluded in Q3(a), so no division by zero can occur (checked above).
row_norm <- sqrt(rowSums(X_raw^2))
stopifnot(all(row_norm > 0))
X_l2 <- X_raw / row_norm
stopifnot(isTRUE(all.equal(unname(rowSums(X_l2^2)), rep(1, nrow(X_l2)))),
          identical(rownames(X_l2), rownames(q3a$clustering_input)),
          identical(colnames(X_l2), colnames(q3a$clustering_input)))

# For unit vectors, ||x - y||^2 = 2 (1 - cos(x, y)): Euclidean distance on the
# L2 representation is a monotone function of cosine similarity.
cos_l2 <- tcrossprod(X_l2)
pair_cos <- cos_l2[upper.tri(cos_l2)]
geometry <- tibble(
  metric = c("Document pairs", "Pairs with no shared term (cosine = 0)",
             "Share of orthogonal pairs (%)", "Mean pairwise cosine",
             "Documents orthogonal to all others",
             "Raw TF-IDF row norm: min", "Raw TF-IDF row norm: max",
             "Dense matrix size (MB)"),
  value = c(length(pair_cos), sum(pair_cos < 1e-12),
            rnd(100 * mean(pair_cos < 1e-12), 2), rnd(mean(pair_cos), 4),
            sum(rowSums(cos_l2 > 1e-12) == 1),
            rnd(min(row_norm), 4), rnd(max(row_norm), 4), dense_mb))
isolated <- rowSums(cos_l2 > 1e-12) == 1
print(geometry)

# ---- 4. Evaluation functions --------------------------------------------------
# Silhouette (Rousseeuw, 1987) uses the same Euclidean distance that k-means
# minimises on the given representation.
fit_k <- function(X, D, k, seed) {
  set.seed(seed)
  km <- kmeans(X, centers = k, nstart = nstart, iter.max = iter_max,
               algorithm = "Hartigan-Wong")
  s <- silhouette(km$cluster, D)
  list(cluster = km$cluster, size = km$size, wss = km$tot.withinss,
       ifault = km$ifault, iter = km$iter, sil_i = s[, "sil_width"])
}

# Adjusted Rand index between two partitions (Hubert and Arabie, 1985)
ari <- function(a, b) {
  t <- table(a, b); n <- sum(t)
  s_ij <- sum(choose(t, 2)); s_a <- sum(choose(rowSums(t), 2))
  s_b <- sum(choose(colSums(t), 2)); e <- s_a * s_b / choose(n, 2)
  (s_ij - e) / ((s_a + s_b) / 2 - e)
}

evaluate <- function(X, ks, label) {
  D <- dist(X, method = "euclidean")
  runs <- list(); fits <- list()
  for (k in ks) for (sd in seeds) {
    f <- withCallingHandlers(fit_k(X, D, k, sd), warning = function(w) {
      message("Warning (", label, ", k = ", k, ", seed = ", sd, "): ",
              conditionMessage(w)); invokeRestart("muffleWarning")
    })
    fits[[paste(k, sd)]] <- f
    runs[[length(runs) + 1]] <- tibble(
      representation = label, k = k, seed = sd, mean_sil = mean(f$sil_i),
      wss = f$wss, min_size = min(f$size), max_size = max(f$size),
      n_micro = sum(f$size < micro_size), n_singleton = sum(f$size == 1),
      ifault = f$ifault, converged = f$ifault == 0)
  }
  list(runs = bind_rows(runs), fits = fits, D = D)
}

# ---- 5. Run the evaluations -----------------------------------------------------
ev_l2  <- evaluate(X_l2, k_all, "L2-normalised TF-IDF")
ev_raw <- evaluate(X_raw, k_primary, "Raw TF-IDF")
stopifnot(all(ev_l2$runs$converged), all(ev_raw$runs$converged))

# Reference solution for each k: lowest WSS over all seeds x nstart starts
# (the k-means objective); ties broken by the smallest seed.
reference <- function(ev) {
  ev$runs %>% group_by(representation, k) %>%
    arrange(wss, seed, .by_group = TRUE) %>% slice(1) %>% ungroup()
}
ref_l2  <- reference(ev_l2)
ref_raw <- reference(ev_raw)

mean_ari <- function(ev, k) {
  cl <- lapply(seeds, function(sd) ev$fits[[paste(k, sd)]]$cluster)
  pr <- combn(length(seeds), 2)
  mean(apply(pr, 2, function(p) ari(cl[[p[1]]], cl[[p[2]]])))
}

summarise_k <- function(ev, ref) {
  ev$runs %>% group_by(representation, k) %>%
    summarise(sil_mean = mean(mean_sil), sil_sd = sd(mean_sil),
              sil_min = min(mean_sil), sil_max = max(mean_sil),
              prop_seeds_micro = mean(n_micro > 0), .groups = "drop") %>%
    left_join(ref %>% transmute(representation, k, ref_seed = seed,
                                ref_wss = wss, ref_sil = mean_sil,
                                ref_min_size = min_size,
                                ref_max_size = max_size,
                                ref_n_micro = n_micro,
                                ref_n_singleton = n_singleton),
              by = c("representation", "k")) %>%
    mutate(mean_ari = vapply(k, function(kk) mean_ari(ev, kk), numeric(1)))
}
scores_l2  <- summarise_k(ev_l2, ref_l2)
scores_raw <- summarise_k(ev_raw, ref_raw)

# Mean silhouette of the reference solution after excluding documents in
# micro-clusters: shows whether tiny clusters inflate the average.
sil_excl_micro <- function(ev, k, seed) {
  f <- ev$fits[[paste(k, seed)]]
  keep <- f$size[f$cluster] >= micro_size
  if (any(keep)) mean(f$sil_i[keep]) else NA_real_
}
scores_l2 <- scores_l2 %>%
  mutate(ref_sil_excl_micro = mapply(function(k, s) sil_excl_micro(ev_l2, k, s),
                                     k, ref_seed),
         ref_largest_share = ref_max_size / nrow(X_l2))

# ---- 6. Selection rule (stated before inspecting the choice) -------------------
# Mean silhouette is the primary criterion. Because silhouette on sparse text
# can rise simply by splitting off a few near-identical documents, a value of
# k is admissible only if
#   (a) its reference (lowest-WSS) solution has no cluster with fewer than
#       micro_size documents, and
#   (b) at most max_prop_micro of the 20 seeds produce such a cluster.
# The selected k is the admissible k in the primary range with the highest
# mean silhouette (averaged over seeds).
scores_l2 <- scores_l2 %>%
  mutate(range = if_else(k %in% k_primary, "primary", "extended"),
         admissible = ref_n_micro == 0 & prop_seeds_micro <= max_prop_micro)
cand <- scores_l2 %>% filter(range == "primary", admissible)
if (nrow(cand) == 0) stop("No admissible k in the primary range.")
k_selected <- cand$k[which.max(cand$sil_mean)]
scores_l2  <- scores_l2 %>% mutate(selected = k == k_selected)

# Boundary check: report where the unconstrained maximum lies.
k_unconstrained_primary  <- scores_l2$k[scores_l2$range == "primary"][
  which.max(scores_l2$sil_mean[scores_l2$range == "primary"])]
k_unconstrained_extended <- scores_l2$k[which.max(scores_l2$sil_mean)]
sil_monotone <- all(diff(scores_l2$sil_mean) > 0)

# Alternative thresholds (sensitivity of the rule itself)
rule_sensitivity <- expand_grid(min_size_alt = c(3, 4, 5, 6),
                                max_prop_alt = c(0.1, 0.2, 0.5)) %>%
  rowwise() %>%
  mutate(selected_k = {
    s <- ev_l2$runs %>% filter(k %in% k_primary) %>% group_by(k) %>%
      summarise(p = mean(min_size < min_size_alt), .groups = "drop") %>%
      left_join(scores_l2 %>% select(k, sil_mean, ref_min_size), by = "k") %>%
      filter(ref_min_size >= min_size_alt, p <= max_prop_alt)
    if (nrow(s) == 0) NA_integer_ else as.integer(s$k[which.max(s$sil_mean)])
  }) %>% ungroup()
print(rule_sensitivity, n = Inf)

print(scores_l2 %>% select(k, sil_mean, sil_sd, ref_wss, ref_min_size,
                           ref_max_size, prop_seeds_micro, mean_ari,
                           admissible, selected), n = Inf)
cat("Selected k:", k_selected, "\n")

# ---- 7. Cluster-size diagnostics (reference solutions) -------------------------
size_diag <- function(ev, ref) {
  bind_rows(lapply(seq_len(nrow(ref)), function(i) {
    f <- ev$fits[[paste(ref$k[i], ref$seed[i])]]
    tibble(cl = f$cluster, s = f$sil_i) %>%
      group_by(cl) %>% summarise(size = n(), mean_sil = mean(s),
                                 .groups = "drop") %>%
      arrange(desc(size), cl) %>%
      transmute(representation = ref$representation[i], k = ref$k[i],
                seed = ref$seed[i], size_rank = row_number(), size,
                share = rnd(size / length(f$cluster), 4),
                mean_sil = rnd(mean_sil, 4),
                micro_cluster = size < micro_size)
  }))
}
cluster_sizes <- bind_rows(size_diag(ev_l2, ref_l2), size_diag(ev_raw, ref_raw))
# Sizes must match the stored assignments exactly
stopifnot(all(cluster_sizes %>% group_by(representation, k) %>%
                summarise(t = sum(size), .groups = "drop") %>% pull(t) ==
                nrow(X_l2)))

# ---- 8. Normalisation comparison and distance sensitivity ---------------------
norm_comparison <- bind_rows(scores_raw,
                             scores_l2 %>% filter(k %in% k_primary)) %>%
  transmute(representation, k, sil_mean = rnd(sil_mean, 4),
            sil_sd = rnd(sil_sd, 5), ref_wss = rnd(ref_wss, 3),
            ref_min_size, ref_max_size,
            ref_largest_share = rnd(ref_max_size / nrow(X_raw), 3),
            ref_n_singleton, mean_ari = rnd(mean_ari, 3))

# The same L2 assignments scored with cosine distance (1 - cos): checks that
# the shape of the silhouette curve does not depend on the distance form.
D_cos <- as.dist(1 - cos_l2)
sensitivity <- tibble(k = k_all) %>%
  mutate(sil_euclidean = scores_l2$sil_mean[match(k, scores_l2$k)],
         sil_cosine = vapply(k, function(kk) mean(vapply(seeds, function(sd)
           mean(silhouette(ev_l2$fits[[paste(kk, sd)]]$cluster, D_cos)[, 3]),
           numeric(1))), numeric(1)),
         sil_euclid_excl_isolated = vapply(k, function(kk) mean(vapply(seeds,
           function(sd) mean(ev_l2$fits[[paste(kk, sd)]]$sil_i[!isolated]),
           numeric(1))), numeric(1)))

# ---- 9. Figure ----------------------------------------------------------------
plot_df <- scores_l2 %>%
  mutate(status = factor(if_else(admissible, "Admissible", "Not admissible"),
                         levels = c("Admissible", "Not admissible")))
y_top <- max(plot_df$sil_max) * 1.15
lab_dir <- if (k_selected - min(k_all) < 3) 1 else -1   # +1 = right, -1 = left
p <- ggplot(plot_df, aes(x = k, y = sil_mean)) +
  annotate("rect", xmin = min(k_primary) - 0.5, xmax = max(k_primary) + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey93") +
  annotate("text", x = mean(k_primary), y = y_top, label = "Primary range",
           size = 3.2, colour = "grey35", vjust = 1) +
  annotate("text", x = mean(k_extended), y = y_top,
           label = "Extended check", size = 3.2, colour = "grey35", vjust = 1) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_linerange(aes(ymin = sil_min, ymax = sil_max), colour = "#9FB8CA",
                 linewidth = 0.8) +
  geom_line(colour = "#3B6E8F", linewidth = 0.7) +
  geom_point(aes(shape = status), colour = "#3B6E8F", fill = "white",
             size = 2.6, stroke = 0.9) +
  scale_shape_manual(values = c("Admissible" = 16, "Not admissible" = 21),
                     drop = FALSE, name = NULL) +
  geom_point(data = filter(plot_df, selected), shape = 21, size = 5.2,
             colour = "#C0392B", stroke = 1.1) +
  # Label placed up and to the left, with a leader line, so it does not
  # overlap the range bars of neighbouring k values.
  # Label to the upper left of the point, or to the lower right (the empty
  # area under the rising curve) if k is near the left edge of the axis.
  annotate("segment", x = k_selected + lab_dir * 0.12,
           xend = k_selected + lab_dir * 0.55,
           y = plot_df$sil_mean[plot_df$selected] - lab_dir * 0.0012,
           yend = plot_df$sil_mean[plot_df$selected] - lab_dir * 0.004,
           colour = "#7B241C", linewidth = 0.4) +
  annotate("text", x = k_selected + lab_dir * 0.6,
           y = plot_df$sil_mean[plot_df$selected] - lab_dir * 0.0045,
           label = paste0("Selected k = ", k_selected), colour = "#7B241C",
           size = 3.3, fontface = "bold", hjust = (1 - lab_dir) / 2,
           vjust = (1 + lab_dir) / 2) +
  scale_x_continuous(breaks = k_all) +
  scale_y_continuous(limits = c(0, y_top), expand = expansion(mult = c(0, 0))) +
  labs(title = "Mean Silhouette Width by Number of Clusters",
       subtitle = paste0("K-means on L2-normalised TF-IDF (", nrow(X_l2),
                         " documents); points = mean over ", length(seeds),
                         " seeds, bars = range"),
       x = "Number of clusters (k)", y = "Mean silhouette width") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot")
f_plot <- file.path(output_dir, "B_Q3b_silhouette_plot.png")
ggsave(f_plot, p, width = 7.5, height = 4.6, dpi = 300, bg = "white")

# Check plotted values equal the exported table
built <- ggplot_build(p)$data
line_layer <- built[[which(vapply(built, function(d) "group" %in% names(d) &&
                                     nrow(d) == length(k_all) &&
                                     !"ymin" %in% names(d), logical(1)))[1]]]
stopifnot(isTRUE(all.equal(line_layer$y[order(line_layer$x)],
                           scores_l2$sil_mean)))

# ---- 10. Exports ------------------------------------------------------------------
k_scores_out <- scores_l2 %>%
  transmute(representation, k, range,
            sil_mean = rnd(sil_mean, 5), sil_sd = rnd(sil_sd, 5),
            sil_min = rnd(sil_min, 5), sil_max = rnd(sil_max, 5),
            ref_seed, ref_wss = rnd(ref_wss, 4), ref_sil = rnd(ref_sil, 5),
            ref_sil_excl_micro = rnd(ref_sil_excl_micro, 5),
            ref_min_size, ref_max_size, ref_largest_share = rnd(ref_largest_share, 3),
            ref_n_micro, ref_n_singleton,
            prop_seeds_micro = rnd(prop_seeds_micro, 2),
            mean_ari = rnd(mean_ari, 3), admissible, selected)
write_out(k_scores_out, file.path(output_dir, "B_Q3_k_scores.csv"))
write_out(cluster_sizes,
          file.path(output_dir, "B_Q3b_cluster_size_diagnostics.csv"))
write_out(norm_comparison,
          file.path(output_dir, "B_Q3b_normalisation_comparison.csv"))
write_out(sensitivity %>% mutate(across(-k, ~ rnd(.x, 5))),
          file.path(output_dir, "B_Q3b_sensitivity.csv"))
write_out(bind_rows(input_checks %>% mutate(value = as.character(value)),
                    geometry %>% transmute(check = metric,
                                           value = as.character(value))),
          file.path(output_dir, "B_Q3b_input_checks.csv"))

sel_ref  <- ref_l2 %>% filter(k == k_selected)
sel_fit  <- ev_l2$fits[[paste(k_selected, sel_ref$seed)]]
selected_settings <- list(
  representation = "L2-normalised TF-IDF (eligible documents only)",
  X_l2 = Matrix(X_l2, sparse = TRUE),          # 211 x V, unit-length rows
  doc_metadata = elig_meta,                    # row order = X_l2 rows
  reply_uri = rownames(X_l2),                  # exact row order (Q3c checks)
  vocabulary_terms = colnames(X_l2),           # exact column order
  q3a_dims = dim(q3a$clustering_input),
  q3a_tokeniser = q3a$snapshot$icu,
  k = k_selected,
  kmeans_call = list(seed = sel_ref$seed, nstart = nstart,
                     iter.max = iter_max, algorithm = "Hartigan-Wong"),
  how_to_reproduce = paste0("set.seed(", sel_ref$seed, "); kmeans(as.matrix(",
                            "X_l2), centers = ", k_selected, ", nstart = ",
                            nstart, ", iter.max = ", iter_max, ")"),
  distance = "Euclidean on L2-normalised rows (= sqrt(2 * (1 - cosine)))",
  reference_cluster = setNames(sel_fit$cluster, rownames(X_l2)),
  reference_wss = sel_fit$wss,
  reference_sizes = sort(sel_fit$size, decreasing = TRUE),
  selection_rule = list(primary_range = k_primary, extended_range = k_extended,
                        seeds = seeds, micro_size = micro_size,
                        max_prop_micro = max_prop_micro),
  q3a_rds_md5 = unname(md5_before[f_q3a_rds]),
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  packages = vapply(pkgs, function(p) as.character(packageVersion(p)),
                    character(1))
)
saveRDS(selected_settings, file.path(output_dir, "B_Q3b_selected_settings.rds"))
writeLines(c(paste("Q3a RDS:", f_q3a_rds), "Q3a output MD5 checksums:",
             paste0("  ", names(md5_before), "  ", md5_before), "",
             paste("stringi ICU version:", stringi::stri_info()$ICU.version),
             paste("Q3a tidytext / tokenizers:",
                   q3a$snapshot$packages[["tidytext"]], "/",
                   as.character(packageVersion("tokenizers"))), "",
             capture.output(sessionInfo())),
           file.path(output_dir, "B_Q3b_session_info.txt"))

# ---- 11. Final checks ----------------------------------------------------------
stopifnot(identical(md5_before, tools::md5sum(q3a_files)))   # Q3a untouched
chk <- readRDS(file.path(output_dir, "B_Q3b_selected_settings.rds"))
set.seed(chk$kmeans_call$seed)
re <- kmeans(as.matrix(chk$X_l2), centers = chk$k, nstart = chk$kmeans_call$nstart,
             iter.max = chk$kmeans_call$iter.max)
stopifnot(identical(chk$reply_uri, rownames(q3a$clustering_input)),
          identical(chk$vocabulary_terms, colnames(q3a$clustering_input)),
          identical(rownames(chk$X_l2), chk$reply_uri),
          identical(colnames(chk$X_l2), chk$vocabulary_terms),
          identical(chk$doc_metadata$reply_uri, chk$reply_uri),
          isTRUE(all.equal(as.matrix(chk$X_l2),
                           as.matrix(q3a$clustering_input) /
                             sqrt(rowSums(as.matrix(q3a$clustering_input)^2)))))
stopifnot(isTRUE(all.equal(re$tot.withinss, chk$reference_wss)),
          ari(re$cluster, chk$reference_cluster) == 1)
cat("\nQ3b complete. Selected k =", k_selected, "| outputs in",
    normalizePath(output_dir), "\n")
