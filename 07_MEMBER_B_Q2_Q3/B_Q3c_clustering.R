
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "readr", "tibble", "Matrix", "cluster")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
  library(Matrix); library(cluster)
})

# ---- 1. Locate inputs -----------------------------------------------------------
# The agreed layout is outputs_B/Q3a/ and outputs_B/Q3b/; copies placed
# directly in the project folder are also accepted. The first existing
# candidate is used and reported.
find_input <- function(file, dirs) {
  cand <- file.path(dirs, file)
  hit <- cand[file.exists(cand)]
  if (length(hit) == 0) {
    stop(file, " not found. Looked in:\n  ", paste(cand, collapse = "\n  "),
         "\nWorking directory: ", getwd())
  }
  hit[1]
}
f_q3a  <- find_input("B_Q3a_tfidf_dtm.rds", c(file.path("outputs_B", "Q3a"), "."))
f_q3b  <- find_input("B_Q3b_selected_settings.rds",
                     c(file.path("outputs_B", "Q3b"), "."))
f_q3b_sizes <- file.path(dirname(f_q3b), "B_Q3b_cluster_size_diagnostics.csv")
f_docs <- file.path("05_CLEAN_TEAM_HANDOFF", "08_Q3_UNIQUE_REPLY_DOCUMENTS.csv")
output_dir <- file.path("outputs_B", "Q3c")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
cat("Q3a input:", f_q3a, "\nQ3b input:", f_q3b, "\n")

input_files <- c(f_q3a, f_q3b, if (file.exists(f_docs)) f_docs)
md5_before  <- tools::md5sum(input_files)

q3a <- readRDS(f_q3a)
st  <- readRDS(f_q3b)

# Export helper: write doubles as plain decimals (readr 2.1.x can otherwise
# print spurious digits such as 0.0018600000000000001).
write_out <- function(df, path) {
  as_txt <- function(x) vapply(x, function(v) if (is.na(v)) NA_character_ else
    format(v, digits = 15, scientific = FALSE), character(1))
  write_csv(mutate(df, across(where(is.double), as_txt)), path)
}

# ---- 2. Q3(a)-Q3(b) consistency checks (stop on any failure) ------------------
Xa   <- q3a$clustering_input                    # authoritative TF-IDF (sparse)
X    <- as.matrix(st$X_l2)                      # Q3(b) L2 representation
Xa_l2 <- as.matrix(Xa) / sqrt(rowSums(as.matrix(Xa)^2))
meta_all  <- q3a$doc_metadata
meta_elig <- meta_all %>% filter(eligible_for_clustering) %>%
  arrange(cluster_input_row)

consistency <- tibble::tribble(
  ~check, ~pass,
  "1. Q3a clustering input is 211 x 883",
    identical(dim(Xa), c(211L, 883L)),
  "1b. Q3a population: 229 documents, 18 zero-feature",
    nrow(meta_all) == 229 && sum(!meta_all$eligible_for_clustering) == 18,
  "2. Q3b matrix has the same rows (reply_uri, same order)",
    identical(rownames(X), rownames(Xa)),
  "3. Q3b matrix has the same vocabulary (names and order)",
    identical(colnames(X), colnames(Xa)) &&
      identical(colnames(X), q3a$vocabulary$term) &&
      identical(st$vocabulary_terms, colnames(Xa)),
  "3b. Tokenisation: 'mu.social' is one term, 'mu'/'social' absent",
    "mu.social" %in% colnames(X) && !any(c("mu", "social") %in% colnames(X)),
  "4. Rows map to the eligible reply_uri of Q3a metadata",
    identical(rownames(X), meta_elig$reply_uri) &&
      identical(st$reply_uri, meta_elig$reply_uri) &&
      identical(st$doc_metadata$reply_uri, meta_elig$reply_uri),
  "5. Q3b matrix equals L2-normalised Q3a matrix (tol 1e-12)",
    isTRUE(all.equal(X, Xa_l2, tolerance = 1e-12, check.attributes = FALSE)),
  "5b. Every Q3b row has unit length",
    isTRUE(all.equal(unname(rowSums(X^2)), rep(1, nrow(X)))),
  "6. Selected k is 5 (stored value)", identical(as.integer(st$k), 5L),
  "6b. Stored settings: Hartigan-Wong, nstart 50, iter.max 100",
    identical(st$kmeans_call$algorithm, "Hartigan-Wong") &&
      st$kmeans_call$nstart == 50 && st$kmeans_call$iter.max == 100,
  "7. Reference assignments cover the same 211 documents",
    identical(names(st$reference_cluster), meta_elig$reply_uri) &&
      length(st$reference_cluster) == 211 &&
      all(st$reference_cluster %in% seq_len(st$k)),
  "8. Settings refer to this Q3a RDS (MD5 match)",
    identical(st$q3a_rds_md5, unname(md5_before[f_q3a])),
  "8b. Settings record the Q3a dimensions and en_AU tokeniser",
    identical(as.integer(st$q3a_dims), dim(Xa)) &&
      grepl("locale en_AU", st$q3a_tokeniser, fixed = TRUE)
)
print(consistency, n = Inf)
if (!all(consistency$pass)) {
  stop("Q3a-Q3b consistency check(s) failed:\n  ",
       paste(consistency$check[!consistency$pass], collapse = "\n  "),
       "\nRe-run B_Q3a_tfidf.R and then B_Q3b_select_k.R on the same machine.")
}

# ---- 3. Reproduce the Q3(b) reference k-means solution -------------------------
k        <- as.integer(st$k)
seed     <- st$kmeans_call$seed
nstart   <- st$kmeans_call$nstart
iter_max <- st$kmeans_call$iter.max
algo     <- st$kmeans_call$algorithm

set.seed(seed)
km <- kmeans(X, centers = k, nstart = nstart, iter.max = iter_max,
             algorithm = algo)
stopifnot(km$ifault == 0)                      # converged without warning

# Align labels to the stored reference by reply_uri. If the reproduced labels
# are a permutation of the reference labels, map them; otherwise stop.
ref <- st$reference_cluster[rownames(X)]       # aligned by name, not position
tab <- table(reproduced = km$cluster, reference = ref)
one_to_one <- all(rowSums(tab > 0) == 1) && all(colSums(tab > 0) == 1)
if (!one_to_one) stop("Reproduced clustering differs from the Q3b reference.")
label_map <- setNames(as.integer(colnames(tab)[apply(tab, 1, which.max)]),
                      rownames(tab))
cluster   <- unname(label_map[as.character(km$cluster)])
labels_permuted <- !all(names(label_map) == as.character(label_map))

# Adjusted Rand index (Hubert and Arabie, 1985)
ari <- function(a, b) {
  t <- table(a, b); n <- sum(t)
  s_ij <- sum(choose(t, 2)); s_a <- sum(choose(rowSums(t), 2))
  s_b <- sum(choose(colSums(t), 2)); e <- s_a * s_b / choose(n, 2)
  (s_ij - e) / ((s_a + s_b) / 2 - e)
}

# Centroids in reference-label order; distances and WSS recomputed directly
centres <- km$centers[match(seq_len(k), label_map), , drop = FALSE]
rownames(centres) <- seq_len(k)
dist_centroid <- sqrt(rowSums((X - centres[cluster, , drop = FALSE])^2))
wss_by_cluster <- tapply(dist_centroid^2, cluster, sum)

sil <- silhouette(cluster, dist(X, method = "euclidean"))
sil_w <- sil[, "sil_width"]

reproduction <- tibble::tribble(
  ~check, ~value, ~pass,
  "Clusters in reproduced model", k, length(unique(cluster)) == k,
  "Documents assigned", length(cluster), length(cluster) == nrow(X),
  "Documents with exactly one cluster", sum(!is.na(cluster)),
    all(!is.na(cluster)) && !any(duplicated(rownames(X))),
  "Zero-feature documents clustered",
    sum(rownames(X) %in% meta_all$reply_uri[!meta_all$eligible_for_clustering]),
    !any(rownames(X) %in%
           meta_all$reply_uri[!meta_all$eligible_for_clustering]),
  "Cluster sizes equal reference sizes (sorted)", NA_real_,
    identical(sort(as.integer(table(cluster)), decreasing = TRUE),
              as.integer(st$reference_sizes)),
  "Total WSS", km$tot.withinss,
    isTRUE(all.equal(km$tot.withinss, st$reference_wss, tolerance = 1e-10)),
  "Total WSS recomputed from centroids", sum(wss_by_cluster),
    isTRUE(all.equal(sum(wss_by_cluster), km$tot.withinss, tolerance = 1e-10)),
  "Assignments identical to reference (after label alignment)",
    sum(cluster == ref), identical(as.integer(cluster), as.integer(ref)),
  "Labels permuted relative to reference (1 = yes)",
    as.numeric(labels_permuted), TRUE,
  "Adjusted Rand index vs reference", ari(cluster, ref),
    isTRUE(all.equal(ari(cluster, ref), 1))
)
print(reproduction, n = Inf)
stopifnot(all(reproduction$pass))

# ---- 4. Cluster-level diagnostics ------------------------------------------------
n_elig <- nrow(X)
cluster_summary <- tibble(cluster = cluster, d = dist_centroid, s = sil_w) %>%
  group_by(cluster) %>%
  summarise(documents = n(), share_pct = round(100 * n() / n_elig, 1),
            wss = sum(d^2), mean_dist_centroid = mean(d),
            max_dist_centroid = max(d), mean_silhouette = mean(s),
            negative_silhouette = sum(s < 0), .groups = "drop") %>%
  arrange(cluster)
stopifnot(isTRUE(all.equal(cluster_summary$wss,
                           unname(km$withinss[match(seq_len(k), label_map)]))),
          sum(cluster_summary$documents) == n_elig)

# Cross-check with the per-cluster silhouettes exported by Q3(b), if present
q3b_sil_check <- if (file.exists(f_q3b_sizes)) {
  q3b_sz <- read_csv(f_q3b_sizes, show_col_types = FALSE) %>%
    filter(representation == "L2-normalised TF-IDF", k == !!k) %>%
    arrange(size_rank)
  ours <- cluster_summary %>% arrange(desc(documents), cluster)
  ok <- identical(as.integer(q3b_sz$size), ours$documents) &&
    all(abs(q3b_sz$mean_sil - ours$mean_silhouette) < 5e-5)
  if (!ok) stop("Per-cluster silhouettes differ from the Q3b diagnostics.")
  "MATCH"
} else "Q3b size diagnostics not found (check skipped)"

model_stats <- tibble(
  statistic = c("Mean silhouette width (all documents)",
                "Documents with negative silhouette",
                "Total within-cluster sum of squares (WSS)",
                "Between-cluster sum of squares",
                "Total sum of squares",
                "Between / total sum of squares",
                "Iterations to convergence (best start)"),
  value = c(mean(sil_w), sum(sil_w < 0), km$tot.withinss, km$betweenss,
            km$totss, km$betweenss / km$totss, km$iter))
print(cluster_summary)
print(model_stats)

# ---- 5. Distinctive terms per cluster ------------------------------------------
# Terms are ranked by their mean L2-normalised TF-IDF weight in the cluster
# (the centroid coordinate). Document counts show whether a term is shared by
# several replies or comes from a single reply.
counts_elig <- q3a$dtm_counts[meta_all$eligible_for_clustering, , drop = FALSE]
stopifnot(identical(rownames(counts_elig), rownames(X)))
has_term <- as.matrix(counts_elig > 0)
docs_total <- colSums(has_term)
top_terms <- bind_rows(lapply(seq_len(k), function(c) {
  in_c <- cluster == c
  tibble(cluster = c, term = colnames(X), centroid_weight = centres[c, ],
         docs_in_cluster = colSums(has_term[in_c, , drop = FALSE]),
         docs_total = docs_total) %>%
    filter(docs_in_cluster > 0) %>%
    mutate(share_of_term_docs_in_cluster = docs_in_cluster / docs_total) %>%
    arrange(desc(centroid_weight), term) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 15)
}))

# ---- 6. Document-level assignment tables -----------------------------------------
docs_text <- if (file.exists(f_docs)) {
  read_csv(f_docs, col_types = cols(.default = col_character()),
           locale = locale(encoding = "UTF-8"))
} else NULL

assignments <- meta_elig %>%
  transmute(cluster_input_row, q3a_row_index = row_index, doc_id, reply_uri,
            root_uri, thread_label, thread,
            cluster = cluster[match(reply_uri, rownames(X))],
            silhouette = sil_w[match(reply_uri, rownames(X))],
            dist_to_centroid = dist_centroid[match(reply_uri, rownames(X))],
            nonzero_tfidf_features)
stopifnot(identical(assignments$reply_uri, rownames(X)),
          !any(duplicated(assignments$reply_uri)), nrow(assignments) == 211)

all_documents <- meta_all %>%
  select(q3a_row_index = row_index, doc_id, reply_uri, root_uri, thread_label,
         thread, eligible_for_clustering, exclusion_reason) %>%
  left_join(assignments %>% select(reply_uri, cluster, silhouette,
                                   dist_to_centroid), by = "reply_uri") %>%
  mutate(cluster_status = if_else(eligible_for_clustering, "Clustered",
                                  "Not clustered: zero-feature document"))
stopifnot(nrow(all_documents) == 229,
          all(is.na(all_documents$cluster[!all_documents$eligible_for_clustering])),
          all(!is.na(all_documents$cluster[all_documents$eligible_for_clustering])))

# Replies per cluster (for interpretation; full text for small clusters)
cluster_replies <- if (!is.null(docs_text)) {
  assignments %>% select(cluster, reply_uri, silhouette) %>%
    left_join(docs_text %>% select(reply_uri, reply_text), by = "reply_uri")
} else NULL

# ---- 7. Exports ------------------------------------------------------------------
diagnostics <- bind_rows(
  consistency %>% transmute(section = "Q3a-Q3b consistency", item = check,
                            value = NA_character_, pass),
  reproduction %>% transmute(section = "Reference reproduction", item = check,
                             value = as.character(signif(value, 10)), pass),
  model_stats %>% transmute(section = "Model statistics", item = statistic,
                            value = as.character(signif(value, 10)),
                            pass = NA),
  tibble(section = "Q3b cross-check", item = "Per-cluster silhouettes vs Q3b",
         value = q3b_sil_check, pass = q3b_sil_check == "MATCH" | NA),
  tibble(section = "Configuration",
         item = c("k", "seed", "nstart", "iter.max", "algorithm",
                  "representation", "distance"),
         value = c(k, seed, nstart, iter_max, algo, st$representation,
                   st$distance), pass = NA))

write_out(assignments, file.path(output_dir, "B_Q3_clusters.csv"))
write_out(all_documents, file.path(output_dir, "B_Q3c_all_documents.csv"))
write_out(cluster_summary, file.path(output_dir, "B_Q3c_cluster_summary.csv"))
write_out(diagnostics, file.path(output_dir, "B_Q3c_diagnostics.csv"))
write_out(top_terms, file.path(output_dir, "B_Q3c_top_terms_by_cluster.csv"))

verified_model <- list(
  k = k, cluster = setNames(cluster, rownames(X)), centres = centres,
  kmeans = km, label_map = label_map, X_l2 = st$X_l2,
  assignments = assignments, all_documents = all_documents,
  cluster_summary = cluster_summary, model_stats = model_stats,
  top_terms = top_terms,
  settings = list(seed = seed, nstart = nstart, iter.max = iter_max,
                  algorithm = algo, representation = st$representation,
                  distance = st$distance),
  provenance = list(q3a_rds = f_q3a, q3b_rds = f_q3b, input_md5 = md5_before,
                    q3a_tokeniser = st$q3a_tokeniser,
                    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
                    packages = vapply(pkgs, function(p)
                      as.character(packageVersion(p)), character(1)))
)
f_model <- file.path(output_dir, "B_Q3c_verified_model.rds")
saveRDS(verified_model, f_model)
writeLines(c(paste("Q3a input:", f_q3a), paste("Q3b input:", f_q3b),
             "Input MD5 checksums:",
             paste0("  ", names(md5_before), "  ", md5_before), "",
             capture.output(sessionInfo())),
           file.path(output_dir, "B_Q3c_session_info.txt"))

# ---- 8. Final checks -------------------------------------------------------------
stopifnot(identical(md5_before, tools::md5sum(input_files)))  # inputs untouched
chk <- readRDS(f_model)
csv <- read_csv(file.path(output_dir, "B_Q3_clusters.csv"), show_col_types = FALSE)
stopifnot(identical(csv$reply_uri, names(chk$cluster)),
          identical(as.integer(csv$cluster), unname(as.integer(chk$cluster))),
          identical(unname(chk$cluster), as.integer(st$reference_cluster)))
cat("\nQ3c complete. k =", k, "| sizes:",
    paste(cluster_summary$documents, collapse = ", "),
    "| outputs in", normalizePath(output_dir), "\n")
