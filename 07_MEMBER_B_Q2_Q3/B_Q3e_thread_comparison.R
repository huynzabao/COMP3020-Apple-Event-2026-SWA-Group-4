
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "readr", "tibble", "Matrix")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
  library(Matrix)
})

# ---- 1. Locate inputs -----------------------------------------------------------
find_input <- function(file, dirs) {
  cand <- file.path(dirs, file)
  hit <- cand[file.exists(cand)]
  if (length(hit) == 0) stop(file, " not found. Looked in:\n  ",
                             paste(cand, collapse = "\n  "),
                             "\nWorking directory: ", getwd())
  hit[1]
}
dir_of <- function(stage) c(file.path("outputs_B", stage), ".")
handoff <- c("05_CLEAN_TEAM_HANDOFF", ".")
f_model  <- find_input("B_Q3c_verified_model.rds", dir_of("Q3c"))
f_clus   <- find_input("B_Q3_clusters.csv", dir_of("Q3c"))
f_alldoc <- find_input("B_Q3c_all_documents.csv", dir_of("Q3c"))
f_q3a    <- find_input("B_Q3a_tfidf_dtm.rds", dir_of("Q3a"))
f_q3b    <- find_input("B_Q3b_selected_settings.rds", dir_of("Q3b"))
f_pca    <- find_input("B_Q3d_pca_coordinates.csv", dir_of("Q3d"))
f_docs   <- find_input("08_Q3_UNIQUE_REPLY_DOCUMENTS.csv", handoff)
f_q2     <- find_input("07_Q2_REPLIES_ENGLISH_BY_THREAD.csv", handoff)
output_dir <- file.path("outputs_B", "Q3e")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

input_files <- c(f_model, f_clus, f_alldoc, f_q3a, f_q3b, f_pca, f_docs, f_q2)
cat("Inputs used:\n", paste0("  ", input_files, "\n"))
md5_before <- tools::md5sum(input_files)

mod  <- readRDS(f_model)
q3a  <- readRDS(f_q3a)
st   <- readRDS(f_q3b)
clus <- read_csv(f_clus, show_col_types = FALSE)
alld <- read_csv(f_alldoc, show_col_types = FALSE)
pcad <- read_csv(f_pca, show_col_types = FALSE)
docs <- read_csv(f_docs, col_types = cols(.default = col_character()),
                 locale = locale(encoding = "UTF-8"))
q2   <- read_csv(f_q2, col_types = cols(.default = col_character()),
                 locale = locale(encoding = "UTF-8"))

write_out <- function(df, path) {          # plain decimals in CSV files
  as_txt <- function(x) vapply(x, function(v) if (is.na(v)) NA_character_ else
    format(v, digits = 15, scientific = FALSE), character(1))
  write_csv(mutate(df, across(where(is.double), as_txt)), path)
}

# ---- 2. Verify the final analytical snapshot (stop on any failure) ------------
meta  <- q3a$doc_metadata
elig  <- meta %>% filter(eligible_for_clustering) %>% arrange(cluster_input_row)
cl    <- mod$cluster                                  # named by reply_uri
dat   <- clus %>% select(reply_uri, root_uri, thread_label, cluster)

checks <- tibble::tribble(
  ~check, ~pass,
  "1. 211 eligible reply_uri, all unique",
    nrow(dat) == 211 && n_distinct(dat$reply_uri) == 211,
  "2. Every eligible reply has one thread label (3 labels)",
    all(!is.na(dat$thread_label)) && n_distinct(dat$thread_label) == 3,
  "3. Every eligible reply has exactly one cluster (1-3)",
    all(dat$cluster %in% 1:3),
  "4. CSV clusters identical to the Q3c model and the Q3b reference",
    identical(as.integer(dat$cluster),
              unname(as.integer(cl[dat$reply_uri]))) &&
      identical(unname(as.integer(cl)), unname(as.integer(st$reference_cluster))),
  "5. Cluster sizes 193, 12, 6 (total 211)",
    identical(as.integer(table(dat$cluster)), c(193L, 12L, 6L)),
  "6. Eligible thread counts 174, 21, 16 (total 211)",
    identical(as.integer(table(dat$thread_label)), c(174L, 21L, 16L)),
  "6b. Q2/Q3 population thread counts 187, 26, 16 (file 07)",
    identical(as.integer(table(q2$root_uri)[unique(elig$root_uri)]),
              c(187L, 26L, 16L)),
  "7. 18 zero-feature replies excluded (cluster missing in audit table)",
    sum(!meta$eligible_for_clustering) == 18 &&
      all(is.na(alld$cluster[!alld$eligible_for_clustering])) &&
      !any(dat$reply_uri %in% meta$reply_uri[!meta$eligible_for_clustering]),
  "8. Thread labels and root_uri equal Q3a metadata",
    identical(dat$thread_label, elig$thread_label) &&
      identical(dat$root_uri, elig$root_uri),
  "9. Same document order in Q3a, Q3b, Q3c and Q3d",
    identical(dat$reply_uri, elig$reply_uri) &&
      identical(dat$reply_uri, names(cl)) &&
      identical(dat$reply_uri, st$reply_uri) &&
      identical(dat$reply_uri, pcad$reply_uri),
  "10. Final snapshot: 883 terms, k = 3, seed 16 (no 884-term or k = 5 files)",
    ncol(q3a$clustering_input) == 883 && "mu.social" %in% st$vocabulary_terms &&
      mod$k == 3 && st$k == 3 && mod$settings$seed == 16
)
print(checks, n = Inf)
if (!all(checks$pass)) stop("Snapshot check(s) failed:\n  ",
                            paste(checks$check[!checks$pass], collapse = "\n  "))

# ---- 3. Contingency table and percentages ---------------------------------------
dat <- dat %>% mutate(cluster = factor(paste("Cluster", cluster)),
                      thread = factor(thread_label))
tab <- table(Cluster = dat$cluster, Thread = dat$thread)
row_tot <- rowSums(tab); col_tot <- colSums(tab); N <- sum(tab)

# Row %: denominator = cluster total (composition of each cluster)
row_pct <- 100 * sweep(tab, 1, row_tot, "/")
# Column %: denominator = thread total (where each thread's replies went)
col_pct <- 100 * sweep(tab, 2, col_tot, "/")
# Expected counts if cluster membership were unrelated to thread
expected <- outer(row_tot, col_tot) / N

table_checks <- tibble::tribble(
  ~check, ~pass,
  "Table is 3 x 3", identical(dim(tab), c(3L, 3L)),
  "Grand total is 211 (no missing or duplicated replies)",
    N == 211 && N == nrow(dat),
  "Row totals equal Q3c cluster sizes",
    identical(as.integer(row_tot), as.integer(mod$cluster_summary$documents)),
  "Column totals equal Q3a eligible thread counts",
    identical(as.integer(col_tot),
              as.integer(table(elig$thread_label)[colnames(tab)])),
  "Row percentages sum to 100 in every cluster",
    isTRUE(all.equal(unname(rowSums(row_pct)), rep(100, 3))),
  "Column percentages sum to 100 in every thread",
    isTRUE(all.equal(unname(colSums(col_pct)), rep(100, 3)))
)
stopifnot(all(table_checks$pass))
print(addmargins(tab)); print(round(row_pct, 1)); print(round(col_pct, 1))

as_long <- function(m, value) {
  as.data.frame.matrix(m) %>% rownames_to_column("cluster") %>%
    pivot_longer(-cluster, names_to = "thread", values_to = value)
}
counts_out <- as.data.frame.matrix(addmargins(tab)) %>%
  rownames_to_column("cluster") %>% mutate(cluster = sub("^Sum$", "Total", cluster))
names(counts_out)[names(counts_out) == "Sum"] <- "Total"
row_out <- as.data.frame.matrix(round(row_pct, 2)) %>%
  rownames_to_column("cluster") %>% mutate(denominator_cluster_total = row_tot)
col_out <- as.data.frame.matrix(round(col_pct, 2)) %>%
  rownames_to_column("cluster") %>%
  bind_rows(tibble(cluster = "Denominator (thread total)",
                   !!!as.list(as.numeric(col_tot)) %>% setNames(colnames(tab))))

# ---- 4. Supplementary association measures ------------------------------------
# All three compare the cluster partition with the thread partition; they
# are descriptive and are not the seed-stability ARI of Q3(b).
ari <- function(t) {                              # adjusted Rand index
  n <- sum(t); s_ij <- sum(choose(t, 2))
  s_a <- sum(choose(rowSums(t), 2)); s_b <- sum(choose(colSums(t), 2))
  e <- s_a * s_b / choose(n, 2); (s_ij - e) / ((s_a + s_b) / 2 - e)
}
nmi <- function(t) {                              # normalised mutual information
  p <- t / sum(t); pr <- rowSums(p); pc <- colSums(p); nz <- p > 0
  mi <- sum(p[nz] * log(p[nz] / outer(pr, pc)[nz]))
  mi / sqrt(sum(-pr * log(pr)) * sum(-pc * log(pc)))
}
chi_sq   <- sum((tab - expected)^2 / expected)
cramers_v <- sqrt(chi_sq / (N * (min(dim(tab)) - 1)))
# Small-cluster check: probability that 18 replies drawn at random from the
# 211 contain at least as many Thread 1 replies as Clusters 2 and 3 together.
small <- c("Cluster 2", "Cluster 3")
t1 <- names(col_tot)[1]
n_small <- sum(row_tot[small]); t1_small <- sum(tab[small, t1])
p_small_t1 <- phyper(t1_small - 1, col_tot[[t1]], N - col_tot[[t1]], n_small,
                     lower.tail = FALSE)

summary_out <- bind_rows(
  as_long(tab, "observed") %>%
    left_join(as_long(expected, "expected_if_unrelated"),
              by = c("cluster", "thread")) %>%
    left_join(as_long(row_pct, "row_pct"), by = c("cluster", "thread")) %>%
    left_join(as_long(col_pct, "col_pct"), by = c("cluster", "thread")) %>%
    mutate(measure = "cell", value = NA_real_),
  tibble(measure = c("Adjusted Rand index (clusters vs threads)",
                     "Normalised mutual information (geometric mean)",
                     "Cramer's V", "Cells with expected count < 5",
                     paste0("Thread 1 share of all eligible replies (%)"),
                     "Thread 1 replies in Clusters 2 and 3",
                     "Replies in Clusters 2 and 3",
                     "P(>= that many Thread 1 replies in 18 random replies)"),
         value = c(ari(tab), nmi(tab), cramers_v, sum(expected < 5),
                   100 * col_tot[[t1]] / N, t1_small, n_small, p_small_t1)))
print(summary_out %>% filter(measure != "cell"))

# ---- 5. Evidence: verified quotations ------------------------------------------
# Each quotation must match an original reply verbatim and lie in the stated
# cluster and thread; its reply URI is recorded.
quotes <- tibble::tribble(
  ~purpose, ~cluster_expected, ~thread_expected, ~quote,
  "Cluster 2: reply about the meaning of the post", 2L, "Thread 1",
    "They're clearly talking about fruit.",
  "Cluster 2: hostility towards Apple (Thread 1)", 2L, "Thread 1",
    "Apple can fuck right off.",
  "Cluster 2: hostility towards Apple (Thread 3)", 2L, "Thread 3",
    "Fuck Apple",
  "Cluster 3: apple-variety wordplay", 3L, "Thread 1",
    "Cosmic Crisp apples are a revelation."
)
lookup <- clus %>% left_join(docs %>% select(reply_uri, reply_text),
                             by = "reply_uri")
evidence <- quotes %>% rowwise() %>%
  mutate(reply_uri = {
    hit <- lookup$reply_uri[!is.na(lookup$reply_text) &
                              grepl(quote, lookup$reply_text, fixed = TRUE)]
    if (length(hit) == 1) hit else NA_character_
  }) %>% ungroup() %>%
  left_join(lookup %>% select(reply_uri, cluster, thread_label, reply_text),
            by = "reply_uri") %>%
  mutate(verified = !is.na(reply_uri) & cluster == cluster_expected &
           thread_label == thread_expected)
if (!all(evidence$verified)) stop("Quotation(s) not verified: ",
                                  paste(evidence$quote[!evidence$verified],
                                        collapse = " | "))

# ---- 6. Exports ------------------------------------------------------------------
write_out(counts_out, file.path(output_dir, "B_Q3e_cluster_thread_counts.csv"))
write_out(row_out, file.path(output_dir, "B_Q3e_cluster_thread_row_pct.csv"))
write_out(col_out, file.path(output_dir, "B_Q3e_cluster_thread_col_pct.csv"))
write_out(summary_out %>% select(measure, cluster, thread, observed,
                                 expected_if_unrelated, row_pct, col_pct, value),
          file.path(output_dir, "B_Q3e_thread_comparison_summary.csv"))
write_out(evidence %>% select(purpose, cluster, thread_label, quote, reply_uri,
                              reply_text, verified),
          file.path(output_dir, "B_Q3e_evidence.csv"))
write_out(bind_rows(checks %>% mutate(section = "Snapshot"),
                    table_checks %>% mutate(section = "Contingency table"),
                    tibble(section = "Evidence", check = evidence$purpose,
                           pass = evidence$verified)) %>%
            select(section, check, pass),
          file.path(output_dir, "B_Q3e_validation_checks.csv"))
writeLines(c("Inputs used:", paste0("  ", names(md5_before), "  ", md5_before),
             "", capture.output(sessionInfo())),
           file.path(output_dir, "B_Q3e_session_info.txt"))

# ---- 7. Final checks -------------------------------------------------------------
stopifnot(identical(md5_before, tools::md5sum(input_files)))  # inputs untouched
re <- read_csv(file.path(output_dir, "B_Q3e_cluster_thread_counts.csv"),
               show_col_types = FALSE)
stopifnot(re$Total[re$cluster == "Total"] == 211,
          identical(as.integer(re$Total[1:3]), as.integer(row_tot)))
cat("\nQ3e complete | outputs in", normalizePath(output_dir), "\n")
