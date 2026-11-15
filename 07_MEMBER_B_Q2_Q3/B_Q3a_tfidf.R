
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "readr", "stringr", "tidyr", "tibble", "tidytext", "Matrix")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(tibble); library(tidytext); library(Matrix)
})

# ---- 1. Paths -----------------------------------------------------------------
data_dir   <- "05_CLEAN_TEAM_HANDOFF"
output_dir <- file.path("outputs_B", "Q3a")

f_docs    <- file.path(data_dir, "08_Q3_UNIQUE_REPLY_DOCUMENTS.csv")
f_replies <- file.path(data_dir, "07_Q2_REPLIES_ENGLISH_BY_THREAD.csv")
f_top3    <- file.path(data_dir, "06_Q2_TOP3_POSTS_FROM_CURRENT_LABELS.csv")
f_status  <- file.path(data_dir, "00_READ_FIRST_HANDOFF_STATUS.txt")
f_q2b_sum <- file.path("outputs_B", "B_Q2_tokenisation_summary.csv")

required_files <- c(f_docs, f_replies, f_top3)
absent <- required_files[!file.exists(required_files)]
if (length(absent) > 0) {
  stop("Input file(s) not found (working directory: ", getwd(), "):\n  ",
       paste(absent, collapse = "\n  "))
}
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# MD5 checksums before the analysis: used to prove the inputs are unchanged.
input_files <- c(required_files, if (file.exists(f_status)) f_status)
md5_before  <- tools::md5sum(input_files)

read_all_chr <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()),
           na = c("", "NA"), locale = locale(encoding = "UTF-8"),
           progress = FALSE)
}
check_cols <- function(df, cols, name) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0)
    stop(name, " is missing column(s): ", paste(miss, collapse = ", "))
}

docs    <- read_all_chr(f_docs)
replies <- read_all_chr(f_replies)
top3    <- read_all_chr(f_top3)

check_cols(docs, c("root_uri", "reply_uri", "reply_text", "thread_label"),
           "08_Q3_DOCS")
check_cols(replies, c("root_uri", "reply_uri", "reply_text"), "07_Q2_REPLIES")
check_cols(top3, c("uri", "author_handle", "reply_count"), "06_Q2_TOP3")

status_lines <- if (file.exists(f_status)) readLines(f_status, warn = FALSE) else
  character(0)
status_value <- function(key) {
  hit <- grep(key, status_lines, value = TRUE, fixed = TRUE)[1]
  if (is.na(hit)) NA_character_ else str_trim(str_remove(hit, fixed(key)))
}
snapshot_created <- status_value("Created at UTC:")
q2q3_status      <- status_value("Q2/Q3 replies status:")
top3_same        <- status_value("Top-3 threads still match pilot:")

# ---- 2. Thread mapping and dataset validation -------------------------------
# T1-T3 follow the Q2(a) order (the 06 file is ordered by reply_count).
stopifnot(nrow(top3) == 3, !any(duplicated(top3$uri)))
threads <- top3 %>%
  transmute(thread = paste0("T", row_number()), root_uri = uri,
            root_author = author_handle)

docs <- docs %>%
  mutate(thread = threads$thread[match(root_uri, threads$root_uri)])

# Correspondence between the original thread_label and T1-T3
label_map <- docs %>% count(root_uri, thread_label, thread, name = "n_docs")

m07 <- match(docs$reply_uri, replies$reply_uri)
blank <- function(x) is.na(x) | str_squish(x) == ""

validation <- tibble::tribble(
  ~check, ~value,
  "1. Rows in 08 (documents)", nrow(docs),
  "2. Distinct reply_uri in 08", n_distinct(docs$reply_uri),
  "3. Duplicate reply_uri in 08", sum(duplicated(docs$reply_uri)),
  "3b. Missing reply_uri in 08", sum(is.na(docs$reply_uri)),
  "4. Missing or blank reply_text in 08", sum(blank(docs$reply_text)),
  "5. Distinct root_uri in 08", n_distinct(docs$root_uri),
  "6. Distinct thread_label values in 08", n_distinct(docs$thread_label),
  "6b. Rows with missing thread_label", sum(is.na(docs$thread_label)),
  "7. Rows whose root_uri is not a Q2(a) root", sum(is.na(docs$thread)),
  "7b. thread_label maps one-to-one to T1-T3 (1 = yes)",
    as.integer(nrow(label_map) == 3 &&
                 all(label_map$thread_label ==
                       paste("Thread", str_remove(label_map$thread, "T")))),
  "8. Rows in 07 (Q2 reply population)", nrow(replies),
  "8b. 08 reply_uri set equals 07 reply_uri set (1 = yes)",
    as.integer(setequal(docs$reply_uri, replies$reply_uri)),
  "8c. Rows with root_uri differing between 07 and 08",
    sum(docs$root_uri != replies$root_uri[m07], na.rm = TRUE),
  "9. Rows with reply_text differing between 07 and 08",
    sum(docs$reply_text != replies$reply_text[m07], na.rm = TRUE),
  "10. Handoff status permits reuse (1 = REUSE_OK)",
    as.integer(!is.na(q2q3_status) && str_starts(q2q3_status, "REUSE_OK")),
  "10b. Top-3 threads still match pilot (1 = TRUE)",
    as.integer(identical(top3_same, "TRUE"))
)
thread_counts <- docs %>% count(thread, thread_label, name = "documents")

print(validation, n = Inf)
print(thread_counts)

# Hard stops: Q3 must use exactly the validated Q2 reply population.
v <- setNames(validation$value, validation$check)
stopifnot(
  v["1. Rows in 08 (documents)"] == v["2. Distinct reply_uri in 08"],
  v["3. Duplicate reply_uri in 08"] == 0, v["3b. Missing reply_uri in 08"] == 0,
  v["4. Missing or blank reply_text in 08"] == 0,
  v["5. Distinct root_uri in 08"] == 3, v["6b. Rows with missing thread_label"] == 0,
  v["7. Rows whose root_uri is not a Q2(a) root"] == 0,
  v["7b. thread_label maps one-to-one to T1-T3 (1 = yes)"] == 1,
  v["8b. 08 reply_uri set equals 07 reply_uri set (1 = yes)"] == 1,
  v["8c. Rows with root_uri differing between 07 and 08"] == 0,
  v["9. Rows with reply_text differing between 07 and 08"] == 0
)
if (v["10. Handoff status permits reuse (1 = REUSE_OK)"] != 1)
  warning("Handoff status file missing or not REUSE_OK: check before use.")

# ---- 3. Deterministic document order ------------------------------------------
# Rows are ordered by thread (T1, T2, T3) and then reply_uri in C-locale order,
# so the row order does not depend on the order of rows in the CSV file.
docs <- docs %>%
  arrange(thread, reply_uri) %>%
  mutate(row_index = row_number(),
         doc_id = sprintf("D%03d", row_index))

# ---- 4. Preprocessing (identical to the final Q2(b) script) -------------------
# Applied to the original reply_text; the original text is never modified.
normalise_text <- function(x) {
  x <- str_replace_all(x, "[\u2018\u2019\u02BC]", "'")  # curly -> straight '
  x <- str_replace_all(x, "[\u201C\u201D]", '"')
  x <- str_replace_all(x, "&amp;", "&")                  # HTML entities
  x <- str_replace_all(x, "&(lt|gt|quot);", " ")
  # URLs: full links, www links and truncated "domain.tld/path" links
  x <- str_replace_all(x, "https?://\\S+|www\\.\\S+", " ")
  x <- str_replace_all(x, "\\b[[:alnum:]-]+(\\.[[:alnum:]-]+)+/\\S*", " ")
  # user mentions (@handle or @handle.domain)
  x <- str_replace_all(x, "(?<![[:alnum:]])@[[:alnum:]._-]+", " ")
  x <- str_replace_all(x, "#", " ")                      # keep hashtag word
  x <- str_replace_all(x, "(\\d),(\\d{3})\\b", "\\1\\2") # 2,000 -> 2000
  str_squish(str_to_lower(x))
}

# Stopwords: Snowball English (tidytext::stop_words, lexicon "snowball") plus
# the 14 custom words documented in Q2(b):
#   A: modal auxiliaries absent from Snowball
#   B: high-frequency adverbs/intensifiers with no topical content
# Product and brand words are retained; no stemming or lemmatisation.
snowball_sw <- stop_words %>% filter(lexicon == "snowball") %>% pull(word)
custom_sw <- tibble(
  word  = c("can", "will", "might", "must", "may", "shall",
            "just", "now", "much", "also", "even", "actually", "really", "still"),
  group = c(rep("A: modal auxiliary", 6), rep("B: adverb/intensifier", 8))
)
stopifnot(nrow(custom_sw) == 14, !any(custom_sw$word %in% snowball_sw))
stopword_list <- unique(c(snowball_sw, custom_sw$word))

docs <- docs %>% mutate(text_clean = normalise_text(reply_text))

tokens_raw <- docs %>%
  select(doc_id, thread, reply_uri, text_clean) %>%
  unnest_tokens(word, text_clean, token = "words", to_lower = TRUE)

tokens <- tokens_raw %>%
  filter(!word %in% stopword_list) %>%
  mutate(word = str_remove(word, "'s$")) %>%       # possessive: apple's -> apple
  filter(!word %in% stopword_list,                 # e.g. "that's" -> "that"
         nchar(word) > 1)                          # single letters / digits

# ---- 5. Term frequencies and TF-IDF -------------------------------------------
# Document-term counts: one row per (document, term) with count n_{t,d}.
doc_term_counts <- tokens %>% count(doc_id, word, name = "n")

# tidytext::bind_tf_idf() (tidytext 0.4.x) computes, for the documents that
# appear in its input table:
#   tf(t,d)  = n_{t,d} / sum_t' n_{t',d}         (count / document length)
#   idf(t)   = ln( N / df_t )                     (natural log, no smoothing)
#   tf_idf   = tf(t,d) * idf(t)
# N is the number of documents present in the input, i.e. documents with at
# least one retained token; df_t is the number of those documents containing t.
tfidf_long <- doc_term_counts %>% bind_tf_idf(word, doc_id, n)

# Independent recomputation of the same formula (guards against misreading it)
N_idf <- n_distinct(doc_term_counts$doc_id)
check_long <- doc_term_counts %>%
  group_by(doc_id) %>% mutate(tf_chk = n / sum(n)) %>% ungroup() %>%
  group_by(word) %>% mutate(idf_chk = log(N_idf / n_distinct(doc_id))) %>%
  ungroup()
stopifnot(
  isTRUE(all.equal(tfidf_long$tf, check_long$tf_chk, tolerance = 1e-12)),
  isTRUE(all.equal(tfidf_long$idf, check_long$idf_chk, tolerance = 1e-12)),
  all(is.finite(tfidf_long$tf_idf)), all(tfidf_long$tf_idf >= 0)
)

# ---- 6. Sparse document-term matrices -----------------------------------------
# Rows: ALL documents in the validated population (zero rows kept, so the
# matrix never silently drops a reply). Columns: vocabulary in C-locale order.
vocab_terms <- sort(unique(tfidf_long$word), method = "radix")
i_idx <- match(tfidf_long$doc_id, docs$doc_id)
j_idx <- match(tfidf_long$word, vocab_terms)
dims  <- c(nrow(docs), length(vocab_terms))
dn    <- list(docs$reply_uri, vocab_terms)

dtm_counts <- sparseMatrix(i = i_idx, j = j_idx, x = as.numeric(tfidf_long$n),
                           dims = dims, dimnames = dn)
dtm_tfidf  <- sparseMatrix(i = i_idx, j = j_idx, x = tfidf_long$tf_idf,
                           dims = dims, dimnames = dn)

# Entries with idf = 0 (term in every eligible document) would be stored as
# explicit zeros; drop them so "nonzero" means a genuine nonzero weight.
dtm_tfidf <- drop0(dtm_tfidf)

# ---- 7. Document metadata and zero-feature audit ----------------------------
has_url     <- str_detect(docs$reply_text,
                          "https?://|www\\.|\\b[[:alnum:]-]+(\\.[[:alnum:]-]+)+/")
has_mention <- str_detect(docs$reply_text, "(?<![[:alnum:]])@[[:alnum:]._-]+")

doc_meta <- docs %>%
  mutate(raw_tokens = as.integer(table(factor(tokens_raw$doc_id,
                                              levels = doc_id))),
         retained_tokens = as.integer(rowSums(dtm_counts)),
         distinct_terms = as.integer(rowSums(dtm_counts > 0)),
         nonzero_tfidf_features = as.integer(rowSums(dtm_tfidf != 0)),
         has_url = has_url, has_mention = has_mention,
         eligible_for_clustering = nonzero_tfidf_features > 0,
         exclusion_reason = case_when(
           eligible_for_clustering ~ NA_character_,
           raw_tokens == 0 & (has_url | has_mention) ~
             "No word tokens after URL/mention removal",
           raw_tokens == 0 ~ "No word tokens (emoji or punctuation only)",
           retained_tokens == 0 ~
             "All tokens are stopwords or single characters",
           TRUE ~ "Retained terms all have zero IDF"),
         document_status = if_else(eligible_for_clustering,
                                   "Eligible (>=1 nonzero TF-IDF feature)",
                                   "Zero-feature: retained, not clusterable")) %>%
  group_by(eligible_for_clustering) %>%
  mutate(cluster_input_row = if_else(eligible_for_clustering,
                                     row_number(), NA_integer_)) %>%
  ungroup() %>%
  arrange(row_index)

zero_feature_docs <- doc_meta %>%
  filter(!eligible_for_clustering) %>%
  select(row_index, doc_id, reply_uri, root_uri, thread_label, thread,
         raw_tokens, retained_tokens, nonzero_tfidf_features,
         exclusion_reason, reply_text)

# ---- 8. Vocabulary statistics ---------------------------------------------------
vocabulary <- tibble(
  col_index = seq_along(vocab_terms),
  term = vocab_terms,
  total_count = as.integer(colSums(dtm_counts)),
  doc_freq = as.integer(colSums(dtm_counts > 0)),
  idf = as.numeric(log(N_idf / colSums(dtm_counts > 0))),
  sum_tfidf = as.numeric(colSums(dtm_tfidf)),
  max_tfidf = as.numeric(apply(dtm_tfidf, 2, max))
) %>%
  left_join(tokens %>% distinct(thread, doc_id, word) %>%
              count(word, thread) %>%
              pivot_wider(names_from = thread, values_from = n,
                          values_fill = 0, names_prefix = "doc_freq_"),
            by = c("term" = "word")) %>%
  mutate(n_threads = (doc_freq_T1 > 0) + (doc_freq_T2 > 0) + (doc_freq_T3 > 0))
stopifnot(isTRUE(all.equal(vocabulary$idf,
                           tfidf_long$idf[match(vocabulary$term,
                                                tfidf_long$word)])))

# ---- 9. Matrix diagnostics ------------------------------------------------------
elig <- doc_meta$eligible_for_clustering
nnz  <- length(dtm_tfidf@x)
cells_all  <- prod(dim(dtm_tfidf))
cells_elig <- sum(elig) * ncol(dtm_tfidf)

matrix_summary <- tibble::tribble(
  ~metric, ~value,
  "Input reply documents (08)", nrow(docs),
  "Documents represented as matrix rows", nrow(dtm_tfidf),
  "Documents with >=1 retained token", sum(doc_meta$retained_tokens > 0),
  "Documents with >=1 nonzero TF-IDF feature (eligible)", sum(elig),
  "Zero-feature documents (all-zero rows)", sum(!elig),
  "N used in IDF (documents in bind_tf_idf input)", N_idf,
  "Vocabulary size (columns)", ncol(dtm_tfidf),
  "Terms occurring in exactly one document (df = 1)",
    sum(vocabulary$doc_freq == 1),
  "Terms occurring in >= 2 documents", sum(vocabulary$doc_freq >= 2),
  "Terms with idf = 0 (in every eligible document)", sum(vocabulary$idf == 0),
  "Raw tokens (before stopword removal)", nrow(tokens_raw),
  "Retained tokens (sum of count DTM)", sum(dtm_counts),
  "Nonzero entries (TF-IDF DTM)", nnz,
  "Density, full matrix (%)", round(100 * nnz / cells_all, 3),
  "Sparsity, full matrix (%)", round(100 * (1 - nnz / cells_all), 3),
  "Density, eligible rows only (%)", round(100 * nnz / cells_elig, 3),
  "Median nonzero features per eligible document",
    median(doc_meta$nonzero_tfidf_features[elig]),
  "Max nonzero features per document", max(doc_meta$nonzero_tfidf_features),
  "Min TF-IDF value (nonzero entries)", signif(min(dtm_tfidf@x), 6),
  "Max TF-IDF value", signif(max(dtm_tfidf@x), 6),
  "Maximum idf (= ln N)", signif(max(vocabulary$idf), 6),
  "Duplicate row identifiers", sum(duplicated(rownames(dtm_tfidf))),
  "Rows without a thread label", sum(is.na(doc_meta$thread_label)),
  "Non-finite or negative values", sum(!is.finite(dtm_tfidf@x) | dtm_tfidf@x < 0)
)
thread_summary <- doc_meta %>%
  group_by(thread, thread_label) %>%
  summarise(documents = n(),
            zero_token_docs = sum(retained_tokens == 0),
            zero_feature_docs = sum(!eligible_for_clustering),
            eligible_docs = sum(eligible_for_clustering),
            retained_tokens = sum(retained_tokens), .groups = "drop")
print(matrix_summary, n = Inf)
print(thread_summary)

# ---- 10. Verification ------------------------------------------------------------
stopifnot(
  nrow(dtm_tfidf) == nrow(docs),                       # no silent row loss
  identical(rownames(dtm_tfidf), doc_meta$reply_uri),  # rows <-> metadata
  identical(rownames(dtm_counts), rownames(dtm_tfidf)),
  identical(colnames(dtm_tfidf), vocabulary$term),     # columns <-> vocabulary
  !any(duplicated(rownames(dtm_tfidf))),
  all(!is.na(doc_meta$thread_label)),
  all(is.finite(dtm_tfidf@x)), all(dtm_tfidf@x >= 0),
  sum(dtm_counts) == nrow(tokens),
  all(doc_meta$retained_tokens ==
        as.integer(table(factor(tokens$doc_id, levels = doc_meta$doc_id)))),
  # term frequencies of every eligible document sum to 1
  isTRUE(all.equal(as.numeric(rowSums(dtm_counts[elig, ] /
                                        rowSums(dtm_counts[elig, ]))),
                   rep(1, sum(elig)))),
  # every zero-token document is a zero-feature document and vice versa
  identical(doc_meta$retained_tokens == 0, !elig),
  nrow(zero_feature_docs) == sum(!elig),
  all(!is.na(zero_feature_docs$exclusion_reason))
)

# Optional cross-check against the Q2(b) outputs (if they are in outputs_B/)
q2b_check <- if (file.exists(f_q2b_sum)) {
  q2b <- read_csv(f_q2b_sum, show_col_types = FALSE)
  cmp <- thread_summary %>%
    left_join(q2b %>% select(thread, replies_analysed, replies_zero_tokens,
                             q2b_retained = retained_tokens), by = "thread")
  ok <- with(cmp, all(documents == replies_analysed,
                      zero_token_docs == replies_zero_tokens,
                      retained_tokens == q2b_retained))
  if (!ok) warning("Q3a token counts differ from Q2(b) outputs.")
  if (ok) "MATCH" else "MISMATCH"
} else "Q2(b) summary not found in outputs_B/ (cross-check skipped)"
cat("Q2(b) cross-check:", q2b_check, "\n")

# ---- 11. Exports ------------------------------------------------------------------
settings <- list(
  tf_definition  = "tf(t,d) = n_td / sum_t' n_t'd (tidytext::bind_tf_idf)",
  idf_definition = "idf(t) = ln(N / df_t), natural log, no smoothing",
  N_idf = N_idf,
  row_order = "thread (T1, T2, T3), then reply_uri (C-locale)",
  column_order = "term (C-locale)",
  stopwords_snowball_n = length(snowball_sw),
  stopwords_custom = custom_sw,
  stemming = FALSE, df_threshold = "none (unpruned base representation)",
  row_normalisation = "none (tf is length-normalised; no L2 row scaling)"
)
snapshot <- list(
  snapshot_created_utc = snapshot_created,
  handoff_q2q3_status = q2q3_status,
  input_md5 = md5_before,
  q2b_cross_check = q2b_check,
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  r_version = R.version.string,
  packages = vapply(pkgs, function(p) as.character(packageVersion(p)),
                    character(1))
)

doc_meta_out <- doc_meta %>%
  select(row_index, doc_id, reply_uri, root_uri, thread_label, thread,
         document_status, eligible_for_clustering, cluster_input_row,
         raw_tokens, retained_tokens, distinct_terms, nonzero_tfidf_features,
         exclusion_reason, has_url, has_mention)

saveRDS(list(
  dtm_tfidf = dtm_tfidf,                    # 229 x V, zero rows retained
  dtm_counts = dtm_counts,                  # raw counts n_{t,d}, same layout
  clustering_input = dtm_tfidf[elig, , drop = FALSE],  # eligible rows only
  doc_metadata = doc_meta_out,
  vocabulary = vocabulary,
  settings = settings,
  snapshot = snapshot
), file.path(output_dir, "B_Q3a_tfidf_dtm.rds"))

write_csv(doc_meta_out, file.path(output_dir, "B_Q3a_document_metadata.csv"))
write_csv(vocabulary, file.path(output_dir, "B_Q3a_vocabulary.csv"))
write_csv(bind_rows(
  matrix_summary %>% mutate(value = as.character(value)),
  tibble(metric = c("Snapshot created (UTC)", "Handoff Q2/Q3 status",
                    "Q2(b) cross-check", "08 input MD5"),
         value = c(snapshot_created, q2q3_status, q2b_check,
                   unname(md5_before[f_docs])))),
  file.path(output_dir, "B_Q3a_matrix_summary.csv"))
write_csv(zero_feature_docs, file.path(output_dir,
                                       "B_Q3a_zero_feature_documents.csv"))
write_csv(bind_rows(validation,
                    thread_counts %>% transmute(
                      check = paste0("Documents in ", thread, " (",
                                     thread_label, ")"),
                      value = documents)),
          file.path(output_dir, "B_Q3a_validation_checks.csv"))
writeLines(c(paste("Snapshot created (UTC):", snapshot_created),
             paste("Handoff Q2/Q3 status:", q2q3_status),
             "Input MD5 checksums:",
             paste0("  ", names(md5_before), "  ", md5_before), "",
             capture.output(sessionInfo())),
           file.path(output_dir, "B_Q3a_session_info.txt"))

# ---- 12. Final checks: inputs unchanged, RDS reloads ---------------------------
stopifnot(identical(md5_before, tools::md5sum(input_files)))
reloaded <- readRDS(file.path(output_dir, "B_Q3a_tfidf_dtm.rds"))
stopifnot(identical(dim(reloaded$dtm_tfidf), dim(dtm_tfidf)),
          identical(rownames(reloaded$clustering_input),
                    doc_meta_out$reply_uri[doc_meta_out$eligible_for_clustering]))

cat("\nQ3a complete. TF-IDF DTM:", nrow(dtm_tfidf), "x", ncol(dtm_tfidf),
    "| eligible rows:", sum(elig), "| outputs in", normalizePath(output_dir), "\n")
