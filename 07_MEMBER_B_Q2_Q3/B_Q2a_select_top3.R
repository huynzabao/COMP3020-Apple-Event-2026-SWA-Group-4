
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "readr", "stringr", "tibble")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tibble)
})

# ---- 1. Paths (relative to the folder containing this script / the Rmd) ----
data_dir   <- "05_CLEAN_TEAM_HANDOFF"
output_dir <- "outputs_B"

f_topic   <- file.path(data_dir, "02_TOPIC_POSTS_FOR_Q1_Q2_Q4.csv")
f_top3    <- file.path(data_dir, "06_Q2_TOP3_POSTS_FROM_CURRENT_LABELS.csv")
f_replies <- file.path(data_dir, "07_Q2_REPLIES_ENGLISH_BY_THREAD.csv")
f_master  <- file.path(data_dir, "01_MASTER_ALL_CLEAN_POSTS_WITH_LABEL_SOURCES.csv")
f_status  <- file.path(data_dir, "00_READ_FIRST_HANDOFF_STATUS.txt")

required_files <- c(f_topic, f_top3, f_replies, f_master)
absent <- required_files[!file.exists(required_files)]
if (length(absent) > 0) {
  stop("Input file(s) not found (working directory: ", getwd(), "):\n  ",
       paste(absent, collapse = "\n  "))
}
dir.create(output_dir, showWarnings = FALSE)

# ---- 2. Read data -----------------------------------------------------------
# All columns are read as character so nothing is silently coerced; the three
# engagement counts are converted explicitly and checked below.
read_all_chr <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()),
           na = c("", "NA"), locale = locale(encoding = "UTF-8"),
           progress = FALSE)
}
count_cols <- c("reply_count", "like_count", "repost_count")
to_int <- function(df) mutate(df, across(all_of(count_cols), as.integer))

topic   <- read_all_chr(f_topic)  %>% to_int()
top3_h  <- read_all_chr(f_top3)   %>% to_int()
master  <- read_all_chr(f_master) %>% to_int()
replies <- read_all_chr(f_replies)

# ---- 3. Column checks (stop with a clear message if a column is missing) ----
check_cols <- function(df, cols, name) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0)
    stop(name, " is missing column(s): ", paste(miss, collapse = ", "))
}
post_cols <- c("uri", "post_url", "text_original", "author_handle", "period",
               "created_at", count_cols, "eligible_topic", "analysis_event_label",
               "auto_event_label", "auto_reason", "manual_event_label", "label_source")
check_cols(topic,   post_cols, "02_TOPIC")
check_cols(master,  post_cols, "01_MASTER")
check_cols(top3_h,  c("uri", "post_url", "text_original", "author_handle",
                      "period", count_cols), "06_Q2_TOP3")
check_cols(replies, c("root_uri", "reply_uri", "parent_uri", "reply_text",
                      "author_handle"), "07_Q2_REPLIES")

# ---- 4. Validation of the Q1 topic dataset ---------------------------------
validation <- tibble(
  check = c("Rows in topic dataset",
            "Before-period rows",
            "After-period rows",
            "Duplicate URIs",
            "Missing URIs",
            "Missing / non-integer engagement counts",
            "Rows with eligible_topic != TRUE",
            "Rows with analysis_event_label != include",
            "Rows with a recorded human label (label_source == human)"),
  value = c(nrow(topic),
            sum(topic$period == "Before"),
            sum(topic$period == "After"),
            sum(duplicated(topic$uri)),
            sum(is.na(topic$uri)),
            sum(is.na(topic[count_cols])),
            sum(topic$eligible_topic != "TRUE"),
            sum(topic$analysis_event_label != "include"),
            sum(topic$label_source == "human"))
)
print(validation, n = Inf)
stopifnot(validation$value[4] == 0, validation$value[5] == 0, validation$value[6] == 0)

# Optional cross-check against the counts written in the handoff status file
if (file.exists(f_status)) {
  status_txt <- readLines(f_status, warn = FALSE)
  get_status <- function(label) {
    line <- grep(label, status_txt, fixed = TRUE, value = TRUE)
    if (length(line) == 0) return(NA_integer_)
    as.integer(str_extract(line[1], "[0-9]+$"))
  }
  status_check <- tibble(
    item     = c("Included posts", "Before", "After"),
    handoff  = c(get_status("Posts included under current rules + overrides:"),
                 get_status("Before included:"), get_status("After included:")),
    computed = validation$value[1:3]
  ) %>% mutate(match = !is.na(handoff) & handoff == computed)
  print(status_check)
  # "match" only if every count was found in the file and equals the data
  status_state <- if (all(status_check$match)) "match" else "mismatch"
  if (status_state == "mismatch")
    warning("Topic dataset counts differ from 00_READ_FIRST_HANDOFF_STATUS.txt")
} else {
  status_check <- NULL
  status_state <- "not_checked"   # absent file is NOT treated as a mismatch
  message("Status file not found; handoff count cross-check skipped.")
}
cat("Handoff status-file check:", status_state, "\n")

# ---- 5. Rank by reply count; check for ties at the rank-3 boundary ---------
ranked <- topic %>%
  arrange(desc(reply_count), desc(like_count), uri) %>%   # deterministic order
  mutate(rank = row_number())

r_counts        <- ranked$reply_count
tie_at_boundary <- sum(r_counts >= r_counts[3]) > 3
cat("\nReply counts ranks 1-5:", r_counts[1:5],
    "| tie at rank 3:", tie_at_boundary, "\n")

top3 <- ranked %>% slice_head(n = 3) %>% mutate(thread = paste0("T", rank))

# ---- 6. Cross-check against the handoff top-3 file -------------------------
compare_cols <- c("post_url", "text_original", "author_handle", "period", count_cols)
joined <- top3 %>% select(uri, all_of(compare_cols)) %>%
  inner_join(top3_h %>% select(uri, all_of(compare_cols)),
             by = "uri", suffix = c("_topic", "_handoff"))
field_check <- vapply(compare_cols, function(v)
  nrow(joined) == 3 &&
    all(joined[[paste0(v, "_topic")]] == joined[[paste0(v, "_handoff")]]),
  logical(1))
same_set     <- setequal(top3$uri, top3_h$uri)
same_order   <- identical(top3$uri, top3_h$uri)
handoff_match <- same_set && same_order && all(field_check)
cat("Handoff cross-check - same URIs:", same_set, "| same order:", same_order,
    "| all fields equal:", all(field_check), "\n")
if (!handoff_match) warning("Top-3 selection differs from the handoff file.")

# ---- 7. Relevance-label status of the selected posts -----------------------
# Distinguishes the label recorded in the data from any later inspection.
relevance_status <- top3 %>%
  transmute(thread, author_handle, label_source, auto_event_label, auto_reason,
            manual_event_label = coalesce(manual_event_label, "(none recorded)"),
            analysis_event_label)
print(relevance_status)

# ---- 8. Retrieved replies per selected thread (from the 07 file) -----------
# parent_uri is blank for replies made directly to the root post.
thread_replies <- top3 %>%
  select(thread, uri, reply_count) %>%
  left_join(replies %>%
              group_by(root_uri) %>%
              summarise(retrieved_replies = n(),
                        direct_replies    = sum(is.na(parent_uri) |
                                                  parent_uri == root_uri),
                        nested_replies    = retrieved_replies - direct_replies,
                        replies_by_root_author = NA_integer_,
                        .groups = "drop"),
            by = c("uri" = "root_uri"))
# replies written by the author of the root post itself (self-replies)
for (i in seq_len(nrow(thread_replies))) {
  root_author <- top3$author_handle[i]
  thread_replies$replies_by_root_author[i] <-
    sum(replies$root_uri == top3$uri[i] & replies$author_handle == root_author)
}
print(thread_replies)

# ---- 9. Verify quoted reply evidence ---------------------------------------
# Every quotation used in the report must be found verbatim in reply_text.
quotes <- tibble(
  thread = "T1",
  quote  = "They're talking about the fruit. It's a joke thread."
)
quote_check <- quotes %>%
  rowwise() %>%
  mutate(
    root     = top3$uri[top3$thread == thread],
    hits     = list(replies %>% filter(root_uri == root, reply_text == quote)),
    n_found  = nrow(hits),
    reply_uri  = if (n_found == 1) hits$reply_uri else NA_character_,
    parent_uri = if (n_found == 1) hits$parent_uri else NA_character_,
    parent_text = if (n_found == 1)
      replies$reply_text[match(hits$parent_uri, replies$reply_uri)] else NA_character_
  ) %>%
  ungroup() %>%
  select(thread, quote, n_found, reply_uri, parent_uri, parent_text)
print(quote_check)
stopifnot(all(quote_check$n_found == 1))

# Evidence for the report: a follow-up post by the root author inside T1
t1_self_replies <- replies %>%
  filter(root_uri == top3$uri[1], author_handle == top3$author_handle[1])
t1_self_honeycrisp <- any(str_detect(t1_self_replies$reply_text, "Honeycrisp"))
stopifnot(nrow(t1_self_replies) >= 1, t1_self_honeycrisp)

# ---- 10. Sensitivity: high-reply posts outside the topic dataset -----------
review_candidates <- master %>%
  filter(!uri %in% topic$uri, reply_count >= r_counts[3]) %>%
  arrange(desc(reply_count)) %>%
  mutate(inside_selected_thread =
           uri %in% replies$reply_uri[replies$root_uri %in% top3$uri])

# Content inspection by Owner B (13 Nov 2026). PROVISIONAL: these notes are
# NOT recorded human labels and do not change the dataset. Joined on the full
# post URI so that every note is matched to exactly one post.
uri_prefix <- "at://did:plc:"
inspection <- tribble(
  ~uri, ~content_assessment,
  paste0(uri_prefix, "z6rujpf4u56jfie7aqic2nfg/app.bsky.feed.post/3muowzoxxwk2n"),
  "Product reference only (pre-event foldable iPhone pricing)",
  paste0(uri_prefix, "z72i7hdynmk6r22z27h6tvur/app.bsky.feed.post/3mv3vulr54c2n"),
  "Self-reply by bsky.app inside thread T1; not a separate thread",
  paste0(uri_prefix, "gnocmkzivy7kvwi4gsajc3em/app.bsky.feed.post/3mv52mvg7pc2m"),
  "Product reference only (joke about the iPhone Duo folding)",
  paste0(uri_prefix, "6fnzney5yhgjcbw664awibcm/app.bsky.feed.post/3mv45zcmxqk2h"),
  "Explicit reference to the Duo announcement",
  paste0(uri_prefix, "6uqqv7asv2xsxwn224tyexng/app.bsky.feed.post/3mv447v4sbk2j"),
  "Explicit reference to the Apple event",
  paste0(uri_prefix, "htw4iaeofjq3pyx72txceiyz/app.bsky.feed.post/3mv6qdmkhz22i"),
  "Mentions the Apple event; remainder of text unclear",
  paste0(uri_prefix, "az4g7422dhv6n55gkm7syfqp/app.bsky.feed.post/3mv464hjm5k2y"),
  "No reference to Apple, the event or a product in the text"
)
stopifnot(!any(duplicated(inspection$uri)))
review_candidates <- review_candidates %>%
  left_join(inspection, by = "uri") %>%
  mutate(content_assessment = coalesce(content_assessment, "Not yet inspected"))
# Every candidate must have exactly one note, and every note must be used.
stopifnot(nrow(review_candidates) == nrow(inspection),
          all(review_candidates$content_assessment != "Not yet inspected"),
          setequal(review_candidates$uri, inspection$uri))
print(review_candidates %>%
        select(author_handle, reply_count, analysis_event_label,
               inside_selected_thread, content_assessment))

# Scenario: which top-3 would result if candidate posts were added to the
# topic dataset? (Self-replies inside a selected thread are excluded because
# they are part of that thread, not separate threads.)
scenario_top3 <- function(extra_uris) {
  bind_rows(topic %>% select(uri, author_handle, reply_count, like_count),
            master %>% filter(uri %in% extra_uris) %>%
              select(uri, author_handle, reply_count, like_count)) %>%
    arrange(desc(reply_count), desc(like_count), uri) %>%
    slice_head(n = 3) %>%
    transmute(label = paste0(author_handle, " (", reply_count, ")")) %>%
    pull(label) %>% paste(collapse = "; ")
}
explicit_uris <- review_candidates %>%
  filter(str_starts(content_assessment, "Explicit")) %>% pull(uri)
product_uris  <- review_candidates %>%
  filter(str_starts(content_assessment, "Explicit|Product")) %>% pull(uri)
scenarios <- tibble(
  scenario = c("Current labels",
               "Add posts with an explicit event/announcement reference",
               "Also add posts referring only to the product"),
  top3 = c(scenario_top3(character(0)), scenario_top3(explicit_uris),
           scenario_top3(product_uris))
)
print(scenarios)

# ---- 11. Exports ------------------------------------------------------------
q2a_table <- top3 %>%
  left_join(thread_replies %>% select(uri, retrieved_replies), by = "uri") %>%
  transmute(Thread = thread,
            Author = author_handle,
            Period = period,
            `Created (UTC)` = str_replace(str_remove(created_at, "Z$"), "T", " "),
            Replies = reply_count,
            Likes = like_count,
            Reposts = repost_count,
            `Label source` = label_source,
            `Retrieved English replies` = retrieved_replies,
            Text = str_squish(text_original),
            URL = post_url,
            root_uri = uri)

write_csv(q2a_table, file.path(output_dir, "B_Q2a_top3_posts.csv"))
write_csv(review_candidates %>%
            select(author_handle, period, created_at, reply_count, like_count,
                   repost_count, analysis_event_label, auto_reason, label_source,
                   inside_selected_thread, content_assessment, text_original,
                   post_url, uri),
          file.path(output_dir, "B_Q2a_review_candidates.csv"))
# Re-read the export and confirm it reproduces the in-memory result
check_export <- read_csv(file.path(output_dir, "B_Q2a_top3_posts.csv"),
                         col_types = cols(.default = col_character()))
stopifnot(identical(check_export$root_uri, q2a_table$root_uri),
          identical(as.integer(check_export$Replies), q2a_table$Replies))

writeLines(capture.output(sessionInfo()),
           file.path(output_dir, "B_Q2a_session_info.txt"))

cat("\nQ2a complete. Outputs written to:", normalizePath(output_dir), "\n")
