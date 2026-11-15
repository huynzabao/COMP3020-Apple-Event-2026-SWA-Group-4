
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "readr", "stringr", "tibble", "tidyr")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tibble); library(tidyr)
})

# ---- 1. Paths and inputs ------------------------------------------------------
data_dir   <- "05_CLEAN_TEAM_HANDOFF"
output_dir <- "outputs_B"
f_replies  <- file.path(data_dir, "07_Q2_REPLIES_ENGLISH_BY_THREAD.csv")
f_top3     <- file.path(data_dir, "06_Q2_TOP3_POSTS_FROM_CURRENT_LABELS.csv")
f_q3docs   <- file.path(data_dir, "08_Q3_UNIQUE_REPLY_DOCUMENTS.csv")
f_codes    <- "B_Q2c_reply_codes.csv"

required_files <- c(f_replies, f_top3, f_q3docs, f_codes)
absent <- required_files[!file.exists(required_files)]
if (length(absent) > 0) {
  stop("Input file(s) not found (working directory: ", getwd(), "):\n  ",
       paste(absent, collapse = "\n  "))
}

# Reuse the verified Q2(b) outputs; regenerate them if they are missing.
q2b_files <- file.path(output_dir, c("B_Q2_word_freq_full.csv",
                                     "B_Q2_tokenisation_summary.csv",
                                     "B_Q2_zero_token_replies.csv"))
if (!all(file.exists(q2b_files))) {
  if (!file.exists("B_Q2_text.R")) stop("Q2(b) outputs and B_Q2_text.R missing")
  message("Q2(b) outputs not found; running B_Q2_text.R")
  local(source("B_Q2_text.R", local = TRUE))
}
dir.create(output_dir, showWarnings = FALSE)

read_all_chr <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()),
           na = c("", "NA"), locale = locale(encoding = "UTF-8"),
           progress = FALSE)
}
replies   <- read_all_chr(f_replies)
top3      <- read_all_chr(f_top3)
q3docs    <- read_all_chr(f_q3docs)
codes     <- read_all_chr(f_codes)
word_freq <- read_csv(q2b_files[1], show_col_types = FALSE,
                      col_types = cols(word = col_character()))
q2b_summary <- read_csv(q2b_files[2], show_col_types = FALSE)
zero_tok    <- read_all_chr(q2b_files[3])

# ---- 2. Thread mapping and consistency checks --------------------------------
threads <- tibble(thread = paste0("T", 1:3), root_uri = top3$uri,
                  root_author = top3$author_handle)
replies <- replies %>%
  mutate(thread = threads$thread[match(root_uri, threads$root_uri)])

stopifnot(
  nrow(top3) == 3,
  !any(is.na(replies$thread)),
  !any(duplicated(replies$reply_uri)),
  setequal(replies$reply_uri, q3docs$reply_uri),
  # thread sizes agree with the Q2(b) summary
  all(as.integer(table(replies$thread)[q2b_summary$thread]) ==
        q2b_summary$replies_analysed)
)
cat("Replies per thread:", paste(names(table(replies$thread)),
                                 table(replies$thread), collapse = ", "), "\n")

# ---- 3. Validate the manual coding --------------------------------------------
code_levels <- c("fruit_wordplay", "apple_product_or_event",
                 "apple_company_criticism", "bluesky_platform",
                 "meaning_of_post", "short_reaction", "other_offtopic")
code_labels <- c("Apple-fruit wordplay or discussion",
                 "Apple products, prices or the event",
                 "Criticism of Apple as a company (politics, ethics)",
                 "Bluesky platform features or relevance",
                 "Confusion or clarification about the post",
                 "Short reaction or emoji only",
                 "Other or off-topic")
stopifnot(
  nrow(codes) == nrow(replies),                      # every reply coded ...
  setequal(codes$reply_uri, replies$reply_uri),
  !any(duplicated(codes$reply_uri)),                 # ... exactly once
  all(codes$primary_code %in% code_levels),
  all(codes$thread == replies$thread[match(codes$reply_uri, replies$reply_uri)])
)
coded <- replies %>%
  select(thread, reply_uri, parent_uri, author_handle, reply_text) %>%
  left_join(codes %>% select(reply_uri, primary_code), by = "reply_uri") %>%
  mutate(primary_code = factor(primary_code, levels = code_levels))

theme_summary <- coded %>%
  count(thread, primary_code, .drop = FALSE) %>%
  group_by(thread) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(theme = code_labels[match(primary_code, code_levels)])
print(theme_summary %>% select(thread, primary_code, n, pct) %>%
        pivot_wider(names_from = thread, values_from = c(n, pct)))

# ---- 4. Document-level keyword counts ----------------------------------------
# Number of replies (not word occurrences) matching each keyword group in the
# ORIGINAL reply text, case-insensitive. These are descriptive aids only.
alt <- function(...) paste(c(...), collapse = "|")   # build long regexes
keyword_groups <- tribble(
  ~group, ~pattern,
  "price", alt("\\$", "\\b(2000|1999|grand|price|overpriced)\\b",
               "\\b(cost|costs|afford|affordable)\\b"),
  "apple_varieties", alt("honey ?crisp", "cosmic crisp", "granny smith",
                         "pink lad", "\\bgala\\b", "mcintosh", "braeburn",
                         "breabun", "bramley", "gravenstein", "cortland",
                         "pitmaston", "reinette", "golden delicious",
                         "red delicious", "cox", "snapdragon", "macoun",
                         "ploughman"),
  "device_mockery", alt("tape", "protective sheet", "temu", "broken", "rotten",
                        "slapped two", "each hand", "two phones",
                        "let alone two"),
  "competitors_lateness", alt("samsung", "android", "fold 8", "\\bbehind\\b",
                              "late to", "7 years", "china"),
  "size_practicality", alt("huge", "small hands", "hardware", "mini ipad",
                           "smaller phones", "in search of a problem"),
  "politics_boycott", alt("trump", "maga", "\\bgop\\b", "djt", "regime",
                          "genocide", "boycott", "spy", "child labor",
                          "oligarchy", "lake ontario", "rename"),
  "bluesky_platform", alt("bluesky", "\\bfeed\\b", "\\bmute\\b",
                          "edit button", "ipad app", "at-proto", "anisota",
                          "blacksky", "mu\\.social", "\\btags\\b"),
  "laughing_emoji", "[\\U0001F602\\U0001F923]"
)
keyword_doc_counts <- keyword_groups %>%
  rowwise() %>%
  mutate(counts = list(coded %>%
                         mutate(hit = str_detect(str_to_lower(reply_text),
                                                 regex(pattern))) %>%
                         group_by(thread) %>%
                         summarise(replies = sum(hit), .groups = "drop"))) %>%
  ungroup() %>%
  select(group, counts) %>%
  unnest(counts) %>%
  pivot_wider(names_from = thread, values_from = replies, names_prefix = "replies_")
print(keyword_doc_counts)

# ---- 5. What does "apple" refer to in T1? ------------------------------------
apple_sense_T1 <- coded %>%
  filter(thread == "T1", str_detect(str_to_lower(reply_text), "apple")) %>%
  count(primary_code, .drop = FALSE, name = "replies_containing_apple") %>%
  mutate(theme = code_labels[match(primary_code, code_levels)])
print(apple_sense_T1)

# ---- 6. Repetition check: word vs document frequency (from Q2(b)) -----------
repetition_check <- word_freq %>%
  filter(rank <= 15) %>%
  mutate(single_reply_driven = n_replies == 1 & n > 1) %>%
  select(thread, rank, word, n, n_replies, max_in_one_reply, single_reply_driven)
cat("\nTop-15 words whose count comes from a single reply:\n")
print(repetition_check %>% filter(single_reply_driven))

# ---- 7. Quotation evidence (verified verbatim) -------------------------------
evidence <- tribble(
  ~thread, ~quote, ~illustrates,
  "T1", "The only apple product I buy is a fruit.",
  "T1 replies play on the company/fruit ambiguity, often while rejecting Apple",
  "T2", "Got mine on Temu.",
  "T2 replies mock the device shown as a cheap or makeshift product",
  "T3", paste("Isn't it cute how apple comes up with all these innovations",
              "7 years after Samsung?"),
  "T3 replies criticise the foldable as late relative to competitors"
)
evidence <- evidence %>%
  rowwise() %>%
  mutate(matches   = sum(coded$reply_text == quote & coded$thread == thread),
         reply_uri = if (matches == 1)
           coded$reply_uri[coded$reply_text == quote & coded$thread == thread]
           else NA_character_,
         primary_code = as.character(coded$primary_code[match(reply_uri,
                                                              coded$reply_uri)]),
         verified_verbatim = matches == 1) %>%
  ungroup() %>%
  select(thread, quote, reply_uri, primary_code, verified_verbatim, illustrates)
print(evidence %>% select(thread, verified_verbatim, reply_uri))
stopifnot(all(evidence$verified_verbatim))

# ---- 8. Exports ----------------------------------------------------------------
write_csv(theme_summary,      file.path(output_dir, "B_Q2c_theme_summary.csv"))
write_csv(keyword_doc_counts, file.path(output_dir, "B_Q2c_keyword_doc_counts.csv"))
write_csv(apple_sense_T1,     file.path(output_dir, "B_Q2c_apple_sense_T1.csv"))
write_csv(repetition_check,   file.path(output_dir, "B_Q2c_repetition_check.csv"))
write_csv(evidence,           file.path(output_dir, "B_Q2c_evidence.csv"))
cat("\nQ2c evidence checks complete. Outputs written to:",
    normalizePath(output_dir), "\n")
