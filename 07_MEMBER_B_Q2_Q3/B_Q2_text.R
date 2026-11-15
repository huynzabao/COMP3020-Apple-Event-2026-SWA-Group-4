
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "readr", "stringr", "tibble", "tidytext", "ggplot2")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tibble)
  library(tidytext); library(ggplot2)
})

# ---- 1. Paths -------------------------------------------------------------
data_dir   <- "05_CLEAN_TEAM_HANDOFF"
output_dir <- "outputs_B"

f_replies  <- file.path(data_dir, "07_Q2_REPLIES_ENGLISH_BY_THREAD.csv")
f_top3     <- file.path(data_dir, "06_Q2_TOP3_POSTS_FROM_CURRENT_LABELS.csv")
f_q3docs   <- file.path(data_dir, "08_Q3_UNIQUE_REPLY_DOCUMENTS.csv")
f_status   <- file.path(data_dir, "00_READ_FIRST_HANDOFF_STATUS.txt")
# Optional pilot retrieval log. Agreed location: 04_PILOT_Q1_Q4 next to
# 05_CLEAN_TEAM_HANDOFF. A copy inside data_dir is accepted as a fallback.
# If neither exists, the retrieval-log counts are reported as NA.
pilot_dir  <- "04_PILOT_Q1_Q4"
coverage_candidates <- c(file.path(pilot_dir, "PILOT_Q2_thread_coverage.csv"),
                         file.path(data_dir, "PILOT_Q2_thread_coverage.csv"))
f_coverage <- coverage_candidates[file.exists(coverage_candidates)][1]
cat("Pilot coverage file used:", ifelse(is.na(f_coverage), "none", f_coverage), "\n")

required_files <- c(f_replies, f_top3, f_q3docs)
absent <- required_files[!file.exists(required_files)]
if (length(absent) > 0) {
  stop("Input file(s) not found (working directory: ", getwd(), "):\n  ",
       paste(absent, collapse = "\n  "))
}
dir.create(output_dir, showWarnings = FALSE)

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

replies <- read_all_chr(f_replies)
top3    <- read_all_chr(f_top3)
q3docs  <- read_all_chr(f_q3docs)

check_cols(replies, c("root_uri", "reply_uri", "parent_uri", "reply_text",
                      "declared_langs", "author_handle", "reply_nonblank",
                      "english_declared_check"), "07_Q2_REPLIES")
check_cols(top3, c("uri", "author_handle", "reply_count"), "06_Q2_TOP3")
check_cols(q3docs, c("reply_uri", "root_uri", "reply_text", "thread_label"),
           "08_Q3_DOCS")

# Snapshot record: creation time from the status file (if present) and an
# MD5 checksum of the analysed reply file.
snapshot_created <- if (file.exists(f_status)) {
  str_remove(grep("Created at UTC:", readLines(f_status, warn = FALSE),
                  value = TRUE, fixed = TRUE)[1], fixed("Created at UTC: "))
} else NA_character_
replies_md5 <- unname(tools::md5sum(f_replies))

# ---- 2. Thread labels and validation ----------------------------------------
# T1-T3 follow the Q2a order (06 file is ordered by reply_count, descending).
stopifnot(nrow(top3) == 3, !any(duplicated(top3$uri)))
threads <- top3 %>%
  transmute(thread = paste0("T", row_number()), root_uri = uri,
            root_author = author_handle,
            api_reply_count = as.integer(reply_count))

replies <- replies %>%
  mutate(thread = threads$thread[match(root_uri, threads$root_uri)])

# The 08 file labels threads "Thread 1".."Thread 3"; check they map to T1-T3.
q3_map <- q3docs %>%
  distinct(root_uri, thread_label) %>%
  mutate(thread = threads$thread[match(root_uri, threads$root_uri)])

validation <- tibble(
  check = c("Reply rows in 07",
            "Rows whose root_uri is not one of the three Q2a roots",
            "Distinct root_uri values",
            "Missing reply_uri",
            "Duplicate reply_uri within a thread",
            "Duplicate reply_uri across threads",
            "Blank reply_text",
            "Rows with reply_nonblank != TRUE",
            "Rows with english_declared_check != TRUE",
            "Rows with declared_langs other than 'en' only",
            "07 reply_uri set equals 08 reply_uri set",
            "07 reply_text equals 08 reply_text (all rows)",
            "08 thread_label consistent with T1-T3"),
  value = c(nrow(replies),
            sum(is.na(replies$thread)),
            n_distinct(replies$root_uri),
            sum(is.na(replies$reply_uri)),
            sum(duplicated(replies[c("root_uri", "reply_uri")])),
            sum(duplicated(replies$reply_uri)),
            sum(is.na(replies$reply_text) | str_squish(replies$reply_text) == ""),
            sum(replies$reply_nonblank != "TRUE"),
            sum(replies$english_declared_check != "TRUE"),
            sum(replies$declared_langs != "en"),
            as.integer(setequal(replies$reply_uri, q3docs$reply_uri)),
            as.integer(all(replies$reply_text ==
                             q3docs$reply_text[match(replies$reply_uri,
                                                     q3docs$reply_uri)])),
            as.integer(all(q3_map$thread_label ==
                             paste("Thread", str_remove(q3_map$thread, "T")))))
)
print(validation, n = Inf)
stopifnot(validation$value[2] == 0, validation$value[3] == 3,
          validation$value[4] == 0, validation$value[5] == 0,
          validation$value[7] == 0, validation$value[11] == 1,
          validation$value[12] == 1, validation$value[13] == 1)

# ---- 3. Preprocessing ---------------------------------------------------------
# Applied to the original reply_text. The original text is kept unchanged in
# the data so every token remains traceable to its reply and thread.
normalise_text <- function(x) {
  x <- str_replace_all(x, "[\u2018\u2019\u02BC]", "'")   # curly -> straight '
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

# Stopwords: Snowball English list (tidytext::stop_words, lexicon "snowball")
# plus a short documented custom list.
#   A: modal auxiliaries absent from Snowball, although Snowball already
#      contains "could", "would" and "should".
#   B: high-frequency adverbs and intensifiers with no topical content.
# Product and brand words (apple, iphone, duo, ...) are NOT removed, and no
# stemming is applied, so "apple" (brand) and "apples" (fruit) stay distinct.
snowball_sw <- stop_words %>% filter(lexicon == "snowball") %>% pull(word)
custom_sw <- tibble(
  word  = c("can", "will", "might", "must", "may", "shall",
            "just", "now", "much", "also", "even", "actually", "really", "still"),
  group = c(rep("A: modal auxiliary", 6), rep("B: adverb/intensifier", 8))
)
stopifnot(!any(custom_sw$word %in% snowball_sw))
stopword_list <- unique(c(snowball_sw, custom_sw$word))

replies <- replies %>% mutate(text_clean = normalise_text(reply_text))

# Tokenisation: tidytext::unnest_tokens(token = "words") uses the Unicode word
# boundary rules of the tokenizers package. It lowercases, drops punctuation
# and emoji, and keeps contractions ("don't") and numbers ("2000") as tokens.
tokens_raw <- replies %>%
  select(thread, reply_uri, text_clean) %>%
  unnest_tokens(word, text_clean, token = "words", to_lower = TRUE)

tokens <- tokens_raw %>%
  filter(!word %in% stopword_list) %>%
  mutate(word = str_remove(word, "'s$")) %>%       # possessive: apple's -> apple
  filter(!word %in% stopword_list,                 # e.g. "that's" -> "that"
         nchar(word) > 1)                          # single letters / digits

# ---- 4. Reply-level quality flags (flagged, NOT removed) -------------------
smart_sw <- stop_words %>% filter(lexicon == "SMART") %>% pull(word)
reply_stats <- replies %>%
  select(thread, reply_uri, author_handle, reply_text) %>%
  left_join(tokens_raw %>% group_by(reply_uri) %>%
              summarise(raw_tokens = n(),
                        english_function_share = mean(word %in% smart_sw),
                        .groups = "drop"), by = "reply_uri") %>%
  left_join(tokens %>% count(reply_uri, name = "retained_tokens"),
            by = "reply_uri") %>%
  mutate(raw_tokens = coalesce(raw_tokens, 0L),
         retained_tokens = coalesce(retained_tokens, 0L),
         english_function_share = coalesce(english_function_share, NA_real_),
         is_root_author = author_handle ==
           threads$root_author[match(thread, threads$thread)],
         short_reply = nchar(str_squish(reply_text)) < 15,
         zero_tokens = retained_tokens == 0,
         possible_non_english = raw_tokens >= 4 & english_function_share < 0.15) %>%
  add_count(thread, reply_text, name = "same_text_in_thread") %>%
  mutate(repeated_text = same_text_in_thread > 1)

reply_flags <- reply_stats %>%
  filter(zero_tokens | possible_non_english | repeated_text | is_root_author) %>%
  select(thread, reply_uri, author_handle, zero_tokens, possible_non_english,
         repeated_text, is_root_author, retained_tokens, reply_text) %>%
  arrange(thread, desc(possible_non_english), reply_uri)

# Why did a reply lose all its tokens? Classified from the original text.
zero_token_replies <- reply_stats %>%
  filter(zero_tokens) %>%
  left_join(replies %>% select(reply_uri, text_clean), by = "reply_uri") %>%
  mutate(reason = case_when(
    !str_detect(reply_text, "[[:alnum:]]") ~ "Emoji or symbols only",
    text_clean == "" ~ "URL only",
    str_detect(reply_text, "@[[:alnum:]]") ~ "Mention plus stopwords",
    TRUE ~ "Stopwords or single characters only")) %>%
  select(thread, reply_uri, reason, raw_tokens, reply_text) %>%
  arrange(thread, reason, reply_uri)
print(count(zero_token_replies, thread, reason))

# ---- 5. Word frequency distributions ----------------------------------------
# Frequency = total occurrences of the word in the thread (not the number of
# replies). n_replies and max_in_one_reply show whether a single reply drives
# a count. Ties: n desc, then n_replies desc, then alphabetical.
word_freq <- tokens %>%
  count(thread, reply_uri, word, name = "n_in_reply") %>%
  group_by(thread, word) %>%
  summarise(n = sum(n_in_reply), n_replies = n(),
            max_in_one_reply = max(n_in_reply), .groups = "drop") %>%
  group_by(thread) %>%
  mutate(total_tokens = sum(n),
         per_1000_tokens = round(1000 * n / total_tokens, 1)) %>%
  arrange(thread, desc(n), desc(n_replies), word) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top15 <- word_freq %>%
  filter(rank <= 15) %>%
  group_by(thread) %>%
  mutate(cutoff_n = n[rank == max(rank)]) %>%
  ungroup()

# How many words share the 15th-ranked frequency but fall outside the top 15?
tie_info <- word_freq %>%
  left_join(top15 %>% distinct(thread, cutoff_n), by = "thread") %>%
  group_by(thread) %>%
  summarise(cutoff_n = first(cutoff_n),
            tied_in_top15 = sum(n == cutoff_n & rank <= 15),
            tied_outside_top15 = sum(n == cutoff_n & rank > 15),
            .groups = "drop")

# Stopword sensitivity: top 15 with the Snowball list only, compared with the
# final list (Snowball + custom). Same tokenisation and tie-break rules.
top15_from <- function(tok) {
  tok %>% count(thread, reply_uri, word, name = "k") %>%
    group_by(thread, word) %>%
    summarise(n = sum(k), n_replies = n(), .groups = "drop") %>%
    arrange(thread, desc(n), desc(n_replies), word) %>%
    group_by(thread) %>% mutate(rank = row_number()) %>%
    filter(rank <= 15) %>% ungroup()
}
tokens_snowball_only <- tokens_raw %>%
  filter(!word %in% snowball_sw) %>%
  mutate(word = str_remove(word, "'s$")) %>%
  filter(!word %in% snowball_sw, nchar(word) > 1)
top15_snow <- top15_from(tokens_snowball_only)
stopword_sensitivity <- lapply(threads$thread, function(th) {
  a <- top15_snow$word[top15_snow$thread == th]
  b <- top15$word[top15$thread == th]
  tibble(thread = th,
         words_in_both = length(intersect(a, b)),
         only_snowball_list = paste(setdiff(a, b), collapse = ", "),
         only_final_list = paste(setdiff(b, a), collapse = ", "),
         custom_tokens_removed = sum(tokens_raw$thread == th &
                                       tokens_raw$word %in% custom_sw$word),
         top15_snowball_only = paste0(a, " (", top15_snow$n[top15_snow$thread == th],
                                      ")", collapse = "; "))
}) %>% bind_rows()
print(stopword_sensitivity %>% select(-top15_snowball_only))

# ---- 6. Summary table ---------------------------------------------------------
retrieved <- if (!is.na(f_coverage)) {
  read_all_chr(f_coverage) %>%
    transmute(root_uri,
              retrieved_replies_log = as.integer(retrieved_nested_replies))
} else tibble(root_uri = threads$root_uri, retrieved_replies_log = NA_integer_)

tokenisation_summary <- threads %>%
  left_join(retrieved, by = "root_uri") %>%
  left_join(reply_stats %>% group_by(thread) %>%
              summarise(replies_analysed = n(),
                        replies_zero_tokens = sum(zero_tokens),
                        replies_short_lt15_chars = sum(short_reply),
                        replies_by_root_author = sum(is_root_author),
                        replies_possible_non_english = sum(possible_non_english),
                        raw_tokens = sum(raw_tokens),
                        retained_tokens = sum(retained_tokens),
                        .groups = "drop"), by = "thread") %>%
  left_join(word_freq %>% count(thread, name = "distinct_words"), by = "thread") %>%
  left_join(tie_info, by = "thread") %>%
  mutate(coverage_source = coalesce(f_coverage, "not available"),
         snapshot_created_utc = snapshot_created,
         replies_file_md5 = replies_md5) %>%
  select(-root_author)
print(as.data.frame(tokenisation_summary))

# ---- 7. Verification ---------------------------------------------------------
stopifnot(
  nrow(tokenisation_summary) == 3,
  sum(tokenisation_summary$replies_analysed) == nrow(replies),
  all(table(top15$thread) == pmin(15, table(word_freq$thread))),
  !any(duplicated(top15[c("thread", "rank")])),
  !any(duplicated(top15[c("thread", "word")]))
)
# Recount the top-15 frequencies directly from the token table
recount <- tokens %>% count(thread, word, name = "n_check")
stopifnot(all(top15$n == recount$n_check[match(paste(top15$thread, top15$word),
                                               paste(recount$thread, recount$word))]))

# ---- 8. Figures ----------------------------------------------------------------
# One horizontal bar chart per thread with identical styling. Each chart has
# its own x-axis scale because thread sizes differ greatly; counts are printed
# on the bars, and the subtitle reports the number of replies and tokens.
make_chart <- function(th) {
  s    <- tokenisation_summary %>% filter(thread == th)
  root <- threads %>% filter(thread == th)
  # Bars whose frequency equals the cut-off value while other words with the
  # same value fall outside the top 15 are shaded lighter: their selection is
  # determined by the tie-break rule, not by frequency.
  d <- top15 %>% filter(thread == th) %>%
    mutate(status = factor(
      if_else(n == s$cutoff_n & s$tied_outside_top15 > 0, "tied", "above"),
      levels = c("above", "tied"),
      labels = c("Above cut-off", "Tied at cut-off (selected by tie-break)")))
  tie_note <- if (s$tied_outside_top15 > 0) {
    paste0(s$tied_outside_top15, " further word(s) also occur ", s$cutoff_n,
           " time(s) but are not shown. Tied words are ordered by number of ",
           "replies, then alphabetically.")
  } else "No ties at the 15th-ranked frequency."
  ggplot(d, aes(x = n, y = reorder(word, -rank), fill = status)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), hjust = -0.25, size = 3.4) +
    scale_fill_manual(values = setNames(c("#3B6E8F", "#A9C4D6"), levels(d$status)),
                      drop = FALSE, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = paste0(th, ": top 15 words in replies to ", root$root_author),
         subtitle = paste0(s$replies_analysed, " replies analysed (",
                           s$replies_zero_tokens, " with no retained tokens); ",
                           format(s$retained_tokens, big.mark = ","),
                           " tokens after preprocessing"),
         x = "Frequency (number of occurrences)", y = NULL,
         caption = str_wrap(tie_note, 100)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, colour = "grey30"),
          plot.title.position = "plot", plot.caption.position = "plot")
}

for (i in 1:3) {
  th <- paste0("T", i)
  p <- make_chart(th)
  # Check the plotted values equal the exported table values
  built <- ggplot_build(p)$data[[1]]
  stopifnot(setequal(built$x, top15$n[top15$thread == th]),
            nrow(built) == sum(top15$thread == th))
  ggsave(file.path(output_dir, paste0("B_Q2_fig_thread", i, ".png")), p,
         width = 7, height = 4.8, dpi = 300, bg = "white")
}

# ---- 9. Exports ------------------------------------------------------------------
write_csv(top15 %>% select(thread, rank, word, n, n_replies, max_in_one_reply,
                           per_1000_tokens),
          file.path(output_dir, "B_Q2_top15_by_thread.csv"))
write_csv(word_freq, file.path(output_dir, "B_Q2_word_freq_full.csv"))
write_csv(tokens, file.path(output_dir, "B_Q2_tokens.csv"))
write_csv(tokenisation_summary,
          file.path(output_dir, "B_Q2_tokenisation_summary.csv"))
write_csv(reply_flags, file.path(output_dir, "B_Q2_reply_flags.csv"))
write_csv(zero_token_replies,
          file.path(output_dir, "B_Q2_zero_token_replies.csv"))
write_csv(stopword_sensitivity,
          file.path(output_dir, "B_Q2_stopword_sensitivity.csv"))
writeLines(capture.output(sessionInfo()),
           file.path(output_dir, "B_Q2_session_info.txt"))

cat("\nQ2b complete. Outputs written to:", normalizePath(output_dir), "\n")
