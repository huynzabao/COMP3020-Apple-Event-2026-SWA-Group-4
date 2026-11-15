# =============================================================================
# COMP3020 | Owner B | Diagnostic: why does my Q3(a) vocabulary differ?
#
# Compares the local Q3(a) vocabulary (outputs_B/Q3a/B_Q3a_vocabulary.csv)
# with the reference run (REFERENCE_B_Q3a_vocabulary_884.csv: 884 terms,
# R 4.3.3, tidytext 0.4.1, tokenizers 0.3.0, stringi 1.8.3, ICU 74.2).
# It changes no file. Run it from the Q3 project folder after Q3(a).
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidytext)
})
f_local <- file.path("outputs_B", "Q3a", "B_Q3a_vocabulary.csv")
f_ref   <- "REFERENCE_B_Q3a_vocabulary_884.csv"
f_docs  <- file.path("05_CLEAN_TEAM_HANDOFF", "08_Q3_UNIQUE_REPLY_DOCUMENTS.csv")
f_rds   <- file.path("outputs_B", "Q3a", "B_Q3a_tfidf_dtm.rds")
stopifnot(file.exists(f_local), file.exists(f_ref), file.exists(f_docs))

loc <- read_csv(f_local, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
ref <- read_csv(f_ref,   show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("== Environment ==\n")
cat(R.version.string, "| tidytext", as.character(packageVersion("tidytext")),
    "| tokenizers", as.character(packageVersion("tokenizers")),
    "| stringi", as.character(packageVersion("stringi")),
    "| ICU", stringi::stri_info()$ICU.version,
    "| Unicode", stringi::stri_info()$Unicode.version, "\n")
cat("Native encoding:", l10n_info()$`UTF-8`, "(UTF-8 locale?) |",
    Sys.getlocale("LC_CTYPE"), "\n")
cat("Snowball stopwords:", sum(stop_words$lexicon == "snowball"), "(reference: 174)\n")
cat("08 file MD5:", unname(tools::md5sum(f_docs)),
    "(reference: 389df96854f40af769a58ab20a8a8575)\n")
if (file.exists(f_rds)) cat("Local Q3a RDS MD5:", unname(tools::md5sum(f_rds)),
                            "(reference: ba1e5a0a2693f17fe5ffe652cd5a549c)\n")

cat("\n== Vocabulary ==\n")
cat("Local terms:", nrow(loc), "| reference terms:", nrow(ref), "\n")
only_ref   <- setdiff(ref$term, loc$term)
only_local <- setdiff(loc$term, ref$term)
cat("In reference only:", if (length(only_ref)) paste(only_ref, collapse = ", ") else "none", "\n")
cat("In local only    :", if (length(only_local)) paste(only_local, collapse = ", ") else "none", "\n")
both <- inner_join(loc, ref, by = "term", suffix = c("_local", "_ref"))
dfd <- both %>% filter(doc_freq_local != doc_freq_ref | total_count_local != total_count_ref)
cat("Shared terms with different counts:", nrow(dfd), "\n")
if (nrow(dfd)) print(as.data.frame(dfd %>% select(term, total_count_local,
                     total_count_ref, doc_freq_local, doc_freq_ref)))

# Show how the local machine tokenises the replies that contain each
# differing term (searched case-insensitively in the original text).
docs <- read_csv(f_docs, col_types = cols(.default = "c"),
                 locale = locale(encoding = "UTF-8"))
for (w in c(only_ref, only_local, dfd$term)) {
  hit <- docs %>% filter(str_detect(str_to_lower(reply_text),
                                    fixed(str_remove(w, "'.*$"))))
  cat("\n-- term:", w, "| replies containing it:", nrow(hit), "\n")
  for (t in head(hit$reply_text, 3)) {
    cat("   text  :", str_squish(t), "\n")
    cat("   tokens:", paste(unnest_tokens(tibble(x = t), w, x)$w, collapse = " | "), "\n")
  }
}
cat("\nPlease send the full console output of this script.\n")
