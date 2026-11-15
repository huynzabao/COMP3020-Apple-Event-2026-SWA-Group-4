
# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "readr", "tibble", "Matrix", "ggplot2")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(Matrix)
  library(ggplot2)
})

# ---- 1. Locate inputs -----------------------------------------------------------
# Agreed layout: outputs_B/Q3a/, Q3b/, Q3c/. Copies placed directly in the
# project folder are also accepted; the files actually used are reported.
find_input <- function(file, dirs, required = TRUE) {
  cand <- file.path(dirs, file)
  hit <- cand[file.exists(cand)]
  if (length(hit) == 0) {
    if (!required) return(NA_character_)
    stop(file, " not found. Looked in:\n  ", paste(cand, collapse = "\n  "),
         "\nWorking directory: ", getwd())
  }
  hit[1]
}
dir_of <- function(stage) c(file.path("outputs_B", stage), ".")
f_model <- find_input("B_Q3c_verified_model.rds", dir_of("Q3c"))
f_clus  <- find_input("B_Q3_clusters.csv", dir_of("Q3c"))
f_alldoc <- find_input("B_Q3c_all_documents.csv", dir_of("Q3c"))
f_q3a   <- find_input("B_Q3a_tfidf_dtm.rds", dir_of("Q3a"))
f_q3b   <- find_input("B_Q3b_selected_settings.rds", dir_of("Q3b"))
f_top3  <- find_input("06_Q2_TOP3_POSTS_FROM_CURRENT_LABELS.csv",
                      c("05_CLEAN_TEAM_HANDOFF", "."), required = FALSE)
output_dir <- file.path("outputs_B", "Q3d")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

input_files <- c(f_model, f_clus, f_alldoc, f_q3a, f_q3b,
                 if (!is.na(f_top3)) f_top3)
cat("Inputs used:\n", paste0("  ", input_files, "\n"))
md5_before <- tools::md5sum(input_files)

mod  <- readRDS(f_model)
q3a  <- readRDS(f_q3a)
st   <- readRDS(f_q3b)
clus_csv <- read_csv(f_clus, show_col_types = FALSE)
all_csv  <- read_csv(f_alldoc, show_col_types = FALSE)

write_out <- function(df, path) {          # plain decimals in CSV files
  as_txt <- function(x) vapply(x, function(v) if (is.na(v)) NA_character_ else
    format(v, digits = 15, scientific = FALSE), character(1))
  write_csv(mutate(df, across(where(is.double), as_txt)), path)
}

# ---- 2. Verify the final analytical snapshot (stop on any failure) ------------
X    <- as.matrix(mod$X_l2)                   # 211 x 883, L2-normalised
Xa   <- q3a$clustering_input
meta_all <- q3a$doc_metadata
elig_uri <- meta_all$reply_uri[meta_all$eligible_for_clustering]
cl   <- mod$cluster                           # named by reply_uri

input_checks <- tibble::tribble(
  ~check, ~pass,
  "1. Matrix is 211 x 883 and equals the Q3b representation",
    identical(dim(X), c(211L, 883L)) &&
      isTRUE(all.equal(X, as.matrix(st$X_l2), tolerance = 0)),
  "1b. Matrix equals L2-normalised Q3a clustering input (tol 1e-12)",
    isTRUE(all.equal(X, as.matrix(Xa) / sqrt(rowSums(as.matrix(Xa)^2)),
                     tolerance = 1e-12, check.attributes = FALSE)),
  "2. Row names equal the Q3a eligible reply_uri (same order)",
    identical(rownames(X), rownames(Xa)) && identical(rownames(X), elig_uri),
  "3. Vocabulary equals Q3a (883 terms, same order, 'mu.social' one term)",
    identical(colnames(X), q3a$vocabulary$term) && ncol(X) == 883 &&
      "mu.social" %in% colnames(X),
  "4. reply_uri values are unique",
    !any(duplicated(rownames(X))) && !any(duplicated(clus_csv$reply_uri)),
  "5. Every document has an original thread label (3 labels)",
    all(!is.na(mod$assignments$thread_label)) &&
      n_distinct(mod$assignments$thread_label) == 3,
  "6. Cluster assignments are named by reply_uri in matrix order",
    identical(names(cl), rownames(X)),
  "7. Number of clusters is 3 (k stored in Q3b and Q3c)",
    mod$k == 5 && st$k == 5 && n_distinct(cl) == 5,
  "8. Q3c assignments equal the Q3b reference model",
    identical(unname(as.integer(cl)), unname(as.integer(st$reference_cluster))) &&
      identical(names(st$reference_cluster), names(cl)),
  "8b. Settings: seed 16, nstart 50, iter.max 100, Hartigan-Wong",
    mod$settings$seed == 16 && mod$settings$nstart == 50 &&
      mod$settings$iter.max == 100 &&
      identical(mod$settings$algorithm, "Hartigan-Wong"),
  "8c. Cluster sizes are 193, 12, 6 (clusters 1, 2, 3)",
    identical(as.integer(table(cl)), c(193L, 12L, 6L)),
  "9. 229 documents in Q3a; 18 zero-feature documents excluded",
    nrow(meta_all) == 229 && sum(!meta_all$eligible_for_clustering) == 18 &&
      !any(rownames(X) %in%
             meta_all$reply_uri[!meta_all$eligible_for_clustering]),
  "10. Q3c CSV assignments equal the model (aligned by reply_uri)",
    nrow(clus_csv) == 211 &&
      identical(as.integer(clus_csv$cluster[match(names(cl), clus_csv$reply_uri)]),
                unname(as.integer(cl))),
  "10b. Q3c audit table: 229 rows, cluster missing for the 18 excluded",
    nrow(all_csv) == 229 && sum(is.na(all_csv$cluster)) == 18 &&
      all(is.na(all_csv$cluster[!all_csv$eligible_for_clustering]))
)
print(input_checks, n = Inf)
if (!all(input_checks$pass)) {
  stop("Input check(s) failed:\n  ",
       paste(input_checks$check[!input_checks$pass], collapse = "\n  "))
}

# ---- 3. Principal component analysis ------------------------------------------
# PCA of the same 211 x 883 L2-normalised TF-IDF matrix used for clustering.
# center = TRUE: PCA describes variation around the mean document.
# scale. = FALSE: features are not rescaled to unit variance, which would
# inflate the many terms that occur in a single reply.
pca <- prcomp(X, center = TRUE, scale. = FALSE)

# The sign of each component is arbitrary (it can differ between linear
# algebra libraries). Fix it so that the largest absolute loading of each
# component is positive; this makes the plot identical on every machine.
flip <- sign(apply(pca$rotation, 2, function(v) v[which.max(abs(v))]))
pca$rotation <- sweep(pca$rotation, 2, flip, `*`)
pca$x        <- sweep(pca$x, 2, flip, `*`)

eig <- pca$sdev^2
Xc  <- scale(X, center = TRUE, scale = FALSE)
pca_checks <- tibble::tribble(
  ~check, ~pass,
  "PCA computed on all 211 documents and 883 features",
    nrow(pca$x) == 211 && nrow(pca$rotation) == 883,
  "Scores equal centred data times loadings (tol 1e-10)",
    isTRUE(all.equal(unname(pca$x[, 1:2]), unname(Xc %*% pca$rotation[, 1:2]),
                     tolerance = 1e-10)),
  "Loadings of PC1 and PC2 are orthonormal",
    isTRUE(all.equal(unname(crossprod(pca$rotation[, 1:2])), diag(2))),
  "Eigenvalues sum to total variance of the matrix",
    isTRUE(all.equal(sum(eig), sum(apply(X, 2, var)))),
  "Eigenvalues are non-increasing", all(diff(eig) <= 1e-12),
  "PC1 and PC2 scores are finite", all(is.finite(pca$x[, 1:2]))
)
stopifnot(all(pca_checks$pass))

pca_variance <- tibble(component = paste0("PC", seq_along(eig)),
                       eigenvalue = eig,
                       variance_pct = 100 * eig / sum(eig),
                       cumulative_pct = cumsum(100 * eig / sum(eig)))
pv <- pca_variance$variance_pct
cat(sprintf("PC1 %.2f%%, PC2 %.2f%%, combined %.2f%%\n", pv[1], pv[2],
            pv[1] + pv[2]))

# ---- 4. Plotting dataset (one row per clustered reply) --------------------------
thread_names <- if (!is.na(f_top3)) {
  t3 <- read_csv(f_top3, show_col_types = FALSE)
  tibble(root_uri = t3$uri, root_author = t3$author_handle)
} else tibble(root_uri = character(), root_author = character())

sizes <- table(cl)
plot_data <- mod$assignments %>%
  select(cluster_input_row, q3a_row_index, reply_uri, root_uri, thread_label,
         thread, cluster, silhouette) %>%
  mutate(PC1 = unname(pca$x[match(reply_uri, rownames(pca$x)), 1]),
         PC2 = unname(pca$x[match(reply_uri, rownames(pca$x)), 2]),
         cluster_size = as.integer(sizes[as.character(cluster)]),
         eligible_for_clustering = TRUE) %>%
  left_join(thread_names, by = "root_uri") %>%
  mutate(thread_legend = if_else(is.na(root_author), thread_label,
                                 paste0(thread_label, " (", root_author, ")")),
         cluster_legend = paste0("Cluster ", cluster, " (n = ", cluster_size, ")"))

thread_counts_q3a <- table(meta_all$thread_label[meta_all$eligible_for_clustering])
plot_checks <- tibble::tribble(
  ~check, ~pass,
  "1. Exactly 211 plotted documents", nrow(plot_data) == 211,
  "2. 211 unique reply_uri", n_distinct(plot_data$reply_uri) == 211,
  "3. No missing PCA coordinates", !anyNA(plot_data[c("PC1", "PC2")]),
  "4. All PC1 and PC2 values finite",
    all(is.finite(plot_data$PC1)) && all(is.finite(plot_data$PC2)),
  "5. Exactly three cluster labels", n_distinct(plot_data$cluster) == 3,
  "6. Exactly three thread labels, each with its root author",
    n_distinct(plot_data$thread_label) == 3 &&
      n_distinct(plot_data$thread_legend) == 3,
  "7. Cluster sizes match Q3c",
    identical(as.integer(table(plot_data$cluster)),
              as.integer(mod$cluster_summary$documents)),
  "8. Thread counts match the eligible Q3a population",
    identical(as.integer(table(plot_data$thread_label)),
              as.integer(thread_counts_q3a[sort(names(thread_counts_q3a))])),
  "9. Rows align with the PCA input (reply_uri order)",
    identical(plot_data$reply_uri, rownames(pca$x)),
  "10. No zero-feature document plotted",
    !any(plot_data$reply_uri %in%
           meta_all$reply_uri[!meta_all$eligible_for_clustering]),
  "11. Plotted clusters equal B_Q3_clusters.csv",
    identical(as.integer(plot_data$cluster),
              as.integer(clus_csv$cluster[match(plot_data$reply_uri,
                                                clus_csv$reply_uri)]))
)
print(plot_checks, n = Inf)
stopifnot(all(plot_checks$pass))

# ---- 5. Projection diagnostics ------------------------------------------------
# How faithfully does the 2-D map represent the 883-D distances?
D_high <- dist(X); D_2d <- dist(plot_data[c("PC1", "PC2")])
coord_key <- paste(round(plot_data$PC1, 6), round(plot_data$PC2, 6))
centroids_2d <- plot_data %>% group_by(cluster, cluster_legend) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2), n = n(), .groups = "drop")
diagnostics <- tibble(
  item = c("Correlation of 883-D and 2-D pairwise distances",
           "Replies sharing identical 2-D coordinates (6 dp)",
           "Distinct coordinate positions occupied by those replies",
           "Cluster 1: SD of PC1 / PC2",
           "Cluster 2: SD of PC1 / PC2",
           "Cluster 3: SD of PC1 / PC2"),
  value = c(sprintf("%.3f", cor(D_high, D_2d)),
            sum(duplicated(coord_key) | duplicated(coord_key, fromLast = TRUE)),
            n_distinct(coord_key[duplicated(coord_key) |
                                   duplicated(coord_key, fromLast = TRUE)]),
            vapply(1:3, function(c) with(plot_data[plot_data$cluster == c, ],
              sprintf("%.3f / %.3f", sd(PC1), sd(PC2))), character(1))))
print(diagnostics)

# ---- 6. Figure -----------------------------------------------------------------
# Colours: validated categorical palette (CVD-separable for all pairs).
# Shapes: filled circle, triangle and square. Larger clusters are drawn first
# so that the small clusters are not hidden; no jitter is applied.
cluster_cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a"),
                         unique(plot_data$cluster_legend[order(plot_data$cluster)]))
thread_shapes <- setNames(c(16, 17, 15), sort(unique(plot_data$thread_legend)))
draw_order <- order(-plot_data$cluster_size, plot_data$cluster_input_row)
pd <- plot_data[draw_order, ]

# Direct cluster labels in dark ink, placed in empty areas near each cluster
# (positions chosen for this projection). The check below stops the script
# if a label anchor lies close to any plotted reply.
lab_pos <- centroids_2d %>%
  mutate(lx = c(-0.20, 0.14, -0.29), ly = c(-0.14, 0.62, 0.32), hj = 0.5)
lab_clear <- vapply(seq_len(nrow(lab_pos)), function(i)
  min(sqrt((plot_data$PC1 - lab_pos$lx[i])^2 + (plot_data$PC2 - lab_pos$ly[i])^2)),
  numeric(1))
stopifnot(all(lab_clear > 0.05))

base_plot <- function(d) {
  ggplot(d, aes(x = PC1, y = PC2, colour = cluster_legend,
                shape = thread_legend)) +
    geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
    geom_point(size = 2.4, alpha = 0.75, stroke = 0.4) +
    scale_colour_manual(values = cluster_cols, name = "K-means cluster") +
    scale_shape_manual(values = thread_shapes, name = "Original thread") +
    labs(x = sprintf("PC1 (%.1f%% variance explained)", pv[1]),
         y = sprintf("PC2 (%.1f%% variance explained)", pv[2])) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "right", legend.box = "vertical",
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot") +
    guides(colour = guide_legend(order = 1, override.aes = list(alpha = 1,
                                                                 shape = 16)),
           shape = guide_legend(order = 2, override.aes = list(colour =
                                                                  "grey25")))
}

p <- base_plot(pd) +
  geom_text(data = lab_pos, aes(x = lx, y = ly, label = cluster_legend,
                                hjust = hj),
            inherit.aes = FALSE, size = 3.2, colour = "grey15") +
  coord_equal() +
  labs(title = "K-means clusters of replies in the first two principal components",
       subtitle = sprintf(paste0("%d replies; colour = cluster (k = 5), shape = ",
                                 "original thread; PC1 + PC2 = %.1f%% of variance"),
                          nrow(pd), pv[1] + pv[2]))
f_png <- file.path(output_dir, "B_Q3_plot2D.png")
ggsave(f_png, p, width = 8.5, height = 6, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "B_Q3_plot2D.pdf"), p, width = 8.5, height = 6,
       bg = "white")

# Supplementary view of the dense region around the origin (same data and
# mappings; axis limits only)
zoom_lim <- c(-0.15, 0.15)
p_zoom <- base_plot(pd) +
  coord_equal(xlim = zoom_lim, ylim = zoom_lim) +
  labs(title = "Supplementary view: dense region around the origin",
       subtitle = sprintf("%d of %d replies lie within this window",
                          sum(abs(pd$PC1) <= 0.15 & abs(pd$PC2) <= 0.15),
                          nrow(pd)))
ggsave(file.path(output_dir, "B_Q3d_plot2D_zoom.png"), p_zoom, width = 8.5,
       height = 6, dpi = 300, bg = "white")

# Check that the figure shows exactly the verified data
built <- ggplot_build(p)
pts <- built$data[[which(vapply(built$plot$layers, function(l)
  inherits(l$geom, "GeomPoint"), logical(1)))]]
fig_check <- tibble::tribble(
  ~check, ~pass,
  "Figure: 211 points drawn", nrow(pts) == 211,
  "Figure: coordinates equal the plotting dataset",
    isTRUE(all.equal(pts$x, pd$PC1)) && isTRUE(all.equal(pts$y, pd$PC2)),
  "Figure: point colour follows the cluster of each reply",
    identical(unname(pts$colour), unname(cluster_cols[pd$cluster_legend])),
  "Figure: point shape follows the thread of each reply",
    identical(as.numeric(pts$shape), unname(thread_shapes[pd$thread_legend]))
)
stopifnot(all(fig_check$pass))

# ---- 7. Exports -----------------------------------------------------------------
write_out(plot_data %>% select(cluster_input_row, q3a_row_index, reply_uri,
                               root_uri, thread_label, thread, root_author,
                               cluster, cluster_size, silhouette,
                               eligible_for_clustering, PC1, PC2),
          file.path(output_dir, "B_Q3d_pca_coordinates.csv"))
write_out(pca_variance %>% filter(row_number() <= 10 |
                                    row_number() == n()),
          file.path(output_dir, "B_Q3d_pca_variance.csv"))
write_out(bind_rows(
  input_checks %>% mutate(section = "Input"),
  pca_checks %>% mutate(section = "PCA"),
  plot_checks %>% mutate(section = "Plotting data"),
  fig_check %>% mutate(section = "Figure"),
  diagnostics %>% transmute(section = "Projection diagnostics", check = item,
                            pass = NA, value)) %>%
  select(section, check, pass, value),
  file.path(output_dir, "B_Q3d_validation_checks.csv"))
writeLines(c("Inputs used:", paste0("  ", names(md5_before), "  ", md5_before),
             "", capture.output(sessionInfo())),
           file.path(output_dir, "B_Q3d_session_info.txt"))

# ---- 8. Final checks -------------------------------------------------------------
stopifnot(identical(md5_before, tools::md5sum(input_files)))  # inputs untouched
chk <- read_csv(file.path(output_dir, "B_Q3d_pca_coordinates.csv"),
                show_col_types = FALSE)
stopifnot(identical(chk$reply_uri, plot_data$reply_uri),
          isTRUE(all.equal(chk$PC1, plot_data$PC1, tolerance = 1e-12)))
cat("\nQ3d complete:", nrow(plot_data), "replies plotted | outputs in",
    normalizePath(output_dir), "\n")
