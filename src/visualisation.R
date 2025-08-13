# ============================================
# Visualisations (no side effects)
# Functions to make/save the plots used in your Rmd
# ============================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr)
})

.save_fig <- function(plot, filename, width = 8, height = 5, dir_out = "figures") {
  if (!dir.exists(dir_out)) dir.create(dir_out, showWarnings = FALSE)
  ggplot2::ggsave(file.path(dir_out, filename), plot = plot, width = width, height = height, dpi = 300)
}

# ---- Stage III papers by cluster (box/violin) ----
plot_stageIII_by_cluster_box <- function(df_long_all, title = "Stage III Grades by Cluster") {
  p <- ggplot(df_long_all, aes(x = as.factor(Cluster), y = Grade, fill = as.factor(Cluster))) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~StageIII_Paper, scales = "free") +
    labs(title = title, x = "Cluster", y = "Standardised Grade") +
    theme_minimal()
  .save_fig(p, "stageIII_by_cluster_box.png"); p
}

plot_stageIII_by_cluster_violin <- function(df_long_all, title = "Stage III Grade Distributions by Cluster") {
  p <- ggplot(df_long_all, aes(x = as.factor(Cluster), y = Grade, fill = as.factor(Cluster))) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
    facet_wrap(~StageIII_Paper, scales = "free") +
    labs(title = title, x = "Cluster", y = "Standardised Grade") +
    theme_minimal()
  .save_fig(p, "stageIII_by_cluster_violin.png"); p
}

# ---- COVID-period visuals (density/box) ----
plot_density_by_period <- function(df_long_std_by_paper, title = "Density of Standardised Grades: COVID vs Post-COVID") {
  p <- ggplot(df_long_std_by_paper, aes(x = Grade_z_paper, fill = COVID_period)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ Paper, scales = "free") +
    labs(title = title, x = "Standardised Grade (by Paper)", fill = "Period") +
    theme_minimal()
  .save_fig(p, "density_by_period.png"); p
}

plot_box_by_period <- function(df_long_std_by_paper) {
  p <- ggplot(df_long_std_by_paper, aes(x = COVID_period, y = Grade_z_paper, fill = COVID_period)) +
    geom_boxplot() +
    facet_wrap(~ Paper, scales = "free") +
    labs(title = "Standardised Grade Distributions: COVID vs Post-COVID",
         y = "Standardised Grade (by Paper)") +
    theme_minimal()
  .save_fig(p, "box_by_period.png"); p
}

# ---- GPA visuals ----
plot_gpa_box <- function(df_gpa_combined) {
  p <- ggplot(df_gpa_combined, aes(x = paste(GPA_type, COVID_period, sep = "\n"), y = GPA, fill = GPA_type)) +
    geom_boxplot() +
    labs(title = "Boxplot of Student GPA by Type and COVID Period",
         x = "GPA Type & Period", y = "Standardised GPA (by Paper)") +
    theme_minimal()
  .save_fig(p, "gpa_box.png"); p
}

plot_boot_hist <- function(boot_vec, title = "Bootstrap distribution of mean difference", xlab = "Difference") {
  df <- data.frame(x = boot_vec)
  p <- ggplot(df, aes(x)) + geom_histogram(bins = 50) +
    labs(title = title, x = xlab, y = "Count") + theme_minimal()
  .save_fig(p, "bootstrap_hist.png"); p
}
