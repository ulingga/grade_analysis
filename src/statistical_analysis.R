# ============================================
# Statistical Analysis (no side effects)
# Classical tests, clustering, bootstrap, GPA helpers, Bayesian wrappers
# ============================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stats); library(purrr)
})

# ---- Clustering helpers (hierarchical with Gower, like your Rmd) ----
compute_gower <- function(df_binary_noid) {
  if (!requireNamespace("cluster", quietly = TRUE)) stop("Package 'cluster' required.")
  cluster::daisy(df_binary_noid, metric = "gower")
}

hclust_ward <- function(distance_matrix, k = 7) {
  hc <- hclust(distance_matrix, method = "ward.D2")
  cutree(hc, k = k)
}

cluster_summary_means <- function(df_binary_with_cluster) {
  df_binary_with_cluster |>
    dplyr::group_by(Cluster) |>
    dplyr::summarise(across(where(is.numeric), mean), .groups = "drop") |>
    dplyr::arrange(Cluster)
}

papers_selected_by_cluster <- function(cluster_summary_tbl, threshold = 0.60) {
  cluster_summary_tbl |>
    tidyr::pivot_longer(-Cluster, names_to = "Paper", values_to = "MeanSelection") |>
    dplyr::filter(MeanSelection >= threshold) |>
    dplyr::group_by(Cluster) |>
    dplyr::summarise(Selected_Papers = paste(Paper, collapse = ", "), .groups = "drop")
}

# ---- Build undergrad cluster data frames (5/6/7) after standardisation ----
split_undergrad_by_cluster <- function(df_ug, df_binary_with_cluster) {
  df_ug |>
    dplyr::left_join(dplyr::select(df_binary_with_cluster, Student, Cluster), by = "Student") |>
    (\(x) list(
      cluster5 = dplyr::filter(x, Cluster == 5),
      cluster6 = dplyr::filter(x, Cluster == 6),
      cluster7 = dplyr::filter(x, Cluster == 7)
    ))()
}

# ---- Classical tests (your Levene, ANOVA, KW, pairwise) ----
levene_by <- function(df, value_col, group_col) {
  if (!requireNamespace("car", quietly = TRUE)) stop("Package 'car' required.")
  car::leveneTest(reformulate(group_col, value_col), data = df)
}
anova_welch   <- function(df, value_col, group_col) oneway.test(reformulate(group_col, value_col), data = df, var.equal = FALSE)
anova_classic <- function(df, value_col, group_col) aov(reformulate(group_col, value_col), data = df)
kw_test       <- function(df, value_col, group_col) kruskal.test(reformulate(group_col, value_col), data = df)
pairwise_t_bonf   <- function(df, value_col, group_col) pairwise.t.test(df[[value_col]], df[[group_col]], p.adjust.method = "bonferroni")
pairwise_wil_bonf <- function(df, value_col, group_col) pairwise.wilcox.test(df[[value_col]], df[[group_col]], p.adjust.method = "bonferroni")

# ---- COVID period flag & GPA tables (matches your Rmd logic) ----
add_covid_flag <- function(df_long) {
  df_long |>
    dplyr::mutate(COVID_period = ifelse(Year %in% c(2020, 2021, 2022), "During COVID", "Post-COVID"))
}

gpa_by_student <- function(df_long_std_by_paper_with_period) {
  df_long_std_by_paper_with_period |>
    dplyr::group_by(Student, Year, COVID_period) |>
    dplyr::summarise(GPA = mean(Grade_z_paper, na.rm = TRUE), .groups = "drop")
}

# ---- Bootstrap differences (mean/median) like your Rmd ----
bootstrap_diff <- function(x, g, n_boot = 10000, stat = c("mean","median"), seed = 2024) {
  set.seed(seed)
  stat <- match.arg(stat)
  g <- factor(g)
  if (nlevels(g) != 2) stop("bootstrap_diff expects 2 groups.")
  x1 <- x[g == levels(g)[1]]
  x2 <- x[g == levels(g)[2]]
  boot <- replicate(n_boot, {
    s1 <- sample(x1, replace = TRUE)
    s2 <- sample(x2, replace = TRUE)
    if (stat == "mean") mean(s2, na.rm = TRUE) - mean(s1, na.rm = TRUE)
    else               median(s2, na.rm = TRUE) - median(s1, na.rm = TRUE)
  })
  ci <- stats::quantile(boot, c(0.025, 0.975), na.rm = TRUE)
  list(obs = if (stat == "mean") mean(x2, na.rm = TRUE) - mean(x1, na.rm = TRUE)
               else               median(x2, na.rm = TRUE) - median(x1, na.rm = TRUE),
       boot = boot, ci = ci, p_one_sided = mean(boot >= 0))
}

# Paper-wise bootstrap with real-scale CI using per-paper SD lookup
bootstrap_paper_diff <- function(df_long_std_by_paper, stats_lookup, n_boot = 10000, exclude_papers = NULL) {
  if (!is.null(exclude_papers)) df_long_std_by_paper <- dplyr::filter(df_long_std_by_paper, !Paper %in% exclude_papers)
  papers <- unique(df_long_std_by_paper$Paper)
  purrr::map_dfr(papers, function(p) {
    sub <- dplyr::filter(df_long_std_by_paper, Paper == p)
    if (length(unique(sub$COVID_period)) < 2) {
      return(tibble::tibble(Paper = p, Mean_Diff_Std = NA_real_, `95% CI (Std)` = NA_character_,
                            Mean_Diff_Real = NA_real_, `95% CI (Real)` = NA_character_))
    }
    paper_sd <- dplyr::filter(stats_lookup, Paper == p) |> dplyr::pull(sd_grade)
    b <- bootstrap_diff(sub$Grade_z_paper, sub$COVID_period, n_boot = n_boot, stat = "mean")
    ci_std  <- b$ci
    ci_real <- ci_std * paper_sd
    tibble::tibble(
      Paper = p,
      Mean_Diff_Std = b$obs,
      `95% CI (Std)` = sprintf("[%.3f, %.3f]", ci_std[1], ci_std[2]),
      Mean_Diff_Real = b$obs * paper_sd,
      `95% CI (Real)` = sprintf("[%.2f, %.2f]", ci_real[1], ci_real[2])
    )
  })
}

# ---- Regression blocks used in your Rmd (R² table) ----
r2_table_stageIII <- function(df_stageIII) {
  df_stageIII$Cluster <- as.factor(df_stageIII$Cluster)
  m1 <- lm(StageIII_mean ~ MEDSCI_142, data = df_stageIII)
  m2 <- lm(StageIII_mean ~ StageII_mean, data = df_stageIII)
  m3 <- lm(StageIII_mean ~ MEDSCI_142 + StageII_mean, data = df_stageIII)
  m4 <- lm(StageIII_mean ~ MEDSCI_142 + StageII_mean + Cluster, data = df_stageIII)
  m5 <- lm(StageIII_mean ~ (MEDSCI_142 + StageII_mean) * Cluster, data = df_stageIII)
  data.frame(
    Model  = c("MEDSCI_142","StageII_mean","Combined","Combined + Cluster","Interaction"),
    R2     = c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared, summary(m4)$r.squared, summary(m5)$r.squared),
    Adj_R2 = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$adj.r.squared, summary(m5)$adj.r.squared)
  )
}

# ---- Optional Bayesian wrappers (only run if brms installed) ----
fit_bayes_covid_mean <- function(df_gpa, family = c("gaussian","student")) {
  family <- match.arg(family)
  if (!requireNamespace("brms", quietly = TRUE)) stop("Package 'brms' required.")
  priors <- c(
    brms::set_prior("normal(0, 1)", class = "b"),
    brms::set_prior("student_t(3, 0, 1)", class = "sigma")
  )
  fam <- if (family == "gaussian") brms::gaussian() else brms::student()
  brms::brm(GPA ~ COVID_period, data = df_gpa, family = fam,
            prior = priors, sample_prior = "yes",
            iter = 4000, warmup = 2000, chains = 4, seed = 123)
}

fit_bayes_per_paper <- function(df_long_std_by_paper, iter = 1000, warmup = 500, chains = 2) {
  if (!requireNamespace("brms", quietly = TRUE)) stop("Package 'brms' required.")
  priors <- c(
    brms::set_prior("normal(0, 1)", class = "b"),
    brms::set_prior("student_t(3, 0, 1)", class = "sigma")
  )
  papers <- unique(df_long_std_by_paper$Paper)
  stats_lookup <- df_long_std_by_paper |>
    dplyr::group_by(Paper) |> dplyr::summarise(sd_grade = sd(Grade, na.rm = TRUE), .groups = "drop")

  purrr::map_dfr(papers, function(paper_id) {
    df_paper <- df_long_std_by_paper |>
      dplyr::filter(Paper == paper_id, !is.na(Grade_z_paper), !is.na(COVID_period))
    if (length(unique(df_paper$COVID_period)) < 2) return(NULL)
    fit <- brms::brm(
      Grade_z_paper ~ COVID_period, data = df_paper, family = brms::student(),
      prior = priors, sample_prior = "yes", iter = iter, warmup = warmup, chains = chains, seed = 123, silent = TRUE
    )
    post <- as.numeric(brms::posterior_samples(fit, pars = grep("COVID_period.*Post", colnames(brms::posterior_samples(fit)), value = TRUE))[[1]])
    pri  <- as.numeric(brms::prior_samples(fit,     pars = grep("COVID_period.*Post", colnames(brms::prior_samples(fit)), value = TRUE))[[1]])
    sd_g <- stats_lookup$sd_grade[stats_lookup$Paper == paper_id]
    tibble::tibble(
      Paper = paper_id,
      N = nrow(df_paper),
      COVID_effect_z = mean(post),
      COVID_2.5 = quantile(post, .025),
      COVID_97.5 = quantile(post, .975),
      SD_grade = sd_g,
      COVID_effect_grade = mean(post) * sd_g,
      COVID_grade_2.5 = quantile(post, .025) * sd_g,
      COVID_grade_97.5 = quantile(post, .975) * sd_g,
      Prior_samples = list(pri),
      Posterior_samples = list(post)
    )
  })
}
