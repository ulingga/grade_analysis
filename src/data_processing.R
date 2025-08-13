# ============================================
# Data Processing & ETL (no side effects)
# Functions extracted from your Rmd so you can reuse them.
# ============================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr); library(purrr); library(tidyselect)
})

# ---- Year ETL: build a wide table for a single year from per-paper CSVs ----
# Matches your per-year loop & MEDSCI_744 merge logic
process_grades_year <- function(year, base_path = "~/Downloads") {
  path  <- file.path(base_path, as.character(year))
  if (!dir.exists(path)) stop("Folder not found: ", path)
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) stop("No CSV files found at: ", path)

  grade_list <- list()

  # MEDSCI_744 special (S1/S2 → one column)
  medsci744_files <- files[grepl("MEDSCI_744", basename(files))]
  other_files     <- setdiff(files, medsci744_files)

  if (length(medsci744_files) > 0) {
    medsci744_list <- lapply(medsci744_files, function(file) {
      data <- readr::read_csv(file, show_col_types = FALSE)
      if (nrow(data) >= 2) data <- data[-c(1,2), , drop = FALSE]
      if ("Unposted Final Score" %in% names(data)) {
        data <- dplyr::select(data, Student, SIS_User_ID = `SIS User ID`, Final_Score = `Unposted Final Score`)
      } else if ("Final Score" %in% names(data)) {
        data <- dplyr::select(data, Student, SIS_User_ID = `SIS User ID`, Final_Score = `Final Score`)
      } else {
        warning("Unrecognised score column in: ", basename(file)); return(NULL)
      }
      dplyr::rename(data, MEDSCI_744 = Final_Score)
    })
    medsci744_list <- medsci744_list[!vapply(medsci744_list, is.null, TRUE)]

    if (length(medsci744_list) > 1) {
      med_combined <- Reduce(function(x,y) dplyr::full_join(x, y, by = c("SIS_User_ID","Student")), medsci744_list)
      medsci_744_cols <- grep("^MEDSCI_744", names(med_combined), value = TRUE)
      med_combined <- med_combined |>
        dplyr::mutate(MEDSCI_744 = dplyr::coalesce(!!!rlang::syms(medsci_744_cols))) |>
        dplyr::select(SIS_User_ID, Student, MEDSCI_744)
    } else {
      med_combined <- medsci744_list[[1]]
    }
    grade_list[["MEDSCI_744"]] <- med_combined
  }

  # All other papers
  for (file in other_files) {
    data <- readr::read_csv(file, show_col_types = FALSE)
    if (nrow(data) >= 2) data <- data[-c(1, 2), , drop = FALSE]
    if ("Unposted Final Score" %in% names(data)) {
      data <- dplyr::select(data, Student, SIS_User_ID = `SIS User ID`, Final_Score = `Unposted Final Score`)
    } else if ("Final Score" %in% names(data)) {
      data <- dplyr::select(data, Student, SIS_User_ID = `SIS User ID`, Final_Score = `Final Score`)
    } else {
      warning("Unrecognised score column in: ", basename(file)); next
    }
    paper_name <- stringr::str_extract(basename(file), "^[A-Z]+_\\d{3}")
    if (is.na(paper_name)) { warning("No paper code in file name: ", basename(file)); next }
    data <- dplyr::rename(data, !!paper_name := Final_Score)
    grade_list[[paper_name]] <- data
  }

  # Combine, drop test students, add Year
  combined <- Reduce(function(x,y) dplyr::full_join(x, y, by = c("SIS_User_ID","Student")), grade_list)
  combined <- combined |>
    dplyr::filter(!grepl("\\bTest\\b", Student, ignore.case = FALSE)) |>
    dplyr::mutate(Year = as.character(year)) |>
    dplyr::relocate(Year, .after = SIS_User_ID)

  # Return (in your Rmd you write_csv; here we only return)
  combined
}

# Wrapper to process multiple years (returns list of dfs)
process_years <- function(years = 2020:2024, base_path = "~/Downloads") {
  purrr::map(years, ~process_grades_year(.x, base_path = base_path))
}

# ---- Review helper (NA proportions, structure) ----
review_medscigrades <- function(csv_path) {
  df <- readr::read_csv(csv_path, show_col_types = FALSE,
                        col_types = cols(.default = col_double(),
                                         Student = col_character(),
                                         SIS_User_ID = col_character(),
                                         Year = col_character()))
  na_props <- df |> summarise(across(everything(), ~ mean(is.na(.))))
  list(data = df, na_props = na_props)
}

# ---- Combine year CSVs into 5-year wide table (matches your code) ----
combine_five_years <- function(file_paths, years = 2020:2024) {
  stopifnot(length(file_paths) == length(years))
  df <- file_paths |>
    lapply(function(file) {
      readr::read_csv(file, show_col_types = FALSE,
                      col_types = cols(.default = col_double(),
                                       Student = col_character(),
                                       SIS_User_ID = col_character()))
    }) |>
    purrr::map2(years, ~ dplyr::mutate(.x, Year = as.character(.y))) |>
    dplyr::bind_rows() |>
    dplyr::select(Student, SIS_User_ID, Year, dplyr::everything())
  df
}

# ---- Anonymisation (same logic as your Rmd) ----
anonymise_students <- function(df) {
  df |>
    dplyr::mutate(
      Anon_ID = dplyr::dense_rank(SIS_User_ID),
      Student = paste0("Student_", Anon_ID),
      SIS_User_ID = as.character(Anon_ID)
    ) |>
    dplyr::select(-Anon_ID) |>
    dplyr::mutate(across(c(Year, `SIS_User_ID`, Student), as.character))
}

# ---- Long/wide transforms & standardisation (you use these repeatedly) ----
to_long_all <- function(df) {
  paper_cols <- grep("^(MEDSCI_|PHARMCOL_|PHYSIOL_)", names(df), value = TRUE)
  df |>
    tidyr::pivot_longer(cols = all_of(paper_cols), names_to = "Paper", values_to = "Grade") |>
    dplyr::filter(!is.na(Grade))
}

standardise_by_year <- function(df_long) {
  df_long |>
    dplyr::group_by(Year) |>
    dplyr::mutate(Grade_z_year = (Grade - mean(Grade, na.rm = TRUE)) / sd(Grade, na.rm = TRUE)) |>
    dplyr::ungroup()
}

standardise_by_paper <- function(df_long) {
  df_long |>
    dplyr::group_by(Paper) |>
    dplyr::mutate(Grade_z_paper = (Grade - mean(Grade, na.rm = TRUE)) / sd(Grade, na.rm = TRUE)) |>
    dplyr::ungroup()
}

keep_latest_attempt <- function(df_long) {
  df_long |>
    dplyr::group_by(Student, Paper) |>
    dplyr::filter(Year == max(Year)) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
}

long_to_wide <- function(df_long, value_col = "Grade_z_year") {
  df_long |>
    dplyr::select(Student, Paper, !!rlang::sym(value_col)) |>
    tidyr::pivot_wider(names_from = Paper, values_from = !!rlang::sym(value_col))
}

# ---- Binary “took paper” matrix for clustering (your cohort step) ----
make_binary_matrix <- function(df_anon) {
  # Drop Year, SIS_User_ID, and postgraduate papers (as in your Rmd)
  pg_cols <- c("MEDSCI_700","MEDSCI_703","MEDSCI_704","MEDSCI_705","MEDSCI_706","MEDSCI_707",
               "MEDSCI_708","MEDSCI_709","MEDSCI_710","MEDSCI_712","MEDSCI_713","MEDSCI_714",
               "MEDSCI_715","MEDSCI_716","MEDSCI_717","MEDSCI_718","MEDSCI_719","MEDSCI_720",
               "MEDSCI_721","MEDSCI_722","MEDSCI_723","MEDSCI_727","MEDSCI_729","MEDSCI_730",
               "MEDSCI_731","MEDSCI_732","MEDSCI_734","MEDSCI_735","MEDSCI_737","MEDSCI_738",
               "MEDSCI_739","MEDSCI_741","MEDSCI_742","MEDSCI_743","MEDSCI_744","MEDSCI_745",
               "MEDSCI_760")
  df_anon |>
    dplyr::select(-Year, -any_of(pg_cols), -SIS_User_ID) |>
    dplyr::group_by(Student) |>
    dplyr::summarise(across(where(is.numeric), ~ as.numeric(any(!is.na(.)))), .groups = "drop")
}
