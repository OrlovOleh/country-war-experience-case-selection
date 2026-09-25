# ============================================================
# HIERARCHICAL CLUSTER ANALYSIS OF STATE WAR EXPERIENCE
# DIRECT 25-VARIABLE MODEL
# Final comparison run
# ============================================================
#
# Expected project layout:
#   new_scripts/
#     war_participation_data.xlsx
#     HCA_direct_25var.R
#     results/
#
# Outputs are written to:
#   results/direct_25var/
#
# Model specification:
#   - primary-party countries only
#   - fixed 25-variable indicator set
#   - no redundancy reduction
#   - no nonlinear transformation
#   - variable-wise median imputation
#   - z-standardization
#   - equal variable contribution
#   - Euclidean distance
#   - Ward.D2 linkage
#   - k = 2,...,10
#   - top 3 solutions ranked by mean silhouette
#   - B = 1000 cluster bootstrap for each top-3 solution
#
# Main outputs:
#   - cluster-level silhouette, Jaccard, dissolution, recovery
#   - standardized centroids
#   - Ukraine cluster membership
#   - global top-10 nearest neighbours of Ukraine in the 25D space
#
# Military expenditure must already be parsed as numeric in the
# master workbook. Preferred source:
#   mil_expenditure_pct_gdp_num
# Numeric mil_expenditure_pct_gdp is accepted as fallback.
# ============================================================

cat("\nSCRIPT VERSION: HCA_direct_25var — 2026-09-12\n")

# ============================================================
# 0. PACKAGES AND CONFIGURATION
# ============================================================

packages <- c(
  "readxl",
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "cluster",
  "fpc",
  "writexl"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install required packages before running this script: ",
      paste(missing_packages, collapse = ", ")
    )
  )
}

invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)

K_GRID <- 2:10
N_TOP_SOLUTIONS <- 3L
BOOT_B <- 1000L
BOOT_SEED <- 20260912L
BOOT_DISSOLUTION_THRESHOLD <- 0.50
BOOT_RECOVERY_THRESHOLD <- 0.75

input_candidates <- c(
  "war_participation_data.xlsx",
  file.path("new_scripts", "war_participation_data.xlsx")
)

existing_inputs <- input_candidates[file.exists(input_candidates)]

if (length(existing_inputs) == 0) {
  stop(
    paste0(
      "Input workbook not found. Expected: ",
      paste(input_candidates, collapse = " or ")
    )
  )
}

INPUT_FILE <- existing_inputs[1]
INPUT_SHEET <- "country_profile"
BASE_DIR <- dirname(normalizePath(INPUT_FILE, mustWork = TRUE))
OUT_DIR <- file.path(BASE_DIR, "results", "direct_25var")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("Input file:", normalizePath(INPUT_FILE), "\n")
cat("Output directory:", normalizePath(OUT_DIR, mustWork = FALSE), "\n")

# ============================================================
# 1. FIXED 25-VARIABLE SPECIFICATION
# ============================================================

vars_25 <- c(
  "peak_mil_personnel_k",
  "peak_mil_population_k",
  "peak_mil_personnel_pct_pop",
  "mil_personnel_latest_k",
  "mil_personnel_latest_population_k",
  "mil_personnel_pct_pop_latest",
  "battle_deaths_cow",
  "conflicts_primary",
  "years_primary",
  "years_war_level",
  "years_minor_level",
  "longest_spell",
  "years_since_last_primary",
  "years_interstate_alt",
  "years_intl_intrastate_ucdp",
  "years_intrastate",
  "years_extrasystemic",
  "years_home_defensive",
  "years_home_civil",
  "years_abroad",
  "defensive_purity",
  "mil_expenditure_pct_gdp",
  "conflicts_support",
  "years_support",
  "years_war_level_support"
)

stopifnot(length(vars_25) == 25L)

# ============================================================
# 2. IMPORT MASTER DATA
# ============================================================

dat <- readxl::read_excel(
  INPUT_FILE,
  sheet = INPUT_SHEET,
  na = c("", "NA", "N/A", "n/a")
)

required_meta <- c("country", "role_class")
missing_meta <- setdiff(required_meta, names(dat))

if (length(missing_meta) > 0) {
  stop(
    paste0(
      "Missing required metadata columns: ",
      paste(missing_meta, collapse = ", ")
    )
  )
}

non_exp_vars <- setdiff(vars_25, "mil_expenditure_pct_gdp")
missing_vars <- setdiff(non_exp_vars, names(dat))

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "Missing required 25-variable model columns: ",
      paste(missing_vars, collapse = ", ")
    )
  )
}

expense_candidates <- c(
  "mil_expenditure_pct_gdp_num",
  "mil_expenditure_pct_gdp"
)

expense_hits <- expense_candidates[expense_candidates %in% names(dat)]

if (length(expense_hits) == 0) {
  stop(
    paste0(
      "No military-expenditure column found. Expected numeric ",
      "'mil_expenditure_pct_gdp_num' (preferred) or numeric ",
      "'mil_expenditure_pct_gdp'."
    )
  )
}

numeric_expense_hits <- expense_hits[
  vapply(dat[expense_hits], is.numeric, logical(1))
]

if (length(numeric_expense_hits) == 0) {
  stop(
    paste0(
      "Military expenditure is not numeric. Add the already parsed ",
      "numeric expenditure column to the master workbook."
    )
  )
}

MIL_EXP_SOURCE <- numeric_expense_hits[1]

if (anyDuplicated(dat$country)) {
  stop("country_profile contains duplicated country identifiers.")
}

cat("Dataset dimensions:", nrow(dat), "rows x", ncol(dat), "columns\n")
cat("Military-expenditure source:", MIL_EXP_SOURCE, "\n")

# ============================================================
# 3. RETAIN PRIMARY-PARTY COUNTRIES ONLY
# ============================================================

dat_primary <- dat %>%
  dplyr::filter(role_class == "primary party")

if (!all(dat_primary$role_class == "primary party")) {
  stop("Primary-party filter failed.")
}

if (anyDuplicated(dat_primary$country)) {
  stop("Primary-party dataset contains duplicated countries.")
}

if (!("Ukraine" %in% dat_primary$country)) {
  stop("Ukraine is absent after the primary-party filter.")
}

cat("Primary-party countries:", nrow(dat_primary), "\n")

if (nrow(dat_primary) != 121L) {
  warning(
    paste0(
      "Expected 121 primary-party countries but found ",
      nrow(dat_primary), "."
    ),
    call. = FALSE
  )
}

# ============================================================
# 4. BUILD FIXED 25-VARIABLE MATRIX
# ============================================================

# All non-expenditure indicators must already be numeric.
non_numeric <- non_exp_vars[
  !vapply(dat_primary[non_exp_vars], is.numeric, logical(1))
]

if (length(non_numeric) > 0) {
  stop(
    paste0(
      "The following analysis columns are not numeric: ",
      paste(non_numeric, collapse = ", ")
    )
  )
}

analysis_data <- dat_primary %>%
  dplyr::transmute(
    country = country,
    dplyr::across(dplyr::all_of(non_exp_vars)),
    mil_expenditure_pct_gdp = as.numeric(.data[[MIL_EXP_SOURCE]])
  )

X_raw <- analysis_data %>%
  dplyr::select(dplyr::all_of(vars_25)) %>%
  as.data.frame()

rownames(X_raw) <- analysis_data$country

if (!all(vapply(X_raw, is.numeric, logical(1)))) {
  stop("At least one 25-variable input is not numeric.")
}

if (any(vapply(X_raw, function(x) any(is.infinite(x), na.rm = TRUE), logical(1)))) {
  stop("Infinite values are present in the 25-variable matrix.")
}

zero_var <- vapply(
  X_raw,
  function(x) {
    z <- x[is.finite(x)]
    length(unique(z)) <= 1
  },
  logical(1)
)

if (any(zero_var)) {
  stop(
    paste0(
      "Zero-variance variables in the fixed 25-variable specification: ",
      paste(names(zero_var)[zero_var], collapse = ", ")
    )
  )
}

# ============================================================
# 5. MISSINGNESS AND MEDIAN IMPUTATION
# ============================================================

missingness <- tibble::tibble(
  variable = vars_25,
  n_missing = vapply(X_raw, function(x) sum(is.na(x)), integer(1)),
  pct_missing = vapply(X_raw, function(x) mean(is.na(x)) * 100, numeric(1)),
  n_unique = vapply(
    X_raw,
    function(x) length(unique(x[!is.na(x)])),
    integer(1)
  ),
  n_zero = vapply(X_raw, function(x) sum(x == 0, na.rm = TRUE), integer(1)),
  pct_zero = vapply(
    X_raw,
    function(x) {
      observed <- !is.na(x)
      if (!any(observed)) return(NA_real_)
      mean(x[observed] == 0) * 100
    },
    numeric(1)
  )
) %>%
  dplyr::arrange(desc(pct_missing), variable)

X_imp <- X_raw
imputation_records <- list()

for (v in vars_25) {

  miss <- is.na(X_imp[[v]])
  med <- median(X_imp[[v]], na.rm = TRUE)

  if (!is.finite(med)) {
    stop(
      paste0(
        "Cannot median-impute variable '",
        v,
        "': no finite median."
      )
    )
  }

  if (any(miss)) {
    imputation_records[[v]] <- tibble::tibble(
      country = rownames(X_imp)[miss],
      variable = v,
      imputed_value = med
    )

    X_imp[[v]][miss] <- med
  }
}

if (length(imputation_records) > 0) {
  imputation_log <- dplyr::bind_rows(imputation_records)
} else {
  imputation_log <- tibble::tibble(
    country = character(),
    variable = character(),
    imputed_value = numeric()
  )
}

imputation_by_country <- tibble::tibble(
  country = rownames(X_raw),
  n_imputed = vapply(
    seq_len(nrow(X_raw)),
    function(i) sum(is.na(X_raw[i, , drop = TRUE])),
    integer(1)
  )
) %>%
  dplyr::arrange(desc(n_imputed), country)

stopifnot(all(stats::complete.cases(X_imp)))

cat("Imputed cells:", nrow(imputation_log), "\n")

# ============================================================
# 6. STANDARDIZATION, DISTANCE, WARD.D2 HCA
# ============================================================

X_z <- scale(X_imp)

D <- stats::dist(
  X_z,
  method = "euclidean"
)

hc <- stats::hclust(
  D,
  method = "ward.D2"
)

# ============================================================
# 7. SILHOUETTE FOR ALL k = 2,...,10
# ============================================================

membership_list <- list()
silhouette_solution_rows <- list()
silhouette_cluster_rows <- list()
case_silhouette_rows <- list()

for (k in K_GRID) {

  groups <- stats::cutree(hc, k = k)
  membership_list[[as.character(k)]] <- groups

  sil <- cluster::silhouette(groups, D)
  sil_df <- as.data.frame(sil)
  sil_df$country <- rownames(sil_df)

  cluster_sizes <- table(groups)

  silhouette_solution_rows[[as.character(k)]] <- tibble::tibble(
    k = k,
    mean_silhouette = mean(sil_df$sil_width),
    median_silhouette = median(sil_df$sil_width),
    min_silhouette = min(sil_df$sil_width),
    n_negative = sum(sil_df$sil_width < 0),
    pct_negative = mean(sil_df$sil_width < 0) * 100,
    smallest_cluster_n = min(as.integer(cluster_sizes)),
    largest_cluster_n = max(as.integer(cluster_sizes))
  )

  silhouette_cluster_rows[[as.character(k)]] <- sil_df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_silhouette = mean(sil_width),
      median_silhouette = median(sil_width),
      min_silhouette = min(sil_width),
      max_silhouette = max(sil_width),
      n_negative = sum(sil_width < 0),
      pct_negative = mean(sil_width < 0) * 100,
      .groups = "drop"
    ) %>%
    dplyr::mutate(k = k, .before = 1)

  case_silhouette_rows[[as.character(k)]] <- sil_df %>%
    dplyr::transmute(
      k = k,
      country = country,
      cluster = as.integer(cluster),
      neighbor_cluster = as.integer(neighbor),
      silhouette = as.numeric(sil_width)
    )
}

silhouette_all_k <- dplyr::bind_rows(silhouette_solution_rows) %>%
  dplyr::arrange(dplyr::desc(mean_silhouette), k) %>%
  dplyr::mutate(
    silhouette_rank = dplyr::row_number(),
    .before = 1
  )

silhouette_by_cluster_all_k <- dplyr::bind_rows(silhouette_cluster_rows)
case_silhouette_all_k <- dplyr::bind_rows(case_silhouette_rows)

top3_solutions <- silhouette_all_k %>%
  dplyr::slice_head(n = N_TOP_SOLUTIONS)

top3_ks <- top3_solutions$k

cat("\nTop-3 solutions by mean silhouette:\n")
print(top3_solutions, n = Inf)

# ============================================================
# 8. TOP-3 MEMBERSHIP AND UKRAINE CLUSTER
# ============================================================

top3_membership <- dplyr::bind_rows(
  lapply(
    seq_len(nrow(top3_solutions)),
    function(i) {

      k <- top3_solutions$k[i]
      groups <- membership_list[[as.character(k)]]

      tibble::tibble(
        approach = "direct_25var",
        solution_rank = i,
        k = k,
        country = names(groups),
        cluster = as.integer(groups),
        is_ukraine = names(groups) == "Ukraine"
      )
    }
  )
)

ukraine_cluster_top3 <- top3_membership %>%
  dplyr::filter(is_ukraine) %>%
  dplyr::left_join(
    top3_membership %>%
      dplyr::count(
        solution_rank,
        k,
        cluster,
        name = "cluster_n"
      ),
    by = c("solution_rank", "k", "cluster")
  ) %>%
  dplyr::select(
    approach,
    solution_rank,
    k,
    ukraine_cluster = cluster,
    cluster_n
  )

# ============================================================
# 9. BOOTSTRAP STABILITY FOR TOP-3 SOLUTIONS
# ============================================================

bootstrap_rows <- vector(
  "list",
  nrow(top3_solutions)
)

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]

  cat(
    "Bootstrap solution rank",
    i,
    "(k =",
    k,
    "), B =",
    BOOT_B,
    "\n"
  )

  set.seed(
    BOOT_SEED + 1000L + 100L * i + k
  )

  cb <- fpc::clusterboot(
    X_z,
    B = BOOT_B,
    clustermethod = fpc::hclustCBI,
    k = k,
    method = "ward.D2",
    scaling = FALSE,
    dissolution = BOOT_DISSOLUTION_THRESHOLD,
    recovery = BOOT_RECOVERY_THRESHOLD
  )

  cluster_sizes <- table(
    membership_list[[as.character(k)]]
  )

  bootstrap_rows[[i]] <- tibble::tibble(
    approach = "direct_25var",
    solution_rank = i,
    k = k,
    cluster = seq_along(cb$bootmean),
    n_original = as.integer(
      cluster_sizes[
        as.character(seq_along(cb$bootmean))
      ]
    ),
    jaccard = as.numeric(cb$bootmean),
    dissolved_n = as.integer(cb$bootbrd),
    recovered_n = as.integer(cb$bootrecover),
    dissolution_pct =
      as.numeric(cb$bootbrd) / BOOT_B * 100,
    recovery_pct =
      as.numeric(cb$bootrecover) / BOOT_B * 100
  )
}

bootstrap_top3 <- dplyr::bind_rows(
  bootstrap_rows
)

# ============================================================
# 10. COMBINED TOP-3 CLUSTER DIAGNOSTICS
# ============================================================

cluster_diagnostics_top3 <-
  silhouette_by_cluster_all_k %>%
  dplyr::filter(k %in% top3_ks) %>%
  dplyr::left_join(
    top3_solutions %>%
      dplyr::select(
        solution_rank = silhouette_rank,
        k,
        solution_mean_silhouette = mean_silhouette
      ),
    by = "k"
  ) %>%
  dplyr::left_join(
    bootstrap_top3,
    by = c(
      "solution_rank",
      "k",
      "cluster"
    )
  ) %>%
  dplyr::left_join(
    ukraine_cluster_top3 %>%
      dplyr::select(
        solution_rank,
        k,
        ukraine_cluster
      ),
    by = c("solution_rank", "k")
  ) %>%
  dplyr::mutate(
    approach = "direct_25var",
    contains_ukraine =
      cluster == ukraine_cluster,
    silhouette_class = dplyr::case_when(
      mean_silhouette >= 0.50 ~ "good",
      mean_silhouette >= 0.25 ~ "moderate",
      mean_silhouette >= 0.00 ~ "weak",
      TRUE ~ "negative"
    ),
    stability_class = dplyr::case_when(
      jaccard >= 0.85 ~ "highly stable",
      jaccard >= 0.75 ~ "stable",
      jaccard >= 0.60 ~ "weak / provisional",
      TRUE ~ "unstable"
    ),
    .before = 1
  ) %>%
  dplyr::arrange(
    solution_rank,
    cluster
  )

# ============================================================
# 11. STANDARDIZED CENTROIDS FOR TOP-3 SOLUTIONS
# ============================================================

centroid_rows <- vector(
  "list",
  nrow(top3_solutions)
)

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]
  groups <- membership_list[[as.character(k)]]

  z_df <- as.data.frame(X_z) %>%
    tibble::rownames_to_column("country") %>%
    dplyr::mutate(
      cluster = as.integer(groups[country])
    )

  centroid_rows[[i]] <- z_df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(
        dplyr::all_of(vars_25),
        mean
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      approach = "direct_25var",
      solution_rank = i,
      k = k,
      .before = 1
    )
}

centroids_top3_wide <- dplyr::bind_rows(
  centroid_rows
)

centroids_top3_long <- centroids_top3_wide %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(vars_25),
    names_to = "variable",
    values_to = "centroid_z"
  ) %>%
  dplyr::arrange(
    solution_rank,
    cluster,
    variable
  )

# ============================================================
# 12. RAW MEDIAN PROFILES FOR TOP-3 SOLUTIONS
# ============================================================

raw_median_rows <- vector(
  "list",
  nrow(top3_solutions)
)

X_raw_with_country <- X_raw %>%
  tibble::rownames_to_column("country")

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]
  groups <- membership_list[[as.character(k)]]

  raw_median_rows[[i]] <-
    X_raw_with_country %>%
    dplyr::mutate(
      cluster = as.integer(groups[country])
    ) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(
        dplyr::all_of(vars_25),
        ~ median(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      approach = "direct_25var",
      solution_rank = i,
      k = k,
      .before = 1
    )
}

raw_medians_top3 <- dplyr::bind_rows(
  raw_median_rows
)

# ============================================================
# 13. GLOBAL TOP-10 NEAREST NEIGHBOURS OF UKRAINE IN 25D SPACE
# ============================================================

D_matrix <- as.matrix(D)

ukraine_top10_global <- tibble::tibble(
  approach = "direct_25var",
  country = rownames(D_matrix),
  distance_to_ukraine =
    as.numeric(D_matrix["Ukraine", ])
) %>%
  dplyr::filter(country != "Ukraine") %>%
  dplyr::arrange(
    distance_to_ukraine,
    country
  ) %>%
  dplyr::mutate(
    neighbor_rank = dplyr::row_number(),
    .before = 2
  ) %>%
  dplyr::slice_head(n = 10)

# Distance ranking is independent of k, but repeated with top-3
# solution metadata for the cross-approach comparison table.
ukraine_top10_by_solution <- dplyr::bind_rows(
  lapply(
    seq_len(nrow(top3_solutions)),
    function(i) {

      k <- top3_solutions$k[i]
      ukr_cluster <-
        ukraine_cluster_top3$ukraine_cluster[
          ukraine_cluster_top3$solution_rank == i
        ]

      ukraine_top10_global %>%
        dplyr::mutate(
          solution_rank = i,
          k = k,
          ukraine_cluster = ukr_cluster,
          .after = approach
        )
    }
  )
)

# ============================================================
# 14. DENDROGRAM
# ============================================================

dendrogram_file <- file.path(
  OUT_DIR,
  "HCA_direct_25var_dendrogram.pdf"
)

pdf(
  dendrogram_file,
  width = 16,
  height = 9,
  onefile = TRUE
)

plot(
  hc,
  labels = rownames(X_z),
  cex = 0.55,
  hang = -1,
  main = "Direct 25-variable HCA",
  xlab = "",
  sub = "",
  ylab = "Ward distance"
)

dev.off()

# ============================================================
# 15. EXPORT CSV TABLES
# ============================================================

readr::write_csv(
  missingness,
  file.path(OUT_DIR, "missingness.csv")
)

readr::write_csv(
  imputation_log,
  file.path(OUT_DIR, "imputation_log.csv")
)

readr::write_csv(
  silhouette_all_k,
  file.path(OUT_DIR, "silhouette_all_k.csv")
)

readr::write_csv(
  top3_solutions,
  file.path(OUT_DIR, "top3_solutions.csv")
)

readr::write_csv(
  cluster_diagnostics_top3,
  file.path(
    OUT_DIR,
    "top3_cluster_diagnostics.csv"
  )
)

readr::write_csv(
  top3_membership,
  file.path(
    OUT_DIR,
    "top3_cluster_membership.csv"
  )
)

readr::write_csv(
  ukraine_cluster_top3,
  file.path(
    OUT_DIR,
    "ukraine_cluster_top3.csv"
  )
)

readr::write_csv(
  centroids_top3_long,
  file.path(
    OUT_DIR,
    "top3_centroids_z_long.csv"
  )
)

readr::write_csv(
  ukraine_top10_global,
  file.path(
    OUT_DIR,
    "ukraine_top10_global.csv"
  )
)

readr::write_csv(
  ukraine_top10_by_solution,
  file.path(
    OUT_DIR,
    "ukraine_top10_by_solution.csv"
  )
)

# ============================================================
# 16. EXPORT EXCEL WORKBOOK
# ============================================================

analysis_settings <- tibble::tibble(
  setting = c(
    "input_file",
    "input_sheet",
    "population_filter",
    "n_countries",
    "n_variables",
    "fixed_variable_set",
    "military_expenditure_source",
    "missing_data",
    "transformation",
    "scaling",
    "variable_weighting",
    "distance",
    "linkage",
    "candidate_k",
    "top_solutions",
    "bootstrap_B",
    "dissolution_threshold",
    "recovery_threshold"
  ),
  value = c(
    basename(INPUT_FILE),
    INPUT_SHEET,
    "role_class == primary party",
    nrow(X_z),
    ncol(X_z),
    "25 prespecified indicators",
    MIL_EXP_SOURCE,
    "variable-wise median imputation",
    "none",
    "z-score",
    "none beyond z-standardization",
    "Euclidean",
    "Ward.D2",
    paste(K_GRID, collapse = ", "),
    N_TOP_SOLUTIONS,
    BOOT_B,
    BOOT_DISSOLUTION_THRESHOLD,
    BOOT_RECOVERY_THRESHOLD
  )
)

xlsx_output <- list(
  settings = analysis_settings,
  retained_variables = tibble::tibble(
    variable = vars_25
  ),
  missingness = missingness,
  imputation_log = imputation_log,
  imputation_by_country = imputation_by_country,
  silhouette_all_k = silhouette_all_k,
  top3_solutions = top3_solutions,
  top3_cluster_diagnostics =
    cluster_diagnostics_top3,
  top3_membership = top3_membership,
  ukraine_cluster_top3 =
    ukraine_cluster_top3,
  top3_centroids_z =
    centroids_top3_wide,
  top3_centroids_z_long =
    centroids_top3_long,
  top3_raw_medians =
    raw_medians_top3,
  bootstrap_top3 =
    bootstrap_top3,
  ukraine_top10_global =
    ukraine_top10_global,
  ukraine_top10_by_solution =
    ukraine_top10_by_solution
)

xlsx_file <- file.path(
  OUT_DIR,
  "HCA_direct_25var_results.xlsx"
)

writexl::write_xlsx(
  xlsx_output,
  xlsx_file
)

# ============================================================
# 17. SAVE REPRODUCIBILITY OBJECT
# ============================================================

reproducibility_objects <- list(
  script_version =
    "HCA_direct_25var — 2026-09-12",
  input_file =
    normalizePath(INPUT_FILE),
  input_sheet =
    INPUT_SHEET,
  vars_25 =
    vars_25,
  military_expenditure_source =
    MIL_EXP_SOURCE,
  X_raw =
    X_raw,
  X_imputed =
    X_imp,
  X_z =
    X_z,
  D =
    D,
  hc =
    hc,
  K_GRID =
    K_GRID,
  membership_list =
    membership_list,
  case_silhouette_all_k =
    case_silhouette_all_k,
  top3_solutions =
    top3_solutions,
  cluster_diagnostics_top3 =
    cluster_diagnostics_top3,
  bootstrap_top3 =
    bootstrap_top3,
  centroids_top3_wide =
    centroids_top3_wide,
  centroids_top3_long =
    centroids_top3_long,
  raw_medians_top3 =
    raw_medians_top3,
  ukraine_cluster_top3 =
    ukraine_cluster_top3,
  ukraine_top10_global =
    ukraine_top10_global,
  ukraine_top10_by_solution =
    ukraine_top10_by_solution,
  imputation_log =
    imputation_log
)

rds_file <- file.path(
  OUT_DIR,
  "HCA_direct_25var_results.rds"
)

saveRDS(
  reproducibility_objects,
  rds_file
)

capture.output(
  sessionInfo(),
  file = file.path(
    OUT_DIR,
    "sessionInfo.txt"
  )
)

# ============================================================
# 18. FINISH
# ============================================================

cat("\n============================================================\n")
cat("DIRECT 25-VARIABLE ANALYSIS COMPLETE\n")
cat("============================================================\n")
cat("Countries:", nrow(X_z), "\n")
cat("Variables:", ncol(X_z), "\n")
cat("Imputed cells:", nrow(imputation_log), "\n")
cat("Top-3 k:", paste(top3_ks, collapse = ", "), "\n")
cat("Military-expenditure source:", MIL_EXP_SOURCE, "\n")
cat("Excel:", xlsx_file, "\n")
cat("RDS:", rds_file, "\n")
cat("Dendrogram:", dendrogram_file, "\n")
