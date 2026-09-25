# ============================================================
# HIERARCHICAL CLUSTER ANALYSIS OF STATE WAR EXPERIENCE
# EMPIRICAL SEVEN-FAMILY SYNTHETIC-SCORE MODEL
# Fresh data-driven run
# ============================================================
#
# Expected project layout:
#   new_scripts/
#     war_participation_data.xlsx
#     HCA_7families.R
#     results/
#
# Outputs are written to:
#   results/families_7/
#
# Workflow:
#   1. Retain role_class == "primary party".
#   2. Start from the fixed 25-indicator pool.
#   3. Run Hmisc::redun() from scratch at adjusted R^2 >= .95.
#   4. Cluster the retained variables empirically with ClustOfVar.
#   5. Cut the variable dendrogram at k = 7 families.
#   6. Represent each family by its first principal component.
#   7. For rows with missing family inputs, calculate the score from
#      observed standardized inputs and normalize by the observed
#      coefficient norm.
#   8. Standardize the seven family scores.
#   9. Run Euclidean / Ward.D2 country HCA in the 7D family-score space.
#  10. Evaluate k = 2,...,10 country-cluster solutions.
#  11. Bootstrap the top 3 country solutions with B = 1000.
#
# No old redundancy deletion set or old seven-family membership map is
# forced. Both are recalculated from the current master data.
#
# Main outputs:
#   - actual redundancy deletions and retained variables
#   - empirical seven-family variable map
#   - family PCA coefficients / loadings / coverage
#   - top-3 country HCA solutions
#   - cluster-level silhouette, Jaccard, dissolution, recovery
#   - standardized 7D cluster centroids
#   - Ukraine cluster membership
#   - global top-10 nearest neighbours of Ukraine in the 7D space
#
# The final comparison script can use the saved RDS object to calculate
# the country closest to each cluster centroid.
#
# Military expenditure must already be parsed as numeric in the master
# workbook. Preferred source:
#   mil_expenditure_pct_gdp_num
# Numeric mil_expenditure_pct_gdp is accepted as fallback.
# ============================================================

cat("\nSCRIPT VERSION: HCA_7families — 2026-09-12\n")

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
  "writexl",
  "Hmisc",
  "ClustOfVar"
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

REDUNDANCY_CUTOFF <- 0.95
N_FAMILIES <- 7L

COUNTRY_K_GRID <- 2:10
N_TOP_SOLUTIONS <- 3L

COUNTRY_BOOT_B <- 1000L
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
OUT_DIR <- file.path(BASE_DIR, "results", "families_7")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("Input file:", normalizePath(INPUT_FILE), "\n")
cat("Output directory:", normalizePath(OUT_DIR, mustWork = FALSE), "\n")

# ============================================================
# 1. FIXED 25-INDICATOR STARTING POOL
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
      "Missing required indicator columns: ",
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
# 4. BUILD 25-INDICATOR MATRIX
# ============================================================

non_numeric <- non_exp_vars[
  !vapply(dat_primary[non_exp_vars], is.numeric, logical(1))
]

if (length(non_numeric) > 0) {
  stop(
    paste0(
      "The following indicator columns are not numeric: ",
      paste(non_numeric, collapse = ", ")
    )
  )
}

analysis_data_25 <- dat_primary %>%
  dplyr::transmute(
    country = country,
    dplyr::across(dplyr::all_of(non_exp_vars)),
    mil_expenditure_pct_gdp = as.numeric(.data[[MIL_EXP_SOURCE]])
  )

X_all <- analysis_data_25 %>%
  dplyr::select(dplyr::all_of(vars_25)) %>%
  as.data.frame()

rownames(X_all) <- analysis_data_25$country

if (!all(vapply(X_all, is.numeric, logical(1)))) {
  stop("At least one indicator is not numeric.")
}

if (any(vapply(X_all, function(x) any(is.infinite(x), na.rm = TRUE), logical(1)))) {
  stop("Infinite values are present in the indicator matrix.")
}

zero_var <- vapply(
  X_all,
  function(x) {
    z <- x[is.finite(x)]
    length(unique(z)) <= 1
  },
  logical(1)
)

if (any(zero_var)) {
  stop(
    paste0(
      "Zero-variance indicators: ",
      paste(names(zero_var)[zero_var], collapse = ", ")
    )
  )
}

# ============================================================
# 5. STARTING-POOL DESCRIPTIVES
# ============================================================

missingness_25 <- tibble::tibble(
  variable = vars_25,
  n_missing = vapply(X_all, function(x) sum(is.na(x)), integer(1)),
  pct_missing = vapply(X_all, function(x) mean(is.na(x)) * 100, numeric(1)),
  n_unique = vapply(
    X_all,
    function(x) length(unique(x[!is.na(x)])),
    integer(1)
  ),
  n_zero = vapply(X_all, function(x) sum(x == 0, na.rm = TRUE), integer(1)),
  pct_zero = vapply(
    X_all,
    function(x) {
      observed <- !is.na(x)
      if (!any(observed)) return(NA_real_)
      mean(x[observed] == 0) * 100
    },
    numeric(1)
  )
) %>%
  dplyr::arrange(desc(pct_missing), variable)

# ============================================================
# 6. DATA-DRIVEN REDUNDANCY SCREENING
# ============================================================

set.seed(BOOT_SEED)

red95 <- Hmisc::redun(
  ~ .,
  data = X_all,
  r2 = REDUNDANCY_CUTOFF,
  type = "adjusted",
  nk = 0
)

redun_initial_r2 <- tibble::tibble(
  variable = names(red95$rsq1),
  adjusted_r2_all_others = as.numeric(red95$rsq1)
) %>%
  dplyr::arrange(desc(adjusted_r2_all_others))

if (length(red95$Out) > 0) {
  redun_deletions <- tibble::tibble(
    deletion_order = seq_along(red95$Out),
    variable = as.character(red95$Out),
    adjusted_r2_at_deletion = as.numeric(red95$rsquared)
  )
} else {
  redun_deletions <- tibble::tibble(
    deletion_order = integer(),
    variable = character(),
    adjusted_r2_at_deletion = numeric()
  )
}

vars_reduced <- as.character(red95$In)

if (length(vars_reduced) < N_FAMILIES) {
  stop(
    paste0(
      "Only ",
      length(vars_reduced),
      " variables remain after redundancy reduction; cannot form ",
      N_FAMILIES,
      " variable families."
    )
  )
}

cat("\nRedundancy screening removed:\n")
print(redun_deletions, n = Inf)

cat("\nVariables retained for variable clustering:", length(vars_reduced), "\n")
print(vars_reduced)

X_red <- X_all[, vars_reduced, drop = FALSE]

# ============================================================
# 7. COMPLETE-CASE TRAINING MATRIX FOR VARIABLE CLUSTERING
# ============================================================

training_rows_global <- stats::complete.cases(X_red)
X_var_cc <- X_red[training_rows_global, , drop = FALSE]

if (nrow(X_var_cc) < 3L) {
  stop("Too few complete cases for variable clustering.")
}

if (nrow(X_var_cc) <= ncol(X_var_cc)) {
  warning(
    "The complete-case variable-clustering training matrix has n <= p.",
    call. = FALSE
  )
}

complete_case_summary <- tibble::tibble(
  n_total_countries = nrow(X_red),
  n_complete_training_countries = nrow(X_var_cc),
  n_retained_variables = ncol(X_red),
  n_families = N_FAMILIES
)

# ============================================================
# 8. EMPIRICAL VARIABLE CLUSTERING
# ============================================================
#
# ClustOfVar groups variables according to their similarity structure.
# We do not force the old seven-family variable membership.
# ============================================================

var_tree <- ClustOfVar::hclustvar(
  X.quanti = X_var_cc
)

part7 <- ClustOfVar::cutreevar(
  var_tree,
  k = N_FAMILIES
)

family_assignment <- part7$cluster[colnames(X_red)]

if (anyNA(family_assignment)) {
  stop("At least one retained variable did not receive a seven-family assignment.")
}

family_assignment <- as.integer(family_assignment)
names(family_assignment) <- colnames(X_red)

# Stable generic family names. Substantive labels can be assigned later
# from the exported member-variable map.
families7 <- split(
  names(family_assignment),
  factor(
    family_assignment,
    levels = sort(unique(family_assignment))
  )
)

names(families7) <- paste0(
  "family_",
  seq_along(families7)
)

family_map7 <- dplyr::bind_rows(
  lapply(
    seq_along(families7),
    function(i) {
      tibble::tibble(
        family_number = i,
        family = names(families7)[i],
        variable = families7[[i]]
      )
    }
  )
)

if (!setequal(family_map7$variable, vars_reduced)) {
  stop("The empirical seven-family map does not cover all retained variables.")
}

if (anyDuplicated(family_map7$variable)) {
  stop("At least one retained variable occurs in more than one family.")
}

cat("\nEmpirical seven-family variable map:\n")
print(family_map7, n = Inf)

# ============================================================
# 9. VARIABLE-PARTITION SILHOUETTE DIAGNOSTIC
# ============================================================
#
# Variable dissimilarity:
#   D_var = 1 - rho_S^2
# ============================================================

rho_s <- stats::cor(
  X_var_cc,
  method = "spearman",
  use = "complete.obs"
)

D_var_matrix <- 1 - rho_s^2
diag(D_var_matrix) <- 0

D_var <- stats::as.dist(D_var_matrix)

variable_silhouette <- cluster::silhouette(
  family_assignment,
  D_var
)

variable_silhouette_df <- as.data.frame(variable_silhouette)
variable_silhouette_df$variable <- rownames(variable_silhouette_df)

variable_silhouette_summary <- tibble::tibble(
  n_variables = length(family_assignment),
  n_families = N_FAMILIES,
  mean_variable_silhouette = mean(variable_silhouette_df$sil_width),
  median_variable_silhouette = median(variable_silhouette_df$sil_width),
  min_variable_silhouette = min(variable_silhouette_df$sil_width),
  n_negative = sum(variable_silhouette_df$sil_width < 0),
  pct_negative = mean(variable_silhouette_df$sil_width < 0) * 100
)

variable_silhouette_by_family <- variable_silhouette_df %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_silhouette = mean(sil_width),
    median_silhouette = median(sil_width),
    min_silhouette = min(sil_width),
    n_negative = sum(sil_width < 0),
    pct_negative = mean(sil_width < 0) * 100,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    family = paste0("family_", cluster),
    .after = cluster
  )

# ============================================================
# 10. FIT MISSING-AWARE FAMILY SCORES
# ============================================================
#
# Each family is summarized by its first PC fitted on the same global
# complete-case training rows used above.
#
# For a country with one or more missing inputs inside a family, the
# score is calculated from the observed standardized family inputs and
# divided by the norm of the observed PC coefficients.
# ============================================================

fit_family_scores <- function(
  X,
  families,
  training_rows = stats::complete.cases(X)
) {

  if (sum(training_rows) < 3L) {
    stop("Too few complete training rows for family-score estimation.")
  }

  score_list <- list()
  loading_list <- list()
  coverage_list <- list()
  model_list <- list()

  for (family_name in names(families)) {

    vars <- families[[family_name]]
    Xi <- X[, vars, drop = FALSE]
    train <- Xi[training_rows, , drop = FALSE]

    mu <- vapply(train, mean, numeric(1))
    sigma <- vapply(train, stats::sd, numeric(1))

    if (any(!is.finite(sigma) | sigma <= 0)) {
      stop(
        paste0(
          "Zero/non-finite SD inside family '",
          family_name,
          "'."
        )
      )
    }

    Z_train <- sweep(
      as.matrix(train),
      2,
      mu,
      "-"
    )

    Z_train <- sweep(
      Z_train,
      2,
      sigma,
      "/"
    )

    pca <- stats::prcomp(
      Z_train,
      center = FALSE,
      scale. = FALSE,
      rank. = 1
    )

    v <- pca$rotation[, 1]

    # Deterministic sign orientation only.
    first_var <- vars[1]
    if (v[first_var] < 0) {
      v <- -v
    }

    Z_all <- sweep(
      as.matrix(Xi),
      2,
      mu,
      "-"
    )

    Z_all <- sweep(
      Z_all,
      2,
      sigma,
      "/"
    )

    score <- apply(
      Z_all,
      1,
      function(z) {

        observed <- is.finite(z)

        if (!any(observed)) {
          return(NA_real_)
        }

        denom <- sqrt(
          sum(v[observed]^2)
        )

        if (!is.finite(denom) || denom <= 0) {
          return(NA_real_)
        }

        sum(
          z[observed] * v[observed]
        ) / denom
      }
    )

    n_observed <- apply(
      Z_all,
      1,
      function(z) sum(is.finite(z))
    )

    train_score <- as.numeric(
      Z_train %*% v
    )

    signed_loading <- vapply(
      seq_along(vars),
      function(j) {
        stats::cor(
          Z_train[, j],
          train_score
        )
      },
      numeric(1)
    )

    score_list[[family_name]] <- score

    coverage_list[[family_name]] <- tibble::tibble(
      country = rownames(X),
      family = family_name,
      n_observed = n_observed,
      n_family_variables = length(vars),
      complete_family =
        n_observed == length(vars)
    )

    loading_list[[family_name]] <- tibble::tibble(
      family = family_name,
      variable = vars,
      pc_coefficient = as.numeric(v[vars]),
      signed_loading = signed_loading,
      squared_loading = signed_loading^2
    )

    model_list[[family_name]] <- list(
      mean = mu,
      sd = sigma,
      coefficients = v,
      pca = pca
    )
  }

  scores_raw <- as.data.frame(score_list)
  rownames(scores_raw) <- rownames(X)

  if (any(!stats::complete.cases(scores_raw))) {
    bad <- rownames(scores_raw)[
      !stats::complete.cases(scores_raw)
    ]

    stop(
      paste0(
        "At least one country could not be assigned all seven family scores: ",
        paste(bad, collapse = ", ")
      )
    )
  }

  scores_z <- as.data.frame(
    scale(scores_raw)
  )

  rownames(scores_z) <- rownames(X)

  list(
    scores_raw = scores_raw,
    scores_z = scores_z,
    loadings = dplyr::bind_rows(loading_list),
    coverage = dplyr::bind_rows(coverage_list),
    models = model_list
  )
}

family7 <- fit_family_scores(
  X_red,
  families7,
  training_rows = training_rows_global
)

F7_raw <- family7$scores_raw
F7 <- family7$scores_z

family_score_coverage_summary <- family7$coverage %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_complete_family = sum(complete_family),
    n_partial_family = sum(!complete_family),
    min_inputs_observed = min(n_observed),
    max_inputs_observed = max(n_observed),
    .groups = "drop"
  )

cat("\nSeven-family score matrix:", nrow(F7), "x", ncol(F7), "\n")
cat("\nFamily-score coverage:\n")
print(family_score_coverage_summary, n = Inf)

# ============================================================
# 11. COUNTRY DISTANCE AND WARD.D2 HCA IN 7D SPACE
# ============================================================

D_country <- stats::dist(
  F7,
  method = "euclidean"
)

hc_country <- stats::hclust(
  D_country,
  method = "ward.D2"
)

# ============================================================
# 12. COUNTRY SILHOUETTE FOR k = 2,...,10
# ============================================================

membership_list <- list()
silhouette_solution_rows <- list()
silhouette_cluster_rows <- list()
case_silhouette_rows <- list()

for (k in COUNTRY_K_GRID) {

  groups <- stats::cutree(
    hc_country,
    k = k
  )

  membership_list[[as.character(k)]] <- groups

  sil <- cluster::silhouette(
    groups,
    D_country
  )

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
    dplyr::mutate(
      k = k,
      .before = 1
    )

  case_silhouette_rows[[as.character(k)]] <- sil_df %>%
    dplyr::transmute(
      k = k,
      country = country,
      cluster = as.integer(cluster),
      neighbor_cluster = as.integer(neighbor),
      silhouette = as.numeric(sil_width)
    )
}

silhouette_all_k <- dplyr::bind_rows(
  silhouette_solution_rows
) %>%
  dplyr::arrange(
    dplyr::desc(mean_silhouette),
    k
  ) %>%
  dplyr::mutate(
    silhouette_rank =
      dplyr::row_number(),
    .before = 1
  )

silhouette_by_cluster_all_k <- dplyr::bind_rows(
  silhouette_cluster_rows
)

case_silhouette_all_k <- dplyr::bind_rows(
  case_silhouette_rows
)

top3_solutions <- silhouette_all_k %>%
  dplyr::slice_head(
    n = N_TOP_SOLUTIONS
  )

top3_ks <- top3_solutions$k

cat("\nTop-3 country solutions by mean silhouette:\n")
print(top3_solutions, n = Inf)

# ============================================================
# 13. TOP-3 MEMBERSHIP AND UKRAINE CLUSTER
# ============================================================

top3_membership <- dplyr::bind_rows(
  lapply(
    seq_len(nrow(top3_solutions)),
    function(i) {

      k <- top3_solutions$k[i]
      groups <- membership_list[[as.character(k)]]

      tibble::tibble(
        approach = "families_7",
        solution_rank = i,
        k = k,
        country = names(groups),
        cluster = as.integer(groups),
        is_ukraine =
          names(groups) == "Ukraine"
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
    by = c(
      "solution_rank",
      "k",
      "cluster"
    )
  ) %>%
  dplyr::select(
    approach,
    solution_rank,
    k,
    ukraine_cluster = cluster,
    cluster_n
  )

# ============================================================
# 14. BOOTSTRAP STABILITY FOR TOP-3 COUNTRY SOLUTIONS
# ============================================================

bootstrap_rows <- vector(
  "list",
  nrow(top3_solutions)
)

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]

  cat(
    "Bootstrap country solution rank",
    i,
    "(k =",
    k,
    "), B =",
    COUNTRY_BOOT_B,
    "\n"
  )

  set.seed(
    BOOT_SEED + 1000L + 100L * i + k
  )

  cb <- fpc::clusterboot(
    F7,
    B = COUNTRY_BOOT_B,
    clustermethod = fpc::hclustCBI,
    k = k,
    method = "ward.D2",
    scaling = FALSE,
    dissolution =
      BOOT_DISSOLUTION_THRESHOLD,
    recovery =
      BOOT_RECOVERY_THRESHOLD
  )

  cluster_sizes <- table(
    membership_list[[as.character(k)]]
  )

  bootstrap_rows[[i]] <- tibble::tibble(
    approach = "families_7",
    solution_rank = i,
    k = k,
    cluster =
      seq_along(cb$bootmean),
    n_original = as.integer(
      cluster_sizes[
        as.character(
          seq_along(cb$bootmean)
        )
      ]
    ),
    jaccard =
      as.numeric(cb$bootmean),
    dissolved_n =
      as.integer(cb$bootbrd),
    recovered_n =
      as.integer(cb$bootrecover),
    dissolution_pct =
      as.numeric(cb$bootbrd) /
        COUNTRY_BOOT_B * 100,
    recovery_pct =
      as.numeric(cb$bootrecover) /
        COUNTRY_BOOT_B * 100
  )
}

bootstrap_top3 <- dplyr::bind_rows(
  bootstrap_rows
)

# ============================================================
# 15. COMBINED TOP-3 CLUSTER DIAGNOSTICS
# ============================================================

cluster_diagnostics_top3 <-
  silhouette_by_cluster_all_k %>%
  dplyr::filter(
    k %in% top3_ks
  ) %>%
  dplyr::left_join(
    top3_solutions %>%
      dplyr::select(
        solution_rank =
          silhouette_rank,
        k,
        solution_mean_silhouette =
          mean_silhouette
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
    by = c(
      "solution_rank",
      "k"
    )
  ) %>%
  dplyr::mutate(
    approach = "families_7",
    contains_ukraine =
      cluster == ukraine_cluster,
    silhouette_class =
      dplyr::case_when(
        mean_silhouette >= 0.50 ~
          "good",
        mean_silhouette >= 0.25 ~
          "moderate",
        mean_silhouette >= 0.00 ~
          "weak",
        TRUE ~
          "negative"
      ),
    stability_class =
      dplyr::case_when(
        jaccard >= 0.85 ~
          "highly stable",
        jaccard >= 0.75 ~
          "stable",
        jaccard >= 0.60 ~
          "weak / provisional",
        TRUE ~
          "unstable"
      ),
    .before = 1
  ) %>%
  dplyr::arrange(
    solution_rank,
    cluster
  )

# ============================================================
# 16. 7D STANDARDIZED CENTROIDS FOR TOP-3 SOLUTIONS
# ============================================================

family_names <- colnames(F7)

centroid_rows <- vector(
  "list",
  nrow(top3_solutions)
)

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]
  groups <- membership_list[[as.character(k)]]

  f_df <- as.data.frame(F7) %>%
    tibble::rownames_to_column("country") %>%
    dplyr::mutate(
      cluster =
        as.integer(groups[country])
    )

  centroid_rows[[i]] <- f_df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(
        dplyr::all_of(family_names),
        mean
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      approach = "families_7",
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
    cols = dplyr::all_of(family_names),
    names_to = "family",
    values_to = "centroid_z"
  ) %>%
  dplyr::arrange(
    solution_rank,
    cluster,
    family
  )

# ============================================================
# 17. RAW ORIGINAL-INDICATOR MEDIANS BY COUNTRY CLUSTER
# ============================================================

raw_median_rows <- vector(
  "list",
  nrow(top3_solutions)
)

X_red_with_country <- X_red %>%
  tibble::rownames_to_column("country")

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]
  groups <- membership_list[[as.character(k)]]

  raw_median_rows[[i]] <-
    X_red_with_country %>%
    dplyr::mutate(
      cluster =
        as.integer(groups[country])
    ) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(
        dplyr::all_of(vars_reduced),
        ~ median(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      approach = "families_7",
      solution_rank = i,
      k = k,
      .before = 1
    )
}

raw_medians_top3 <- dplyr::bind_rows(
  raw_median_rows
)

# ============================================================
# 18. GLOBAL TOP-10 NEAREST NEIGHBOURS OF UKRAINE IN 7D SPACE
# ============================================================

D_matrix <- as.matrix(D_country)

ukraine_top10_global <- tibble::tibble(
  approach = "families_7",
  country = rownames(D_matrix),
  distance_to_ukraine =
    as.numeric(
      D_matrix["Ukraine", ]
    )
) %>%
  dplyr::filter(
    country != "Ukraine"
  ) %>%
  dplyr::arrange(
    distance_to_ukraine,
    country
  ) %>%
  dplyr::mutate(
    neighbor_rank =
      dplyr::row_number(),
    .before = 2
  ) %>%
  dplyr::slice_head(n = 10)

# Distance ranking is independent of country k, but repeated with
# top-3 solution metadata for the combined comparison table.
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
          ukraine_cluster =
            ukr_cluster,
          .after = approach
        )
    }
  )
)

# ============================================================
# 19. DENDROGRAMS
# ============================================================

country_dendrogram_file <- file.path(
  OUT_DIR,
  "HCA_7families_country_dendrogram.pdf"
)

pdf(
  country_dendrogram_file,
  width = 16,
  height = 9,
  onefile = TRUE
)

plot(
  hc_country,
  labels = rownames(F7),
  cex = 0.55,
  hang = -1,
  main = "Country HCA in empirical seven-family score space",
  xlab = "",
  sub = "",
  ylab = "Ward distance"
)

dev.off()

variable_dendrogram_file <- file.path(
  OUT_DIR,
  "HCA_7families_variable_dendrogram.pdf"
)

pdf(
  variable_dendrogram_file,
  width = 14,
  height = 8,
  onefile = TRUE
)

plot(
  var_tree,
  main = "Empirical clustering of retained variables"
)

dev.off()

# ============================================================
# 20. EXPORT CSV TABLES
# ============================================================

readr::write_csv(
  missingness_25,
  file.path(
    OUT_DIR,
    "missingness_starting_25.csv"
  )
)

readr::write_csv(
  redun_initial_r2,
  file.path(
    OUT_DIR,
    "redundancy_initial_r2.csv"
  )
)

readr::write_csv(
  redun_deletions,
  file.path(
    OUT_DIR,
    "redundancy_deletions.csv"
  )
)

readr::write_csv(
  family_map7,
  file.path(
    OUT_DIR,
    "empirical_family_map_7.csv"
  )
)

readr::write_csv(
  family7$loadings,
  file.path(
    OUT_DIR,
    "family_loadings.csv"
  )
)

readr::write_csv(
  family7$coverage,
  file.path(
    OUT_DIR,
    "family_score_coverage_by_country.csv"
  )
)

readr::write_csv(
  variable_silhouette_summary,
  file.path(
    OUT_DIR,
    "variable_family_silhouette_summary.csv"
  )
)

readr::write_csv(
  variable_silhouette_by_family,
  file.path(
    OUT_DIR,
    "variable_family_silhouette_by_family.csv"
  )
)

readr::write_csv(
  silhouette_all_k,
  file.path(
    OUT_DIR,
    "silhouette_all_k.csv"
  )
)

readr::write_csv(
  top3_solutions,
  file.path(
    OUT_DIR,
    "top3_solutions.csv"
  )
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
# 21. EXPORT EXCEL WORKBOOK
# ============================================================

analysis_settings <- tibble::tibble(
  setting = c(
    "input_file",
    "input_sheet",
    "population_filter",
    "n_countries",
    "starting_indicators",
    "redundancy_cutoff_adjusted_r2",
    "redundancy_removed_n",
    "retained_variables_n",
    "variable_families",
    "family_score_method",
    "family_score_training_rows",
    "family_score_missing_handling",
    "family_score_scaling",
    "country_distance",
    "country_linkage",
    "candidate_country_k",
    "top_country_solutions",
    "country_bootstrap_B",
    "dissolution_threshold",
    "recovery_threshold",
    "military_expenditure_source"
  ),
  value = c(
    basename(INPUT_FILE),
    INPUT_SHEET,
    "role_class == primary party",
    nrow(F7),
    length(vars_25),
    REDUNDANCY_CUTOFF,
    length(red95$Out),
    length(vars_reduced),
    N_FAMILIES,
    "PC1 per empirical variable family",
    sum(training_rows_global),
    "observed standardized inputs / observed coefficient norm",
    "z-score across countries",
    "Euclidean in 7D standardized family-score space",
    "Ward.D2",
    paste(COUNTRY_K_GRID, collapse = ", "),
    N_TOP_SOLUTIONS,
    COUNTRY_BOOT_B,
    BOOT_DISSOLUTION_THRESHOLD,
    BOOT_RECOVERY_THRESHOLD,
    MIL_EXP_SOURCE
  )
)

family_scores_export <- F7 %>%
  tibble::rownames_to_column("country")

family_scores_raw_export <- F7_raw %>%
  tibble::rownames_to_column("country")

variable_silhouette_export <- variable_silhouette_df %>%
  dplyr::transmute(
    variable = variable,
    family_number = as.integer(cluster),
    family = paste0("family_", cluster),
    neighbor_family_number = as.integer(neighbor),
    silhouette = as.numeric(sil_width)
  )

xlsx_output <- list(
  settings = analysis_settings,
  starting_variables_25 =
    tibble::tibble(variable = vars_25),
  missingness_starting_25 =
    missingness_25,
  redundancy_initial_R2 =
    redun_initial_r2,
  redundancy_deletions =
    redun_deletions,
  retained_variables =
    tibble::tibble(variable = vars_reduced),
  complete_case_training =
    complete_case_summary,
  empirical_family_map_7 =
    family_map7,
  variable_silhouette_summary =
    variable_silhouette_summary,
  variable_silhouette =
    variable_silhouette_export,
  variable_silhouette_family =
    variable_silhouette_by_family,
  family_loadings =
    family7$loadings,
  family_score_coverage =
    family7$coverage,
  family_coverage_summary =
    family_score_coverage_summary,
  family_scores_raw =
    family_scores_raw_export,
  family_scores_z =
    family_scores_export,
  silhouette_all_k =
    silhouette_all_k,
  top3_solutions =
    top3_solutions,
  top3_cluster_diagnostics =
    cluster_diagnostics_top3,
  top3_membership =
    top3_membership,
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
  "HCA_7families_results.xlsx"
)

writexl::write_xlsx(
  xlsx_output,
  xlsx_file
)

# ============================================================
# 22. SAVE REPRODUCIBILITY OBJECT
# ============================================================

reproducibility_objects <- list(
  script_version =
    "HCA_7families — 2026-09-12",
  input_file =
    normalizePath(INPUT_FILE),
  input_sheet =
    INPUT_SHEET,
  vars_25 =
    vars_25,
  redundancy_cutoff =
    REDUNDANCY_CUTOFF,
  redun_object =
    red95,
  redun_initial_r2 =
    redun_initial_r2,
  redun_deletions =
    redun_deletions,
  vars_reduced =
    vars_reduced,
  X_all =
    X_all,
  X_red =
    X_red,
  training_rows_global =
    training_rows_global,
  X_var_cc =
    X_var_cc,
  variable_tree =
    var_tree,
  variable_partition_7 =
    part7,
  family_assignment =
    family_assignment,
  families7 =
    families7,
  family_map7 =
    family_map7,
  family_score_models =
    family7$models,
  family_loadings =
    family7$loadings,
  family_coverage =
    family7$coverage,
  F7_raw =
    F7_raw,
  F7 =
    F7,
  X_model =
    F7,
  D_country =
    D_country,
  hc_country =
    hc_country,
  COUNTRY_K_GRID =
    COUNTRY_K_GRID,
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
  military_expenditure_source =
    MIL_EXP_SOURCE
)

rds_file <- file.path(
  OUT_DIR,
  "HCA_7families_results.rds"
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
# 23. FINISH
# ============================================================

cat("\n============================================================\n")
cat("EMPIRICAL SEVEN-FAMILY ANALYSIS COMPLETE\n")
cat("============================================================\n")
cat("Countries:", nrow(F7), "\n")
cat("Starting indicators:", length(vars_25), "\n")
cat("Redundancy deletions:", length(red95$Out), "\n")
cat("Retained variables:", length(vars_reduced), "\n")
cat("Families:", N_FAMILIES, "\n")
cat("Complete training countries:", sum(training_rows_global), "\n")
cat("Top-3 country k:", paste(top3_ks, collapse = ", "), "\n")
cat("Military-expenditure source:", MIL_EXP_SOURCE, "\n")
cat("Excel:", xlsx_file, "\n")
cat("RDS:", rds_file, "\n")
cat("Country dendrogram:", country_dendrogram_file, "\n")
cat("Variable dendrogram:", variable_dendrogram_file, "\n")
