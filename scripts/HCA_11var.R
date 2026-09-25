# ============================================================
# HIERARCHICAL CLUSTER ANALYSIS OF STATE WAR EXPERIENCE
# 11-VARIABLE THEORETICAL MODEL — VARIANT B
# Final comparison run
# ============================================================
#
# Expected project layout:
#   new_scripts/
#     war_participation_data.xlsx
#     HCA_11var.R
#     results/
#
# Outputs are written to:
#   results/11var/
#
# Main comparison outputs:
#   - top 3 k-solutions ranked by mean silhouette
#   - cluster-level mean silhouette, Jaccard, dissolution, recovery
#   - standardized cluster centroids
#   - Ukraine cluster membership in each top-3 solution
#   - global top-10 nearest neighbours of Ukraine in the 11D space
#   - B = 1000 bootstrap replications for each top-3 solution
# ============================================================

cat("\nSCRIPT VERSION: HCA_11var — 2026-09-12\n")

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
OUT_DIR <- file.path(BASE_DIR, "results", "11var")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("Input file:", normalizePath(INPUT_FILE), "\n")
cat("Output directory:", normalizePath(OUT_DIR, mustWork = FALSE), "\n")

# ============================================================
# 1. IMPORT MASTER DATA
# ============================================================

dat <- readxl::read_excel(
  INPUT_FILE,
  sheet = INPUT_SHEET,
  na = c("", "NA", "N/A", "n/a")
)

required_raw <- c(
  "country",
  "role_class",
  "conflicts_primary",
  "years_primary",
  "years_war_level",
  "longest_spell",
  "years_since_last_primary",
  "years_interstate_ucdp",
  "years_interstate_alt",
  "years_intl_intrastate_ucdp",
  "years_intrastate",
  "years_home_defensive",
  "years_home_civil",
  "years_abroad",
  "conflicts_support"
)

missing_required <- setdiff(required_raw, names(dat))

if (length(missing_required) > 0) {
  stop(
    paste0(
      "Missing required columns in country_profile: ",
      paste(missing_required, collapse = ", ")
    )
  )
}

if (anyDuplicated(dat$country)) {
  stop("country_profile contains duplicated country identifiers.")
}

cat("Dataset dimensions:", nrow(dat), "rows x", ncol(dat), "columns\n")

# ============================================================
# 2. RETAIN PRIMARY-PARTY COUNTRIES ONLY
# ============================================================

dat_primary <- dat %>%
  dplyr::filter(role_class == "primary party")

if (!all(dat_primary$role_class == "primary party")) {
  stop("Primary-party filter failed.")
}

if (anyDuplicated(dat_primary$country)) {
  stop("Primary-party dataset contains duplicated countries.")
}

cat("Primary-party countries:", nrow(dat_primary), "\n")

if (nrow(dat_primary) != 121) {
  warning(
    paste0(
      "Expected 121 primary-party countries but found ",
      nrow(dat_primary), "."
    ),
    call. = FALSE
  )
}

if (!("Ukraine" %in% dat_primary$country)) {
  stop("Ukraine is absent after the primary-party filter.")
}

# ============================================================
# 3. READ AND VALIDATE VARIANT B RECODING TABLE
# ============================================================

if (!("variant_A_vs_B" %in% readxl::excel_sheets(INPUT_FILE))) {
  stop("Required sheet 'variant_A_vs_B' is missing from the master workbook.")
}

normalize_vb_indicator <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(tolower(x))
}

vb_indicators_raw <- c(
  "conflicts as primary party",
  "years as primary party",
  "years of interstate conflict",
  "years of internationalised intrastate conflict",
  "years of purely intrastate conflict",
  "years on own territory, defensive",
  "years abroad",
  "conflicts in a supporting role"
)

vb_indicators <- normalize_vb_indicator(vb_indicators_raw)

vb_raw <- readxl::read_excel(
  INPUT_FILE,
  sheet = "variant_A_vs_B",
  range = "A4:E13",
  col_types = rep("text", 5)
)

names(vb_raw)[1] <- "indicator_raw"

required_vb_cols <- c(
  "indicator_raw",
  "Ukraine, variant B",
  "Russia / USSR, variant B"
)

if (!all(required_vb_cols %in% names(vb_raw))) {
  stop(
    paste0(
      "variant_A_vs_B does not contain the expected columns: ",
      paste(required_vb_cols, collapse = ", ")
    )
  )
}

vb <- vb_raw %>%
  dplyr::transmute(
    indicator = normalize_vb_indicator(indicator_raw),
    Ukraine_text = `Ukraine, variant B`,
    Russia_text = `Russia / USSR, variant B`
  ) %>%
  dplyr::filter(indicator %in% vb_indicators) %>%
  dplyr::mutate(
    Ukraine = readr::parse_double(Ukraine_text),
    Russia = readr::parse_double(Russia_text)
  ) %>%
  dplyr::select(indicator, Ukraine, Russia)

missing_vb_indicators <- setdiff(vb_indicators, vb$indicator)
duplicated_vb_indicators <- unique(vb$indicator[duplicated(vb$indicator)])

if (length(missing_vb_indicators) > 0) {
  stop(
    paste0(
      "Missing Variant B indicators: ",
      paste(missing_vb_indicators, collapse = "; ")
    )
  )
}

if (length(duplicated_vb_indicators) > 0) {
  stop(
    paste0(
      "Duplicated Variant B indicators: ",
      paste(duplicated_vb_indicators, collapse = "; ")
    )
  )
}

if (nrow(vb) != length(vb_indicators)) {
  stop("Variant B table does not contain exactly the required indicators.")
}

if (anyNA(vb$Ukraine) || anyNA(vb$Russia)) {
  stop("At least one required Variant B value is not numeric.")
}

get_vb_value <- function(ind, ctry) {
  key <- normalize_vb_indicator(ind)
  idx <- match(key, vb$indicator)

  if (is.na(idx)) {
    stop(paste0("Variant B indicator not found: '", ind, "'."))
  }

  value <- vb[[ctry]][idx]

  if (length(value) != 1 || is.na(value)) {
    stop(
      paste0(
        "Expected one numeric Variant B value for indicator '",
        ind,
        "' and country '",
        ctry,
        "'."
      )
    )
  }

  as.numeric(value)
}

vb_conflicts_primary_ukr <- get_vb_value("conflicts as primary party", "Ukraine")
vb_conflicts_primary_rus <- get_vb_value("conflicts as primary party", "Russia")

vb_years_primary_ukr <- get_vb_value("years as primary party", "Ukraine")
vb_years_primary_rus <- get_vb_value("years as primary party", "Russia")

vb_years_interstate_ukr <- get_vb_value("years of interstate conflict", "Ukraine")
vb_years_interstate_rus <- get_vb_value("years of interstate conflict", "Russia")

vb_years_intl_intrastate_ukr <- get_vb_value(
  "years of internationalised intrastate conflict", "Ukraine"
)
vb_years_intl_intrastate_rus <- get_vb_value(
  "years of internationalised intrastate conflict", "Russia"
)

vb_years_intrastate_ukr <- get_vb_value("years of purely intrastate conflict", "Ukraine")
vb_years_intrastate_rus <- get_vb_value("years of purely intrastate conflict", "Russia")

vb_years_home_defensive_ukr <- get_vb_value(
  "years on own territory, defensive", "Ukraine"
)
vb_years_home_defensive_rus <- get_vb_value(
  "years on own territory, defensive", "Russia"
)

vb_years_abroad_ukr <- get_vb_value("years abroad", "Ukraine")
vb_years_abroad_rus <- get_vb_value("years abroad", "Russia")

vb_conflicts_support_ukr <- get_vb_value("conflicts in a supporting role", "Ukraine")
vb_conflicts_support_rus <- get_vb_value("conflicts in a supporting role", "Russia")

# years_interstate_alt is retained as a workbook-level audit field.
n_alt_differs <- sum(
  as.numeric(dat_primary$years_interstate_alt) !=
    as.numeric(dat_primary$years_interstate_ucdp),
  na.rm = TRUE
)

if (n_alt_differs != 2) {
  warning(
    paste0(
      "Expected years_interstate_alt to differ from UCDP coding for 2 countries; found ",
      n_alt_differs, "."
    ),
    call. = FALSE
  )
}

# ============================================================
# 4. CONSTRUCT VARIANT B VARIABLES
# ============================================================

clust_data <- dat_primary %>%
  dplyr::mutate(
    conflicts_primary_b = dplyr::case_when(
      country == "Russia" ~ vb_conflicts_primary_rus,
      country == "Ukraine" ~ vb_conflicts_primary_ukr,
      TRUE ~ as.numeric(conflicts_primary)
    ),

    years_primary_b = dplyr::case_when(
      country == "Russia" ~ vb_years_primary_rus,
      country == "Ukraine" ~ vb_years_primary_ukr,
      TRUE ~ as.numeric(years_primary)
    ),

    years_interstate_b = dplyr::case_when(
      country == "Russia" ~ vb_years_interstate_rus,
      country == "Ukraine" ~ vb_years_interstate_ukr,
      TRUE ~ as.numeric(years_interstate_ucdp)
    ),

    years_intl_intrastate_b = dplyr::case_when(
      country == "Russia" ~ vb_years_intl_intrastate_rus,
      country == "Ukraine" ~ vb_years_intl_intrastate_ukr,
      TRUE ~ as.numeric(years_intl_intrastate_ucdp)
    ),

    years_intrastate_b = dplyr::case_when(
      country == "Russia" ~ vb_years_intrastate_rus,
      country == "Ukraine" ~ vb_years_intrastate_ukr,
      TRUE ~ as.numeric(years_intrastate)
    ),

    years_home_defensive_b = dplyr::case_when(
      country == "Russia" ~ vb_years_home_defensive_rus,
      country == "Ukraine" ~ vb_years_home_defensive_ukr,
      TRUE ~ as.numeric(years_home_defensive)
    ),

    # No separate Variant B value is supplied for home-civil years.
    years_home_civil_b = as.numeric(years_home_civil),

    years_abroad_b = dplyr::case_when(
      country == "Russia" ~ vb_years_abroad_rus,
      country == "Ukraine" ~ vb_years_abroad_ukr,
      TRUE ~ as.numeric(years_abroad)
    ),

    conflicts_support_b = dplyr::case_when(
      country == "Russia" ~ vb_conflicts_support_rus,
      country == "Ukraine" ~ vb_conflicts_support_ukr,
      TRUE ~ as.numeric(conflicts_support)
    ),

    # A. Magnitude
    n_conflicts = log1p(conflicts_primary_b),
    total_exposure = log1p(years_primary_b),

    # B. Temporal structure / intensity
    war_share = as.numeric(years_war_level) / years_primary_b,
    continuity = as.numeric(longest_spell) / years_primary_b,
    recency = as.numeric(years_since_last_primary),

    # C. Conflict type
    interstate_share = years_interstate_b / years_primary_b,
    intl_intrastate_share = years_intl_intrastate_b / years_primary_b,
    intrastate_share = years_intrastate_b / years_primary_b,

    # D. Locus
    home_defensive_share = years_home_defensive_b / years_primary_b,
    home_civil_share = years_home_civil_b / years_primary_b,
    abroad_share = years_abroad_b / years_primary_b
  )

variant_b_check <- clust_data %>%
  dplyr::filter(country %in% c("Ukraine", "Russia")) %>%
  dplyr::select(
    country,
    conflicts_primary,
    conflicts_primary_b,
    years_primary,
    years_primary_b,
    years_interstate_ucdp,
    years_interstate_alt,
    years_interstate_b,
    years_intl_intrastate_ucdp,
    years_intl_intrastate_b,
    years_intrastate,
    years_intrastate_b,
    years_home_defensive,
    years_home_defensive_b,
    years_home_civil,
    years_home_civil_b,
    years_abroad,
    years_abroad_b,
    years_war_level,
    longest_spell,
    years_since_last_primary
  )

# ============================================================
# 5. DEFINE THE FINAL 11-VARIABLE CONSTRUCT
# ============================================================

hca_vars_primary <- c(
  "n_conflicts",
  "total_exposure",
  "war_share",
  "continuity",
  "recency",
  "interstate_share",
  "intl_intrastate_share",
  "intrastate_share",
  "home_defensive_share",
  "home_civil_share",
  "abroad_share"
)

stopifnot(length(hca_vars_primary) == 11)

raw_profile_vars <- c(
  "conflicts_primary_b",
  "years_primary_b",
  "years_war_level",
  "longest_spell",
  "years_since_last_primary",
  "years_interstate_b",
  "years_intl_intrastate_b",
  "years_intrastate_b",
  "years_home_defensive_b",
  "years_home_civil_b",
  "years_abroad_b"
)

analysis_data <- clust_data %>%
  dplyr::select(
    country,
    dplyr::all_of(hca_vars_primary),
    dplyr::all_of(raw_profile_vars)
  )

missing_summary <- tibble::tibble(
  variable = hca_vars_primary,
  n_missing = vapply(
    analysis_data[hca_vars_primary],
    function(x) sum(is.na(x)),
    integer(1)
  ),
  pct_missing = vapply(
    analysis_data[hca_vars_primary],
    function(x) mean(is.na(x)) * 100,
    numeric(1)
  )
)

if (any(missing_summary$n_missing > 0)) {
  stop(
    "The 11-variable model contains missing values. No imputation is used in this model."
  )
}

# ============================================================
# 6. ANALYTICAL MATRIX AND DIAGNOSTICS
# ============================================================

X <- analysis_data %>%
  dplyr::select(dplyr::all_of(hca_vars_primary)) %>%
  as.data.frame()

rownames(X) <- analysis_data$country

if (any(!vapply(X, is.numeric, logical(1)))) {
  stop("At least one 11-variable input is not numeric.")
}

if (any(vapply(X, function(x) any(!is.finite(x)), logical(1)))) {
  stop("Non-finite values are present in the 11-variable analytical matrix.")
}

zero_var <- vapply(X, function(x) length(unique(x)) <= 1, logical(1))
if (any(zero_var)) {
  stop(
    paste0(
      "Zero-variance analytical variables: ",
      paste(names(zero_var)[zero_var], collapse = ", ")
    )
  )
}

unique_summary <- tibble::tibble(
  variable = colnames(X),
  n_unique = vapply(X, function(x) length(unique(x)), integer(1)),
  n_zero = vapply(X, function(x) sum(x == 0), integer(1)),
  pct_zero = vapply(X, function(x) mean(x == 0) * 100, numeric(1))
)

cor_mat <- stats::cor(
  X,
  method = "spearman",
  use = "pairwise.complete.obs"
)

idx <- which(
  abs(cor_mat) > 0.85 & lower.tri(cor_mat),
  arr.ind = TRUE
)

if (nrow(idx) > 0) {
  high_correlations <- tibble::tibble(
    variable1 = rownames(cor_mat)[idx[, 1]],
    variable2 = colnames(cor_mat)[idx[, 2]],
    spearman_rho = as.numeric(cor_mat[idx])
  ) %>%
    dplyr::arrange(desc(abs(spearman_rho)))
} else {
  high_correlations <- tibble::tibble(
    variable1 = character(),
    variable2 = character(),
    spearman_rho = numeric()
  )
}

closure_check <- analysis_data %>%
  dplyr::transmute(
    country,
    sum_type = interstate_share + intl_intrastate_share + intrastate_share,
    sum_locus = home_defensive_share + home_civil_share + abroad_share,
    type_eq_1 = abs(sum_type - 1) < 1e-9,
    locus_eq_1 = abs(sum_locus - 1) < 1e-9
  )

# ============================================================
# 7. STANDARDIZATION, EQUAL WEIGHTS, DISTANCE
# ============================================================

X_z <- scale(X)

variable_weights <- rep(
  1 / length(hca_vars_primary),
  length(hca_vars_primary)
)
names(variable_weights) <- hca_vars_primary
variable_weights <- variable_weights[colnames(X_z)]

stopifnot(abs(sum(variable_weights) - 1) < 1e-12)

# Equal weighting implemented as sqrt(w_j) scaling before Euclidean distance.
X_weighted <- sweep(
  X_z,
  2,
  sqrt(variable_weights),
  FUN = "*"
)

D_weighted <- stats::dist(
  X_weighted,
  method = "euclidean"
)

# ============================================================
# 8. WARD.D2 HIERARCHICAL CLUSTERING
# ============================================================

hc <- stats::hclust(
  D_weighted,
  method = "ward.D2"
)

# ============================================================
# 9. SILHOUETTE FOR ALL k = 2,...,10
# ============================================================

membership_list <- list()
silhouette_solution_rows <- list()
silhouette_cluster_rows <- list()
case_silhouette_rows <- list()

for (k in K_GRID) {

  groups <- stats::cutree(hc, k = k)
  membership_list[[as.character(k)]] <- groups

  sil <- cluster::silhouette(groups, D_weighted)
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
  dplyr::mutate(silhouette_rank = dplyr::row_number(), .before = 1)

silhouette_by_cluster_all_k <- dplyr::bind_rows(silhouette_cluster_rows)
case_silhouette_all_k <- dplyr::bind_rows(case_silhouette_rows)

top3_solutions <- silhouette_all_k %>%
  dplyr::slice_head(n = N_TOP_SOLUTIONS) %>%
  dplyr::select(silhouette_rank, k, dplyr::everything())

top3_ks <- top3_solutions$k

cat("\nTop-3 solutions by mean silhouette:\n")
print(top3_solutions, n = Inf)

# ============================================================
# 10. TOP-3 MEMBERSHIP AND UKRAINE CLUSTER
# ============================================================

top3_membership <- dplyr::bind_rows(
  lapply(
    seq_len(nrow(top3_solutions)),
    function(i) {
      k <- top3_solutions$k[i]
      groups <- membership_list[[as.character(k)]]

      tibble::tibble(
        approach = "11var",
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
      dplyr::count(solution_rank, k, cluster, name = "cluster_n"),
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
# 11. BOOTSTRAP STABILITY FOR TOP-3 SOLUTIONS ONLY
# ============================================================

bootstrap_rows <- vector("list", nrow(top3_solutions))

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]
  cat("Bootstrap solution rank", i, "(k =", k, "), B =", BOOT_B, "\n")

  set.seed(BOOT_SEED + k)

  cb <- fpc::clusterboot(
    X_weighted,
    B = BOOT_B,
    clustermethod = fpc::hclustCBI,
    k = k,
    method = "ward.D2",
    scaling = FALSE,
    dissolution = BOOT_DISSOLUTION_THRESHOLD,
    recover = BOOT_RECOVERY_THRESHOLD
  )

  bootstrap_rows[[i]] <- tibble::tibble(
    approach = "11var",
    solution_rank = i,
    k = k,
    cluster = seq_along(cb$bootmean),
    jaccard = as.numeric(cb$bootmean),
    dissolved_n = as.integer(cb$bootbrd),
    recovered_n = as.integer(cb$bootrecover),
    dissolution_pct = as.numeric(cb$bootbrd) / BOOT_B * 100,
    recovery_pct = as.numeric(cb$bootrecover) / BOOT_B * 100
  )
}

bootstrap_top3 <- dplyr::bind_rows(bootstrap_rows)

# ============================================================
# 12. COMBINED CLUSTER-LEVEL DIAGNOSTICS FOR TOP-3
# ============================================================

cluster_diagnostics_top3 <- silhouette_by_cluster_all_k %>%
  dplyr::filter(k %in% top3_ks) %>%
  dplyr::left_join(
    top3_solutions %>%
      dplyr::select(solution_rank = silhouette_rank, k, solution_mean_silhouette = mean_silhouette),
    by = "k"
  ) %>%
  dplyr::left_join(
    bootstrap_top3,
    by = c("solution_rank", "k", "cluster")
  ) %>%
  dplyr::left_join(
    ukraine_cluster_top3 %>%
      dplyr::select(solution_rank, k, ukraine_cluster),
    by = c("solution_rank", "k")
  ) %>%
  dplyr::mutate(
    approach = "11var",
    contains_ukraine = cluster == ukraine_cluster,
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
  dplyr::arrange(solution_rank, cluster)

# ============================================================
# 13. STANDARDIZED CENTROIDS FOR TOP-3 SOLUTIONS
# ============================================================

centroid_wide_rows <- vector("list", nrow(top3_solutions))

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]
  groups <- membership_list[[as.character(k)]]

  tmp <- as.data.frame(X_z) %>%
    tibble::rownames_to_column("country") %>%
    dplyr::mutate(cluster = as.integer(groups[country]))

  centroid_wide_rows[[i]] <- tmp %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(dplyr::all_of(hca_vars_primary), mean),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      approach = "11var",
      solution_rank = i,
      k = k,
      .before = 1
    )
}

centroids_top3_wide <- dplyr::bind_rows(centroid_wide_rows)

centroids_top3_long <- centroids_top3_wide %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(hca_vars_primary),
    names_to = "variable",
    values_to = "centroid_z"
  ) %>%
  dplyr::arrange(solution_rank, cluster, variable)

# ============================================================
# 14. RAW MEDIAN PROFILES FOR TOP-3 SOLUTIONS
# ============================================================

raw_profile_rows <- vector("list", nrow(top3_solutions))

for (i in seq_len(nrow(top3_solutions))) {

  k <- top3_solutions$k[i]
  groups <- membership_list[[as.character(k)]]

  raw_profile_rows[[i]] <- analysis_data %>%
    dplyr::mutate(cluster = as.integer(groups[country])) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(
        dplyr::all_of(raw_profile_vars),
        ~ median(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      approach = "11var",
      solution_rank = i,
      k = k,
      .before = 1
    )
}

raw_medians_top3 <- dplyr::bind_rows(raw_profile_rows)

# ============================================================
# 15. GLOBAL TOP-10 NEAREST NEIGHBOURS OF UKRAINE IN 11D SPACE
# ============================================================

D_matrix <- as.matrix(D_weighted)

ukraine_top10_global <- tibble::tibble(
  approach = "11var",
  country = rownames(D_matrix),
  distance_to_ukraine = as.numeric(D_matrix["Ukraine", ])
) %>%
  dplyr::filter(country != "Ukraine") %>%
  dplyr::arrange(distance_to_ukraine, country) %>%
  dplyr::mutate(neighbor_rank = dplyr::row_number(), .before = 2) %>%
  dplyr::slice_head(n = 10)

# Repeated with solution metadata only to support one publication table
# across approaches and top-3 solutions. Distances do not depend on k.
ukraine_top10_by_solution <- dplyr::bind_rows(
  lapply(
    seq_len(nrow(top3_solutions)),
    function(i) {
      k <- top3_solutions$k[i]
      ukr_cluster <- ukraine_cluster_top3$ukraine_cluster[
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
# 16. DENDROGRAM
# ============================================================

dendrogram_file <- file.path(OUT_DIR, "HCA_11var_dendrogram.pdf")

pdf(
  dendrogram_file,
  width = 16,
  height = 9,
  onefile = TRUE
)

plot(
  hc,
  labels = rownames(X_weighted),
  cex = 0.55,
  hang = -1,
  main = "State war-experience profiles — 11-variable Variant B model",
  xlab = "",
  sub = "",
  ylab = "Ward distance"
)

dev.off()

# ============================================================
# 17. EXPORT CSV TABLES
# ============================================================

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
  file.path(OUT_DIR, "top3_cluster_diagnostics.csv")
)

readr::write_csv(
  top3_membership,
  file.path(OUT_DIR, "top3_cluster_membership.csv")
)

readr::write_csv(
  ukraine_cluster_top3,
  file.path(OUT_DIR, "ukraine_cluster_top3.csv")
)

readr::write_csv(
  centroids_top3_long,
  file.path(OUT_DIR, "top3_centroids_z_long.csv")
)

readr::write_csv(
  ukraine_top10_global,
  file.path(OUT_DIR, "ukraine_top10_global.csv")
)

readr::write_csv(
  ukraine_top10_by_solution,
  file.path(OUT_DIR, "ukraine_top10_by_solution.csv")
)

# ============================================================
# 18. EXPORT EXCEL WORKBOOK
# ============================================================

analysis_settings <- tibble::tibble(
  setting = c(
    "input_file",
    "input_sheet",
    "population_filter",
    "n_countries",
    "n_variables",
    "transform_n_conflicts",
    "transform_total_exposure",
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
    nrow(X),
    ncol(X),
    "log1p",
    "log1p",
    "z-score",
    "equal, 1/11 each",
    "weighted Euclidean",
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
  variant_B_check = variant_b_check,
  missingness = missing_summary,
  unique_values = unique_summary,
  high_correlations = high_correlations,
  closure_check = closure_check,
  variable_weights = tibble::tibble(
    variable = names(variable_weights),
    weight = as.numeric(variable_weights)
  ),
  silhouette_all_k = silhouette_all_k,
  top3_solutions = top3_solutions,
  top3_cluster_diagnostics = cluster_diagnostics_top3,
  top3_membership = top3_membership,
  ukraine_cluster_top3 = ukraine_cluster_top3,
  top3_centroids_z = centroids_top3_wide,
  top3_centroids_z_long = centroids_top3_long,
  top3_raw_medians = raw_medians_top3,
  bootstrap_top3 = bootstrap_top3,
  ukraine_top10_global = ukraine_top10_global,
  ukraine_top10_by_solution = ukraine_top10_by_solution
)

xlsx_file <- file.path(OUT_DIR, "HCA_11var_results.xlsx")
writexl::write_xlsx(xlsx_output, xlsx_file)

# ============================================================
# 19. SAVE REPRODUCIBILITY OBJECT
# ============================================================

reproducibility_objects <- list(
  script_version = "HCA_11var — 2026-09-12",
  input_file = normalizePath(INPUT_FILE),
  input_sheet = INPUT_SHEET,
  hca_vars_primary = hca_vars_primary,
  raw_profile_vars = raw_profile_vars,
  analysis_data = analysis_data,
  X = X,
  X_z = X_z,
  variable_weights = variable_weights,
  X_weighted = X_weighted,
  D_weighted = D_weighted,
  hc = hc,
  K_GRID = K_GRID,
  top3_solutions = top3_solutions,
  membership_list = membership_list,
  case_silhouette_all_k = case_silhouette_all_k,
  cluster_diagnostics_top3 = cluster_diagnostics_top3,
  bootstrap_top3 = bootstrap_top3,
  centroids_top3_wide = centroids_top3_wide,
  centroids_top3_long = centroids_top3_long,
  raw_medians_top3 = raw_medians_top3,
  ukraine_cluster_top3 = ukraine_cluster_top3,
  ukraine_top10_global = ukraine_top10_global,
  ukraine_top10_by_solution = ukraine_top10_by_solution,
  variant_b_check = variant_b_check
)

rds_file <- file.path(OUT_DIR, "HCA_11var_results.rds")
saveRDS(reproducibility_objects, rds_file)

capture.output(
  sessionInfo(),
  file = file.path(OUT_DIR, "sessionInfo.txt")
)

# ============================================================
# 20. FINISH
# ============================================================

cat("\n============================================================\n")
cat("11-VARIABLE ANALYSIS COMPLETE\n")
cat("============================================================\n")
cat("Countries:", nrow(X), "\n")
cat("Top-3 k:", paste(top3_ks, collapse = ", "), "\n")
cat("Excel:", xlsx_file, "\n")
cat("RDS:", rds_file, "\n")
cat("Dendrogram:", dendrogram_file, "\n")
