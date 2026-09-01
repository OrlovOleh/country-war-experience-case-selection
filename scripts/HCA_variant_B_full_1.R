# ============================================================
# EXTENDED HIERARCHICAL CLUSTER ANALYSIS
# POST-1945 WAR PARTICIPATION AND MILITARY PROFILE
# PRIMARY MODEL = VARIANT B
#
# Variant B:
#   - UCDP conflicts 13246, 13247 and 13306 (2014-2022)
#     are treated as interstate rather than internationalized
#     intrastate conflict.
#   - Ukraine and Russia are treated as primary parties.
#   - All other conflicts retain their original coding.
#
# PRIMARY MODEL:
#   15 variables
#   equal variable weights (1/15)
#   6 conceptual domains retained for interpretation/sensitivity
#
# CLUSTERING:
#   z-standardization
#   equal-variable weighting
#   Euclidean distance
#   Ward.D2 linkage
#
# SENSITIVITY:
#   - equal-variable Euclidean nearest neighbours
#   - Manhattan nearest neighbours
#   - casualty model (+ battle deaths; complete cases only)
#   - Variant A (original UCDP classification)
#   - complete-case model without expenditure imputation
#   - domain-balanced weighting
#
# IMPORTANT:
#   The workbook's variant_A_vs_B sheet explicitly provides the
#   Variant B values used for primary-party conflict counts/years,
#   conflict-type years, defensive home-territory years, years
#   abroad, and supporting-role conflict counts for Ukraine/Russia.
#
#   It does not provide alternative values for years_war_level
#   or longest_spell. These numerators are therefore retained
#   from country_profile, while Variant B years_primary is used
#   as the denominator for war_share and continuity.
# ============================================================


cat("\nSCRIPT VERSION: HCA_variant_B_full_1 — 2026-08-31 fix-5\n")

# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "readxl",
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "ggplot2",
  "cluster",
  "factoextra",
  "fpc",
  "purrr",
  "writexl",
  "mclust"
)

# Package installation is intentionally not performed here.
# Required packages should be installed before running the script.
invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)


# ============================================================
# 1. IMPORT DATA
# ============================================================

# Repository-relative paths. Run this script from the repository root.
input_file <- file.path("data", "war_participation_variables_v8.xlsx")
RESULTS_DIR <- "results"
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

dat <- read_excel(
  input_file,
  sheet = "country_profile",
  na = c("", "NA", "N/A", "n/a")
)

cat("Dataset dimensions:", dim(dat), "\n")


# ============================================================
# 2. RETAIN PRIMARY-PARTY COUNTRIES
# ============================================================

cat("\nRole classes:\n")

print(
  table(
    dat$role_class,
    useNA = "ifany"
  )
)

dat_primary <- dat %>%
  filter(
    role_class == "primary party"
  )

cat(
  "\nPrimary-party countries:",
  nrow(dat_primary),
  "\n"
)


# ============================================================
# 3. CONSTRUCT VARIANT B RAW VARIABLES
# ============================================================
#
# Variant B values for Ukraine and Russia are read directly from
# the workbook sheet variant_A_vs_B. All other countries retain
# the original country_profile coding.
#
# The table occupies A4:E13. It is read as TEXT deliberately so
# the mixed text/numeric first data row cannot affect readxl's
# column-type guessing. Indicator labels are normalized before
# matching, and every required value is resolved once BEFORE
# dplyr::case_when() is evaluated.
# ============================================================

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

vb_raw <- read_excel(
  input_file,
  sheet = "variant_A_vs_B",
  range = "A4:E13",
  col_types = rep("text", 5)
)

names(vb_raw)[1] <- "indicator_raw"

vb <- vb_raw %>%
  transmute(
    indicator = normalize_vb_indicator(indicator_raw),
    Ukraine_text = `Ukraine, variant B`,
    Russia_text = `Russia / USSR, variant B`
  ) %>%
  filter(indicator %in% vb_indicators) %>%
  mutate(
    Ukraine = readr::parse_double(Ukraine_text),
    Russia = readr::parse_double(Russia_text)
  ) %>%
  select(indicator, Ukraine, Russia)

missing_vb_indicators <- setdiff(vb_indicators, vb$indicator)
duplicated_vb_indicators <- unique(vb$indicator[duplicated(vb$indicator)])

if (length(missing_vb_indicators) > 0) {
  stop(
    paste0(
      "Missing Variant B indicators in workbook: ",
      paste(missing_vb_indicators, collapse = "; ")
    )
  )
}

if (length(duplicated_vb_indicators) > 0) {
  stop(
    paste0(
      "Duplicated Variant B indicators in workbook: ",
      paste(duplicated_vb_indicators, collapse = "; ")
    )
  )
}

if (nrow(vb) != length(vb_indicators)) {
  stop("Variant B table does not contain exactly the 8 required indicators.")
}

if (anyNA(vb$Ukraine) || anyNA(vb$Russia)) {
  bad <- vb %>% filter(is.na(Ukraine) | is.na(Russia))
  print(bad, width = Inf)
  stop("At least one required Variant B workbook value is not numeric.")
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

cat("\nVariant B values parsed from workbook (validated table):\n")
print(vb, width = Inf)

# Resolve all workbook values ONCE. These scalars are then used in
# case_when(), avoiding repeated string lookup during mutate().
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

# The workbook states that years_interstate_alt differs from the
# original UCDP coding only for Russia and Ukraine.
n_alt_differs <- sum(
  as.numeric(dat_primary$years_interstate_alt) !=
    as.numeric(dat_primary$years_interstate_ucdp),
  na.rm = TRUE
)
stopifnot(n_alt_differs == 2)

clust_data <- dat_primary %>%
  mutate(

    # --------------------------------------------------------
    # VARIANT B RAW COUNTS / YEARS
    # --------------------------------------------------------

    conflicts_primary_b = case_when(
      country == "Russia" ~ vb_conflicts_primary_rus,
      country == "Ukraine" ~ vb_conflicts_primary_ukr,
      TRUE ~ as.numeric(conflicts_primary)
    ),

    years_primary_b = case_when(
      country == "Russia" ~ vb_years_primary_rus,
      country == "Ukraine" ~ vb_years_primary_ukr,
      TRUE ~ as.numeric(years_primary)
    ),

    years_interstate_b = case_when(
      country == "Russia" ~ vb_years_interstate_rus,
      country == "Ukraine" ~ vb_years_interstate_ukr,
      TRUE ~ as.numeric(years_interstate_ucdp)
    ),

    years_intl_intrastate_b = case_when(
      country == "Russia" ~ vb_years_intl_intrastate_rus,
      country == "Ukraine" ~ vb_years_intl_intrastate_ukr,
      TRUE ~ as.numeric(years_intl_intrastate_ucdp)
    ),

    years_intrastate_b = case_when(
      country == "Russia" ~ vb_years_intrastate_rus,
      country == "Ukraine" ~ vb_years_intrastate_ukr,
      TRUE ~ as.numeric(years_intrastate)
    ),

    years_home_defensive_b = case_when(
      country == "Russia" ~ vb_years_home_defensive_rus,
      country == "Ukraine" ~ vb_years_home_defensive_ukr,
      TRUE ~ as.numeric(years_home_defensive)
    ),

    # No separate Variant B value is supplied for home-civil years.
    years_home_civil_b = as.numeric(years_home_civil),

    years_abroad_b = case_when(
      country == "Russia" ~ vb_years_abroad_rus,
      country == "Ukraine" ~ vb_years_abroad_ukr,
      TRUE ~ as.numeric(years_abroad)
    ),

    conflicts_support_b = case_when(
      country == "Russia" ~ vb_conflicts_support_rus,
      country == "Ukraine" ~ vb_conflicts_support_ukr,
      TRUE ~ as.numeric(conflicts_support)
    ),

    # --------------------------------------------------------
    # A. CONFLICT MAGNITUDE
    # --------------------------------------------------------

    n_conflicts = log1p(conflicts_primary_b),
    total_exposure = log1p(years_primary_b),

    # --------------------------------------------------------
    # B. TEMPORAL STRUCTURE / INTENSITY
    # --------------------------------------------------------
    # Variant B does not provide alternative years_war_level or
    # longest_spell values. The source numerators are retained.

    war_share = years_war_level / years_primary_b,
    continuity = longest_spell / years_primary_b,
    recency = years_since_last_primary,

    # --------------------------------------------------------
    # C. CONFLICT TYPE — CONSISTENT VARIANT B
    # --------------------------------------------------------

    interstate_share = years_interstate_b / years_primary_b,
    intl_intrastate_share = years_intl_intrastate_b / years_primary_b,
    intrastate_share = years_intrastate_b / years_primary_b,

    # --------------------------------------------------------
    # D. LOCUS — CONSISTENT VARIANT B
    # --------------------------------------------------------

    home_defensive_share = years_home_defensive_b / years_primary_b,
    home_civil_share = years_home_civil_b / years_primary_b,
    abroad_share = years_abroad_b / years_primary_b,

    # --------------------------------------------------------
    # E. POPULATION / MILITARY SCALE
    # --------------------------------------------------------

    population_size = log1p(mil_personnel_latest_population_k),
    personnel_size = log1p(mil_personnel_latest_k),
    personnel_share = mil_personnel_pct_pop_latest,

    # --------------------------------------------------------
    # F. CASUALTIES
    # --------------------------------------------------------

    battle_deaths = log1p(battle_deaths_cow),

    # --------------------------------------------------------
    # G. MILITARY EXPENDITURE
    # --------------------------------------------------------

    mil_expenditure = readr::parse_number(mil_expenditure_pct_gdp)
  )


# ============================================================
# 4. VERIFY VARIANT B RECODING
# ============================================================

variant_b_check <- clust_data %>%
  filter(country %in% c("Ukraine", "Russia")) %>%
  select(
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
    conflicts_support,
    conflicts_support_b,
    years_war_level,
    longest_spell,
    years_since_last_primary,
    interstate_share,
    intl_intrastate_share,
    intrastate_share,
    home_defensive_share,
    home_civil_share,
    abroad_share
  )

cat("\nVariant B verification — Ukraine and Russia:\n")
print(variant_b_check, width = Inf)

vb_expected <- tibble::tibble(
  country = c("Ukraine", "Russia"),
  conflicts_primary_b = c(vb_conflicts_primary_ukr, vb_conflicts_primary_rus),
  years_primary_b = c(vb_years_primary_ukr, vb_years_primary_rus),
  years_interstate_b = c(vb_years_interstate_ukr, vb_years_interstate_rus),
  years_intl_intrastate_b = c(
    vb_years_intl_intrastate_ukr,
    vb_years_intl_intrastate_rus
  ),
  years_intrastate_b = c(vb_years_intrastate_ukr, vb_years_intrastate_rus),
  years_home_defensive_b = c(
    vb_years_home_defensive_ukr,
    vb_years_home_defensive_rus
  ),
  years_abroad_b = c(vb_years_abroad_ukr, vb_years_abroad_rus),
  conflicts_support_b = c(vb_conflicts_support_ukr, vb_conflicts_support_rus)
)

vb_actual <- clust_data %>%
  filter(country %in% c("Ukraine", "Russia")) %>%
  select(all_of(names(vb_expected))) %>%
  arrange(match(country, c("Ukraine", "Russia")))

vb_expected <- vb_expected %>%
  arrange(match(country, c("Ukraine", "Russia")))

stopifnot(identical(vb_actual$country, vb_expected$country))

for (nm in setdiff(names(vb_expected), "country")) {
  stopifnot(
    isTRUE(
      all.equal(
        as.numeric(vb_actual[[nm]]),
        as.numeric(vb_expected[[nm]]),
        tolerance = 1e-12,
        check.attributes = FALSE
      )
    )
  )
}

cat("\nVariant B recoding checks passed.\n")


# ============================================================
# 5. VERIFY MILITARY-EXPENDITURE PARSING
# ============================================================

cat("\nMilitary expenditure type:\n")
print(
  class(
    clust_data$mil_expenditure
  )
)

cat("\nMilitary expenditure summary:\n")
print(
  summary(
    clust_data$mil_expenditure
  )
)


# ============================================================
# 6. DEFINE PRIMARY HCA VARIABLES
# ============================================================
#
# Primary HCA = 15 variables.
# Бойові втрати виключено з основної моделі: у COW відсутнє
# значення для України (загалом 4 пропуски зі 121). Включення
# показника вилучило б з аналізу саме ту державу, навколо якої
# він будується. Використовується лише в моделі чутливості
# (секція 40) за повними випадками.
# ============================================================

hca_vars_primary <- c(

  # Conflict magnitude
  "n_conflicts",
  "total_exposure",

  # Temporal / intensity
  "war_share",
  "continuity",
  "recency",

  # Conflict type
  "interstate_share",
  "intl_intrastate_share",
  "intrastate_share",

  # Locus
  "home_defensive_share",
  "home_civil_share",
  "abroad_share",

  # Population / military scale
  "population_size",
  "personnel_size",
  "personnel_share",

  # Military expenditure
  "mil_expenditure"
)

stopifnot(
  length(hca_vars_primary) == 15
)


# ============================================================
# 7. MISSINGNESS BEFORE IMPUTATION
# ============================================================

missing_summary_primary <- data.frame(

  variable =
    hca_vars_primary,

  n_missing =
    sapply(
      clust_data[hca_vars_primary],
      function(x) sum(is.na(x))
    ),

  pct_missing =
    sapply(
      clust_data[hca_vars_primary],
      function(x) mean(is.na(x)) * 100
    )

) %>%
  arrange(
    desc(pct_missing)
  )

cat("\nPrimary-model missingness:\n")

print(
  missing_summary_primary,
  row.names = FALSE
)


# ============================================================
# 8. IDENTIFY COUNTRIES MISSING MILITARY EXPENDITURE
# ============================================================

missing_expenditure_countries <- clust_data %>%
  filter(
    is.na(mil_expenditure)
  ) %>%
  select(
    country,
    mil_expenditure_pct_gdp
  )

cat("\nCountries missing military expenditure:\n")

print(
  missing_expenditure_countries
)


# ============================================================
# 9. IMPUTE MISSING MILITARY EXPENDITURE
# ============================================================
#
# Median imputation is used ONLY for military expenditure.
# ============================================================

mil_exp_median <- median(
  clust_data$mil_expenditure,
  na.rm = TRUE
)

cat(
  "\nMedian military expenditure used for imputation:",
  mil_exp_median,
  "\n"
)

clust_data <- clust_data %>%
  mutate(

    mil_expenditure_missing =
      ifelse(
        is.na(mil_expenditure),
        1,
        0
      ),

    mil_expenditure =
      ifelse(
        is.na(mil_expenditure),
        mil_exp_median,
        mil_expenditure
      )
  )


# ============================================================
# 10. VERIFY COMPLETE PRIMARY MODEL
# ============================================================

primary_complete <- complete.cases(
  clust_data[hca_vars_primary]
)

cat(
  "\nComplete primary-model cases:",
  sum(primary_complete),
  "of",
  nrow(clust_data),
  "\n"
)

cat(
  "Ukraine retained:",
  "Ukraine" %in% clust_data$country[primary_complete],
  "\n"
)

analysis_data <- clust_data %>%
  filter(
    complete.cases(
      across(
        all_of(hca_vars_primary)
      )
    )
  )


# ============================================================
# 11. BUILD PRIMARY ANALYTICAL MATRIX
# ============================================================

X <- analysis_data %>%
  select(
    all_of(hca_vars_primary)
  ) %>%
  as.data.frame()

rownames(X) <-
  analysis_data$country


# ============================================================
# 12. VERIFY ALL VARIABLES ARE NUMERIC
# ============================================================

numeric_check <- sapply(
  X,
  is.numeric
)

cat("\nNumeric-variable check:\n")
print(numeric_check)

if (!all(numeric_check)) {
  stop(
    paste(
      "Non-numeric variables remain:",
      paste(
        names(numeric_check)[!numeric_check],
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 13. DESCRIPTIVE DISTRIBUTIONS
# ============================================================

cat("\nVariable summaries:\n")
print(
  summary(X)
)

unique_summary <- data.frame(

  variable =
    colnames(X),

  n_unique =
    sapply(
      X,
      function(x) length(unique(x))
    ),

  n_zero =
    sapply(
      X,
      function(x) sum(
        x == 0,
        na.rm = TRUE
      )
    ),

  pct_zero =
    sapply(
      X,
      function(x) mean(
        x == 0,
        na.rm = TRUE
      ) * 100
    )
)

cat("\nUnique values / zero inflation:\n")

print(
  unique_summary,
  row.names = FALSE
)


# ============================================================
# 14. CORRELATION / REDUNDANCY CHECK
# ============================================================

cor_mat <- cor(
  X,
  method = "spearman",
  use = "pairwise.complete.obs"
)

cat("\nSpearman correlation matrix:\n")

print(
  round(
    cor_mat,
    2
  )
)

idx <- which(
  abs(cor_mat) > 0.85 &
    lower.tri(cor_mat),
  arr.ind = TRUE
)

if (nrow(idx) > 0) {

  high_correlations <- data.frame(

    variable1 =
      rownames(cor_mat)[idx[, 1]],

    variable2 =
      colnames(cor_mat)[idx[, 2]],

    rho =
      cor_mat[idx]

  ) %>%
    arrange(
      desc(abs(rho))
    )

} else {

  high_correlations <- data.frame(
    variable1 = character(),
    variable2 = character(),
    rho = numeric()
  )
}

cat("\nHigh correlations |rho| > .85:\n")
print(high_correlations)

# Diagnostic for closure of share groups. Values are retained as-is;
# this block only documents whether the component shares sum to one.
closure_check <- analysis_data %>%
  transmute(
    country,
    sum_type =
      interstate_share + intl_intrastate_share + intrastate_share,
    sum_locus =
      home_defensive_share + home_civil_share + abroad_share
  ) %>%
  mutate(
    type_eq_1 = abs(sum_type - 1) < 1e-9,
    locus_eq_1 = abs(sum_locus - 1) < 1e-9
  )

cat("\nЗамкненість часток:\n")
print(summary(closure_check[c("sum_type", "sum_locus")]))
print(table(closure_check$type_eq_1, closure_check$locus_eq_1))


# ============================================================
# 15. STANDARDIZE VARIABLES
# ============================================================

X_z <- scale(X)

cat("\nStandardized means:\n")

print(
  round(
    colMeans(X_z),
    4
  )
)

cat("\nStandardized SDs:\n")

print(
  round(
    apply(
      X_z,
      2,
      sd
    ),
    4
  )
)


# ============================================================
# 16. DEFINE CONCEPTUAL DOMAINS
# ============================================================
#
# Domains are retained for interpretation and sensitivity analysis.
# The PRIMARY model uses equal variable weights; the equal-domain
# weighting scheme is constructed separately in section 17.
# ============================================================

domains_primary <- list(

  conflict_magnitude = c(
    "n_conflicts",
    "total_exposure"
  ),

  temporal_intensity = c(
    "war_share",
    "continuity",
    "recency"
  ),

  conflict_type = c(
    "interstate_share",
    "intl_intrastate_share",
    "intrastate_share"
  ),

  locus = c(
    "home_defensive_share",
    "home_civil_share",
    "abroad_share"
  ),

  population_military_scale = c(
    "population_size",
    "personnel_size",
    "personnel_share"
  ),

  military_expenditure = c(
    "mil_expenditure"
  )
)


# ============================================================
# 17. CALCULATE VARIABLE WEIGHTS
# ============================================================

# Основна модель: рівні ваги
variable_weights <- rep(
  1 / length(hca_vars_primary),
  length(hca_vars_primary)
)
names(variable_weights) <- hca_vars_primary
variable_weights <- variable_weights[colnames(X_z)]

# Доменна схема — тільки для аналізу чутливості (секція 45)
domain_weight_scheme <- unlist(
  lapply(
    domains_primary,
    function(vars) {
      rep(
        (1 / length(domains_primary)) / length(vars),
        length(vars)
      )
    }
  )
)
names(domain_weight_scheme) <- unlist(domains_primary)
domain_weight_scheme <- domain_weight_scheme[colnames(X_z)]

stopifnot(
  abs(sum(variable_weights) - 1) < 1e-9,
  abs(sum(domain_weight_scheme) - 1) < 1e-9
)

cat("\nPrimary equal-variable weights:\n")
print(variable_weights)

cat("\nDomain weighting scheme for sensitivity analysis:\n")
print(domain_weight_scheme)


# ============================================================
# 18. APPLY VARIABLE WEIGHTS
# ============================================================
#
# sqrt(weight) is used because Euclidean distance squares the
# coordinate differences.
# ============================================================

X_weighted <- sweep(
  X_z,
  2,
  sqrt(variable_weights),
  FUN = "*"
)


# ============================================================
# 19. WEIGHTED EUCLIDEAN DISTANCE
# ============================================================

D_weighted <- dist(
  X_weighted,
  method = "euclidean"
)


# ============================================================
# 20. WARD.D2 HIERARCHICAL CLUSTERING
# ============================================================

hc <- hclust(
  D_weighted,
  method = "ward.D2"
)


# ============================================================
# 21. DENDROGRAM
# ============================================================

dendrogram_file <-
  "war_participation_HCA_extended_variant_B_dendrogram.pdf"

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
  main =
    "Extended post-1945 war-participation profiles — Variant B",
  xlab = "",
  sub = "",
  ylab = "Ward distance"
)

dev.off()

cat("\nDendrogram saved to:", dendrogram_file, "\n")


# ============================================================
# 22. SILHOUETTE ANALYSIS: k = 2 TO 10
# ============================================================

ks <- 2:10

silhouette_values <- sapply(
  ks,
  function(k) {

    groups <- cutree(
      hc,
      k = k
    )

    mean(
      silhouette(
        groups,
        D_weighted
      )[, "sil_width"]
    )
  }
)

silhouette_table <- data.frame(
  k = ks,
  mean_silhouette = silhouette_values
)

cat("\nSilhouette results — Variant B:\n")
print(silhouette_table)

best_k <- ks[
  which.max(
    silhouette_values
  )
]

cat(
  "\nBest silhouette k:",
  best_k,
  "\n"
)

print(
  ggplot(
    silhouette_table,
    aes(
      x = k,
      y = mean_silhouette
    )
  ) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = ks
    ) +
    labs(
      title =
        "Silhouette analysis — extended HCA, Variant B",
      x =
        "Number of clusters",
      y =
        "Mean silhouette width"
    ) +
    theme_minimal()
)


# ============================================================
# 23. k = 2 SOLUTION
# ============================================================

cluster2 <- cutree(
  hc,
  k = 2
)

cat("\nk = 2 cluster sizes:\n")
print(
  table(cluster2)
)

print(
  fviz_dend(
    hc,
    k = 2,
    rect = TRUE,
    cex = 0.55
  )
)


# ============================================================
# 24. k = 5 SOLUTION
# ============================================================

cluster5 <- cutree(
  hc,
  k = 5
)

cat("\nk = 5 cluster sizes:\n")
print(
  table(cluster5)
)

print(
  fviz_dend(
    hc,
    k = 5,
    rect = TRUE,
    cex = 0.55
  )
)

cat("\nNesting of k=5 inside k=2:\n")

print(
  table(
    cluster2,
    cluster5
  )
)


# ============================================================
# 25. CLUSTER-SPECIFIC SILHOUETTES
# ============================================================

sil2 <- silhouette(
  cluster2,
  D_weighted
)

sil5 <- silhouette(
  cluster5,
  D_weighted
)

cat("\nMean silhouette k=2:\n")
print(
  mean(
    sil2[, "sil_width"]
  )
)

cat("\nMean silhouette k=5:\n")
print(
  mean(
    sil5[, "sil_width"]
  )
)

cat("\nk=5 silhouette by cluster:\n")

print(
  aggregate(
    sil_width ~ cluster,
    data = as.data.frame(sil5),
    FUN = mean
  )
)


# ============================================================
# 26. BOOTSTRAP STABILITY — k = 2
# ============================================================

# Controls bootstrap resampling reproducibly.
set.seed(20260827)

boot2 <- clusterboot(
  X_weighted,
  B = 500,
  clustermethod = hclustCBI,
  k = 2,
  method = "ward.D2",
  scaling = FALSE
)

cat("\nk=2 bootstrap Jaccard:\n")
print(
  boot2$bootmean
)

cat("\nk=2 dissolved:\n")
print(
  boot2$bootbrd
)

cat("\nk=2 recovered:\n")
print(
  boot2$bootrecover
)


# ============================================================
# 27. BOOTSTRAP STABILITY — k = 5
# ============================================================

# Controls bootstrap resampling reproducibly.
set.seed(20260827)

boot5 <- clusterboot(
  X_weighted,
  B = 500,
  clustermethod = hclustCBI,
  k = 5,
  method = "ward.D2",
  scaling = FALSE
)

cat("\nk=5 bootstrap Jaccard:\n")
print(
  boot5$bootmean
)

cat("\nk=5 dissolved:\n")
print(
  boot5$bootbrd
)

cat("\nk=5 recovered:\n")
print(
  boot5$bootrecover
)


bootstrap_stability_table <- bind_rows(
  data.frame(
    k = 2,
    cluster = seq_along(boot2$bootmean),
    jaccard = as.numeric(boot2$bootmean),
    dissolved = as.numeric(boot2$bootbrd),
    recovered = as.numeric(boot2$bootrecover)
  ),
  data.frame(
    k = 5,
    cluster = seq_along(boot5$bootmean),
    jaccard = as.numeric(boot5$bootmean),
    dissolved = as.numeric(boot5$bootbrd),
    recovered = as.numeric(boot5$bootrecover)
  )
)


# ============================================================
# 28. ADD CLUSTER MEMBERSHIP
# ============================================================

stopifnot(
  identical(names(cluster2), names(cluster5))
)

cluster_membership <- tibble::tibble(
  country = names(cluster2),
  cluster2 = factor(unname(cluster2)),
  cluster5 = factor(unname(cluster5))
)

results <- analysis_data %>%
  left_join(
    cluster_membership,
    by = "country"
  )

stopifnot(
  nrow(results) == nrow(analysis_data),
  !anyNA(results$cluster2),
  !anyNA(results$cluster5)
)


# ============================================================
# 29. COUNTRY MEMBERSHIP
# ============================================================

countries2 <- results %>%
  select(
    all_of(c("country", "cluster2"))
  ) %>%
  arrange(
    cluster2,
    country
  )

countries5 <- results %>%
  select(
    all_of(c("country", "cluster5"))
  ) %>%
  arrange(
    cluster5,
    country
  )

cat("\nk=5 countries:\n")

print(
  split(
    countries5$country,
    countries5$cluster5
  )
)


# ============================================================
# 30. DERIVED CLUSTER PROFILES — MEDIANS
# ============================================================

profile_vars <- c(

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
  "abroad_share",

  "population_size",
  "personnel_size",
  "personnel_share",

  "mil_expenditure"
)

profile2 <- results %>%
  group_by(
    cluster2
  ) %>%
  summarise(
    n = n(),
    across(
      all_of(profile_vars),
      ~ median(
        .x,
        na.rm = TRUE
      )
    ),
    .groups = "drop"
  )

profile5 <- results %>%
  group_by(
    cluster5
  ) %>%
  summarise(
    n = n(),
    across(
      all_of(profile_vars),
      ~ median(
        .x,
        na.rm = TRUE
      )
    ),
    .groups = "drop"
  )

cat("\nk=2 median profiles:\n")
print(
  profile2,
  width = Inf
)

cat("\nk=5 median profiles:\n")
print(
  profile5,
  width = Inf
)


# ============================================================
# 31. RAW-SCALE CLUSTER PROFILES — VARIANT B
# ============================================================

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
  "years_abroad_b",

  "mil_personnel_latest_population_k",
  "mil_personnel_latest_k",
  "mil_personnel_pct_pop_latest",

  "mil_expenditure"
)

raw_profile5 <- results %>%
  group_by(
    cluster5
  ) %>%
  summarise(
    n = n(),
    across(
      all_of(raw_profile_vars),
      ~ median(
        .x,
        na.rm = TRUE
      )
    ),
    .groups = "drop"
  )

cat("\nRaw-scale k=5 profiles — Variant B:\n")

print(
  raw_profile5,
  width = Inf
)


# ============================================================
# 32. STANDARDIZED CENTROIDS
# ============================================================

centroids5 <- as.data.frame(
  X_z
) %>%
  mutate(
    cluster =
      factor(cluster5)
  ) %>%
  group_by(
    cluster
  ) %>%
  summarise(
    across(
      everything(),
      mean
    ),
    .groups = "drop"
  )

cat("\nk=5 standardized centroids:\n")

print(
  centroids5,
  width = Inf
)


# Initialize optional result objects explicitly so that a previously
# auto-loaded .RData workspace cannot leak stale objects into this run.
ukraine_neighbors <- data.frame()
neighbors_equal <- data.frame()
neighbors_manhattan <- data.frame()
neighbor_comparison <- data.frame()
ukraine_top20_profiles <- data.frame()
ukraine_neighbors_A <- data.frame()
variant_neighbor_comparison <- data.frame()

# ============================================================
# 33. UKRAINE NEAREST NEIGHBOURS — PRIMARY EQUAL-VARIABLE MODEL
# ============================================================

if ("Ukraine" %in% rownames(X_weighted)) {

  D_matrix <-
    as.matrix(
      D_weighted
    )

  ukraine_neighbors <- data.frame(

    country =
      rownames(X_weighted),

    distance =
      D_matrix[
        "Ukraine",
      ],

    cluster2 =
      unname(cluster2[rownames(X_weighted)]),

    cluster5 =
      unname(cluster5[rownames(X_weighted)])

  ) %>%
    filter(
      country != "Ukraine"
    ) %>%
    arrange(
      distance
    ) %>%
    mutate(
      rank =
        row_number()
    ) %>%
    select(
      all_of(
        c(
          "rank",
          "country",
          "distance",
          "cluster2",
          "cluster5"
        )
      )
    )

  cat(
    "\n20 nearest neighbours of Ukraine — Variant B:\n"
  )

  print(
    tibble::as_tibble(
      ukraine_neighbors %>%
        slice_head(
          n = 20
        )
    ),
    n = 20,
    width = Inf
  )
}


# ============================================================
# 33A. SENSITIVITY: NO MILITARY-EXPENDITURE IMPUTATION
# ============================================================
#
# The imputation flag is aligned through analysis_data, which is
# the exact row set used to build X and X_z.
# ============================================================

stopifnot(
  length(analysis_data$mil_expenditure_missing) == nrow(X_z)
)

noimp_keep <- analysis_data$mil_expenditure_missing == 0

X_noimp <- X_z[
  noimp_keep,
  ,
  drop = FALSE
]

X_noimp_w <- sweep(
  X_noimp,
  2,
  sqrt(variable_weights),
  FUN = "*"
)

D_noimp <- dist(
  X_noimp_w,
  method = "euclidean"
)

hc_noimp <- hclust(
  D_noimp,
  method = "ward.D2"
)

noimp_ks <- 2:8

noimp_silhouette_values <- sapply(
  noimp_ks,
  function(k) {
    groups <- cutree(hc_noimp, k = k)
    mean(
      silhouette(
        groups,
        D_noimp
      )[, "sil_width"]
    )
  }
)

noimp_silhouette_table <- data.frame(
  k = noimp_ks,
  mean_silhouette = noimp_silhouette_values
)

cat("\nNo-imputation sensitivity — silhouette k=2...8:\n")
print(noimp_silhouette_table)

cluster5_noimp <- cutree(
  hc_noimp,
  k = 5
)

cluster5_main_noimp_shared <- cluster5[
  rownames(X_noimp_w)
]

noimp_cluster5_comparison <- data.frame(
  country = rownames(X_noimp_w),
  cluster5_primary = as.integer(cluster5_main_noimp_shared),
  cluster5_noimp = as.integer(cluster5_noimp)
)

cat("\nPrimary vs no-imputation k=5 cross-tabulation:\n")
print(
  table(
    noimp_cluster5_comparison$cluster5_primary,
    noimp_cluster5_comparison$cluster5_noimp
  )
)

ukraine_neighbors_noimp <- data.frame()
noimp_neighbor_overlap_summary <- data.frame()

if (
  "Ukraine" %in% rownames(X_noimp_w) &&
    nrow(ukraine_neighbors) > 0
) {

  D_noimp_matrix <- as.matrix(D_noimp)

  ukraine_neighbors_noimp <- data.frame(
    country = rownames(X_noimp_w),
    distance_noimp = D_noimp_matrix["Ukraine", ]
  ) %>%
    filter(country != "Ukraine") %>%
    arrange(distance_noimp) %>%
    mutate(rank_noimp = row_number())

  main_top20 <- ukraine_neighbors %>%
    slice_head(n = 20) %>%
    pull(country)

  noimp_top20 <- ukraine_neighbors_noimp %>%
    slice_head(n = 20) %>%
    pull(country)

  noimp_top20_overlap <- intersect(
    main_top20,
    noimp_top20
  )

  noimp_neighbor_overlap_summary <- data.frame(
    primary_top20_n = length(main_top20),
    noimp_top20_n = length(noimp_top20),
    overlap_n = length(noimp_top20_overlap),
    overlap_countries = paste(
      noimp_top20_overlap,
      collapse = "; "
    )
  )

  cat("\n20 nearest neighbours of Ukraine — no-imputation model:\n")
  print(
    tibble::as_tibble(
      ukraine_neighbors_noimp %>%
        slice_head(n = 20)
    ),
    n = 20,
    width = Inf
  )

  cat("\nTop-20 overlap with primary model:\n")
  print(noimp_neighbor_overlap_summary)
}


# ============================================================
# 34. SENSITIVITY:
#     ORDINARY EQUAL-VARIABLE EUCLIDEAN DISTANCE
# ============================================================

D_equal <- dist(
  X_z,
  method = "euclidean"
)

if ("Ukraine" %in% rownames(X_z)) {

  D_equal_matrix <-
    as.matrix(
      D_equal
    )

  neighbors_equal <- data.frame(

    country =
      rownames(X_z),

    distance_equal =
      D_equal_matrix[
        "Ukraine",
      ]

  ) %>%
    filter(
      country != "Ukraine"
    ) %>%
    arrange(
      distance_equal
    ) %>%
    mutate(
      rank_equal =
        row_number()
    )
}


# ============================================================
# 35. SENSITIVITY:
#     MANHATTAN DISTANCE
# ============================================================

D_manhattan <- dist(
  X_z,
  method = "manhattan"
)

if ("Ukraine" %in% rownames(X_z)) {

  D_manhattan_matrix <-
    as.matrix(
      D_manhattan
    )

  neighbors_manhattan <- data.frame(

    country =
      rownames(X_z),

    distance_manhattan =
      D_manhattan_matrix[
        "Ukraine",
      ]

  ) %>%
    filter(
      country != "Ukraine"
    ) %>%
    arrange(
      distance_manhattan
    ) %>%
    mutate(
      rank_manhattan =
        row_number()
    )
}


# ============================================================
# 36. COMPARE UKRAINE RANKINGS ACROSS METRICS
# ============================================================

if (
  nrow(ukraine_neighbors) > 0 &&
  nrow(neighbors_equal) > 0 &&
  nrow(neighbors_manhattan) > 0
) {

  neighbor_comparison <- ukraine_neighbors %>%
    select(
      country,
      rank_domain = rank,
      distance_domain = distance
    ) %>%
    left_join(
      neighbors_equal,
      by = "country"
    ) %>%
    left_join(
      neighbors_manhattan,
      by = "country"
    ) %>%
    mutate(

      median_rank =
        apply(
          cbind(
            rank_domain,
            rank_equal,
            rank_manhattan
          ),
          1,
          median
        ),

      best_rank =
        pmin(
          rank_domain,
          rank_equal,
          rank_manhattan
        ),

      worst_rank =
        pmax(
          rank_domain,
          rank_equal,
          rank_manhattan
        )

    ) %>%
    arrange(
      median_rank,
      worst_rank
    )

  cat(
    "\nUkraine neighbour robustness — Variant B:\n"
  )

  print(
    tibble::as_tibble(
      neighbor_comparison %>%
        slice_head(
          n = 30
        )
    ),
    n = 30,
    width = Inf
  )
}


# ============================================================
# 37. TOP-10 UKRAINE NEIGHBOURS WITHIN EACH DOMAIN
# ============================================================
#
# Distances are calculated from the same z-standardized data.
# Within a domain, each variable receives equal weight.
# A constant domain-level scaling factor would not change rank.
# ============================================================

domain_neighbor_tables <- list()

for (domain_name in names(domains_primary)) {

  vars <-
    domains_primary[[domain_name]]

  X_domain <-
    X_z[
      ,
      vars,
      drop = FALSE
    ]

  X_domain_weighted <-
    sweep(
      X_domain,
      2,
      sqrt(
        rep(
          1 / length(vars),
          length(vars)
        )
      ),
      FUN = "*"
    )

  D_domain <-
    as.matrix(
      dist(
        X_domain_weighted,
        method = "euclidean"
      )
    )

  domain_table <- data.frame(

    country =
      rownames(X_domain_weighted),

    distance_domain =
      D_domain[
        "Ukraine",
      ]

  ) %>%
    filter(
      country != "Ukraine"
    ) %>%
    arrange(
      distance_domain
    ) %>%
    mutate(
      rank =
        row_number(),
      domain =
        domain_name
    ) %>%
    select(
      domain,
      rank,
      country,
      distance_domain
    )

  domain_neighbor_tables[[domain_name]] <-
    domain_table

  cat(
    "\nTop 10 Ukraine neighbours — domain:",
    domain_name,
    "\n"
  )

  print(
    domain_table %>%
      slice_head(
        n = 10
      )
  )
}

domain_neighbors_top10 <- bind_rows(
  lapply(
    domain_neighbor_tables,
    function(x) {
      x %>%
        slice_head(
          n = 10
        )
    }
  )
)


# ============================================================
# 38. UKRAINE + TOP-20 RAW/DERIVED PROFILES FOR INTERPRETATION
# ============================================================

if (nrow(ukraine_neighbors) > 0) {

  top20_names <-
    ukraine_neighbors %>%
    slice_head(
      n = 20
    ) %>%
    pull(
      country
    )

  ukraine_top20_profiles <- results %>%
    filter(
      country %in% c(
        "Ukraine",
        top20_names
      )
    ) %>%
    mutate(
      neighbour_rank =
        match(
          country,
          top20_names
        ),
      neighbour_rank =
        ifelse(
          country == "Ukraine",
          0,
          neighbour_rank
        )
    ) %>%
    arrange(
      neighbour_rank
    ) %>%
    select(
      neighbour_rank,
      country,

      conflicts_primary_b,
      years_primary_b,
      years_war_level,
      longest_spell,
      years_since_last_primary,

      years_interstate_b,
      years_intl_intrastate_b,
      years_intrastate_b,

      years_home_defensive_b,
      years_home_civil_b,
      years_abroad_b,

      mil_personnel_latest_population_k,
      mil_personnel_latest_k,
      mil_personnel_pct_pop_latest,
      mil_expenditure,

      n_conflicts,
      total_exposure,
      war_share,
      continuity,
      recency,
      interstate_share,
      intl_intrastate_share,
      intrastate_share,
      home_defensive_share,
      home_civil_share,
      abroad_share,

      all_of(c("cluster2", "cluster5"))
    )
}


# ============================================================
# 39. CASUALTY DATA DIAGNOSTICS
# ============================================================

casualty_missing <- clust_data %>%
  filter(
    is.na(battle_deaths)
  ) %>%
  select(
    country,
    battle_deaths_cow
  )

cat(
  "\nCountries missing battle-death data:\n"
)

print(
  casualty_missing
)


# ============================================================
# 40. CASUALTY SENSITIVITY MODEL
# ============================================================
#
# Includes battle_deaths and only countries with observed
# casualty data. No casualty imputation.
# ============================================================

hca_vars_casualty <- c(
  hca_vars_primary,
  "battle_deaths"
)

casualty_data <- clust_data %>%
  filter(
    complete.cases(
      across(
        all_of(
          hca_vars_casualty
        )
      )
    )
  )

cat(
  "\nCasualty-model sample size:",
  nrow(casualty_data),
  "\n"
)

cat(
  "Ukraine present in casualty model:",
  "Ukraine" %in% casualty_data$country,
  "\n"
)

X_casualty <- casualty_data %>%
  select(
    all_of(
      hca_vars_casualty
    )
  ) %>%
  as.data.frame()

rownames(X_casualty) <-
  casualty_data$country

X_casualty_z <-
  scale(
    X_casualty
  )

domains_casualty <- list(

  conflict_magnitude = c(
    "n_conflicts",
    "total_exposure"
  ),

  temporal_intensity = c(
    "war_share",
    "continuity",
    "recency"
  ),

  conflict_type = c(
    "interstate_share",
    "intl_intrastate_share",
    "intrastate_share"
  ),

  locus = c(
    "home_defensive_share",
    "home_civil_share",
    "abroad_share"
  ),

  population_military_scale = c(
    "population_size",
    "personnel_size",
    "personnel_share"
  ),

  casualties = c(
    "battle_deaths"
  ),

  military_expenditure = c(
    "mil_expenditure"
  )
)

n_domains_casualty <-
  length(
    domains_casualty
  )

variable_weights_casualty <- unlist(
  lapply(
    domains_casualty,
    function(vars) {
      rep(
        (1 / n_domains_casualty) /
          length(vars),
        length(vars)
      )
    }
  )
)

names(
  variable_weights_casualty
) <-
  unlist(
    domains_casualty
  )

variable_weights_casualty <-
  variable_weights_casualty[
    colnames(
      X_casualty_z
    )
  ]

X_casualty_weighted <- sweep(
  X_casualty_z,
  2,
  sqrt(
    variable_weights_casualty
  ),
  FUN = "*"
)

D_casualty <- dist(
  X_casualty_weighted,
  method = "euclidean"
)

hc_casualty <- hclust(
  D_casualty,
  method = "ward.D2"
)

silhouette_casualty <- sapply(
  2:10,
  function(k) {

    groups <- cutree(
      hc_casualty,
      k = k
    )

    mean(
      silhouette(
        groups,
        D_casualty
      )[, "sil_width"]
    )
  }
)

casualty_silhouette_table <- data.frame(
  k = 2:10,
  mean_silhouette =
    silhouette_casualty
)

cat(
  "\nCasualty-model silhouette results:\n"
)

print(
  casualty_silhouette_table
)


# ============================================================
# 41. VARIANT A SENSITIVITY MODEL
# ============================================================
#
# Original UCDP classification:
#   interstate = years_interstate_ucdp
#   internationalized intrastate =
#       years_intl_intrastate_ucdp
#
# Original primary-party counts/years and locus variables are
# used throughout this sensitivity model.
#
# Population, personnel and expenditure are identical to the
# main Variant B model.
# ============================================================

analysis_data_A <- clust_data %>%
  mutate(

    n_conflicts_A =
      log1p(
        conflicts_primary
      ),

    total_exposure_A =
      log1p(
        years_primary
      ),

    war_share_A =
      years_war_level /
        years_primary,

    continuity_A =
      longest_spell /
        years_primary,

    recency_A =
      years_since_last_primary,

    interstate_share_A =
      years_interstate_ucdp /
        years_primary,

    intl_intrastate_share_A =
      years_intl_intrastate_ucdp /
        years_primary,

    intrastate_share_A =
      years_intrastate /
        years_primary,

    home_defensive_share_A =
      years_home_defensive /
        years_primary,

    home_civil_share_A =
      years_home_civil /
        years_primary,

    abroad_share_A =
      years_abroad /
        years_primary

  ) %>%
  transmute(

    country,

    n_conflicts =
      n_conflicts_A,

    total_exposure =
      total_exposure_A,

    war_share =
      war_share_A,

    continuity =
      continuity_A,

    recency =
      recency_A,

    interstate_share =
      interstate_share_A,

    intl_intrastate_share =
      intl_intrastate_share_A,

    intrastate_share =
      intrastate_share_A,

    home_defensive_share =
      home_defensive_share_A,

    home_civil_share =
      home_civil_share_A,

    abroad_share =
      abroad_share_A,

    population_size,
    personnel_size,
    personnel_share,
    mil_expenditure
  ) %>%
  filter(
    complete.cases(
      across(
        all_of(
          hca_vars_primary
        )
      )
    )
  )

X_A <- analysis_data_A %>%
  select(
    all_of(
      hca_vars_primary
    )
  ) %>%
  as.data.frame()

rownames(X_A) <-
  analysis_data_A$country

X_A_z <-
  scale(
    X_A
  )

variable_weights_A <-
  variable_weights[
    colnames(
      X_A_z
    )
  ]

X_A_weighted <- sweep(
  X_A_z,
  2,
  sqrt(
    variable_weights_A
  ),
  FUN = "*"
)

D_A <- dist(
  X_A_weighted,
  method = "euclidean"
)

hc_A <- hclust(
  D_A,
  method = "ward.D2"
)

silhouette_A <- sapply(
  2:10,
  function(k) {

    groups <- cutree(
      hc_A,
      k = k
    )

    mean(
      silhouette(
        groups,
        D_A
      )[, "sil_width"]
    )
  }
)

variant_A_silhouette_table <- data.frame(
  k = 2:10,
  mean_silhouette =
    silhouette_A
)

cluster2_A <- cutree(
  hc_A,
  k = 2
)

cluster5_A <- cutree(
  hc_A,
  k = 5
)

cat(
  "\nVariant A sensitivity — silhouette:\n"
)

print(
  variant_A_silhouette_table
)

cat(
  "\nVariant A / Variant B k=5 cross-tabulation:\n"
)

variant_shared_countries <- intersect(
  names(cluster5),
  names(cluster5_A)
)

variant_cluster_comparison <- data.frame(
  country = variant_shared_countries,
  cluster5_B = unname(cluster5[variant_shared_countries]),
  cluster5_A = unname(cluster5_A[variant_shared_countries])
)

print(
  table(
    variant_cluster_comparison$cluster5_B,
    variant_cluster_comparison$cluster5_A
  )
)

if ("Ukraine" %in% rownames(X_A_weighted)) {

  D_A_matrix <-
    as.matrix(
      D_A
    )

  ukraine_neighbors_A <- data.frame(

    country =
      rownames(X_A_weighted),

    distance_A =
      D_A_matrix[
        "Ukraine",
      ],

    cluster2_A =
      unname(cluster2_A[rownames(X_A_weighted)]),

    cluster5_A =
      unname(cluster5_A[rownames(X_A_weighted)])

  ) %>%
    filter(
      country != "Ukraine"
    ) %>%
    arrange(
      distance_A
    ) %>%
    mutate(
      rank_A =
        row_number()
    )

  cat(
    "\n20 nearest neighbours of Ukraine — Variant A sensitivity:\n"
  )

  print(
    tibble::as_tibble(
      ukraine_neighbors_A %>%
        slice_head(
          n = 20
        )
    ),
    n = 20,
    width = Inf
  )
}


# ============================================================
# 45. SENSITIVITY TO WEIGHTING SCHEME
# ============================================================
#
# Compares the primary equal-variable model against the retained
# six-domain weighting scheme.
# ============================================================

X_dom_w <- sweep(
  X_z,
  2,
  sqrt(domain_weight_scheme),
  FUN = "*"
)

D_dom <- dist(
  X_dom_w,
  method = "euclidean"
)

hc_dom <- hclust(
  D_dom,
  method = "ward.D2"
)

domain_weight_ks <- 2:8

domain_weight_silhouette_values <- sapply(
  domain_weight_ks,
  function(k) {
    groups <- cutree(hc_dom, k = k)
    mean(
      silhouette(
        groups,
        D_dom
      )[, "sil_width"]
    )
  }
)

domain_weight_silhouette_table <- data.frame(
  k = domain_weight_ks,
  mean_silhouette = domain_weight_silhouette_values
)

cat("\nDomain-weight sensitivity — silhouette k=2...8:\n")
print(domain_weight_silhouette_table)

weighting_ari_table <- data.frame(
  k = 2:6,
  ARI = sapply(
    2:6,
    function(k) {
      mclust::adjustedRandIndex(
        cutree(hc, k = k),
        cutree(hc_dom, k = k)
      )
    }
  )
)

cat("\nEqual-variable vs domain-weighted ARI:\n")
print(weighting_ari_table)

ukraine_neighbors_dom <- data.frame()
weighting_neighbor_comparison <- data.frame()
weighting_summary <- data.frame()

if (
  "Ukraine" %in% rownames(X_dom_w) &&
    nrow(ukraine_neighbors) > 0
) {

  D_dom_matrix <- as.matrix(D_dom)

  ukraine_neighbors_dom <- data.frame(
    country = rownames(X_dom_w),
    distance_domain = D_dom_matrix["Ukraine", ]
  ) %>%
    filter(country != "Ukraine") %>%
    arrange(distance_domain) %>%
    mutate(rank_domain = row_number())

  weighting_neighbor_comparison <- ukraine_neighbors %>%
    select(
      country,
      rank_equal = rank,
      distance_equal = distance
    ) %>%
    inner_join(
      ukraine_neighbors_dom %>%
        select(
          country,
          rank_domain,
          distance_domain
        ),
      by = "country"
    )

  primary_top20_weight <- weighting_neighbor_comparison %>%
    arrange(rank_equal) %>%
    slice_head(n = 20) %>%
    pull(country)

  domain_top20_weight <- weighting_neighbor_comparison %>%
    arrange(rank_domain) %>%
    slice_head(n = 20) %>%
    pull(country)

  weighting_top20_overlap <- intersect(
    primary_top20_weight,
    domain_top20_weight
  )

  weighting_spearman <- cor(
    weighting_neighbor_comparison$rank_equal,
    weighting_neighbor_comparison$rank_domain,
    method = "spearman",
    use = "complete.obs"
  )

  weighting_summary <- data.frame(
    top20_overlap_n = length(weighting_top20_overlap),
    full_rank_spearman = weighting_spearman,
    overlap_countries = paste(
      weighting_top20_overlap,
      collapse = "; "
    )
  )

  cat("\nTop 20 Ukraine neighbours — primary equal-variable model:\n")
  print(
    tibble::as_tibble(
      ukraine_neighbors %>%
        slice_head(n = 20)
    ),
    n = 20,
    width = Inf
  )

  cat("\nTop 20 Ukraine neighbours — domain-weighted model:\n")
  print(
    tibble::as_tibble(
      ukraine_neighbors_dom %>%
        slice_head(n = 20)
    ),
    n = 20,
    width = Inf
  )

  cat("\nWeighting-scheme neighbour robustness:\n")
  print(weighting_summary)
}


# ============================================================
# 42. COMPARE VARIANT A AND VARIANT B UKRAINE NEIGHBOURS
# ============================================================

if (
  nrow(ukraine_neighbors) > 0 &&
  nrow(ukraine_neighbors_A) > 0
) {

  variant_neighbor_comparison <- ukraine_neighbors %>%
    select(
      country,
      rank_B = rank,
      distance_B = distance,
      cluster5_B = cluster5
    ) %>%
    left_join(
      ukraine_neighbors_A %>%
        select(
          country,
          rank_A,
          distance_A,
          cluster5_A
        ),
      by = "country"
    ) %>%
    mutate(
      rank_change_A_to_B =
        rank_B - rank_A
    ) %>%
    arrange(
      rank_B
    )

  cat(
    "\nVariant A vs Variant B Ukraine-neighbour comparison:\n"
  )

  print(
    tibble::as_tibble(
      variant_neighbor_comparison %>%
        slice_head(
          n = 30
        )
    ),
    n = 30,
    width = Inf
  )
}


# ============================================================
# 43. SAVE RESULTS TO EXCEL
# ============================================================

output <- list(

  variant_B_check =
    variant_b_check,

  primary_missingness =
    missing_summary_primary,

  expenditure_missing =
    missing_expenditure_countries,

  unique_values =
    unique_summary,

  high_correlations =
    high_correlations,

  closure_check =
    closure_check,

  bootstrap_stability =
    bootstrap_stability_table,

  noimp_silhouette =
    noimp_silhouette_table,

  noimp_cluster5_cross =
    noimp_cluster5_comparison,

  weighting_silhouette =
    domain_weight_silhouette_table,

  weighting_ARI =
    weighting_ari_table,

  variable_weights =
    data.frame(
      variable =
        names(variable_weights),
      weight =
        as.numeric(variable_weights)
    ),

  domain_weight_scheme =
    data.frame(
      variable =
        names(domain_weight_scheme),
      weight =
        as.numeric(domain_weight_scheme)
    ),

  silhouette_variant_B =
    silhouette_table,

  cluster2_membership_B =
    countries2,

  cluster5_membership_B =
    countries5,

  cluster2_profiles_B =
    profile2,

  cluster5_profiles_B =
    profile5,

  cluster5_raw_profiles_B =
    raw_profile5,

  cluster5_centroids_B =
    centroids5,

  domain_neighbors_top10_B =
    domain_neighbors_top10,

  casualty_missing =
    casualty_missing,

  casualty_silhouette_B =
    casualty_silhouette_table,

  silhouette_variant_A =
    variant_A_silhouette_table,

  cluster5_A_vs_B =
    variant_cluster_comparison
)

if (nrow(ukraine_neighbors) > 0) {
  output$ukraine_neighbors_B <-
    ukraine_neighbors
}

if (nrow(neighbor_comparison) > 0) {
  output$ukraine_neighbor_robustness_B <-
    neighbor_comparison
}

if (nrow(ukraine_top20_profiles) > 0) {
  output$ukraine_top20_profiles_B <-
    ukraine_top20_profiles
}

if (nrow(ukraine_neighbors_A) > 0) {
  output$ukraine_neighbors_A <-
    ukraine_neighbors_A
}

if (nrow(variant_neighbor_comparison) > 0) {
  output$variant_A_B_neighbors <-
    variant_neighbor_comparison
}

if (nrow(ukraine_neighbors_noimp) > 0) {
  output$ukraine_neighbors_noimp <-
    ukraine_neighbors_noimp
}

if (nrow(noimp_neighbor_overlap_summary) > 0) {
  output$noimp_neighbor_overlap <-
    noimp_neighbor_overlap_summary
}

if (nrow(ukraine_neighbors_dom) > 0) {
  output$ukraine_neighbors_domain <-
    ukraine_neighbors_dom
}

if (nrow(weighting_neighbor_comparison) > 0) {
  output$weighting_neighbors <-
    weighting_neighbor_comparison
}

if (nrow(weighting_summary) > 0) {
  output$weighting_summary <-
    weighting_summary
}

write_xlsx(
  output,
  file.path(RESULTS_DIR, "war_participation_HCA_extended_variant_B.xlsx")
)


# ============================================================
# 44. SAVE REPRODUCIBLE R OBJECTS
# ============================================================

hca_reproducibility_objects <- list(
  hc = hc,
  D_weighted = D_weighted,
  variable_weights = variable_weights,
  domain_weight_scheme = domain_weight_scheme,
  analysis_data = analysis_data,
  silhouette_variant_B = silhouette_table,
  silhouette_no_imputation = noimp_silhouette_table,
  silhouette_domain_weighting = domain_weight_silhouette_table,
  silhouette_casualty = casualty_silhouette_table,
  silhouette_variant_A = variant_A_silhouette_table,
  bootstrap_stability = bootstrap_stability_table,
  weighting_ARI = weighting_ari_table
)

saveRDS(
  hca_reproducibility_objects,
  file.path(RESULTS_DIR, "war_participation_HCA_extended_variant_B.rds")
)


# ============================================================
# SESSION INFORMATION
# ============================================================

cat("\nSession information:\n")
print(sessionInfo())


# ============================================================
# END
# ============================================================
