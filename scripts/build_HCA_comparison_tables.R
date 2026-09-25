# ============================================================
# BUILD FINAL HCA COMPARISON TABLES
# Five primary representations of state war experience
# ============================================================
#
# Expected project layout:
#   new_scripts/
#     build_HCA_comparison_tables.R
#     results/
#       11var/
#       15var/
#       direct_25var/
#       redundancy_reduced/
#       families_7/
#
# This script does NOT rerun any HCA.
# It reads the saved RDS objects from the five completed analyses and
# creates cross-model comparison tables.
#
# It also calculates, for every cluster in every top-3 solution:
#   - the country closest to that cluster's centroid
#   - Euclidean distance of that country to the centroid
#
# IMPORTANT:
# "closest to centroid" is not the same as a medoid.
# The representative is the observed country with minimum Euclidean
# distance to the arithmetic centroid in the SAME model space that was
# used for clustering.
#
# Outputs:
#   results/comparison/
#     HCA_comparison_tables.xlsx
#     comparison_*.csv
#     HCA_comparison_tables.rds
# ============================================================

cat("\nSCRIPT VERSION: build_HCA_comparison_tables — 2026-09-12\n")

# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "readr",
  "writexl"
)

missing_packages <- packages[
  !vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
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

# ============================================================
# 1. PROJECT PATHS
# ============================================================

# The script can be run either from new_scripts/ itself or from the
# repository root.

base_candidates <- c(
  ".",
  "new_scripts"
)

valid_bases <- base_candidates[
  dir.exists(
    file.path(
      base_candidates,
      "results"
    )
  )
]

if (length(valid_bases) == 0) {
  stop(
    "Could not find the results/ directory. Run this script from the repository root or new_scripts/."
  )
}

BASE_DIR <- normalizePath(
  valid_bases[1],
  mustWork = TRUE
)

RESULTS_DIR <- file.path(
  BASE_DIR,
  "results"
)

OUT_DIR <- file.path(
  RESULTS_DIR,
  "comparison"
)

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

cat("Base directory:", BASE_DIR, "\n")
cat("Comparison output:", OUT_DIR, "\n")

# ============================================================
# 2. MODEL REGISTRY
# ============================================================

model_registry <- tibble::tribble(
  ~approach, ~display_name, ~folder, ~preferred_rds,
  "11var",
  "11-variable theoretical",
  "11var",
  "HCA_11var_results.rds",

  "15var",
  "15-variable expanded theoretical",
  "15var",
  "HCA_15var_results.rds",

  "direct_25var",
  "Direct 25-variable",
  "direct_25var",
  "HCA_direct_25var_results.rds",

  "redundancy_reduced",
  "Redundancy-reduced direct",
  "redundancy_reduced",
  "HCA_redundancy_reduced_results.rds",

  "families_7",
  "Seven-family synthetic scores",
  "families_7",
  "HCA_7families_results.rds"
)

# ============================================================
# 3. HELPERS
# ============================================================

resolve_rds <- function(folder, preferred_name) {

  model_dir <- file.path(
    RESULTS_DIR,
    folder
  )

  if (!dir.exists(model_dir)) {
    stop(
      paste0(
        "Missing results directory: ",
        model_dir
      )
    )
  }

  preferred <- file.path(
    model_dir,
    preferred_name
  )

  if (file.exists(preferred)) {
    return(preferred)
  }

  candidates <- list.files(
    model_dir,
    pattern = "\\.rds$",
    full.names = TRUE
  )

  if (length(candidates) == 1L) {
    warning(
      paste0(
        "Preferred RDS not found for ",
        folder,
        "; using ",
        basename(candidates),
        "."
      ),
      call. = FALSE
    )

    return(candidates)
  }

  if (length(candidates) == 0L) {
    stop(
      paste0(
        "No RDS result file found in ",
        model_dir
      )
    )
  }

  stop(
    paste0(
      "Multiple RDS files found in ",
      model_dir,
      " and preferred file is absent. Files: ",
      paste(
        basename(candidates),
        collapse = ", "
      )
    )
  )
}

get_model_matrix <- function(obj, approach) {

  # Preferred explicit model-space object.
  if (!is.null(obj$X_model)) {
    X <- obj$X_model

  # 11- and 15-variable models used weighted standardized coordinates.
  } else if (!is.null(obj$X_weighted)) {
    X <- obj$X_weighted

  # Direct models use standardized coordinates.
  } else if (!is.null(obj$X_z)) {
    X <- obj$X_z

  # Seven-family fallback.
  } else if (!is.null(obj$F7)) {
    X <- obj$F7

  } else {
    stop(
      paste0(
        "Could not identify the clustering model matrix for approach '",
        approach,
        "'."
      )
    )
  }

  X <- as.matrix(X)

  if (!is.numeric(X)) {
    stop(
      paste0(
        "Model matrix is not numeric for approach '",
        approach,
        "'."
      )
    )
  }

  if (is.null(rownames(X))) {
    stop(
      paste0(
        "Model matrix has no country row names for approach '",
        approach,
        "'."
      )
    )
  }

  if (anyDuplicated(rownames(X))) {
    stop(
      paste0(
        "Duplicated country row names in model matrix for approach '",
        approach,
        "'."
      )
    )
  }

  if (any(!is.finite(X))) {
    stop(
      paste0(
        "Non-finite values in model matrix for approach '",
        approach,
        "'."
      )
    )
  }

  X
}

get_membership_list <- function(obj, approach) {

  if (is.null(obj$membership_list)) {
    stop(
      paste0(
        "membership_list is missing from RDS for approach '",
        approach,
        "'."
      )
    )
  }

  obj$membership_list
}

get_top3 <- function(obj, approach) {

  if (is.null(obj$top3_solutions)) {
    stop(
      paste0(
        "top3_solutions is missing from RDS for approach '",
        approach,
        "'."
      )
    )
  }

  out <- as.data.frame(
    obj$top3_solutions
  )

  if (!("k" %in% names(out))) {
    stop(
      paste0(
        "top3_solutions has no k column for approach '",
        approach,
        "'."
      )
    )
  }

  if (nrow(out) < 3L) {
    stop(
      paste0(
        "Fewer than three top solutions stored for approach '",
        approach,
        "'."
      )
    )
  }

  out <- out[
    seq_len(3L),
    ,
    drop = FALSE
  ]

  if (!("solution_rank" %in% names(out))) {

    if ("silhouette_rank" %in% names(out)) {
      out$solution_rank <-
        as.integer(
          out$silhouette_rank
        )
    } else {
      out$solution_rank <-
        seq_len(
          nrow(out)
        )
    }
  }

  out
}

calculate_cluster_representatives <- function(
  X,
  membership_list,
  top3,
  approach,
  display_name
) {

  rows <- list()
  counter <- 1L

  for (i in seq_len(nrow(top3))) {

    k <- as.integer(
      top3$k[i]
    )

    groups <-
      membership_list[[as.character(k)]]

    if (is.null(groups)) {
      stop(
        paste0(
          "No membership vector stored for k=",
          k,
          " in approach '",
          approach,
          "'."
        )
      )
    }

    if (is.null(names(groups))) {
      stop(
        paste0(
          "Membership vector has no country names for k=",
          k,
          " in approach '",
          approach,
          "'."
        )
      )
    }

    missing_in_X <- setdiff(
      names(groups),
      rownames(X)
    )

    if (length(missing_in_X) > 0) {
      stop(
        paste0(
          "Countries in membership but absent from model matrix for approach '",
          approach,
          "': ",
          paste(
            missing_in_X,
            collapse = ", "
          )
        )
      )
    }

    # Preserve model-matrix order.
    groups <- groups[
      rownames(X)
    ]

    cluster_ids <- sort(
      unique(
        as.integer(groups)
      )
    )

    ukraine_cluster <- as.integer(groups["Ukraine"])

    for (cl in cluster_ids) {

      member_names <- names(groups)[
        as.integer(groups) == cl
      ]

      Xi <- X[
        member_names,
        ,
        drop = FALSE
      ]

      centroid <- colMeans(
        Xi
      )

      delta <- sweep(
        Xi,
        2,
        centroid,
        "-"
      )

      distance_to_centroid <- sqrt(
        rowSums(
          delta^2
        )
      )

      candidates <- tibble::tibble(
        country =
          rownames(Xi),

        distance_to_centroid =
          as.numeric(
            distance_to_centroid
          )
      ) %>%
        dplyr::arrange(
          distance_to_centroid,
          country
        )

      representative <-
        candidates %>%
        dplyr::slice_head(
          n = 1
        )

      rows[[counter]] <-
        tibble::tibble(

          approach =
            approach,

          display_name =
            display_name,

          solution_rank = i,

          k = k,

          cluster = cl,

          n =
            length(
              member_names
            ),

          ukraine_cluster =
            ukraine_cluster,

          is_ukraine_cluster =
            cl == ukraine_cluster,

          representative_country =
            representative$country,

          distance_to_centroid =
            representative$distance_to_centroid
        )

      counter <- counter + 1L
    }
  }

  dplyr::bind_rows(
    rows
  )
}

normalize_cluster_diagnostics <- function(
  obj,
  approach,
  display_name
) {

  if (is.null(
    obj$cluster_diagnostics_top3
  )) {
    stop(
      paste0(
        "cluster_diagnostics_top3 is missing for approach '",
        approach,
        "'."
      )
    )
  }

  x <- tibble::as_tibble(
    obj$cluster_diagnostics_top3
  )

  # Replace potentially inconsistent approach labels from individual
  # scripts with the registry label.
  x$approach <- approach

  if (!("display_name" %in% names(x))) {
    x$display_name <- display_name
  } else {
    x$display_name <- display_name
  }

  if (!("solution_rank" %in% names(x))) {
    stop(
      paste0(
        "solution_rank missing from cluster diagnostics for approach '",
        approach,
        "'."
      )
    )
  }

  x
}

normalize_centroids <- function(
  obj,
  approach,
  display_name
) {

  if (is.null(
    obj$centroids_top3_long
  )) {
    return(
      tibble::tibble()
    )
  }

  x <- tibble::as_tibble(
    obj$centroids_top3_long
  )

  x$approach <- approach
  x$display_name <- display_name

  # Standardize coordinate-name field across models.
  if ("variable" %in% names(x)) {

    x <- x %>%
      dplyr::rename(
        coordinate = variable
      )

  } else if ("family" %in% names(x)) {

    x <- x %>%
      dplyr::rename(
        coordinate = family
      )

  } else {

    stop(
      paste0(
        "Could not find variable/family column in centroid table for approach '",
        approach,
        "'."
      )
    )
  }

  x
}

normalize_ukraine_clusters <- function(
  obj,
  approach,
  display_name
) {

  if (is.null(
    obj$ukraine_cluster_top3
  )) {
    stop(
      paste0(
        "ukraine_cluster_top3 is missing for approach '",
        approach,
        "'."
      )
    )
  }

  x <- tibble::as_tibble(
    obj$ukraine_cluster_top3
  )

  x$approach <- approach
  x$display_name <- display_name

  x
}

normalize_neighbours <- function(
  obj,
  approach,
  display_name
) {

  if (is.null(
    obj$ukraine_top10_global
  )) {
    stop(
      paste0(
        "ukraine_top10_global is missing for approach '",
        approach,
        "'."
      )
    )
  }

  x <- tibble::as_tibble(
    obj$ukraine_top10_global
  )

  x$approach <- approach
  x$display_name <- display_name

  x
}

# ============================================================
# 4. LOAD ALL FIVE ANALYSES
# ============================================================

objects <- list()
resolved_files <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  rds_path <- resolve_rds(
    model_registry$folder[i],
    model_registry$preferred_rds[i]
  )

  cat(
    "Loading",
    approach,
    "from",
    rds_path,
    "\n"
  )

  objects[[approach]] <-
    readRDS(
      rds_path
    )

  resolved_files[[approach]] <-
    rds_path
}

# ============================================================
# 5. MODEL METADATA
# ============================================================

model_metadata_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  display_name <-
    model_registry$display_name[i]

  obj <- objects[[approach]]
  X <- get_model_matrix(
    obj,
    approach
  )

  starting_dimension <- dplyr::case_when(
    approach == "11var" ~ 11L,
    approach == "15var" ~ 15L,
    approach == "direct_25var" ~ 25L,
    approach == "redundancy_reduced" ~
      if (!is.null(obj$vars_start)) {
        length(obj$vars_start)
      } else {
        NA_integer_
      },
    approach == "families_7" ~
      if (!is.null(obj$vars_25)) {
        length(obj$vars_25)
      } else {
        25L
      },
    TRUE ~ NA_integer_
  )

  retained_indicator_n <- dplyr::case_when(
    approach == "redundancy_reduced" ~
      if (!is.null(obj$cluster_vars)) {
        length(obj$cluster_vars)
      } else {
        NA_integer_
      },

    approach == "families_7" ~
      if (!is.null(obj$vars_reduced)) {
        length(obj$vars_reduced)
      } else {
        NA_integer_
      },

    TRUE ~ starting_dimension
  )

  model_metadata_rows[[i]] <-
    tibble::tibble(

      approach =
        approach,

      display_name =
        display_name,

      n_countries =
        nrow(X),

      starting_indicator_n =
        starting_dimension,

      retained_indicator_n =
        retained_indicator_n,

      clustering_dimension =
        ncol(X),

      clustering_coordinates =
        paste(
          colnames(X),
          collapse = "; "
        ),

      rds_file =
        normalizePath(
          resolved_files[[approach]]
        )
    )
}

model_metadata <- dplyr::bind_rows(
  model_metadata_rows
)

# ============================================================
# 6. TOP-3 SOLUTION SUMMARY ACROSS APPROACHES
# ============================================================

solution_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  display_name <-
    model_registry$display_name[i]

  obj <- objects[[approach]]

  top3 <- get_top3(
    obj,
    approach
  )

  top3 <- tibble::as_tibble(
    top3
  )

  top3$approach <- approach
  top3$display_name <- display_name

  # Keep a consistent solution_rank based on the top-3 order used by
  # each model.
  top3$solution_rank <-
    seq_len(
      nrow(top3)
    )

  solution_rows[[i]] <- top3
}

solution_summary_raw <-
  dplyr::bind_rows(
    solution_rows
  )

# ============================================================
# 7. CLUSTER DIAGNOSTICS
# ============================================================

diag_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  diag_rows[[i]] <-
    normalize_cluster_diagnostics(
      objects[[approach]],
      approach,
      model_registry$display_name[i]
    )
}

cluster_diagnostics <-
  dplyr::bind_rows(
    diag_rows
  )

# ============================================================
# 8. COUNTRY CLOSEST TO EACH CLUSTER CENTROID
# ============================================================

representative_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  display_name <-
    model_registry$display_name[i]

  obj <- objects[[approach]]

  X <- get_model_matrix(
    obj,
    approach
  )

  membership_list <-
    get_membership_list(
      obj,
      approach
    )

  top3 <-
    get_top3(
      obj,
      approach
    )

  representative_rows[[i]] <-
    calculate_cluster_representatives(
      X = X,
      membership_list =
        membership_list,
      top3 = top3,
      approach = approach,
      display_name =
        display_name
    )
}

cluster_representatives <-
  dplyr::bind_rows(
    representative_rows
  ) %>%
  dplyr::arrange(
    match(
      approach,
      model_registry$approach
    ),
    solution_rank,
    cluster
  )

# Add representative country directly to the main cluster table.
cluster_diagnostics_full <-
  cluster_diagnostics %>%
  dplyr::left_join(

    cluster_representatives %>%
      dplyr::select(
        approach,
        solution_rank,
        k,
        cluster,
        representative_country,
        distance_to_centroid,
        is_ukraine_cluster
      ),

    by = c(
      "approach",
      "solution_rank",
      "k",
      "cluster"
    )
  ) %>%
  dplyr::arrange(
    match(
      approach,
      model_registry$approach
    ),
    solution_rank,
    cluster
  )

# ============================================================
# 9. SOLUTION-LEVEL MULTI-CRITERION DIAGNOSTICS
# ============================================================
#
# No composite score is created. The table simply places the relevant
# internal-validity criteria side by side.
# ============================================================

solution_diagnostics <-
  cluster_diagnostics_full %>%
  dplyr::group_by(
    approach,
    display_name,
    solution_rank,
    k
  ) %>%
  dplyr::summarise(

    solution_mean_silhouette =
      dplyr::first(
        solution_mean_silhouette
      ),

    n_clusters =
      dplyr::n(),

    smallest_cluster_n =
      min(n),

    largest_cluster_n =
      max(n),

    min_cluster_mean_silhouette =
      min(
        mean_silhouette,
        na.rm = TRUE
      ),

    mean_cluster_mean_silhouette =
      mean(
        mean_silhouette,
        na.rm = TRUE
      ),

    min_jaccard =
      min(
        jaccard,
        na.rm = TRUE
      ),

    mean_jaccard =
      mean(
        jaccard,
        na.rm = TRUE
      ),

    max_dissolution_pct =
      max(
        dissolution_pct,
        na.rm = TRUE
      ),

    min_recovery_pct =
      min(
        recovery_pct,
        na.rm = TRUE
      ),

    ukraine_cluster =
      dplyr::first(
        ukraine_cluster
      ),

    ukraine_cluster_n =
      n[
        cluster ==
          dplyr::first(
            ukraine_cluster
          )
      ][1],

    .groups = "drop"
  ) %>%
  dplyr::arrange(
    match(
      approach,
      model_registry$approach
    ),
    solution_rank
  )

# ============================================================
# 10. CENTROIDS ACROSS APPROACHES
# ============================================================

centroid_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  centroid_rows[[i]] <-
    normalize_centroids(
      objects[[approach]],
      approach,
      model_registry$display_name[i]
    )
}

centroids_long <-
  dplyr::bind_rows(
    centroid_rows
  ) %>%
  dplyr::arrange(
    match(
      approach,
      model_registry$approach
    ),
    solution_rank,
    cluster,
    coordinate
  )

# ============================================================
# 11. UKRAINE CLUSTER MEMBERSHIP
# ============================================================

ukr_cluster_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  ukr_cluster_rows[[i]] <-
    normalize_ukraine_clusters(
      objects[[approach]],
      approach,
      model_registry$display_name[i]
    )
}

ukraine_clusters <-
  dplyr::bind_rows(
    ukr_cluster_rows
  ) %>%
  dplyr::arrange(
    match(
      approach,
      model_registry$approach
    ),
    solution_rank
  )

# ============================================================
# 12. UKRAINE GLOBAL TOP-10 NEIGHBOURS
# ============================================================

neighbour_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  neighbour_rows[[i]] <-
    normalize_neighbours(
      objects[[approach]],
      approach,
      model_registry$display_name[i]
    )
}

ukraine_neighbours <-
  dplyr::bind_rows(
    neighbour_rows
  ) %>%
  dplyr::arrange(
    match(
      approach,
      model_registry$approach
    ),
    neighbor_rank
  )

# ============================================================
# 13. PAIRWISE TOP-10 NEIGHBOUR OVERLAP
# ============================================================

overlap_rows <- list()
counter <- 1L

for (i in seq_len(
  nrow(model_registry) - 1L
)) {

  for (j in (i + 1L):nrow(model_registry)) {

    a1 <- model_registry$approach[i]
    a2 <- model_registry$approach[j]

    set1 <- ukraine_neighbours %>%
      dplyr::filter(
        approach == a1
      ) %>%
      dplyr::pull(
        country
      ) %>%
      unique()

    set2 <- ukraine_neighbours %>%
      dplyr::filter(
        approach == a2
      ) %>%
      dplyr::pull(
        country
      ) %>%
      unique()

    common <- intersect(
      set1,
      set2
    )

    union_set <- union(
      set1,
      set2
    )

    overlap_rows[[counter]] <-
      tibble::tibble(

        approach_1 = a1,

        approach_1_name =
          model_registry$display_name[i],

        approach_2 = a2,

        approach_2_name =
          model_registry$display_name[j],

        overlap_n =
          length(common),

        jaccard_top10 =
          length(common) /
          length(union_set),

        common_countries =
          paste(
            sort(common),
            collapse = "; "
          )
      )

    counter <- counter + 1L
  }
}

ukraine_neighbour_overlap <-
  dplyr::bind_rows(
    overlap_rows
  ) %>%
  dplyr::arrange(
    desc(overlap_n),
    desc(jaccard_top10)
  )

# ============================================================
# 14. NEIGHBOUR RANK MATRIX
# ============================================================

ukraine_neighbour_rank_matrix <-
  ukraine_neighbours %>%
  dplyr::select(
    approach,
    country,
    neighbor_rank
  ) %>%
  tidyr::pivot_wider(
    names_from = approach,
    values_from = neighbor_rank
  ) %>%
  dplyr::mutate(
    models_in_top10 =
      rowSums(
        !is.na(
          dplyr::across(
            -country
          )
        )
      )
  ) %>%
  dplyr::arrange(
    desc(models_in_top10),
    country
  )

# ============================================================
# 15. CROSS-MODEL COUNTRY-CLUSTER MEMBERSHIP TABLE
# ============================================================
#
# This table is useful for checking whether a country's grouping is
# persistent across representations. Cluster numbers are model-specific
# and should NOT be interpreted as equivalent labels across approaches.
# ============================================================

membership_rows <- list()

for (i in seq_len(
  nrow(model_registry)
)) {

  approach <-
    model_registry$approach[i]

  obj <- objects[[approach]]
  top3 <- get_top3(
    obj,
    approach
  )
  membership_list <-
    get_membership_list(
      obj,
      approach
    )

  rows_i <- list()

  for (s in seq_len(
    nrow(top3)
  )) {

    k <- top3$k[s]

    groups <-
      membership_list[[as.character(k)]]

    rows_i[[s]] <-
      tibble::tibble(
        approach = approach,
        display_name =
          model_registry$display_name[i],
        solution_rank = s,
        k = as.integer(k),
        country = names(groups),
        cluster =
          as.integer(groups)
      )
  }

  membership_rows[[i]] <-
    dplyr::bind_rows(
      rows_i
    )
}

membership_all <-
  dplyr::bind_rows(
    membership_rows
  )

# ============================================================
# 16. VALIDATION
# ============================================================

expected_approaches <-
  model_registry$approach

check_approaches <- function(
  x,
  table_name
) {

  missing <- setdiff(
    expected_approaches,
    unique(x$approach)
  )

  if (length(missing) > 0) {
    stop(
      paste0(
        table_name,
        " is missing approaches: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }

  invisible(TRUE)
}

check_approaches(
  cluster_diagnostics_full,
  "cluster_diagnostics_full"
)

check_approaches(
  cluster_representatives,
  "cluster_representatives"
)

check_approaches(
  ukraine_neighbours,
  "ukraine_neighbours"
)

check_approaches(
  ukraine_clusters,
  "ukraine_clusters"
)

# Each approach should contribute exactly three solutions.
solution_count_check <-
  cluster_diagnostics_full %>%
  dplyr::distinct(
    approach,
    solution_rank,
    k
  ) %>%
  dplyr::count(
    approach,
    name = "n_top_solutions"
  )

if (any(
  solution_count_check$n_top_solutions != 3L
)) {
  stop(
    "At least one approach does not contribute exactly three top solutions."
  )
}

# Every cluster must have one nearest-centroid representative.
rep_key_check <-
  cluster_representatives %>%
  dplyr::count(
    approach,
    solution_rank,
    k,
    cluster
  )

if (any(
  rep_key_check$n != 1L
)) {
  stop(
    "Representative-country table contains duplicate cluster keys."
  )
}

# ============================================================
# 17. EXPORT CSV FILES
# ============================================================

readr::write_csv(
  model_metadata,
  file.path(
    OUT_DIR,
    "comparison_model_metadata.csv"
  )
)

readr::write_csv(
  solution_diagnostics,
  file.path(
    OUT_DIR,
    "comparison_solution_diagnostics.csv"
  )
)

readr::write_csv(
  cluster_diagnostics_full,
  file.path(
    OUT_DIR,
    "comparison_cluster_diagnostics.csv"
  )
)

readr::write_csv(
  cluster_representatives,
  file.path(
    OUT_DIR,
    "comparison_cluster_representatives.csv"
  )
)

readr::write_csv(
  centroids_long,
  file.path(
    OUT_DIR,
    "comparison_centroids_long.csv"
  )
)

readr::write_csv(
  ukraine_clusters,
  file.path(
    OUT_DIR,
    "comparison_ukraine_clusters.csv"
  )
)

readr::write_csv(
  ukraine_neighbours,
  file.path(
    OUT_DIR,
    "comparison_ukraine_neighbours.csv"
  )
)

readr::write_csv(
  ukraine_neighbour_overlap,
  file.path(
    OUT_DIR,
    "comparison_ukraine_neighbour_overlap.csv"
  )
)

readr::write_csv(
  ukraine_neighbour_rank_matrix,
  file.path(
    OUT_DIR,
    "comparison_ukraine_neighbour_rank_matrix.csv"
  )
)

readr::write_csv(
  membership_all,
  file.path(
    OUT_DIR,
    "comparison_all_memberships.csv"
  )
)

# ============================================================
# 18. EXPORT FINAL EXCEL WORKBOOK
# ============================================================

notes <- tibble::tribble(
  ~topic, ~note,

  "Cluster representative",
  "representative_country is the observed country with minimum Euclidean distance to the arithmetic centroid of its cluster in the same model space used for clustering; it is not a medoid. The column ukraine_cluster gives the cluster number containing Ukraine for that solution, and is_ukraine_cluster marks the corresponding cluster row.",

  "Centroid distance",
  "distance_to_centroid is model-specific and should not be compared numerically across representations with different dimensionality or scaling.",

  "Ukraine neighbours",
  "Ukraine top-10 neighbours are calculated globally in each model's full multidimensional representation and are independent of the selected country-cluster k.",

  "Neighbour distances",
  "Raw distance magnitudes should not be compared across models; compare neighbour ranks and set overlap instead.",

  "Cluster labels",
  "Cluster numbers are local to each model and each k solution. Cluster 1 in one model is not assumed to correspond to cluster 1 in another model.",

  "Solution evaluation",
  "No composite validity score is calculated. Mean silhouette, cluster silhouette, Jaccard, dissolution and recovery are kept as separate criteria."
)

xlsx_output <- list(

  notes =
    notes,

  model_metadata =
    model_metadata,

  solution_diagnostics =
    solution_diagnostics,

  cluster_diagnostics =
    cluster_diagnostics_full,

  cluster_representatives =
    cluster_representatives,

  centroids_long =
    centroids_long,

  ukraine_clusters =
    ukraine_clusters,

  ukraine_neighbours =
    ukraine_neighbours,

  neighbour_overlap =
    ukraine_neighbour_overlap,

  neighbour_rank_matrix =
    ukraine_neighbour_rank_matrix,

  all_memberships =
    membership_all
)

xlsx_file <- file.path(
  OUT_DIR,
  "HCA_comparison_tables.xlsx"
)

writexl::write_xlsx(
  xlsx_output,
  xlsx_file
)

# ============================================================
# 19. SAVE COMPARISON RDS
# ============================================================

comparison_object <- list(

  script_version =
    "build_HCA_comparison_tables — 2026-09-12",

  model_registry =
    model_registry,

  resolved_files =
    resolved_files,

  model_metadata =
    model_metadata,

  solution_summary_raw =
    solution_summary_raw,

  solution_diagnostics =
    solution_diagnostics,

  cluster_diagnostics =
    cluster_diagnostics_full,

  cluster_representatives =
    cluster_representatives,

  centroids_long =
    centroids_long,

  ukraine_clusters =
    ukraine_clusters,

  ukraine_neighbours =
    ukraine_neighbours,

  ukraine_neighbour_overlap =
    ukraine_neighbour_overlap,

  ukraine_neighbour_rank_matrix =
    ukraine_neighbour_rank_matrix,

  membership_all =
    membership_all
)

rds_file <- file.path(
  OUT_DIR,
  "HCA_comparison_tables.rds"
)

saveRDS(
  comparison_object,
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
# 20. FINISH
# ============================================================

cat("\n============================================================\n")
cat("FINAL HCA COMPARISON TABLES COMPLETE\n")
cat("============================================================\n")

cat(
  "Approaches:",
  nrow(model_registry),
  "\n"
)

cat(
  "Cluster rows:",
  nrow(cluster_diagnostics_full),
  "\n"
)

cat(
  "Cluster representatives:",
  nrow(cluster_representatives),
  "\n"
)

cat(
  "Ukraine neighbour rows:",
  nrow(ukraine_neighbours),
  "\n"
)

cat(
  "Excel:",
  xlsx_file,
  "\n"
)

cat(
  "RDS:",
  rds_file,
  "\n"
)
