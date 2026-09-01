# ============================================================
# HCA VARIABLE IMPORTANCE ANALYSIS — VARIANT B
# ============================================================
#
# Окремий аналіз важливості змінних для основної HCA-моделі:
#
# 1. Частка міжкластерної дисперсії:
#       eta^2 = BSS / TSS
#
# 2. Leave-one-variable-out sensitivity:
#       почергово вилучаємо одну змінну, повторюємо HCA,
#       порівнюємо розбиття з основним через Adjusted Rand Index (ARI)
#
# 3. Permutation importance:
#       випадково перемішуємо одну змінну між країнами,
#       повторюємо HCA багато разів і оцінюємо падіння ARI
#
# Основна модель:
#   - Variant B
#   - 15 змінних
#   - стандартизація z
#   - рівні ваги змінних
#   - Euclidean distance
#   - Ward.D2
#   - основна інтерпретація: k = 5
#
# Скрипт очікує файл:
#   results/war_participation_HCA_extended_variant_B.rds
#
# Виходи:
#   results/HCA_variable_importance_variant_B/
#     between_cluster_variance.csv
#     leave_one_out_k5.csv
#     leave_one_out_k2_k6.csv
#     permutation_importance_k5.csv
#     permutation_raw_k5.csv
#     importance_comparison_k5.csv
#     HCA_variable_importance_variant_B.xlsx   [якщо є openxlsx]
#
# ============================================================


# ============================================================
# 0. НАЛАШТУВАННЯ
# ============================================================

RDS_FILE <- file.path("results", "war_participation_HCA_extended_variant_B.rds")

OUT_DIR <- file.path("results", "HCA_variable_importance_variant_B")

TARGET_K <- 5

# Для leave-one-variable-out додатково оцінюємо кілька k.
LOO_K_GRID <- 2:6

# Кількість перестановок для permutation importance.
# 1000 — рекомендований фінальний варіант.
# Для швидкого тесту можна тимчасово поставити 100.
PERM_B <- 1000

SEED <- 20260901

# На Linux/macOS permutation analysis може використовувати кілька ядер.
# Для максимальної консервативності можна поставити N_CORES <- 1.
detected_cores <- parallel::detectCores(logical = TRUE)

N_CORES <- max(
  1L,
  min(
    4L,
    ifelse(
      is.na(detected_cores),
      1L,
      detected_cores - 1L
    )
  )
)

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)


# ============================================================
# 1. ДОПОМІЖНІ ФУНКЦІЇ
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


adjusted_rand_index <- function(x, y) {

  if (length(x) != length(y)) {
    stop("ARI: vectors have different lengths.")
  }

  tab <- table(x, y)

  comb2 <- function(z) {
    z * (z - 1) / 2
  }

  n <- sum(tab)

  if (n < 2) {
    return(NA_real_)
  }

  sum_nij <- sum(
    comb2(tab)
  )

  sum_ai <- sum(
    comb2(
      rowSums(tab)
    )
  )

  sum_bj <- sum(
    comb2(
      colSums(tab)
    )
  )

  total_pairs <- comb2(n)

  expected_index <-
    (sum_ai * sum_bj) /
    total_pairs

  max_index <-
    0.5 *
    (sum_ai + sum_bj)

  denominator <-
    max_index -
    expected_index

  if (
    !is.finite(denominator) ||
    abs(denominator) <
      .Machine$double.eps
  ) {
    return(1)
  }

  (
    sum_nij -
      expected_index
  ) /
    denominator
}


make_hc <- function(
  X_z,
  weights
) {

  weights <-
    weights[
      colnames(X_z)
    ]

  weights <-
    weights /
    sum(weights)

  X_weighted <-
    sweep(
      X_z,
      2,
      sqrt(weights),
      FUN = "*"
    )

  stats::hclust(
    stats::dist(
      X_weighted,
      method = "euclidean"
    ),
    method = "ward.D2"
  )
}


variable_labels_ua <- c(

  n_conflicts =
    "Кількість конфліктів",

  total_exposure =
    "Сукупна тривалість участі",

  war_share =
    "Частка років високої інтенсивності",

  continuity =
    "Безперервність участі",

  recency =
    "Давність останньої участі",

  interstate_share =
    "Частка міждержавного конфлікту",

  intl_intrastate_share =
    "Частка інтернаціоналізованого внутрішньодержавного конфлікту",

  intrastate_share =
    "Частка внутрішньодержавного конфлікту",

  home_defensive_share =
    "Частка оборонної участі на власній території",

  home_civil_share =
    "Частка внутрішнього конфлікту на власній території",

  abroad_share =
    "Частка участі за кордоном",

  population_size =
    "Чисельність населення",

  personnel_size =
    "Чисельність військового персоналу",

  personnel_share =
    "Частка військового персоналу в населенні",

  mil_expenditure =
    "Військові видатки, % ВВП"
)


get_label_ua <- function(variable) {

  z <-
    unname(
      variable_labels_ua[
        variable
      ]
    )

  z[
    is.na(z)
  ] <-
    variable[
      is.na(z)
    ]

  z
}


# ============================================================
# 2. ЗАВАНТАЖЕННЯ ОСНОВНОЇ HCA-МОДЕЛІ
# ============================================================

if (
  file.exists(
    RDS_FILE
  )
) {

  hca_obj <-
    readRDS(
      RDS_FILE
    )

  required_names <- c(
    "hc",
    "variable_weights",
    "analysis_data"
  )

  missing_names <-
    setdiff(
      required_names,
      names(hca_obj)
    )

  if (
    length(
      missing_names
    ) > 0
  ) {

    stop(
      paste0(
        "RDS does not contain required objects: ",
        paste(
          missing_names,
          collapse = ", "
        )
      )
    )
  }

  hc <-
    hca_obj$hc

  variable_weights <-
    hca_obj$variable_weights

  analysis_data <-
    hca_obj$analysis_data

  D_weighted <-
    hca_obj$D_weighted %||%
    NULL

  cat(
    "Loaded:",
    RDS_FILE,
    "\n"
  )

} else {

  required_global <- c(
    "hc",
    "variable_weights",
    "analysis_data"
  )

  missing_global <-
    required_global[
      !vapply(
        required_global,
        exists,
        logical(1),
        envir = .GlobalEnv,
        inherits = FALSE
      )
    ]

  if (
    length(
      missing_global
    ) > 0
  ) {

    stop(
      paste0(
        "Cannot find ",
        RDS_FILE,
        " and required workspace objects are missing: ",
        paste(
          missing_global,
          collapse = ", "
        )
      )
    )
  }

  hc <-
    get(
      "hc",
      envir = .GlobalEnv
    )

  variable_weights <-
    get(
      "variable_weights",
      envir = .GlobalEnv
    )

  analysis_data <-
    get(
      "analysis_data",
      envir = .GlobalEnv
    )

  D_weighted <-
    if (
      exists(
        "D_weighted",
        envir = .GlobalEnv,
        inherits = FALSE
      )
    ) {

      get(
        "D_weighted",
        envir = .GlobalEnv
      )

    } else {

      NULL
    }

  cat(
    "RDS not found; using current R workspace.\n"
  )
}


# ============================================================
# 3. РЕКОНСТРУКЦІЯ 15-ВИМІРНОГО ПРОСТОРУ
# ============================================================

if (
  !"country" %in%
  names(
    analysis_data
  )
) {
  stop(
    "analysis_data does not contain country."
  )
}


if (
  is.null(
    names(
      variable_weights
    )
  )
) {
  stop(
    "variable_weights must be a named vector."
  )
}


hca_vars <-
  names(
    variable_weights
  )


missing_vars <-
  setdiff(
    hca_vars,
    names(
      analysis_data
    )
  )


if (
  length(
    missing_vars
  ) > 0
) {

  stop(
    paste0(
      "analysis_data does not contain: ",
      paste(
        missing_vars,
        collapse = ", "
      )
    )
  )
}


# Відновлюємо точний порядок країн,
# який використовувався в матриці відстаней / hc.

target_order <-
  if (
    !is.null(
      D_weighted
    ) &&
    !is.null(
      attr(
        D_weighted,
        "Labels"
      )
    )
  ) {

    attr(
      D_weighted,
      "Labels"
    )

  } else if (
    !is.null(
      hc$labels
    )
  ) {

    hc$labels

  } else {

    analysis_data$country
  }


if (
  !setequal(
    target_order,
    analysis_data$country
  )
) {
  stop(
    "Country set in analysis_data differs from hc / D_weighted."
  )
}


analysis_data <-
  analysis_data[
    match(
      target_order,
      analysis_data$country
    ),
    ,
    drop = FALSE
  ]


X <-
  as.data.frame(
    analysis_data[
      ,
      hca_vars,
      drop = FALSE
    ]
  )


rownames(
  X
) <-
  analysis_data$country


if (
  anyNA(
    X
  )
) {
  stop(
    "X contains missing values."
  )
}


X_z <-
  scale(
    X
  )


X_z <-
  as.matrix(
    X_z
  )


rownames(
  X_z
) <-
  rownames(
    X
  )


variable_weights <-
  variable_weights[
    colnames(
      X_z
    )
  ]


variable_weights <-
  variable_weights /
  sum(
    variable_weights
  )


n_countries <-
  nrow(
    X_z
  )


n_variables <-
  ncol(
    X_z
  )


cat(
  "Countries:",
  n_countries,
  "\n"
)

cat(
  "Variables:",
  n_variables,
  "\n"
)


if (
  max(
    abs(
      variable_weights -
        1 / n_variables
    )
  ) > 1e-10
) {

  warning(
    paste0(
      "Saved model does not appear to use exactly equal variable weights. ",
      "The script will preserve the saved relative weights."
    )
  )

} else {

  cat(
    "Variable weights: equal (1/",
    n_variables,
    ").\n",
    sep = ""
  )
}


# ============================================================
# 4. ПЕРЕВІРКА ВІДТВОРЕННЯ ОСНОВНОЇ МОДЕЛІ
# ============================================================

hc_reconstructed <-
  make_hc(
    X_z,
    variable_weights
  )


for (
  k in unique(
    c(
      TARGET_K,
      LOO_K_GRID
    )
  )
) {

  baseline_saved <-
    stats::cutree(
      hc,
      k = k
    )

  baseline_reconstructed <-
    stats::cutree(
      hc_reconstructed,
      k = k
    )

  ari_check <-
    adjusted_rand_index(
      baseline_saved,
      baseline_reconstructed
    )

  if (
    !is.finite(
      ari_check
    ) ||
    ari_check < 0.999999
  ) {

    stop(
      paste0(
        "Reconstructed HCA does not match saved hc at k = ",
        k,
        "; ARI = ",
        round(
          ari_check,
          6
        )
      )
    )
  }
}


cat(
  "Baseline HCA reconstruction: OK.\n"
)


baseline_clusters <-
  lapply(
    unique(
      c(
        TARGET_K,
        LOO_K_GRID
      )
    ),
    function(k) {

      z <-
        stats::cutree(
          hc,
          k = k
        )

      names(
        z
      ) <-
        rownames(
          X_z
        )

      z
    }
  )


names(
  baseline_clusters
) <-
  as.character(
    unique(
      c(
        TARGET_K,
        LOO_K_GRID
      )
    )
  )


cluster_target <-
  baseline_clusters[[as.character(TARGET_K)]]


# ============================================================
# 5. АНАЛІЗ 1:
#    ЧАСТКА МІЖКЛАСТЕРНОЇ ДИСПЕРСІЇ
#    eta^2 = BSS / TSS
# ============================================================

cat(
  "\n[1/3] Between-cluster variance...\n"
)


between_cluster_variance <-
  do.call(
    rbind,
    lapply(
      colnames(
        X_z
      ),
      function(v) {

        x <-
          X_z[
            ,
            v
          ]

        grand_mean <-
          mean(
            x
          )

        TSS <-
          sum(
            (
              x -
                grand_mean
            )^2
          )

        cluster_means <-
          tapply(
            x,
            cluster_target,
            mean
          )

        cluster_n <-
          table(
            cluster_target
          )

        BSS <-
          sum(
            as.numeric(
              cluster_n
            ) *
              (
                as.numeric(
                  cluster_means
                ) -
                  grand_mean
              )^2
          )

        WSS <-
          sum(
            vapply(
              split(
                x,
                cluster_target
              ),
              function(z) {

                sum(
                  (
                    z -
                      mean(
                        z
                      )
                  )^2
                )
              },
              numeric(1)
            )
          )

        eta2 <-
          if (
            TSS > 0
          ) {

            BSS /
              TSS

          } else {

            NA_real_
          }

        data.frame(

          variable =
            v,

          variable_ua =
            get_label_ua(
              v
            ),

          TSS =
            TSS,

          BSS =
            BSS,

          WSS =
            WSS,

          eta2_BSS_TSS =
            eta2,

          stringsAsFactors =
            FALSE
        )
      }
    )
  )


between_cluster_variance$share_of_total_BSS <-
  between_cluster_variance$BSS /
  sum(
    between_cluster_variance$BSS
  )


between_cluster_variance <-
  between_cluster_variance[
    order(
      -between_cluster_variance$eta2_BSS_TSS
    ),
    ,
    drop = FALSE
  ]


between_cluster_variance$rank_eta2 <-
  seq_len(
    nrow(
      between_cluster_variance
    )
  )


row.names(
  between_cluster_variance
) <-
  NULL


# ============================================================
# 6. АНАЛІЗ 2:
#    LEAVE-ONE-VARIABLE-OUT SENSITIVITY
# ============================================================

cat(
  "[2/3] Leave-one-variable-out sensitivity...\n"
)


loo_results <-
  do.call(
    rbind,
    lapply(
      colnames(
        X_z
      ),
      function(v_removed) {

        keep <-
          setdiff(
            colnames(
              X_z
            ),
            v_removed
          )

        X_loo <-
          X_z[
            ,
            keep,
            drop = FALSE
          ]

        # Перенормовуємо ваги решти змінних до суми 1.
        # За рівних первинних ваг це еквівалентно
        # рівним вагам 1/(p-1).

        w_loo <-
          variable_weights[
            keep
          ]

        w_loo <-
          w_loo /
          sum(
            w_loo
          )

        hc_loo <-
          make_hc(
            X_loo,
            w_loo
          )

        do.call(
          rbind,
          lapply(
            LOO_K_GRID,
            function(k) {

              baseline_k <-
                baseline_clusters[[as.character(k)]]

              loo_k <-
                stats::cutree(
                  hc_loo,
                  k = k
                )

              ari <-
                adjusted_rand_index(
                  baseline_k,
                  loo_k
                )

              data.frame(

                removed_variable =
                  v_removed,

                variable_ua =
                  get_label_ua(
                    v_removed
                  ),

                k =
                  k,

                ARI =
                  ari,

                importance_1_minus_ARI =
                  1 -
                  ari,

                stringsAsFactors =
                  FALSE
              )
            }
          )
        )
      }
    )
  )


loo_k5 <-
  loo_results[
    loo_results$k ==
      TARGET_K,
    ,
    drop = FALSE
  ]


loo_k5 <-
  loo_k5[
    order(
      loo_k5$ARI
    ),
    ,
    drop = FALSE
  ]


loo_k5$rank_LOO <-
  seq_len(
    nrow(
      loo_k5
    )
  )


row.names(
  loo_k5
) <-
  NULL


# ============================================================
# 7. АНАЛІЗ 3:
#    PERMUTATION IMPORTANCE
# ============================================================

cat(
  "[3/3] Permutation importance...\n"
)

cat(
  "Permutation repetitions per variable:",
  PERM_B,
  "\n"
)

cat(
  "Parallel cores:",
  N_CORES,
  "\n"
)


permutation_one_variable <- function(
  variable_name,
  variable_index
) {

  cat(
    sprintf(
      "  [%02d/%02d] %s\n",
      variable_index,
      n_variables,
      variable_name
    )
  )

  one_rep <- function(b) {

    # Окремий детермінований seed для кожної
    # змінної та кожної перестановки.

    set.seed(
      SEED +
        variable_index *
        100000L +
        b
    )

    perm_index <-
      sample.int(
        n_countries
      )

    X_perm <-
      X_z

    X_perm[
      ,
      variable_name
    ] <-
      X_z[
        perm_index,
        variable_name
      ]

    hc_perm <-
      make_hc(
        X_perm,
        variable_weights
      )

    perm_cluster <-
      stats::cutree(
        hc_perm,
        k = TARGET_K
      )

    adjusted_rand_index(
      cluster_target,
      perm_cluster
    )
  }


  if (
    .Platform$OS.type != "windows" &&
    N_CORES > 1
  ) {

    ari_values <-
      unlist(
        parallel::mclapply(

          X =
            seq_len(
              PERM_B
            ),

          FUN =
            one_rep,

          mc.cores =
            N_CORES,

          mc.set.seed =
            FALSE
        ),
        use.names = FALSE
      )

  } else {

    ari_values <-
      vapply(
        seq_len(
          PERM_B
        ),
        one_rep,
        numeric(1)
      )
  }


  data.frame(

    variable =
      variable_name,

    variable_ua =
      get_label_ua(
        variable_name
      ),

    permutation =
      seq_len(
        PERM_B
      ),

    ARI =
      ari_values,

    stringsAsFactors =
      FALSE
  )
}


permutation_raw <-
  do.call(
    rbind,
    lapply(
      seq_along(
        colnames(
          X_z
        )
      ),
      function(j) {

        permutation_one_variable(
          colnames(
            X_z
          )[
            j
          ],
          j
        )
      }
    )
  )


split_perm <-
  split(
    permutation_raw,
    permutation_raw$variable
  )


permutation_summary <-
  do.call(
    rbind,
    lapply(
      split_perm,
      function(df) {

        mean_ari <-
          mean(
            df$ARI
          )

        q <-
          stats::quantile(
            df$ARI,
            probs = c(
              0.025,
              0.25,
              0.50,
              0.75,
              0.975
            ),
            na.rm = TRUE,
            names = FALSE
          )

        data.frame(

          variable =
            df$variable[
              1
            ],

          variable_ua =
            df$variable_ua[
              1
            ],

          B =
            nrow(
              df
            ),

          mean_ARI =
            mean_ari,

          sd_ARI =
            stats::sd(
              df$ARI
            ),

          q025_ARI =
            q[
              1
            ],

          q25_ARI =
            q[
              2
            ],

          median_ARI =
            q[
              3
            ],

          q75_ARI =
            q[
              4
            ],

          q975_ARI =
            q[
              5
            ],

          permutation_importance =
            1 -
              mean_ari,

          importance_q025 =
            1 -
              q[
                5
              ],

          importance_q975 =
            1 -
              q[
                1
              ],

          pct_ARI_below_0_80 =
            100 *
              mean(
                df$ARI <
                  0.80
              ),

          pct_ARI_below_0_60 =
            100 *
              mean(
                df$ARI <
                  0.60
              ),

          stringsAsFactors =
            FALSE
        )
      }
    )
  )


permutation_summary <-
  permutation_summary[
    order(
      -permutation_summary$permutation_importance
    ),
    ,
    drop = FALSE
  ]


permutation_summary$rank_permutation <-
  seq_len(
    nrow(
      permutation_summary
    )
  )


row.names(
  permutation_summary
) <-
  NULL


# ============================================================
# 8. ПОРІВНЯЛЬНА ТАБЛИЦЯ ТРЬОХ МЕТОДІВ
# ============================================================

importance_comparison <-
  data.frame(

    variable =
      colnames(
        X_z
      ),

    variable_ua =
      get_label_ua(
        colnames(
          X_z
        )
      ),

    stringsAsFactors =
      FALSE
  )


importance_comparison <-
  merge(

    importance_comparison,

    between_cluster_variance[
      ,
      c(
        "variable",
        "eta2_BSS_TSS",
        "share_of_total_BSS",
        "rank_eta2"
      )
    ],

    by =
      "variable",

    all.x =
      TRUE,

    sort =
      FALSE
  )


importance_comparison <-
  merge(

    importance_comparison,

    loo_k5[
      ,
      c(
        "removed_variable",
        "ARI",
        "importance_1_minus_ARI",
        "rank_LOO"
      )
    ],

    by.x =
      "variable",

    by.y =
      "removed_variable",

    all.x =
      TRUE,

    sort =
      FALSE
  )


names(
  importance_comparison
)[
  names(
    importance_comparison
  ) == "ARI"
] <-
  "LOO_ARI_k5"


names(
  importance_comparison
)[
  names(
    importance_comparison
  ) == "importance_1_minus_ARI"
] <-
  "LOO_importance_k5"


importance_comparison <-
  merge(

    importance_comparison,

    permutation_summary[
      ,
      c(
        "variable",
        "mean_ARI",
        "permutation_importance",
        "rank_permutation"
      )
    ],

    by =
      "variable",

    all.x =
      TRUE,

    sort =
      FALSE
  )


names(
  importance_comparison
)[
  names(
    importance_comparison
  ) == "mean_ARI"
] <-
  "permutation_mean_ARI_k5"


importance_comparison <-
  importance_comparison[
    match(
      colnames(
        X_z
      ),
      importance_comparison$variable
    ),
    ,
    drop = FALSE
  ]


row.names(
  importance_comparison
) <-
  NULL


# ============================================================
# 9. НАЛАШТУВАННЯ / МЕТАДАНІ
# ============================================================

settings <- data.frame(

  parameter = c(

    "RDS_FILE",
    "n_countries",
    "n_variables",
    "TARGET_K",
    "LOO_K_GRID",
    "PERM_B",
    "SEED",
    "N_CORES",
    "distance",
    "linkage",
    "standardization",
    "weights_equal",
    "weight_each_if_equal"
  ),

  value = c(

    RDS_FILE,

    n_countries,

    n_variables,

    TARGET_K,

    paste(
      LOO_K_GRID,
      collapse = ","
    ),

    PERM_B,

    SEED,

    N_CORES,

    "Euclidean",

    "Ward.D2",

    "z-score",

    max(
      abs(
        variable_weights -
          1 / n_variables
      )
    ) <=
      1e-10,

    if (
      max(
        abs(
          variable_weights -
            1 / n_variables
        )
      ) <=
        1e-10
    ) {

      1 /
        n_variables

    } else {

      NA_real_
    }
  ),

  stringsAsFactors =
    FALSE
)


# ============================================================
# 10. ЗБЕРЕЖЕННЯ CSV
# ============================================================

utils::write.csv(

  between_cluster_variance,

  file =
    file.path(
      OUT_DIR,
      "between_cluster_variance.csv"
    ),

  row.names =
    FALSE,

  fileEncoding =
    "UTF-8"
)


utils::write.csv(

  loo_k5,

  file =
    file.path(
      OUT_DIR,
      "leave_one_out_k5.csv"
    ),

  row.names =
    FALSE,

  fileEncoding =
    "UTF-8"
)


utils::write.csv(

  loo_results,

  file =
    file.path(
      OUT_DIR,
      "leave_one_out_k2_k6.csv"
    ),

  row.names =
    FALSE,

  fileEncoding =
    "UTF-8"
)


utils::write.csv(

  permutation_summary,

  file =
    file.path(
      OUT_DIR,
      "permutation_importance_k5.csv"
    ),

  row.names =
    FALSE,

  fileEncoding =
    "UTF-8"
)


utils::write.csv(

  permutation_raw,

  file =
    file.path(
      OUT_DIR,
      "permutation_raw_k5.csv"
    ),

  row.names =
    FALSE,

  fileEncoding =
    "UTF-8"
)


utils::write.csv(

  importance_comparison,

  file =
    file.path(
      OUT_DIR,
      "importance_comparison_k5.csv"
    ),

  row.names =
    FALSE,

  fileEncoding =
    "UTF-8"
)


# ============================================================
# 11. EXCEL WORKBOOK
# ============================================================

xlsx_file <-
  file.path(
    OUT_DIR,
    "HCA_variable_importance_variant_B.xlsx"
  )


if (
  requireNamespace(
    "openxlsx",
    quietly = TRUE
  )
) {

  wb <-
    openxlsx::createWorkbook()


  openxlsx::addWorksheet(
    wb,
    "between_cluster_variance"
  )

  openxlsx::writeData(
    wb,
    "between_cluster_variance",
    between_cluster_variance
  )


  openxlsx::addWorksheet(
    wb,
    "leave_one_out_k5"
  )

  openxlsx::writeData(
    wb,
    "leave_one_out_k5",
    loo_k5
  )


  openxlsx::addWorksheet(
    wb,
    "leave_one_out_k2_k6"
  )

  openxlsx::writeData(
    wb,
    "leave_one_out_k2_k6",
    loo_results
  )


  openxlsx::addWorksheet(
    wb,
    "permutation_importance_k5"
  )

  openxlsx::writeData(
    wb,
    "permutation_importance_k5",
    permutation_summary
  )


  openxlsx::addWorksheet(
    wb,
    "permutation_raw_k5"
  )

  openxlsx::writeData(
    wb,
    "permutation_raw_k5",
    permutation_raw
  )


  openxlsx::addWorksheet(
    wb,
    "importance_comparison_k5"
  )

  openxlsx::writeData(
    wb,
    "importance_comparison_k5",
    importance_comparison
  )


  openxlsx::addWorksheet(
    wb,
    "settings"
  )

  openxlsx::writeData(
    wb,
    "settings",
    settings
  )


  # Просте форматування

  header_style <-
    openxlsx::createStyle(
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      wrapText = TRUE
    )


  for (
    sheet in names(
      wb
    )
  ) {

    openxlsx::addStyle(
      wb,
      sheet,
      header_style,
      rows = 1,
      cols = 1:50,
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::freezePane(
      wb,
      sheet,
      firstRow = TRUE
    )

    openxlsx::setColWidths(
      wb,
      sheet,
      cols = 1:50,
      widths = "auto"
    )
  }


  openxlsx::saveWorkbook(
    wb,
    xlsx_file,
    overwrite = TRUE
  )


  cat(
    "Excel saved:",
    xlsx_file,
    "\n"
  )

} else {

  message(
    paste0(
      "Package 'openxlsx' is not installed. ",
      "CSV files were saved, but XLSX was skipped. ",
      "Install with: install.packages('openxlsx')"
    )
  )
}


# ============================================================
# 12. КОНСОЛЬНИЙ ПІДСУМОК
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "HCA VARIABLE IMPORTANCE COMPLETED\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Top variables by between-cluster eta^2:\n"
)

print(
  head(
    between_cluster_variance[
      ,
      c(
        "variable",
        "variable_ua",
        "eta2_BSS_TSS",
        "rank_eta2"
      )
    ],
    10
  )
)


cat(
  "\nTop variables by leave-one-out sensitivity (lowest ARI):\n"
)

print(
  head(
    loo_k5[
      ,
      c(
        "removed_variable",
        "variable_ua",
        "ARI",
        "importance_1_minus_ARI",
        "rank_LOO"
      )
    ],
    10
  )
)


cat(
  "\nTop variables by permutation importance:\n"
)

print(
  head(
    permutation_summary[
      ,
      c(
        "variable",
        "variable_ua",
        "mean_ARI",
        "permutation_importance",
        "rank_permutation"
      )
    ],
    10
  )
)


cat(
  "\nOutput directory:",
  normalizePath(
    OUT_DIR,
    mustWork = FALSE
  ),
  "\n"
)


cat(
  "\nInterpretation:\n",
  "  eta^2: higher = clusters differ more strongly on the variable.\n",
  "  LOO ARI: lower = removing the variable changes clustering more.\n",
  "  permutation importance: higher = destroying the variable's information changes clustering more.\n",
  sep = ""
)


cat(
  "\nIMPORTANT:\n",
  "eta^2 here is descriptive, not an inferential ANOVA test,\n",
  "because the same variables were used to construct the clusters.\n",
  sep = ""
)


cat(
  "\n========================================\n"
)
