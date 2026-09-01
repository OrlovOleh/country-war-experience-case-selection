# ============================================================
# КЛАСТЕРНА СТРУКТУРА КРАЇН
#
# Панель A:
#   дендрограма Ward.D2
#
# Панель B:
#   PCA-проєкція 15-вимірного простору основної моделі
#   з рівними вагами змінних
#
# Виділено:
#   - Україну
#   - 10 найближчих сусідів України
#   - центроїди 5 кластерів
#   - країни, найближчі до центроїдів
#   - контури K1–K5 на дендрограмі з кольорами й типами
#     ліній, що відповідають легенді панелі B
#
# Для чорно-білого друку:
#   - кластери мають різні типи контурних ліній
#
# Вихід:
#   TIFF A4 landscape — 600 dpi
#   PDF A4 landscape
#   PNG A4 landscape — 300 dpi
#
# ============================================================

cat(
  "IMAGE SCRIPT VERSION: image_generation_hca — 2026-08-31 equal-weight Variant B\n"
)



# ============================================================
# 0. ПАКЕТИ
# ============================================================

packages_fig <- c(
  "ggplot2",
  "factoextra",
  "ggrepel",
  "dplyr",
  "tibble",
  "patchwork"
)

new_packages <- packages_fig[
  !(packages_fig %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

invisible(
  lapply(
    packages_fig,
    library,
    character.only = TRUE
  )
)


# ============================================================
# 1. ЗАВАНТАЖЕННЯ РЕЗУЛЬТАТІВ HCA
# ============================================================
#
# Основний шлях: читаємо відтворюваний RDS, створений
# HCA_variant_B_full_1.R. Це усуває залежність від .RData
# та від об'єктів, що могли залишитися у Workspace.
#
# Із RDS відновлюються:
#   X_weighted
#   hc
#   cluster5
#   ukraine_neighbors
#
# Якщо RDS відсутній, дозволено fallback на вже наявні
# об'єкти поточної R-сесії.
# ============================================================

# Repository-relative paths. Run this script from the repository root.
FIGURES_DIR <- "figures"
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

rds_file <-
  file.path("results", "war_participation_HCA_extended_variant_B.rds")

if (file.exists(rds_file)) {

  hca_results <- readRDS(rds_file)

  required_rds_objects <- c(
    "hc",
    "D_weighted",
    "variable_weights",
    "analysis_data"
  )

  missing_rds_objects <- setdiff(
    required_rds_objects,
    names(hca_results)
  )

  if (length(missing_rds_objects) > 0) {
    stop(
      paste0(
        "У RDS відсутні необхідні об'єкти: ",
        paste(
          missing_rds_objects,
          collapse = ", "
        )
      )
    )
  }

  hc <- hca_results$hc
  D_weighted <- hca_results$D_weighted
  variable_weights <- hca_results$variable_weights
  analysis_data <- hca_results$analysis_data

  if (
    is.null(names(variable_weights)) ||
      anyNA(variable_weights) ||
      length(variable_weights) != 15
  ) {
    stop(
      "Некоректний variable_weights у RDS: очікуються 15 іменованих ваг."
    )
  }

  hca_vars_primary <- names(variable_weights)

  missing_analysis_vars <- setdiff(
    c("country", hca_vars_primary),
    names(analysis_data)
  )

  if (length(missing_analysis_vars) > 0) {
    stop(
      paste0(
        "У analysis_data відсутні змінні: ",
        paste(
          missing_analysis_vars,
          collapse = ", "
        )
      )
    )
  }

  X <- analysis_data %>%
    select(
      all_of(hca_vars_primary)
    ) %>%
    as.data.frame()

  rownames(X) <- analysis_data$country

  X_z <- scale(X)

  variable_weights <-
    variable_weights[colnames(X_z)]

  X_weighted <- sweep(
    X_z,
    2,
    sqrt(variable_weights),
    FUN = "*"
  )

  # Контроль: відновлена матриця повинна давати ті самі
  # евклідові відстані, що й D_weighted з HCA.
  D_reconstructed <- dist(
    X_weighted,
    method = "euclidean"
  )

  if (
    !identical(
      attr(D_weighted, "Labels"),
      rownames(X_weighted)
    )
  ) {
    stop(
      "Порядок країн у D_weighted не відповідає analysis_data."
    )
  }

  if (
    !isTRUE(
      all.equal(
        as.vector(D_reconstructed),
        as.vector(D_weighted),
        tolerance = 1e-10
      )
    )
  ) {
    stop(
      "Відновлений X_weighted не відповідає D_weighted у RDS."
    )
  }

  cluster2 <- cutree(
    hc,
    k = 2
  )

  cluster5 <- cutree(
    hc,
    k = 5
  )

  cluster2 <- cluster2[rownames(X_weighted)]
  cluster5 <- cluster5[rownames(X_weighted)]

  if (!"Ukraine" %in% rownames(X_weighted)) {
    stop("Україна відсутня у X_weighted.")
  }

  D_matrix <- as.matrix(D_weighted)

  ukraine_neighbors <- data.frame(
    country = rownames(X_weighted),
    distance = D_matrix[
      "Ukraine",
    ],
    cluster2 = unname(
      cluster2[rownames(X_weighted)]
    ),
    cluster5 = unname(
      cluster5[rownames(X_weighted)]
    )
  ) %>%
    filter(
      country != "Ukraine"
    ) %>%
    arrange(
      distance
    ) %>%
    mutate(
      rank = row_number()
    ) %>%
    select(
      rank,
      country,
      distance,
      cluster2,
      cluster5
    )

  cat(
    "\nРезультати HCA завантажено з:",
    rds_file,
    "\n"
  )

} else {

  required_objects <- c(
    "X_weighted",
    "hc",
    "cluster5",
    "ukraine_neighbors"
  )

  missing_objects <- required_objects[
    !vapply(
      required_objects,
      function(x) {
        exists(
          x,
          envir = .GlobalEnv,
          inherits = FALSE
        )
      },
      logical(1)
    )
  ]

  if (length(missing_objects) > 0) {
    stop(
      paste0(
        "Не знайдено ",
        rds_file,
        " і у поточній R-сесії відсутні об'єкти: ",
        paste(
          missing_objects,
          collapse = ", "
        )
      )
    )
  }

  if (!is.null(names(cluster5))) {
    cluster5 <- cluster5[
      rownames(X_weighted)
    ]
  }

  cat(
    "\nRDS не знайдено; використано об'єкти поточної R-сесії.\n"
  )
}

stopifnot(
  length(cluster5) == nrow(X_weighted),
  !anyNA(cluster5),
  setequal(
    names(cluster5),
    rownames(X_weighted)
  )
)


# ============================================================
# 2. УКРАЇНСЬКІ НАЗВИ КРАЇН
# ============================================================

country_ua <- c(
  
  "Afghanistan" = "Афганістан",
  "Albania" = "Албанія",
  "Algeria" = "Алжир",
  "Angola" = "Ангола",
  "Argentina" = "Аргентина",
  "Australia" = "Австралія",
  "Azerbaijan" = "Азербайджан",
  
  "Bangladesh" = "Бангладеш",
  "Benin" = "Бенін",
  "Bolivia" = "Болівія",
  "Bosnia-Herzegovina" = "Боснія і Герцеговина",
  "Burkina Faso" = "Буркіна-Фасо",
  "Burundi" = "Бурунді",
  
  "Cambodia" = "Камбоджа",
  "Cameroon" = "Камерун",
  "Central African Republic" =
    "Центральноафриканська Республіка",
  "Chad" = "Чад",
  "Chile" = "Чилі",
  "China" = "Китай",
  "Colombia" = "Колумбія",
  "Comoros" = "Коморські Острови",
  "Congo" = "Республіка Конго",
  "Congo Democratic Republic of" =
    "Демократична Республіка Конго",
  "Congo, Democratic Republic of" =
    "Демократична Республіка Конго",
  "Costa Rica" = "Коста-Рика",
  "Cote D'Ivoire" = "Кот-д’Івуар",
  "Croatia" = "Хорватія",
  "Cuba" = "Куба",
  "Cyprus" = "Кіпр",
  
  "Djibouti" = "Джибуті",
  "Dominican Republic" =
    "Домініканська Республіка",
  
  "Ecuador" = "Еквадор",
  "Egypt" = "Єгипет",
  "El Salvador" = "Сальвадор",
  "Eritrea" = "Еритрея",
  "Ethiopia" = "Ефіопія",
  
  "France" = "Франція",
  
  "Gabon" = "Габон",
  "Gambia" = "Гамбія",
  "Georgia" = "Грузія",
  "Ghana" = "Гана",
  "Greece" = "Греція",
  "Guatemala" = "Гватемала",
  "Guinea" = "Гвінея",
  "Guinea-Bissau" = "Гвінея-Бісау",
  
  "Haiti" = "Гаїті",
  "Honduras" = "Гондурас",
  "Hungary" = "Угорщина",
  
  "India" = "Індія",
  "Indonesia" = "Індонезія",
  "Iran" = "Іран",
  "Iraq" = "Ірак",
  "Israel" = "Ізраїль",
  
  "Jordan" = "Йорданія",
  
  "Kenya" = "Кенія",
  "Kuwait" = "Кувейт",
  "Kyrgyzstan" = "Киргизстан",
  
  "Laos" = "Лаос",
  "Lebanon" = "Ліван",
  "Lesotho" = "Лесото",
  "Liberia" = "Ліберія",
  "Libya" = "Лівія",
  
  "Macedonia" = "Північна Македонія",
  "Madagascar" = "Мадагаскар",
  "Malaysia" = "Малайзія",
  "Mali" = "Малі",
  "Mauritania" = "Мавританія",
  "Mexico" = "Мексика",
  "Moldova" = "Молдова",
  "Morocco" = "Марокко",
  "Mozambique" = "Мозамбік",
  "Myanmar" = "М’янма",
  
  "Nepal" = "Непал",
  "Netherlands" = "Нідерланди",
  "Nicaragua" = "Нікарагуа",
  "Niger" = "Нігер",
  "Nigeria" = "Нігерія",
  "North Korea" = "Північна Корея",
  
  "Oman" = "Оман",
  
  "Pakistan" = "Пакистан",
  "Panama" = "Панама",
  "Papua New Guinea" = "Папуа-Нова Гвінея",
  "Paraguay" = "Парагвай",
  "Peru" = "Перу",
  "Philippines" = "Філіппіни",
  "Portugal" = "Португалія",
  
  "Romania" = "Румунія",
  "Russia" = "Росія",
  "Rwanda" = "Руанда",
  
  "Saudi Arabia" = "Саудівська Аравія",
  "Senegal" = "Сенегал",
  "Sierra Leone" = "Сьєрра-Леоне",
  "Somalia" = "Сомалі",
  "South Africa" = "Південна Африка",
  "South Korea" = "Південна Корея",
  "South Sudan" = "Південний Судан",
  "South Vietnam" = "Південний В’єтнам",
  "South Yemen" = "Південний Ємен",
  "Spain" = "Іспанія",
  "Sri Lanka" = "Шрі-Ланка",
  "Sudan" = "Судан",
  "Surinam" = "Суринам",
  "Syria" = "Сирія",
  
  "Taiwan" = "Тайвань",
  "Tajikistan" = "Таджикистан",
  "Tanzania" = "Танзанія",
  "Thailand" = "Таїланд",
  "Togo" = "Того",
  "Trinidad and Tobago" =
    "Тринідад і Тобаго",
  "Tunisia" = "Туніс",
  "Turkey" = "Туреччина",
  
  "Uganda" = "Уганда",
  "Ukraine" = "Україна",
  "United Kingdom" = "Велика Британія",
  "United States" = "США",
  "Uruguay" = "Уругвай",
  "Uzbekistan" = "Узбекистан",
  
  "Venezuela" = "Венесуела",
  "Vietnam" = "В’єтнам",
  
  "Yemen" = "Ємен",
  "Yugoslavia" = "Югославія",
  
  "Zimbabwe" = "Зімбабве"
)


# ============================================================
# 3. ФУНКЦІЯ ПЕРЕКЛАДУ
# ============================================================

to_ua <- function(x) {
  
  translated <- unname(
    country_ua[x]
  )
  
  missing_translation <- is.na(
    translated
  )
  
  translated[
    missing_translation
  ] <- x[
    missing_translation
  ]
  
  translated
}


# ============================================================
# 4. НАЗВИ КЛАСТЕРІВ
# ============================================================

cluster_names_ua <- c(
  
  "1" =
    "Інтернаціоналізований іррегулярний",
  
  "2" =
    "Міждержавний / оборонний",
  
  "3" =
    "Масштабний іррегулярний",
  
  "4" =
    "Обмежений іррегулярний",
  
  "5" =
    "Зовнішній / експедиційний"
)


# Повні підписи для легенди

cluster_legend_labels <- paste0(
  
  "К",
  names(cluster_names_ua),
  
  " — ",
  
  unname(
    cluster_names_ua
  )
)


# ============================================================
# 5. КОЛЬОРИ КЛАСТЕРІВ
# ============================================================

cluster_cols <- c(
  
  "1" = "#0072B2",
  
  "2" = "#D55E00",
  
  "3" = "#009E73",
  
  "4" = "#CC79A7",
  
  "5" = "#E69F00"
)


# ============================================================
# 6. ТИПИ ЛІНІЙ КЛАСТЕРІВ
# ============================================================
#
# Важливо для чорно-білого друку.
#
# ============================================================

cluster_linetypes <- c(
  
  "1" = "solid",
  
  "2" = "dashed",
  
  "3" = "dotted",
  
  "4" = "dotdash",
  
  "5" = "longdash"
)


# ============================================================
# 7. 10 НАЙБЛИЖЧИХ СУСІДІВ УКРАЇНИ
# ============================================================

top10_neighbors <- ukraine_neighbors %>%
  
  arrange(
    rank
  ) %>%
  
  slice_head(
    n = 10
  ) %>%
  
  pull(
    country
  )


cat(
  "\n10 найближчих сусідів України:\n"
)

print(
  
  data.frame(
    
    rank =
      1:length(top10_neighbors),
    
    country =
      top10_neighbors,
    
    country_ua =
      to_ua(
        top10_neighbors
      )
  )
)


# ============================================================
# 8. PCA 15-ВИМІРНОГО ПРОСТОРУ ОСНОВНОЇ МОДЕЛІ
# ============================================================
#
# PCA використовується лише для візуалізації.
#
# Кластеризацію виконано у повному X_weighted.
#
# ============================================================

pca_fit <- prcomp(
  
  X_weighted,
  
  center = FALSE,
  
  scale. = FALSE
)


pca_var <- 100 *
  
  pca_fit$sdev^2 /
  
  sum(
    pca_fit$sdev^2
  )


cat(
  "\nГК1:",
  round(
    pca_var[1],
    1
  ),
  "%\n"
)

cat(
  "ГК2:",
  round(
    pca_var[2],
    1
  ),
  "%\n"
)

cat(
  "ГК1 + ГК2:",
  round(
    sum(
      pca_var[1:2]
    ),
    1
  ),
  "%\n"
)


# ============================================================
# 9. DATA FRAME PCA
# ============================================================

pca_df <- as.data.frame(
  
  pca_fit$x[
    ,
    1:2
  ]
  
) %>%
  
  rownames_to_column(
    "country"
  ) %>%
  
  mutate(
    
    country_ua =
      to_ua(
        country
      ),
    
    cluster =
      factor(
        cluster5,
        levels = 1:5
      )
  )


# ============================================================
# 10. РЕПРЕЗЕНТАТИВНА КРАЇНА КОЖНОГО КЛАСТЕРА
# ============================================================
#
# Це країна, найближча до центроїда у ПОВНОМУ
# 15-вимірному просторі основної моделі з рівними вагами змінних.
#
# ============================================================

representatives <- lapply(
  
  sort(
    unique(
      cluster5
    )
  ),
  
  function(cl) {
    
    members <- which(
      cluster5 == cl
    )
    
    X_sub <- X_weighted[
      members,
      ,
      drop = FALSE
    ]
    
    centroid <- colMeans(
      X_sub
    )
    
    distances <- apply(
      
      X_sub,
      
      1,
      
      function(x) {
        
        sqrt(
          sum(
            (x - centroid)^2
          )
        )
      }
    )
    
    
    data.frame(
      
      cluster =
        factor(
          cl,
          levels = 1:5
        ),
      
      country =
        rownames(
          X_sub
        )[
          which.min(
            distances
          )
        ],
      
      distance_to_centroid =
        min(
          distances
        )
    )
  }
  
) %>%
  
  bind_rows() %>%
  
  mutate(
    
    country_ua =
      to_ua(
        country
      )
  )


cat(
  "\nКраїни, найближчі до центроїдів:\n"
)

print(
  representatives
)


# ============================================================
# 11. ЦЕНТРОЇДИ У PCA-ПРОЄКЦІЇ
# ============================================================

centroids_pca <- pca_df %>%
  
  group_by(
    cluster
  ) %>%
  
  summarise(
    
    PC1 =
      mean(
        PC1
      ),
    
    PC2 =
      mean(
        PC2
      ),
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    centroid_label =
      paste0(
        "К",
        cluster
      )
  )


# ============================================================
# 12. PCA-КООРДИНАТИ РЕПРЕЗЕНТАТИВНИХ КРАЇН
# ============================================================

representative_df <- representatives %>%
  
  left_join(
    
    pca_df %>%
      select(
        country,
        PC1,
        PC2
      ),
    
    by =
      "country"
  )


# Якщо Україна виявиться репрезентативною,
# не накладаємо квадрат поверх ромба.

representative_plot_df <- representative_df %>%
  
  filter(
    country != "Ukraine"
  )


# ============================================================
# 13. ОКРЕМІ НАБОРИ ТОЧОК
# ============================================================

ukraine_df <- pca_df %>%
  
  filter(
    country == "Ukraine"
  )


neighbors_df <- pca_df %>%
  
  filter(
    country %in%
      top10_neighbors
  )


# Сусіди, які НЕ є репрезентативними країнами

neighbors_point_df <- neighbors_df %>%
  
  filter(
    !country %in%
      representative_plot_df$country
  )


background_df <- pca_df %>%
  
  filter(
    
    country != "Ukraine",
    
    !country %in%
      top10_neighbors,
    
    !country %in%
      representative_plot_df$country
  )


# ============================================================
# 14. ЄДИНИЙ DATA FRAME ДЛЯ ПІДПИСІВ
# ============================================================
#
# Усі підписи передаються одному geom_text_repel().
#
# ============================================================


# ------------------------------------------------------------
# 14.1. Сусіди
# ------------------------------------------------------------

labels_neighbors <- neighbors_df %>%
  
  filter(
    
    country != "Ukraine",
    
    !country %in%
      representative_plot_df$country
  ) %>%
  
  transmute(
    
    PC1,
    
    PC2,
    
    label =
      country_ua,
    
    label_size =
      2.75,
    
    label_face =
      "plain"
  )


# ------------------------------------------------------------
# 14.2. Репрезентативні країни
# ------------------------------------------------------------

labels_representatives <- representative_plot_df %>%
  
  transmute(
    
    PC1,
    
    PC2,
    
    label =
      country_ua,
    
    label_size =
      3.15,
    
    label_face =
      "bold"
  )


# ------------------------------------------------------------
# 14.3. Україна
# ------------------------------------------------------------

labels_ukraine <- ukraine_df %>%
  
  transmute(
    
    PC1,
    
    PC2,
    
    label =
      country_ua,
    
    label_size =
      3.6,
    
    label_face =
      "bold"
  )


# ------------------------------------------------------------
# 14.4. Центроїди
# ------------------------------------------------------------

labels_centroids <- centroids_pca %>%
  
  transmute(
    
    PC1,
    
    PC2,
    
    label =
      centroid_label,
    
    label_size =
      3.25,
    
    label_face =
      "bold"
  )


# ------------------------------------------------------------
# 14.5. Об'єднання
# ------------------------------------------------------------

labels_all <- bind_rows(
  
  labels_neighbors,
  
  labels_representatives,
  
  labels_ukraine,
  
  labels_centroids
)


# ============================================================
# 15. КОНТУРИ П'ЯТИ КЛАСТЕРІВ НА ДЕНДРОГРАМІ
# ============================================================
#
# Контури обчислюються з фактичного порядку листків hc$order
# та п'ятикластерного розбиття cluster5.
#
# Кожний контур використовує той самий колір і той самий
# тип лінії, що й відповідний кластер у легенді панелі B.
#
# ============================================================

n_leaves <- length(hc$order)
k_dend <- 5L

if (n_leaves <= k_dend) {
  stop(
    "Недостатньо об'єктів для побудови 5 кластерних контурів."
  )
}


# ------------------------------------------------------------
# 15.1. Висота зрізу для k = 5
# ------------------------------------------------------------
#
# Для n об'єктів рішення k = 5 існує між:
#   height[n - 5]     — останнім злиттям усередині 5 кластерів;
#   height[n - 5 + 1] — наступним злиттям, після якого лишається 4.
#
# Беремо середину цього інтервалу.
# ------------------------------------------------------------

last_within_merge_index <-
  n_leaves - k_dend

next_between_merge_index <-
  last_within_merge_index + 1L

dend_cut_height <-
  mean(
    c(
      hc$height[
        last_within_merge_index
      ],
      hc$height[
        next_between_merge_index
      ]
    )
  )


# ------------------------------------------------------------
# 15.2. Порядок країн на дендрограмі
# ------------------------------------------------------------

if (is.null(hc$labels)) {
  stop(
    "У hc відсутні labels; неможливо зіставити листки з cluster5."
  )
}

dend_leaf_df <- data.frame(

  x =
    seq_len(
      n_leaves
    ),

  country =
    hc$labels[
      hc$order
    ],

  stringsAsFactors =
    FALSE
)

dend_leaf_df$cluster <-
  as.character(
    cluster5[
      dend_leaf_df$country
    ]
  )

if (anyNA(dend_leaf_df$cluster)) {
  stop(
    "Не вдалося зіставити частину листків дендрограми з cluster5."
  )
}


# ------------------------------------------------------------
# 15.3. Межі п'яти кластерів уздовж осі X
# ------------------------------------------------------------

cluster_rect_df <-
  dend_leaf_df %>%

  group_by(
    cluster
  ) %>%

  summarise(

    xmin =
      min(x) - 0.45,

    xmax =
      max(x) + 0.45,

    n =
      n(),

    .groups =
      "drop"
  ) %>%

  arrange(
    as.integer(cluster)
  )


# Для cutree(hc, k = 5) кожний кластер має утворювати
# один суцільний блок листків.

cluster_contiguity_check <-
  dend_leaf_df %>%

  group_by(
    cluster
  ) %>%

  summarise(

    n_positions =
      n(),

    span =
      max(x) -
      min(x) +
      1L,

    contiguous =
      n_positions == span,

    .groups =
      "drop"
  )

if (!all(cluster_contiguity_check$contiguous)) {
  stop(
    "Щонайменше один кластер не є суцільним блоком на дендрограмі."
  )
}


cat(
  "\nКонтури кластерів на дендрограмі:\n"
)

print(
  cluster_rect_df
)

cat(
  "Висота верхньої межі контурів:",
  round(
    dend_cut_height,
    4
  ),
  "\n"
)


# ------------------------------------------------------------
# 15.4. Шари контурів
# ------------------------------------------------------------
#
# Створюємо окремий annotate("rect") для кожного кластера.
# Завдяки цьому не втручаємося в колірну шкалу fviz_dend(),
# але використовуємо точні cluster_cols і cluster_linetypes.
# ------------------------------------------------------------

cluster_rect_layers <-
  lapply(

    seq_len(
      nrow(
        cluster_rect_df
      )
    ),

    function(i) {

      cl <-
        as.character(
          cluster_rect_df$cluster[
            i
          ]
        )

      ggplot2::annotate(

        "rect",

        xmin =
          cluster_rect_df$xmin[
            i
          ],

        xmax =
          cluster_rect_df$xmax[
            i
          ],

        ymin =
          0,

        ymax =
          dend_cut_height,

        fill =
          NA,

        colour =
          unname(
            cluster_cols[
              cl
            ]
          ),

        linetype =
          unname(
            cluster_linetypes[
              cl
            ]
          ),

        linewidth =
          0.85
      )
    }
  )


# ============================================================
# 16. ПАНЕЛЬ A — ДЕНДРОГРАМА
# ============================================================

p_dend <- factoextra::fviz_dend(
  
  hc,
  
  k = 5,
  
  k_colors =
    unname(
      cluster_cols
    ),
  
  color_labels_by_k =
    TRUE,
  
  show_labels =
    FALSE,
  
  rect =
    FALSE,
  
  lwd =
    0.65
) +
  
  
  cluster_rect_layers +
  
  labs(
    
    title =
      "A. Ієрархічна кластерна структура",
    
    x =
      NULL,
    
    y =
      "Висота Ward.D2"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    legend.position =
      "none",
    
    plot.title =
      element_text(
        face = "bold",
        size = 12
      ),
    
    panel.grid.major.x =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    axis.text.x =
      element_blank(),
    
    axis.ticks.x =
      element_blank()
  )


# ============================================================
# 16. ПАНЕЛЬ B — PCA
# ============================================================

p_pca <- ggplot() +
  
  
  # ----------------------------------------------------------
# 16.1. КОЛЬОРОВА ЗАЛИВКА ЕЛІПСІВ
# ----------------------------------------------------------

stat_ellipse(
  
  data = pca_df,
  
  aes(
    x = PC1,
    y = PC2,
    fill = cluster,
    group = cluster
  ),
  
  geom =
    "polygon",
  
  type =
    "norm",
  
  level =
    0.80,
  
  alpha =
    0.04,
  
  color =
    NA,
  
  show.legend =
    FALSE
) +
  
  
  # ----------------------------------------------------------
# 16.2. ЧІТКІ КОНТУРИ ЕЛІПСІВ
#
# Колір + тип лінії.
#
# У чорно-білому друці кластери залишаться
# розрізнюваними за типом лінії.
# ----------------------------------------------------------

stat_ellipse(
  
  data = pca_df,
  
  aes(
    x = PC1,
    y = PC2,
    color = cluster,
    linetype = cluster,
    group = cluster
  ),
  
  geom =
    "path",
  
  type =
    "norm",
  
  level =
    0.80,
  
  linewidth =
    0.85,
  
  alpha =
    0.95,
  
  show.legend =
    TRUE
) +
  
  
  # ----------------------------------------------------------
# 16.3. ФОНОВІ КРАЇНИ
# ----------------------------------------------------------

geom_point(
  
  data = background_df,
  
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  ),
  
  size =
    1.6,
  
  alpha =
    0.32,
  
  show.legend =
    FALSE
) +
  
  
  # ----------------------------------------------------------
# 16.4. 10 НАЙБЛИЖЧИХ СУСІДІВ
# ----------------------------------------------------------

geom_point(
  
  data = neighbors_point_df,
  
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  ),
  
  shape =
    21,
  
  fill =
    "white",
  
  size =
    3.3,
  
  stroke =
    1.1,
  
  show.legend =
    FALSE
) +
  
  
  # ----------------------------------------------------------
# 16.5. РЕПРЕЗЕНТАТИВНІ КРАЇНИ
# ----------------------------------------------------------

geom_point(
  
  data =
    representative_plot_df,
  
  aes(
    x = PC1,
    y = PC2,
    color = cluster
  ),
  
  shape =
    22,
  
  fill =
    "white",
  
  size =
    4.3,
  
  stroke =
    1.35,
  
  show.legend =
    FALSE
) +
  
  
  # ----------------------------------------------------------
# 16.6. ЦЕНТРОЇДИ
# ----------------------------------------------------------

geom_point(
  
  data =
    centroids_pca,
  
  aes(
    x = PC1,
    y = PC2
  ),
  
  shape =
    4,
  
  color =
    "black",
  
  size =
    5.4,
  
  stroke =
    1.5,
  
  inherit.aes =
    FALSE,
  
  show.legend =
    FALSE
) +
  
  
  # ----------------------------------------------------------
# 16.7. УКРАЇНА
# ----------------------------------------------------------

geom_point(
  
  data =
    ukraine_df,
  
  aes(
    x = PC1,
    y = PC2
  ),
  
  shape =
    23,
  
  fill =
    "#FFD200",
  
  color =
    "black",
  
  size =
    5.5,
  
  stroke =
    1.35,
  
  inherit.aes =
    FALSE,
  
  show.legend =
    FALSE
) +
  
  
  # ----------------------------------------------------------
# 16.8. УСІ ПІДПИСИ ОДНИМ REPEL-ШАРОМ
# ----------------------------------------------------------

ggrepel::geom_text_repel(
  
  data =
    labels_all,
  
  aes(
    
    x = PC1,
    
    y = PC2,
    
    label = label,
    
    size = label_size,
    
    fontface = label_face
  ),
  
  color =
    "grey15",
  
  box.padding =
    0.75,
  
  point.padding =
    0.45,
  
  force =
    4,
  
  force_pull =
    0.18,
  
  min.segment.length =
    0,
  
  segment.color =
    "grey60",
  
  segment.linewidth =
    0.30,
  
  max.overlaps =
    Inf,
  
  max.time =
    10,
  
  max.iter =
    100000,
  
  seed =
    20260827,
  
  show.legend =
    FALSE,
  
  inherit.aes =
    FALSE
) +
  
  
  # ----------------------------------------------------------
# 16.9. РОЗМІР ПІДПИСІВ
# ----------------------------------------------------------

scale_size_identity(
  guide = "none"
) +
  
  
  # ----------------------------------------------------------
# 16.10. КОЛЬОРИ КЛАСТЕРІВ
#
# Легенда містить:
#
# К1 — ...
# К2 — ...
# ...
# ----------------------------------------------------------

scale_color_manual(
  
  values =
    cluster_cols,
  
  breaks =
    names(
      cluster_names_ua
    ),
  
  labels =
    cluster_legend_labels,
  
  name =
    "Кластер"
) +
  
  
  # ----------------------------------------------------------
# 16.11. ЗАЛИВКА ЕЛІПСІВ
# ----------------------------------------------------------

scale_fill_manual(
  
  values =
    cluster_cols,
  
  guide =
    "none"
) +
  
  
  # ----------------------------------------------------------
# 16.12. ТИПИ ЛІНІЙ
#
# Та сама назва, breaks і labels, що у color.
# ggplot об'єднає color + linetype у спільну легенду.
# ----------------------------------------------------------

scale_linetype_manual(
  
  values =
    cluster_linetypes,
  
  breaks =
    names(
      cluster_names_ua
    ),
  
  labels =
    cluster_legend_labels,
  
  name =
    "Кластер"
) +
  
  
  # ----------------------------------------------------------
# 16.13. ДОДАТКОВИЙ ПРОСТІР ДЛЯ ПІДПИСІВ
# ----------------------------------------------------------

scale_x_continuous(
  
  expand =
    expansion(
      mult = c(
        0.10,
        0.13
      )
    )
) +
  
  scale_y_continuous(
    
    expand =
      expansion(
        mult = c(
          0.10,
          0.12
        )
      )
  ) +
  
  
  # ----------------------------------------------------------
# 16.14. НАЗВИ ОСЕЙ
# ----------------------------------------------------------

labs(
  
  title =
    "B. Країни у багатовимірному просторі основної моделі",
  
  x =
    sprintf(
      "Головна компонента 1 (%.1f%%)",
      pca_var[1]
    ),
  
  y =
    sprintf(
      "Головна компонента 2 (%.1f%%)",
      pca_var[2]
    )
) +
  
  
  # ----------------------------------------------------------
# 16.15. ЛЕГЕНДА
# ----------------------------------------------------------

guides(
  
  color =
    guide_legend(
      
      nrow =
        2,
      
      byrow =
        TRUE,
      
      override.aes =
        list(
          linewidth = 1,
          alpha = 1
        )
    ),
  
  linetype =
    guide_legend(
      
      nrow =
        2,
      
      byrow =
        TRUE
    )
) +
  
  
  # ----------------------------------------------------------
# 16.16. ТЕМА
# ----------------------------------------------------------

theme_minimal(
  base_size = 11
) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 12
      ),
    
    legend.position =
      "bottom",
    
    legend.box =
      "horizontal",
    
    legend.title =
      element_text(
        face = "bold",
        size = 9
      ),
    
    legend.text =
      element_text(
        size = 8.0
      ),
    
    legend.key.width =
      grid::unit(
        1.1,
        "cm"
      ),
    
    legend.spacing.x =
      grid::unit(
        0.25,
        "cm"
      ),
    
    panel.grid.minor =
      element_blank(),
    
    panel.grid.major =
      element_line(
        linewidth = 0.25,
        color = "grey90"
      ),
    
    axis.title =
      element_text(
        size = 10
      ),
    
    plot.margin =
      margin(
        t = 10,
        r = 25,
        b = 10,
        l = 15
      )
  ) +
  
  
  # Дозволяємо ggrepel використовувати зовнішні поля
  
  coord_cartesian(
    clip = "off"
  )


# ============================================================
# 17. ОБ'ЄДНАНИЙ РИСУНОК
# ============================================================

figure_clusters <- p_dend / p_pca +
  
  plot_layout(
    
    heights = c(
      0.74,
      1.26
    )
  ) +
  
  plot_annotation(
    
    title =
      "Кластерна структура країн за профілем досвіду участі у війні",
    
    theme =
      theme(
        
        plot.title =
          element_text(
            face = "bold",
            size = 15
          )
      )
  )


# ============================================================
# 18. ПОКАЗАТИ РИСУНОК
# ============================================================

figure_clusters


# ============================================================
# 19. TIFF — A4 LANDSCAPE — 600 DPI
# ============================================================

ggsave(
  
  filename =
    file.path(FIGURES_DIR, "figure_1_hca_clusters_ukraine_neighbors.tiff"),
  
  plot =
    figure_clusters,
  
  width =
    297,
  
  height =
    210,
  
  units =
    "mm",
  
  dpi =
    600,
  
  compression =
    "lzw",
  
  bg =
    "white"
)


# ============================================================
# 20. PDF — A4 LANDSCAPE
# ============================================================

ggsave(
  
  filename =
    file.path(FIGURES_DIR, "figure_1_hca_clusters_ukraine_neighbors.pdf"),
  
  plot =
    figure_clusters,
  
  width =
    297,
  
  height =
    210,
  
  units =
    "mm",
  
  device =
    cairo_pdf,
  
  bg =
    "white"
)


# ============================================================
# 21. PNG — A4 LANDSCAPE — 300 DPI
# ============================================================

ggsave(
  
  filename =
    file.path(FIGURES_DIR, "figure_1_hca_clusters_ukraine_neighbors.png"),
  
  plot =
    figure_clusters,
  
  width =
    297,
  
  height =
    210,
  
  units =
    "mm",
  
  dpi =
    300,
  
  bg =
    "white"
)


# ============================================================
# 22. ДАНІ ДЛЯ ПІДПИСУ ДО РИСУНКА
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "ГК1:",
  round(
    pca_var[1],
    1
  ),
  "%\n"
)

cat(
  "ГК2:",
  round(
    pca_var[2],
    1
  ),
  "%\n"
)

cat(
  "ГК1 + ГК2:",
  round(
    sum(
      pca_var[1:2]
    ),
    1
  ),
  "%\n"
)


cat(
  "\nРепрезентативні країни кластерів:\n"
)

print(
  
  representatives %>%
    
    select(
      cluster,
      country,
      country_ua,
      distance_to_centroid
    )
)


cat(
  "\n========================================\n"
)


# ============================================================
# КІНЕЦЬ
# ============================================================