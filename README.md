# Country War Experience Case Selection

**Replication data and R scripts accompanying the study**  
**Реплікаційні дані та R-скрипти до дослідження**

> **Article / Стаття:**  
> *Case selection in cross-national comparative research: a state's war experience as a selection criterion and its limits*  
> *Добір кейсів у міжнародних порівняльних дослідженнях: воєнний досвід держави як критерій відбору та межі його застосовності*

[English](#english) · [Українська](#українська)

---

## English

### Overview

This repository contains the country-level analytical dataset and R scripts used to study **state war experience as a multidimensional criterion for systematic case selection in cross-national comparative research**, with Ukraine used as the reference case.

The analytical dataset contains **160 country/state-system profiles**. All analyses retain the **121 countries that participated in at least one armed conflict as a primary party** during the study period.

Rather than a single specification, this release compares **five parallel representations** of state war experience, each clustered independently and then cross-compared:

| Specification | Dimensionality | Basis |
|---|---|---|
| 11-variable theoretical (Variant B) | 11 | fixed, theory-driven; war-experience indicators only |
| 15-variable expanded (Variant B) | 15 | fixed, theory-driven; adds population, personnel, and expenditure |
| Direct 25-variable | 25 | fixed pool, no reduction |
| Redundancy-reduced | data-driven | `Hmisc::redun()` on the 25-variable pool, adjusted R² ≥ .95 |
| Seven-family synthetic score | 7 | `ClustOfVar`-based empirical clustering of the 25-variable pool, one principal component per family |

All five specifications use primary-party countries only, z-standardization, Euclidean distance, and **Ward.D2** linkage; evaluate cluster solutions for **k = 2,…,10**; retain the top three solutions by mean silhouette; and assess each with **B = 1,000** bootstrap replications (cluster-wise Jaccard, dissolution, and recovery). All five apply the same `log1p` transformation to skewed count, duration, and magnitude indicators, leaving proportion and percentage indicators on their original scale.

### Repository structure

```text
country-war-experience-case-selection/
├── README.md
├── LICENSE
├── DATA_NOTICE.md
├── SOURCES.md
├── CITATION.cff
├── MANIFEST.md
├── .gitignore
├── data/
│   └── war_participation_data.xlsx
└── scripts/
    ├── HCA_11var.R
    ├── HCA_15var.R
    ├── HCA_direct_25var_log1p.R
    ├── HCA_redundancy_reduced_log1p.R
    ├── HCA_7families_log1p.R
    └── build_HCA_comparison_tables.R
```

The `results/` directory is created automatically when the analysis scripts are run and is excluded from version control. Each of the five specification scripts writes to its own subfolder (`results/11var/`, `results/15var/`, `results/direct_25var/`, `results/redundancy_reduced/`, `results/families_7/`); `build_HCA_comparison_tables.R` writes to `results/comparison/`.

**This release does not include a figures directory or figure-generation script.**

### Files

#### `data/war_participation_data.xlsx`

Author-created, country-level analytical dataset integrating and transforming information from UCDP/PRIO, Correlates of War, the CIA World Factbook, the Gleditsch-Ward state list, and derived/manual coding. The workbook contains four sheets: `legend`, `легенда (укр)`, `country_profile` (160 country rows, 54 columns), and `variant_A_vs_B` (the Variant A/Variant B coding table).

#### `scripts/HCA_11var.R`

Clusters countries on 11 fixed, theory-driven war-experience variables (Variant B): number of conflicts, total exposure, high-intensity war-year share, continuity, recency, interstate/internationalized-intrastate/intrastate shares, home-defensive/home-civil/abroad participation shares. No capability variables are included. The model uses complete cases only — the script halts if any of the 11 indicators contain missing values.

#### `scripts/HCA_15var.R`

Extends the 11-variable set with four capability variables — population size, military personnel size, personnel as a share of population, and military expenditure as a percentage of GDP. Missing military-expenditure values are median-imputed; this was the sole primary model in the previous release.

#### `scripts/HCA_direct_25var.R`

Clusters countries on the full fixed 25-indicator pool with no redundancy reduction. Skewed count, duration, and magnitude indicators are `log1p`-transformed; the four proportion/percentage indicators are left untransformed. Missing values are imputed variable-wise by the median after transformation.

#### `scripts/HCA_redundancy_reduced.R`

Starts from the same 25-indicator pool and applies `Hmisc::redun()` at an adjusted R² cutoff of .95 to remove redundant indicators, with no hard-coded deletion set and no hard-coded target dimensionality — the retained set and its size are determined at runtime and reported in the script's own output. The same `log1p` convention is applied before reduction; missing values are median-imputed after reduction.

#### `scripts/HCA_7families_log1p.R`

Applies the same `Hmisc::redun()` reduction to the 25-indicator pool, then empirically clusters the retained indicators into variable families using `ClustOfVar`, cutting the variable dendrogram at **k = 7** families. Each family is represented by its first principal component; for a country with missing inputs to a family, the score is computed from the observed standardized inputs and normalized by the observed coefficient norm rather than imputed. The seven standardized family scores are then used for the country-level HCA.

#### `scripts/build_HCA_comparison_tables.R`

Does not run any clustering itself. Reads the saved result objects from the five specifications above and builds cross-model comparison tables: model metadata, solution diagnostics, cluster-level diagnostics, the country closest to each cluster's centroid in that model's own space (explicitly **not** a medoid), standardized centroids, Ukraine's cluster membership across all top-three solutions in each model, Ukraine's global top-10 nearest neighbours per model, neighbour overlap and rank agreement across models, and full country-cluster membership across all five specifications. Outputs are written to `results/comparison/` as `HCA_comparison_tables.xlsx`, a set of CSV files, and `HCA_comparison_tables.rds`.

Cluster numbers are local to each model and each *k* solution and are not comparable across specifications; only neighbour ranks, set overlap, and cluster membership should be compared across models, not raw distance magnitudes.

### Variant B coding

Variant B, used in the 11- and 15-variable models, treats UCDP conflicts `13246`, `13247`, and `13306` — associated with the Russian-Ukrainian war in 2014–2022 — as **interstate rather than internationalized intrastate**, with Ukraine and Russia as primary parties. All other conflict classifications retain their original coding. The alternative classification is an author-defined analytical recoding, kept explicitly separate from the original UCDP coding in the workbook.

### Reproduction

The analyses were conducted with **R 4.3.3** in **RStudio 2025.05.0+496**.

Run the scripts **from the repository root**. The first five specifications are independent of one another and can be run in any order; `build_HCA_comparison_tables.R` must be run last, after all five result files exist:

```bash
Rscript scripts/HCA_11var.R
Rscript scripts/HCA_15var.R
Rscript scripts/HCA_direct_25var_log1p.R
Rscript scripts/HCA_redundancy_reduced_log1p.R
Rscript scripts/HCA_7families_log1p.R
Rscript scripts/build_HCA_comparison_tables.R
```

Packages required across the six scripts:

```r
c(
  "readxl", "readr", "dplyr", "tidyr", "tibble",
  "cluster", "fpc", "writexl", "Hmisc", "ClustOfVar"
)
```

### Data provenance and licensing

The workbook is a **derived analytical dataset**, not a redistribution of the original UCDP/PRIO or Correlates of War source datasets in their original row-level structure. See [SOURCES.md](SOURCES.md) for provenance and full citations and [DATA_NOTICE.md](DATA_NOTICE.md) for source-specific licensing and redistribution notes.

The MIT License in this repository applies to the **original source code written for this project**. It does not relicense third-party source data or override the terms of the original data providers.

### Citation

For the GitHub version, the provisional software citation is:

> Orlov, O. (2026). *Country war experience case selection: Data and R scripts* (Version 1.1.0) [Computer software and data]. GitHub. https://github.com/OrlovOleh/country-war-experience-case-selection

After the repository is re-archived in Zenodo, cite the **version-specific Zenodo DOI** for the release used in the analysis. `CITATION.cff` is included to support GitHub's **Cite this repository** function and repository metadata export.

---

## Українська

### Опис

Репозитарій містить аналітичний масив даних на рівні держав і R-скрипти, використані для дослідження **воєнного досвіду держави як багатовимірного критерію систематичного добору кейсів у міжнародних порівняльних дослідженнях**, де Україна виступає референтним кейсом.

Аналітичний масив містить **160 профілів держав / елементів системи держав**. У всіх аналізах збережено **121 державу, яка брала участь принаймні в одному збройному конфлікті як основна сторона** протягом досліджуваного періоду.

Замість єдиної специфікації цей реліз порівнює **п'ять паралельних представлень** воєнного досвіду держав, кожне з яких кластеризовано незалежно, з подальшим порівнянням між моделями:

| Специфікація | Розмірність | Основа |
|---|---|---|
| 11-змінна теоретична (Variant B) | 11 | фіксована, теоретично обґрунтована; лише показники воєнного досвіду |
| 15-змінна розширена (Variant B) | 15 | фіксована, теоретично обґрунтована; додає населення, персонал і видатки |
| Пряма 25-змінна | 25 | фіксований пул, без скорочення |
| Зі скороченою надлишковістю | data-driven | `Hmisc::redun()` на 25-змінному пулі, скоригований R² ≥ .95 |
| Сім синтетичних факторних оцінок | 7 | емпірична кластеризація 25-змінного пулу за допомогою `ClustOfVar`, одна головна компонента на родину |

У всіх п'яти специфікаціях використано лише держави — основні сторони конфліктів, z-стандартизацію, евклідову відстань та ієрархічну кластеризацію методом **Ward.D2**; оцінено кластерні рішення для **k = 2,…,10**; збережено три найкращі рішення за середнім силуетом; кожне оцінено **B = 1000** бутстреп-реплікаціями (Jaccard, dissolution, recovery на рівні кластера). У всіх п'яти специфікаціях застосовано однакову `log1p`-трансформацію до асиметричних кількісних, часових і масштабних показників; частки та відсотки залишено на початковій шкалі.

### Структура репозитарію

```text
country-war-experience-case-selection/
├── README.md
├── LICENSE
├── DATA_NOTICE.md
├── SOURCES.md
├── CITATION.cff
├── MANIFEST.md
├── .gitignore
├── data/
│   └── war_participation_data.xlsx
└── scripts/
    ├── HCA_11var.R
    ├── HCA_15var.R
    ├── HCA_direct_25var_log1p.R
    ├── HCA_redundancy_reduced_log1p.R
    ├── HCA_7families_log1p.R
    └── build_HCA_comparison_tables.R
```

Каталог `results/` створюється автоматично під час виконання аналітичних скриптів і не включається до системи контролю версій. Кожен із п'яти скриптів-специфікацій записує результати у власний підкаталог (`results/11var/`, `results/15var/`, `results/direct_25var/`, `results/redundancy_reduced/`, `results/families_7/`); `build_HCA_comparison_tables.R` записує результати до `results/comparison/`.

**Цей реліз не містить каталогу з ілюстраціями та скрипту побудови рисунків.**

### Файли

#### `data/war_participation_data.xlsx`

Авторський аналітичний масив на рівні держав, отриманий шляхом інтеграції та трансформації інформації з UCDP/PRIO, Correlates of War, CIA World Factbook, переліку держав Gleditsch-Ward, а також авторського ручного кодування і розрахованих показників. Робоча книга містить чотири аркуші: `legend`, `легенда (укр)`, `country_profile` (160 рядків держав, 54 стовпці) та `variant_A_vs_B` (таблиця кодування Variant A / Variant B).

#### `scripts/HCA_11var.R`

Кластеризує держави за 11 фіксованими, теоретично обґрунтованими змінними воєнного досвіду (Variant B): кількість конфліктів, сукупна тривалість участі, частка років високої інтенсивності, безперервність, давність останньої участі, частки міждержавного / інтернаціоналізованого внутрішньодержавного / внутрішньодержавного конфлікту, частки оборонної участі на власній території, внутрішнього конфлікту на власній території та участі за кордоном. Змінні військового потенціалу не включено. Модель використовує лише повні випадки — скрипт зупиняється, якщо будь-яка з 11 змінних містить пропущені значення.

#### `scripts/HCA_15var.R`

Розширює набір 11 змінних чотирма змінними військового потенціалу — чисельністю населення, чисельністю військового персоналу, часткою персоналу в населенні та військовими видатками як часткою ВВП. Пропущені значення військових видатків імпутуються медіаною; ця модель була єдиною основною моделлю попереднього релізу.

#### `scripts/HCA_direct_25var.R`

Кластеризує держави за повним фіксованим пулом із 25 показників без скорочення надлишковості. Асиметричні кількісні, часові та масштабні показники трансформовано за допомогою `log1p`; чотири показники-частки/відсотки залишено без трансформації. Пропущені значення імпутуються медіаною окремо для кожної змінної після трансформації.

#### `scripts/HCA_redundancy_reduced.R`

Починається з того самого пулу 25 показників і застосовує `Hmisc::redun()` при скоригованому R² ≥ .95 для видалення надлишкових показників, без наперед заданого переліку видалень і без наперед заданої цільової розмірності — збережений набір і його розмір визначаються під час виконання й фіксуються у власному виводі скрипту. Перед скороченням застосовано те саме правило `log1p`; пропущені значення імпутуються медіаною після скорочення.

#### `scripts/HCA_7families_log1p.R`

Застосовує те саме скорочення `Hmisc::redun()` до пулу 25 показників, після чого емпірично кластеризує збережені показники в родини змінних за допомогою `ClustOfVar`, розрізаючи дендрограму змінних на рівні **k = 7** родин. Кожну родину представлено її першою головною компонентою; для держави з пропущеними вхідними даними родини оцінку розраховано зі спостережених стандартизованих вхідних даних і нормалізовано за нормою спостережених коефіцієнтів, а не імпутовано. Сім стандартизованих факторних оцінок далі використовуються для HCA на рівні держав.

#### `scripts/build_HCA_comparison_tables.R`

Не виконує жодної кластеризації самостійно. Читає збережені об'єкти результатів п'яти зазначених вище специфікацій і будує таблиці міжмодельного порівняння: метадані моделей, діагностику рішень, діагностику на рівні кластера, державу, найближчу до центроїда кожного кластера у власному просторі моделі (явно **не** медоїд), стандартизовані центроїди, кластерну належність України у всіх трьох найкращих рішеннях кожної моделі, глобальних 10 найближчих сусідів України в кожній моделі, перетин і узгодженість рангів сусідів між моделями та повну кластерну належність держав у всіх п'яти специфікаціях. Результати записуються до `results/comparison/` у вигляді `HCA_comparison_tables.xlsx`, набору CSV-файлів та `HCA_comparison_tables.rds`.

Номери кластерів є локальними для кожної моделі та кожного рішення k і не є порівнянними між специфікаціями; між моделями коректно порівнювати лише ранги сусідів, перетин множин і кластерну належність, а не абсолютні величини відстаней.

### Кодування Variant B

Variant B, використане в 11- та 15-змінних моделях, розглядає конфлікти UCDP `13246`, `13247` та `13306`, пов'язані з російсько-українською війною у 2014–2022 рр., як **міждержавні, а не інтернаціоналізовані внутрішньодержавні**, а Україну і Росію — як основні сторони. Для інших конфліктів збережено вихідне кодування. Альтернативна класифікація є авторським аналітичним перекодуванням, чітко відокремленим від вихідного кодування UCDP у робочій книзі.

### Відтворення аналізу

Аналіз виконано в **R 4.3.3** у середовищі **RStudio 2025.05.0+496**.

Скрипти слід запускати **з кореневого каталогу репозитарію**. Перші п'ять специфікацій незалежні одна від одної й можуть виконуватися в будь-якому порядку; `build_HCA_comparison_tables.R` слід запускати останнім, після появи всіх п'яти файлів результатів:

```bash
Rscript scripts/HCA_11var.R
Rscript scripts/HCA_15var.R
Rscript scripts/HCA_direct_25var_log1p.R
Rscript scripts/HCA_redundancy_reduced_log1p.R
Rscript scripts/HCA_7families_log1p.R
Rscript scripts/build_HCA_comparison_tables.R
```

Пакети, необхідні для шести скриптів:

```r
c(
  "readxl", "readr", "dplyr", "tidyr", "tibble",
  "cluster", "fpc", "writexl", "Hmisc", "ClustOfVar"
)
```

### Походження даних і ліцензування

Excel-файл є **похідним аналітичним масивом**, а не повторною публікацією вихідних UCDP/PRIO чи Correlates of War у їхній первинній построковій структурі. Повне походження показників і бібліографічні посилання наведено у [SOURCES.md](SOURCES.md), а особливості ліцензування та повторного використання — у [DATA_NOTICE.md](DATA_NOTICE.md).

Ліцензія MIT у цьому репозитарії поширюється на **оригінальний програмний код, створений для цього проєкту**. Вона не перелiцензовує дані третіх сторін і не скасовує умов їхніх первинних постачальників.

### Цитування

Попереднє цитування GitHub-версії:

> Orlov, O. (2026). *Country war experience case selection: Data and R scripts* (Version 1.1.0) [Computer software and data]. GitHub. https://github.com/OrlovOleh/country-war-experience-case-selection

Після повторного архівування репозитарію в Zenodo слід цитувати **DOI конкретної версії Zenodo**, яка відповідає використаному релізу. Файл `CITATION.cff` забезпечує функцію GitHub **Cite this repository** та передачу метаданих репозитарію.
