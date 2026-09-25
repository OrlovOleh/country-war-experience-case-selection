# Repository manifest / Маніфест репозитарію

Release: **v1.1.0**  
Prepared: **2026-09-25**

| Path | Role / Призначення |
|---|---|
| `data/war_participation_data.xlsx` | Country-level analytical workbook feeding all five HCA specifications / Аналітична книга на рівні держав, що використовується у всіх п'яти специфікаціях HCA |
| `scripts/HCA_11var.R` | 11-variable theoretical model, Variant B, war-experience only / 11-змінна теоретична модель, Variant B, лише воєнний досвід |
| `scripts/HCA_15var.R` | 15-variable expanded model, Variant B + capability variables / 15-змінна розширена модель, Variant B + змінні військового потенціалу |
| `scripts/HCA_direct_25var_log1p.R` | Direct 25-variable model, log1p-transformed / Пряма 25-змінна модель, log1p-трансформація |
| `scripts/HCA_redundancy_reduced_log1p.R` | Data-driven redundancy-reduced model (`Hmisc::redun`), log1p-transformed / Модель зі скороченою надлишковістю (`Hmisc::redun`), log1p-трансформація |
| `scripts/HCA_7families_log1p.R` | Seven-family synthetic-score model (`ClustOfVar` + PCA), log1p-transformed / Модель семи синтетичних факторних оцінок (`ClustOfVar` + PCA), log1p-трансформація |
| `scripts/build_HCA_comparison_tables.R` | Cross-model comparison tables across all five specifications; runs no clustering itself / Таблиці міжмодельного порівняння п'яти специфікацій; не виконує кластеризацію самостійно |
| `README.md` | Repository documentation / Документація репозитарію |
| `SOURCES.md` | Data provenance and references / Походження даних і джерела |
| `DATA_NOTICE.md` | Data reuse and third-party rights notice / Повідомлення про повторне використання даних і права третіх сторін |
| `LICENSE` | MIT license for original project code / MIT-ліцензія для авторського коду |
| `CITATION.cff` | Citation metadata / Метадані для цитування |
| `.gitignore` | Git exclusions / Виключення Git |

This release does not include a figures directory or figure-generation script.  
Цей реліз не містить каталогу з ілюстраціями та скрипту побудови рисунків.

## Changes since v1.0.0 / Зміни з версії v1.0.0

Replaced the single Variant B script (`HCA_variant_B_full_1.R`) with five parallel specifications of state war experience — 11-variable, 15-variable, direct 25-variable, redundancy-reduced, and seven-family synthetic-score — each clustered independently and cross-compared. Added `build_HCA_comparison_tables.R`, which aggregates the five saved result objects into cross-model comparison tables (cluster diagnostics, nearest-to-centroid representatives, Ukraine's cluster membership and nearest neighbours across models) without re-running any clustering. All five specifications now share a single, consistent `log1p` transformation rule for skewed count/duration/magnitude indicators, with proportion and percentage indicators left untransformed. Removed `HCA_variable_importance_variant_B.R` and `image_generation_hca_top10_cluster_contours.R`; the figures directory is not part of this release. Replaced `data/war_participation_variables_v8.xlsx` with `data/war_participation_data.xlsx`.

Замінено єдиний скрипт Variant B (`HCA_variant_B_full_1.R`) на п'ять паралельних специфікацій воєнного досвіду держав — 11-змінну, 15-змінну, пряму 25-змінну, зі скороченою надлишковістю та модель семи синтетичних факторних оцінок, — кожну з яких кластеризовано незалежно та порівняно між моделями. Додано `build_HCA_comparison_tables.R`, який об'єднує п'ять збережених об'єктів результатів у таблиці міжмодельного порівняння (діагностика кластерів, найближчі до центроїда представники, кластерна належність України та її найближчі сусіди в усіх моделях) без повторного запуску кластеризації. У всіх п'яти специфікаціях тепер застосовано єдине узгоджене правило `log1p`-трансформації для асиметричних кількісних/часових/масштабних показників; частки та відсотки залишено без трансформації. Вилучено `HCA_variable_importance_variant_B.R` та `image_generation_hca_top10_cluster_contours.R`; каталог з ілюстраціями не входить до цього релізу. Файл `data/war_participation_variables_v8.xlsx` замінено на `data/war_participation_data.xlsx`.

The scripts in this package default to expecting the analytical workbook in the same directory as the scripts themselves; the input-file paths in this repository's copies have been adjusted for the `data/` + `scripts/` split used here.  
Скрипти за замовчуванням очікують аналітичну книгу в тому самому каталозі, що й самі скрипти; шляхи до вхідного файлу в копіях цього репозитарію адаптовано під розподіл на `data/` та `scripts/`.
