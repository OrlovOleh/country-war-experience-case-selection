# Sources / Джерела

This file documents the principal data sources, provenance of variable groups, and supporting references used to construct `data/war_participation_variables_v8.xlsx`.  
Цей файл документує основні джерела даних, походження груп змінних та допоміжні публікації, використані для формування `data/war_participation_variables_v8.xlsx`.

---

## 1. Data provenance / Походження даних

| Variable group / Група змінних | Primary provenance / Основне джерело |
|---|---|
| Conflict participation, primary/supporting role, conflict type, intensity, first/last participation year / Участь у конфліктах, роль, тип конфлікту, інтенсивність, перший/останній рік участі | UCDP/PRIO Armed Conflict Dataset v25.1 (1946–2024) |
| Historical conflict-location field used for locus coding / Історичне поле локації для кодування локусу | UCDP/PRIO Armed Conflict Dataset v19.1 (`gwno_loc`) |
| State identification and Gleditsch-Ward codes / Ідентифікація держав і коди Gleditsch-Ward | Gleditsch & Ward (1999) |
| COW state codes / Коди держав COW | Correlates of War State System Membership List, v2016 |
| Military personnel and population used with NMC indicators / Військовий персонал і населення для показників NMC | Correlates of War National Material Capabilities, v6.0 |
| Interstate battle deaths and last interstate-war year / Бойові втрати у міждержавних війнах та останній рік участі | Correlates of War Inter-State War Data, v4.0 |
| Cross-check of intrastate-war coding / Перехресна перевірка кодування внутрішньодержавних воєн | Correlates of War Intra-State War Data, v4.1 |
| Manning model, military-service obligation, active personnel and current military-expenditure fields / Модель комплектування, військовий обов'язок, активний склад і поточні військові видатки | CIA World Factbook, 2025 edition |
| Continuity, recency, shares, overlap measures and other constructed indicators / Безперервність, давність, частки, показники перекриття та інші похідні змінні | Author calculations / Авторські розрахунки |
| Variant B conflict-type changes and selected locus decisions / Перекодування типу конфлікту Variant B та окремі рішення щодо локусу | Author coding / Авторське кодування |

The R package **peacesciencer** was used during data construction as a standardized delivery channel for several peace-science datasets; source values were checked against the corresponding original data sources.  
Пакет R **peacesciencer** використовувався під час формування даних як стандартизований канал отримання кількох масивів peace science; значення звірялися з відповідними першоджерелами.

---

## 2. Primary datasets / Основні набори даних

### UCDP/PRIO Armed Conflict Dataset

Uppsala Conflict Data Program, & Peace Research Institute Oslo. (2025). *UCDP/PRIO Armed Conflict Dataset* (Version 25.1) [Data set]. https://ucdp.uu.se/downloads/

Historical versions, including v25.1 and v19.1: https://ucdp.uu.se/downloads/olddw.html

Associated publications:

- Davies, S., Pettersson, T., Sollenberg, M., & Öberg, M. (2025). Organized violence 1989–2024, and the challenges of identifying civilian victims. *Journal of Peace Research, 62*(4), 1223–1240. https://doi.org/10.1177/00223433251345636
- Gleditsch, N. P., Wallensteen, P., Eriksson, M., Sollenberg, M., & Strand, H. (2002). Armed conflict 1946–2001: A new dataset. *Journal of Peace Research, 39*(5), 615–637. https://doi.org/10.1177/0022343302039005007

**Note / Примітка.** The analysis is based on **v25.1 covering 1946–2024**, not the later v26.1 release. / Аналіз базується на **v25.1 з покриттям 1946–2024 рр.**, а не на пізнішій версії v26.1.

### Correlates of War: State System Membership

Correlates of War Project. (2017). *State System Membership List* (Version 2016) [Data set]. https://correlatesofwar.org/data-sets/state-system-membership/

### Correlates of War: Inter-State War Data

Correlates of War Project. (2020). *Inter-State War Data* (Version 4.0) [Data set]. https://correlatesofwar.org/data-sets/cow-war/

### Correlates of War: National Material Capabilities

Correlates of War Project. (2021). *National Material Capabilities* (Version 6.0) [Data set]. https://correlatesofwar.org/data-sets/national-material-capabilities/

### Correlates of War: Intra-State War Data

Correlates of War Project. *Intra-State War Data* (Version 4.1) [Data set]. https://correlatesofwar.org/data-sets/cow-war/

### CIA World Factbook

Central Intelligence Agency. (2025). *The World Factbook*. https://www.cia.gov/the-world-factbook/

### Gleditsch-Ward state list

Gleditsch, K. S., & Ward, M. D. (1999). A revised list of independent states since the Congress of Vienna. *International Interactions, 25*(4), 393–413. https://doi.org/10.1080/03050629908434958

---

## 3. Supporting COW references / Допоміжні джерела COW

- Dixon, J., & Sarkees, M. R. (2016). *A guide to intra-state wars: An examination of civil, regional, and intercommunal wars, 1816–2014*. CQ Press.
- Sarkees, M. R., & Wayman, F. W. (2010). *Resort to war: A data guide to inter-state, extra-state, intra-state, and non-state wars, 1816–2007*. CQ Press. https://doi.org/10.4135/9781608718276
- Singer, J. D. (1987). Reconstructing the Correlates of War dataset on material capabilities of states, 1816–1985. *International Interactions, 14*(2), 115–132. https://doi.org/10.1080/03050628808434695
- Singer, J. D., Bremer, S., & Stuckey, J. (1972). Capability distribution, uncertainty, and major power war, 1820–1965. In B. Russett (Ed.), *Peace, war, and numbers* (pp. 19–48). Sage.

---

## 4. Software and computational references / Програмні та обчислювальні джерела

These references document software used during dataset construction. They are not separate licenses for the derived analytical workbook.  
Ці публікації документують програмні засоби, використані під час формування масиву. Вони не є окремими ліцензіями на похідну аналітичну книгу.

- Miller, S. V. (2022). peacesciencer: An R package for quantitative peace science research. *Conflict Management and Peace Science, 39*(6), 755–779. https://doi.org/10.1177/07388942221077926
- Harris, C. R., Millman, K. J., van der Walt, S. J., Gommers, R., Virtanen, P., Cournapeau, D., Wieser, E., Taylor, J., Berg, S., Smith, N. J., Kern, R., Picus, M., Hoyer, S., van Kerkwijk, M. H., Brett, M., Haldane, A., del Río, J. F., Wiebe, M., Peterson, P., … Oliphant, T. E. (2020). Array programming with NumPy. *Nature, 585*, 357–362. https://doi.org/10.1038/s41586-020-2649-2
- McKinney, W. (2010). Data structures for statistical computing in Python. In S. van der Walt & J. Millman (Eds.), *Proceedings of the 9th Python in Science Conference* (pp. 56–61). https://doi.org/10.25080/Majora-92bf1922-00a

---

## 5. Source-specific notes / Примітки щодо окремих джерел

- UCDP/PRIO source categories are retained in the dataset except where the workbook explicitly identifies an author-defined alternative coding (Variant B).
- The main HCA uses the author's Variant B specification, while the original UCDP classification remains preserved in separate variables for sensitivity analysis.
- COW battle-death coverage ends earlier than the main UCDP conflict series; coverage variables in the workbook document these differences.
- Data-source coverage, known limitations, and selected case notes are additionally documented in the `legend` and `легенда (укр)` worksheets of the Excel file.

- Категорії UCDP/PRIO збережено без змін, крім випадків, де книга прямо позначає авторське альтернативне кодування Variant B.
- Основна HCA-модель використовує Variant B, тоді як вихідне кодування UCDP збережене в окремих змінних для аналізу чутливості.
- Покриття бойових втрат COW завершується раніше, ніж основний ряд конфліктів UCDP; різницю документують змінні покриття в Excel-файлі.
- Межі покриття, відомі обмеження та примітки щодо окремих кейсів також наведені на аркушах `legend` і `легенда (укр)`.
