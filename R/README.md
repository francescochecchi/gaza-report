### Quickstart

To prepare and analyse data:

1. Save data into `data/raw`

- `gaza_adult_weight_form1.csv`
- `gaza_adult_weight_form2.csv`

2. Prepare the data for analysis. To do this, run the `main.R` script in the `R` sub-folder, up to and including the following lines:

```
# Pre-process datasets
source(paste0(dir_path, "R/clean.R"))
```

Note that for completeness, the data processing step keeps all recorded measurements (with duplicates, unidentified, etc).
Invalid records are described in the `record_remove` field, and any modified values recorded in `record_notes`.

- Weight data come from raw records across two ODK survey forms
  - `form1`: baseline enrolment record with all characteristics
  - `form2`: follow-up (date, weight measurement only)
- `R/clean.R` combines both forms into a single long dataset, `data/processed/participants.csv`
- All collected records are included in the data-processed csv, including invalid records or implausible values
  - Records are assessed against each exclusion criterion in turn
  - Reasons for exclusion are flagged in the field `record_remove`
  - Records passing all criteria are labelled `include`


3. Analyse the data. To do this, run the following lines of the `main.R` script:

```
# Visualise data
source(paste0(dir_path, "R/visualise.R"))
```

This step will filter records to `record_remove == "include"` to use only the valid records (a `r dim_desc(participants)` dataframe).
You can also selectively exclude using `record_remove` labels. To do this, modify the `visualise.R` script directly.

To render this report locally, use `quarto::quarto_render(here::here("R", "cleaning.qmd"))`.
Add interactivity to plots with `ggplotly(plot)`.

