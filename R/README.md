### Quickstart

To prepare and load data for analysis:

1. Save data into `data/raw`

- `gaza_adult_weight_form1.csv`
- `gaza_adult_weight_form2.csv`

2. Run:

```
# Process raw data
source(here::here("R", "clean.R"))
# Read in processed data
data_processed <- read.csv(here::here("data", "processed", "participants.csv"))
# Filter to valid records
participants <- data_processed[data_processed$record_remove == "include", ]
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

Filter to `record_remove == "include"` to use only the valid records (a `r dim_desc(participants)` dataframe).
Or selectively exclude using `record_remove` labels.

To render this report locally, use `quarto::quarto_render(here::here("R", "cleaning.qmd"))`.
Add interactivity to plots with `ggplotly(plot)`.

