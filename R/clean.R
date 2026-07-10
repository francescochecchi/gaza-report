# =============================================================================
# Clean data from two raw ODK outputs to one clean formatted csv
# =============================================================================
# Input : data/raw/gaza_adult_weight_form1.csv  (form1)
#         data/raw/gaza_adult_weight_form2.csv  (form2)
# Output: data/processed/participants.csv
#
# Usage notes:
#   - All raw recorded data is retained in the output
#     Column `record_remove` explains whether and why each record should be included/excluded from analysis
#     Exclusions is summarised in data/processed/exclusions.csv
#  - Long format: one row per participant per measurement date
#    Baseline characteristics repeated on every row for that participant
#    Form type (form1 or form2) identified in columnn `record_source`
#  - Examples:
#    To use only valid measurements, filter to `record_remove == "include"`
# =============================================================================


set.seed(20260612)
pacman::p_load(readr, janitor, dplyr, tidyr, lubridate, forcats)



# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# audit trail for any manual intervention during cleaning
add_note <- function(notes, condition, text) {
  if_else(condition & !is.na(condition),
    if_else(is.na(notes) | notes == "", text, paste(notes, text, sep = "; ")),
    notes
  )
}

# Dummy submission to pilot the form
test_ids <- c(
  "YJVuva7EtWgsJn3lzGvuSXGV2p0jVIaq8oFc+3ZTpG0="
)

# Earliest valid submission
study_start <- ymd_hms("2025-07-30 14:00:00")

# Map a numeric BMI to a WHO category
bmi_to_category <- function(bmi) {
  cut(bmi,
    breaks = c(-Inf, 18.5, 25, 30, Inf),
    labels = c("Underweight", "Normal", "Overweight", "Obese"),
    right = FALSE
  )
}

# Plausible-range limits for anomaly flagging
limits <- list(
  weight = c(30, 180), # kg
  bmi = c(10, 60), # kg/m^2
  age = c(16, 99), # years
  children = c(0, 20), # dependent children
  daily_rate = 10 # max rate of daily absolute % weight change since study entry
)

# -----------------------------------------------------------------------------
# 1. Tidy column names
# -----------------------------------------------------------------------------
rename_cols <- c(
  "note_identification2-" = "",
  "consent_group-" = "",
  "ngo" = "organisation",
  "gov" = "governorate",
  "ht" = "height",
  "wt_1" = "weight_prewar",
  "wt_2" = "weight_baseline",
  "wt_3" = "weight_record"
)

form1_raw <- read_csv(paste0(dir_path, "data/raw/gaza_adult_weight_form1.csv"),
  show_col_types = FALSE
) |>
  clean_names(replace = rename_cols)
form2_raw <- read_csv(paste0(dir_path, "data/raw/gaza_adult_weight_form2.csv"),
  show_col_types = FALSE
) |>
  clean_names(replace = rename_cols)

# Drop dummy submissions
form1_raw <- filter(form1_raw, !id_number_hashed %in% test_ids)
form2_raw <- filter(form2_raw, !id_number_hashed %in% test_ids)

# Drop superfluous ODK columns
drop_cols <- c(
  "id_initial_name", "id_father_name", "id_mother_name", "id_initial_family",
  "id_favourite_artist", "id_dob", "id_type", "id_type1", "id_type2",
  "id_type3", "id_number_confirmed", "consent_paper",
  "deviceid", "deviceid_hashed", "invalid_hash", "st", "end",
  "submitter_id", "submitter_name", "attachments_present",
  "attachments_expected", "status", "review_state", "device_id", "edits",
  "meta_instance_id", "form_version",
  "comments_ingo_unrwa", "comments_ingo_sci"
)
form1 <- select(form1_raw, -any_of(drop_cols))
form2 <- select(form2_raw, -any_of(drop_cols))

# Set up audit trail
form1 <- mutate(form1, record_notes = NA_character_)
form2 <- mutate(form2, record_notes = NA_character_)

# -----------------------------------------------------------------------------
# 1b. Recode values
# -----------------------------------------------------------------------------
# Missing data is coded 999 (and 666/888 for height) in ODK
recode_factor_codes <- function(x, map) {
  factor(map[as.character(x)], levels = unname(map))
}

form1 <- form1 |>
  mutate(
    # missing codes -> NA
    height = if_else(height %in% c(666, 888, 999), NA_real_, height),
    weight_prewar = na_if(weight_prewar, 999),
    weight_baseline = na_if(weight_baseline, 999),
    children_feeding = na_if(children_feeding, 999),
    # numeric ODK codes -> labels
    organisation = recode_factor_codes(
      organisation, c("1" = "Save the Children International", "3" = "UNRWA")
    ),
    sex = recode_factor_codes(
      sex, c("1" = "Male", "2" = "Female", "9" = "Other/prefer not to answer")
    ),
    governorate = recode_factor_codes(
      governorate, c(
        "55" = "North Gaza", "60" = "Gaza City",
        "65" = "Deir Al Balah", "70" = "Khan Yunis", "75" = "Rafah"
      )
    ),
    role = recode_factor_codes(
      role, c(
        "1" = "Expatriate", "2" = "National staff",
        "3" = "Consultant/contractor", "4" = "Casual/daily worker",
        "5" = "Other", "6" = "Prefer not to answer"
      )
    )
  )

form2 <- form2 |>
  mutate(weight_record = na_if(weight_record, 999))

# -----------------------------------------------------------------------------
# 2. Form1 (collects full participant characteristics)
# -----------------------------------------------------------------------------
# Measurement date = entered date_today, else the automated submission date
form1 <- form1 |>
  mutate(
    record_date = as_date(if_else(is.na(date_today), submission_date, date_today)),
    record_date = if_else(record_date < as_date(study_start),
      as_date(submission_date), record_date
    )
  )

# Manual override: these participants likely entered pre-war weight in the baseline
# weight field (wt_2); use same-day follow-up value as the baseline
prewar_override <- tibble::tribble(
  ~id_number_hashed,                               ~weight_baseline_fixed,
  "8cjh34RpDf/SQuLks8jQFNGuUn5+7IdTjuottwQcans=",  72.5, # id 108
  "VkdVauLOScx0JtRsfnhzQEo0xCQAamk8+nneEYYIKsc=",  49.5, # id 339
  "s/x/yqoniZuhCXTZ96mFI/wAuaWzw9dTXMYMhNh4WG8=",  82.0 # id 564
)
form1 <- form1 |>
  left_join(prewar_override, by = "id_number_hashed") |>
  mutate(
    record_notes = add_note(
      record_notes, !is.na(weight_baseline_fixed),
      sprintf(
        "baseline weight overridden %s->%s (pre-war entered as baseline; using same-day follow-up)",
        weight_baseline, weight_baseline_fixed
      )
    ),
    weight_baseline = coalesce(weight_baseline_fixed, weight_baseline)
  ) |>
  select(-weight_baseline_fixed)

# Classify each form1 submission for a participant
#   - first submission           -> "first" (the true enrolment record)
#   - later submission, <24h on   -> "invalid_double_entry" (a true duplicate)
#   - later submission, >=24h on  -> "rehomed_form2"
#       (a follow-up measurement mistakenly entered on form1;
#        moved into the form2 stream below)
form1 <- form1 |>
  group_by(id_number_hashed) |>
  arrange(submission_date, .by_group = TRUE) |>
  mutate(
    dup_lag = as.numeric(submission_date - lag(submission_date), units = "secs"),
    form1_dup = case_when(
      is.na(id_number_hashed) ~ "first",
      row_number() == 1 ~ "first",
      dup_lag <= 86400 ~ "invalid_double_entry",
      TRUE ~ "rehomed_form2"
    )
  ) |>
  ungroup()

# Consent: all four consent items must be 1
form1 <- form1 |>
  mutate(
    pilot = submission_date < study_start,
    consent_all = consent1 == 1 & consent2 == 1 &
      consent3 == 1 & consent4 == 1,
    record_remove_form1 = case_when(
      pilot ~ "invalid_pilot",
      # note all non-consenting are already replaced with blank fields in raw data
      is.na(id_number_hashed) ~ "invalid_nonconsent",
      !consent_all | is.na(consent_all) ~ "invalid_nonconsent",
      form1_dup == "invalid_double_entry" ~ "invalid_double_entry",
      TRUE ~ "include"
    )
  )

# Split out mixed-up forms to add to the follow-up
rehomed <- form1 |>
  filter(form1_dup == "rehomed_form2") |>
  transmute(id_number_hashed, key,
    record_date,
    weight_record = weight_baseline,
    submission_date
  )

# -----------------------------------------------------------------------------
# 3. Form2: collects ID and weight
# -----------------------------------------------------------------------------
form2 <- form2 |>
  mutate(
    record_date = as_date(if_else(is.na(date_collected),
      submission_date, date_collected
    )),
    record_date = if_else(record_date < as_date(study_start),
      as_date(submission_date), record_date
    )
  ) |>
  bind_rows(rehomed)

# Find participants with baseline & follow-up on same day; keep baseline record
form1_dates <- form1 |>
  filter(form1_dup != "rehomed_form2") |>
  distinct(id_number_hashed, record_date) |>
  mutate(has_form1_same_day = TRUE)
# Otherwise, if >1 record per participant per day: keep later, flag earlier
form2 <- form2 |>
  left_join(form1_dates, by = c("id_number_hashed", "record_date")) |>
  mutate(has_form1_same_day = coalesce(has_form1_same_day, FALSE)) |>
  group_by(id_number_hashed, record_date) |>
  mutate(
    record_remove_form2 = case_when(
      submission_date < study_start ~ "invalid_pilot",
      is.na(id_number_hashed) ~ "invalid_nonconsent",
      n() > 1 & submission_date != max(submission_date) ~ "invalid_double_entry",
      # same-day collision with baseline measurement
      has_form1_same_day ~ "invalid_double_entry",
      TRUE ~ "include"
    )
  ) |>
  ungroup()

# -----------------------------------------------------------------------------
# 4. Baseline characteristics
# -----------------------------------------------------------------------------
# use one enrolment record per participant (one "first" row per id that consented)
characteristics <- form1 |>
  filter(record_remove_form1 == "include", form1_dup == "first") |>
  # age at baseline from date of birth
  mutate(age_years = floor(
    time_length(interval(as_date(dob), record_date), "years")
  )) |>
  select(id_number_hashed, organisation, sex, governorate, role,
    children_feeding, chronic_condition, height, weight_prewar,
    age_years,
    date_entry = record_date, weight_entry = weight_baseline
  )

# -----------------------------------------------------------------------------
# 5. Long data: baseline measurement + all follow-ups
# -----------------------------------------------------------------------------
# Baseline weights
form1_weights <- form1 |>
  filter(form1_dup != "rehomed_form2") |>
  transmute(id_number_hashed, record_date,
    weight = weight_baseline,
    record_source = "form1",
    record_remove = record_remove_form1,
    record_notes
  )
# long single dataframe with baseline and followup
weights <- bind_rows(
  form1_weights,
  form2 |>
    transmute(id_number_hashed, record_date,
      weight = weight_record,
      record_source = "form2",
      record_remove = record_remove_form2,
      record_notes
    )
)

# Assign a sequential, non-hashed participant id to each hashed id
ids <- weights |>
  filter(!is.na(id_number_hashed)) |>
  distinct(id_number_hashed) |>
  arrange(id_number_hashed) |>
  mutate(id = row_number())

data <- weights |>
  left_join(ids, by = "id_number_hashed") |>
  left_join(characteristics, by = "id_number_hashed") |>
  # add IDs for non-consenting records so they are counted in summaries
  mutate(id = if_else(
    is.na(id) & record_remove == "invalid_nonconsent",
    max(c(0L, id), na.rm = TRUE) +
      cumsum(is.na(id) & record_remove == "invalid_nonconsent"),
    id
  ))

# Mark which ids have a valid baseline (so a participant has characteristics)
ids_with_baseline <- unique(characteristics$id_number_hashed)

data <- data |>
  # flag follow-up records whose id has no baseline
  mutate(record_remove = if_else(
    record_remove == "include" &
      record_source == "form2" &
      !(id_number_hashed %in% ids_with_baseline),
    "invalid_no_baseline", record_remove
  )) |>
  select(-id_number_hashed) |>
  arrange(id, record_date)

# -----------------------------------------------------------------------------
# 5b. Anchor study entry date to the participant's earliest record
# -----------------------------------------------------------------------------
# Issue: form1 recorded after form2;
#   may be due to lag from offline to sync to ODK server
# Strategy: anchor entry to each participant's earliest recorded
#   measurement date across any date in either form instead
entry <- data |>
  filter(
    !is.na(id),
    record_remove != "invalid_nonconsent", # keep non-consent derived fields NA
    !is.na(record_date),
    record_date >= as_date(study_start) # never anchor entry before study start
  ) |>
  arrange(id, record_date, record_remove != "include") |>
  group_by(id) |>
  summarise(
    date_entry_new = min(record_date),
    # weight at the earliest record: prefer an included, non-missing measurement
    weight_entry_new = {
      first_date <- min(record_date)
      cand <- weight[record_date == first_date & !is.na(weight)]
      if (length(cand)) cand[1] else NA_real_
    },
    .groups = "drop"
  )

data <- data |>
  left_join(entry, by = "id") |>
  mutate(
    record_notes = add_note(
      record_notes,
      !is.na(date_entry_new) & date_entry_new != date_entry,
      sprintf(
        "study entry re-anchored %s->%s (earliest recorded measurement)",
        date_entry, date_entry_new
      )
    ),
    date_entry = coalesce(date_entry_new, date_entry),
    weight_entry = coalesce(weight_entry_new, weight_entry)
  ) |>
  select(-date_entry_new, -weight_entry_new)

# -----------------------------------------------------------------------------
# 6. Derived measures: BMI, change over time, and categories
# -----------------------------------------------------------------------------
data <- data |>
  group_by(id) |>
  mutate(
    days_since_entry = 1L + as.integer(record_date - date_entry),
    # BMI
    bmi = weight / (height / 100)^2,
    bmi_prewar = weight_prewar / (height / 100)^2,
    bmi_entry = weight_entry / (height / 100)^2,
    # change since study entry
    weight_pct_daily_rate = (100 * (weight - weight_entry) / weight_entry) / days_since_entry
  ) |>
  ungroup() |>
  mutate(
    # categorical groupings (age/children ranges; out-of-range -> NA)
    age = case_when(
      is.na(age_years) | !between(age_years, limits$age[1], limits$age[2]) ~ NA_character_,
      age_years < 30 ~ "Age under 30",
      age_years <= 45 ~ "Age 30-45",
      TRUE ~ "Age over 45"
    ),
    children = case_when(
      is.na(children_feeding) |
        !between(children_feeding, limits$children[1], limits$children[2]) ~ NA_character_,
      children_feeding == 0 ~ "0",
      children_feeding == 1 ~ "1",
      children_feeding == 2 ~ "2",
      TRUE ~ "3+"
    ),
    bmi_category = bmi_to_category(bmi),
    bmi_category_entry = bmi_to_category(bmi_entry),
    bmi_category_prewar = bmi_to_category(bmi_prewar)
  )

# -----------------------------------------------------------------------------
# 7. Anomaly flagging written into record_remove
# -----------------------------------------------------------------------------
data <- data |>
  mutate(
    record_remove = case_when(
      record_remove != "include" ~ record_remove,
      is.na(weight) ~ "invalid_missing",
      !between(weight, limits$weight[1], limits$weight[2]) ~ "invalid_anomaly_weight",
      !between(bmi, limits$bmi[1], limits$bmi[2]) ~ "invalid_anomaly_bmi",
      abs(weight_pct_daily_rate) >= limits$daily_rate ~ "invalid_anomaly_rate",
      TRUE ~ "include"
    )
  )

# -----------------------------------------------------------------------------
# 8. Sequence each participant's records by date
# -----------------------------------------------------------------------------
# record_number: 1 = first valid record, 2 = next, ...
# NA for excluded rows so only finally-included rows count
data <- data |>
  arrange(id, record_date, record_source) |>
  group_by(id) |>
  mutate(
    record_number = if_else(
      record_remove == "include",
      cumsum(record_remove == "include"),
      NA_integer_
    )
  ) |>
  ungroup()

# -----------------------------------------------------------------------------
# 9. Order columns and write
# -----------------------------------------------------------------------------
out <- data |>
  select(
    id, record_number, record_remove, record_notes,
    record_date, date_entry, days_since_entry,
    organisation, sex, age_years, age, governorate, role, children, chronic_condition,
    height, weight_prewar, weight_entry, weight, weight_pct_daily_rate,
    bmi_prewar, bmi_entry, bmi,
    bmi_category_prewar, bmi_category_entry, bmi_category
  ) |>
  arrange(id, record_date)

dir.create(paste0(dir_path, "data/processed"), showWarnings = FALSE)
write_csv(out, paste0(dir_path, "data/processed/participants.csv"))

# -----------------------------------------------------------------------------
# 10. Documentation: data quality
# -----------------------------------------------------------------------------
# One row per exclusion reason, in the order reasons are applied. Records flow
# top to bottom: each reason removes records, the rest carry on to "include".
reason_levels <- c(
  "invalid_nonconsent"     = "No consent",
  "invalid_no_baseline"    = "No baseline data",
  "invalid_double_entry"   = "Duplicate record (same participant/day)",
  "invalid_missing"        = "Missing weight measurement",
  "invalid_anomaly_weight" = "Weight outside 30-180 kg",
  "invalid_anomaly_bmi"    = "BMI outside 10-60",
  "invalid_anomaly_rate"   = "Weight change >10%/day since entry",
  "include"                = "Included in analysis"
)

# Participant funnel based on exclusion reasons
n_reasons <- length(reason_levels)
participant_max_rank <- out |>
  filter(!is.na(id)) |>
  mutate(rank = match(as.character(record_remove), names(reason_levels))) |>
  group_by(id) |>
  summarise(max_rank = max(rank), .groups = "drop") |>
  pull(max_rank)

quality <- out |>
  mutate(record_remove = factor(record_remove, levels = names(reason_levels))) |>
  group_by(record_remove, .drop = FALSE) |>
  summarise(
    n_records = n(),
    n_participants = n_distinct(id[!is.na(id)]),
    .groups = "drop"
  ) |>
  arrange(record_remove) |>
  mutate(
    reason = reason_levels[as.character(record_remove)],
    rank = match(as.character(record_remove), names(reason_levels)),
    n_remaining = nrow(out) - cumsum(n_records),
    # participants still in after applying this step (and all steps above)
    n_participants_remaining = vapply(
      rank, function(k) sum(participant_max_rank > k), integer(1)
    )
  ) |>
  # for the final include row, show kept records and final N participants
  mutate(
    n_remaining = if_else(record_remove == "include", n_records, n_remaining),
    n_participants_remaining = if_else(
      record_remove == "include",
      sum(participant_max_rank == n_reasons),
      n_participants_remaining
    )
  ) |>
  select(
    reason, record_remove, n_records, n_participants,
    n_remaining, n_participants_remaining
  )

write_csv(quality, paste0(dir_path, "data/processed/data-quality.csv"))

message(sprintf(
  "Wrote %s rows, %s participants -> data/processed/participants.csv",
  nrow(out), n_distinct(out$id[!is.na(out$id)])
))
message("record_remove breakdown:")
print(count(out, record_remove))
