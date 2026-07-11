# =============================================================================
# Visualise data for paper
# =============================================================================


# -----------------------------------------------------------------------------
# Load packages and read dataset
# -----------------------------------------------------------------------------

set.seed(20260612)
pacman::p_load(caret, flextable, gbm, ggpubr, ggplot2, here, mgcv, scales, 
  tidyverse)

# Read in processed data
data_processed <- read.csv(paste0(dir_path, "data/processed/participants.csv"))

# Filter to valid records
df_main <- data_processed[data_processed$record_remove == "include", ]

# -----------------------------------------------------------------------------
# Specify plot theme and colours
# -----------------------------------------------------------------------------

lshtm_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "#01454f", fill = NA, size = 0.5),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    #panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_line(colour = "aliceblue"),
    #panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_line(colour = "aliceblue"),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "#01454f"),
    axis.title = element_text(colour = "#01454f"),
    axis.ticks = element_line(colour = "#01454f"),
    # legend at the bottom 6)
    #legend.position = "bottom"
    strip.text.x = element_text(colour = "white"),
    strip.text.y = element_text(colour = "white", angle = 270),
    strip.background = element_rect(
      color="#01454f", fill="#01454f", linewidth=1.5, linetype="solid"
    ),
    legend.position = "bottom",
    legend.title = element_text(colour = "#01454f", face = "bold"),
    legend.text = element_text(colour = "#01454f")
  )
}
theme_set(lshtm_theme())

lshtm_palette <- list(
  bmi_categories = c(
    "obese (>= 30.0)" = "#01454f",
    "overweight (25.0 to 29.9)" = "#007457",
    "normal (18.5 to 24.9)" = "#709b28",
    "underweight (< 18.5)" = "#ffa600"),
  period = c(
    "Aug-Sep 2025" = "#007457",
    "Oct-Nov 2025" = "#709b28",
    "Overall" = "#01454f"
    ),
  record_number_cat = c(
    "first" = "#709b28",
    "second" = "#ffa600",
    "third or more" = "#01454f"
  ),
  lshtm_generic = "#01454f"
)

other_palette <- c("#007457", "#ffa600", "#709b28", "#01454f")



# -----------------------------------------------------------------------------
# Produce Table: Participant characteristics
# -----------------------------------------------------------------------------

# Data for this plot
df_tab <- df_main

# Compute number of records per participant
df_tab$n_obs <- 1
x <- aggregate(n_obs ~ id, data = df_tab, FUN = sum)
df_tab <- subset(df_tab, select = -n_obs)
df_tab <- merge(df_tab, x, by = "id", all.x = T)

# Retain only first observations
df_tab <- subset(df_tab, record_date == date_entry)

# Categorise records per participant
df_tab$n_obs_cat <- df_tab$n_obs
df_tab[which(df_tab$n_obs_cat > 2), "n_obs_cat"] <- "3+"
df_tab$n_obs_cat <- as.character(df_tab$n_obs_cat)
table(df_tab$n_obs_cat, useNA = "always")

# Recategorise organisation
df_tab[which(df_tab$organisation == "Save the Children International"), 
  "organisation"] <- "SCI"

# Categorise date into month
df_tab$month_entry <- month(df_tab$date_entry)
df_tab$month_entry <- ifelse(df_tab$month_entry %in% 7:9, "Aug-Sep 2025",
  "Oct-Nov 2025")
table(df_tab$month_entry, useNA = "always")
  # Note: 30-31 July 2025 included under month of August

# Recategorise age
df_tab$age_cat <- "missing"
df_tab[which(df_tab$age_years < 30), "age_cat"] <- "< 30y"
df_tab[which(df_tab$age_years %in% 30:44), "age_cat"] <- "30 to 44y"
df_tab[which(df_tab$age_years >= 45), "age_cat"] <- ">= 45y"
table(df_tab$age_cat, useNA = "always")

# Categorise role into fewer categories
df_tab$role_cat <- df_tab$role
df_tab[which(df_tab$role %in% c("Casual/daily worker", "Consultant/contractor", 
  "Expatriate", "Other")), "role_cat"] <- "Casual worker/other"
df_tab[which(df_tab$role_cat == "Prefer not to answer"), "role_cat"] <-
  "missing"
table(df_tab$role_cat, useNA = "always")

# Categorise chronic conditions into binary (yes/no)
df_tab$chronic_condition_cat <- ifelse(df_tab$chronic_condition == 0, 
  "no", "yes")
table(df_tab$chronic_condition_cat, useNA = "always")

# Variables of interest
vars <- c("month_entry", "age_cat", "sex", "governorate", "role_cat", 
  "children", "chronic_condition_cat", "n_obs", "n_obs_cat")
names(vars) <- c("month of first observation", "age", "sex", 
  "governorate of residence", "professional role", 
  "number of dependent children", "living with a chronic condition",
  "mean number of weight observations (range)",
  "number of weight observations")

# Identify missing data as missing
for (i in vars) {df_tab[which(is.na(df_tab[, i])), i] <- "missing"}

# Convert variables to factors
df_tab$month_entry <- factor(df_tab$month_entry, levels = c("Aug-Sep 2025", 
  "Oct-Nov 2025", "missing"))
df_tab$age_cat <- factor(df_tab$age_cat, 
  levels = c("< 30y", "30 to 44y", ">= 45y", 
  "missing"))
df_tab$sex <- factor(df_tab$sex, levels = c("Female", "Male", "missing"))
df_tab$governorate <- factor(df_tab$governorate, 
  levels = c("North Gaza", "Gaza City",
  "Deir Al Balah", "Khan Yunis", "Rafah", "missing"))
df_tab$role_cat <- factor(df_tab$role_cat, levels = c("National staff", 
  "Casual worker/other", "missing"))
df_tab$children <- factor(df_tab$children, 
  levels = c("0", "1", "2", "3+", "missing"))
df_tab$chronic_condition_cat <- factor(df_tab$chronic_condition_cat, 
  levels = c("yes", "no", "missing"))
df_tab$n_obs_cat <- factor(df_tab$n_obs_cat, 
  levels = c("1", "2", "3+", "missing"))

# Create a blank table
x <- sapply(vars, function(xx) {length(levels(df_tab[, xx]))})
tab1 <- matrix(NA, nrow = sum(x) + length(vars) + 1, ncol = 4)
tab1 <- as.data.frame(tab1)
colnames(tab1) <- c("Variable", "SCI", "UNRWA", "Total")

# Fill in number of participants
tab1[1, ] <- c("number of participants", 
  table(df_tab$organisation), nrow(df_tab))

# Fill in rest of the table one variable at a time
nrows <- 2
for (i in 1:length(vars)) {

  # variable column
  var_i <- vars[i]
  tab1[nrows, "Variable"] <- names(vars)[i]
  
  # for continuous variables...
  if (var_i == "n_obs") {
    tab1[nrows+1, "Variable"] <- "  mean (range)"
    
    # SCI and UNRWA columns
    for (j in c("SCI", "UNRWA")) {
      x <- df_tab[which(df_tab$organisation == j), var_i]
      tab1[nrows+1, j] <- paste0(formatC(mean(x), 
        digits = 2), " (", min(x), " to ", max(x), ")")
    }
  
    # Total column
    x <- df_tab[, var_i]
    tab1[nrows+1, "Total"] <- paste0(formatC(mean(x), 
        digits = 2), " (", min(x), " to ", max(x), ")")
    
    # Update position in table
    nrows <- nrows+2
  }
  
  # for categorical variables...
  else {
    x <- levels(df_tab[, var_i])
    tab1[(nrows+1):(nrows+length(x)), "Variable"] <- paste0("  ", x)
    
    # SCI and UNRWA columns
    for (j in c("SCI", "UNRWA")) {
      x <- table(df_tab[which(df_tab$organisation == j), var_i])
      tab1[(nrows+1):(nrows+length(x)), j] <- paste0(x, " (", 
        formatC(percent(as.vector(prop.table(x)), 0.1), 
          format = "f", digits = 1, width = -1), ")")
    }
  
    # Total column
    x <- table(df_tab[, var_i])
    tab1[(nrows+1):(nrows+length(x)), "Total"] <- paste0(x, " (", 
      formatC(percent(as.vector(prop.table(x)), 0.1), 
        format = "f", digits = 1, width = -1), ")")
    
    # Update position in table
    nrows <- nrows+length(x)+1 
  }
}

# Write table
write.csv(tab1, paste0(dir_path, "report/tab1.csv"), row.names = F)
x <- flextable(tab1)
flextable::save_as_docx(x, path = paste0(dir_path, "report/tab1.docx"))


# -----------------------------------------------------------------------------
# Visualise weight change from pre-war to first observation
# -----------------------------------------------------------------------------

# Data for this plot
df_fig <- df_tab

# Convert dataset so that the two periods (month_entry) and all periods combined
    # are stacked on top of each other
df_fig$period <- df_fig$month_entry
x <- df_fig
x$period <- "Overall"
df_fig <- rbind(df_fig, x)
df_fig$period <- factor(df_fig$period, 
  levels = c("Aug-Sep 2025", "Oct-Nov 2025", "Overall"))

# Compute percent weight loss
df_fig$weight_change <- (df_fig$weight_entry - df_fig$weight_prewar) / 
  (df_fig$weight_prewar)

# Variables of interest
vars <- c("age_cat", "sex", "governorate", "role_cat", 
  "children", "chronic_condition_cat")
names(vars) <- c("age", "sex", 
  "governorate of residence", "professional role", 
  "number of dependent children", "living with a chronic condition")

# Produce violin plots for each variable, by period and overall
for (i in vars) {
  
  # data for this plot
  df_i <- df_fig
  
  # variable
  df_i$var_i <- df_i[, i]
  
  # eliminate missing
  df_i <- subset(df_i, var_i != "missing")
  df_i <- subset(df_i, !is.na(var_i))
    
  # labels with sample size
  df_i$n <- 1
  labs <- aggregate(n~var_i + period, data = df_i, FUN = sum)
  labs$weight_change <- 0.28
  
  # eliminate from the graph groups with n < 5
  for (j in 1:nrow(labs)) {
    if (labs[j, "n"] < 5) {
      x <- which(df_i$var_i == labs[j, "var_i"] & 
          df_i$period == labs[j, "period"])
      df_i <- df_i[-x, ]
    }
  }
 
  # plot
  pl <- ggplot(data = df_i, aes(y = weight_change, x = period, 
    colour = period, fill = period, alpha = period)) +
    geom_violin(linewidth = 1.0, trim = F, quantiles = 0.50,
      quantile.linetype = "11") +
    scale_y_continuous("percent weight change", labels = percent,
      limits = c(-0.52, 0.32), breaks = seq(-0.50, 0.30, 0.10)) +
    scale_x_discrete("period", drop = F) +
    lshtm_theme() +
    scale_colour_manual("period", values = lshtm_palette$period, drop = F) +
    scale_fill_manual("period", values = lshtm_palette$period, drop = F) +
    scale_alpha_manual("period", values = c(0.50, 0.50, 0.75), drop = F) +
    geom_hline(yintercept = 0, colour = "grey70", linetype = "21") +
    facet_grid(.~var_i) +
    geom_text(data = labs, aes(y = weight_change, x = period, label = n),
      show.legend = F) +
    theme(legend.position = "bottom", axis.text.x = element_blank(),
      axis.ticks.x = element_blank(), plot.margin = margin(20,10,20,10))
  
  # assign plot name
  assign(paste0("pl_", i), pl)
}

# Produce combination plot
x <- grep("pl_", ls(), value = T)
x <- x[x!= "pl_combi"]
pl_combi <- ggarrange(plotlist = sapply(x, get), labels = names(sort(vars)), 
  ncol = 2, nrow = 3, common.legend = T, align = "v", legend = "bottom",
  hjust = 0, vjust = 1)
pl_combi <- annotate_figure(pl_combi, 
  bottom = text_grob("note: groups with < 5 observations are omitted", 
    hjust = 0, x = unit(5.5, "pt"), y = unit(55, "pt")))
ggsave(paste0(dir_path, "report/weight_change.png"), dpi = "print", height = 25, 
  width = 30, units = "cm")
ggsave(paste0(dir_path, "report/weight_change.pdf"), dpi = "print", height = 25, 
  width = 30, units = "cm")
rm(list = ls(pattern = "^pl"))


# -----------------------------------------------------------------------------
# Visualise BMI category share pre-war and at first observation - full version
# -----------------------------------------------------------------------------

# Data for this plot
df_fig <- df_tab

# Convert dataset so that the pre-war, two periods (month_entry) 
    # and all periods combined are stacked on top of each other
df_fig$period <- df_fig$month_entry
x <- df_fig
x$period <- "Overall"
df_fig <- rbind(df_fig, x)
x <- df_fig
x$period <- "Pre-war"
df_fig <- rbind(df_fig, x)
df_fig$period <- factor(df_fig$period, 
  levels = c("Pre-war", "Aug-Sep 2025", "Oct-Nov 2025", "Overall"))

# Select BMI category for different periods
df_fig$bmi_category_fig <- ifelse(df_fig$period == "Pre-war", 
  df_fig$bmi_category_prewar, df_fig$bmi_category_entry)
df_fig$bmi_category_fig <- tolower(df_fig$bmi_category_fig)
df_fig$bmi_category_fig <- factor(df_fig$bmi_category_fig, 
  levels = c("obese", "overweight", "normal", "underweight"),
  labels = c("obese (>= 30.0)", "overweight (25.0 to 29.9)", 
    "normal (18.5 to 24.9)", "underweight (< 18.5)"))
table(df_fig$bmi_category_fig, useNA = "always")

# Remove two missing BMI values
df_fig <- df_fig[complete.cases(df_fig$bmi_category_fig), ]

# Variables of interest
vars <- c("age_cat", "sex", "governorate", "role_cat", 
  "children", "chronic_condition_cat")
names(vars) <- c("age", "sex", 
  "governorate of residence", "professional role", 
  "number of dependent children", "living with a chronic condition")

# Produce bar charts for each variable, by period and overall
for (i in vars) {
  
  # data for this plot
  df_i <- df_fig
  
  # variable
  df_i$var_i <- df_i[, i]
  
  # eliminate missing
  df_i <- subset(df_i, var_i != "missing")
  
  # labels with sample size
  df_i$n <- 1
  labs <- aggregate(n~var_i + period, data = df_i, FUN = sum)

  # eliminate from the graph groups with n < 20
  for (j in 1:nrow(labs)) {
    if (labs[j, "n"] < 20) {
      x <- which(df_i$var_i == labs[j, "var_i"] & 
          df_i$period == labs[j, "period"])
      df_i <- df_i[-x, ]
    }
  }
  
  # plot
  pl <- ggplot(data = df_i, aes(x = period, fill = bmi_category_fig)) +
    geom_bar(position = "fill") +
    scale_y_continuous("percent in each BMI category", labels = percent,
      expand = expansion(add = c(0, 0.05))) +
    scale_x_discrete("") +
    lshtm_theme() +
    scale_fill_manual("", values = lshtm_palette$bmi_categories) +
    geom_hline(yintercept = 0, colour = "grey70", linetype = "21") +
    facet_grid(.~var_i) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 30,
      hjust = 1, vjust = 1), plot.margin = margin(20,10,20,10))
  
  # assign plot name
  assign(paste0("pl_", i), pl)
}

# Produce combination plot
x <- grep("pl_", ls(), value = T)
x <- x[x!= "pl_combi"]
pl_combi <- ggarrange(plotlist = sapply(x, get), labels = names(sort(vars)), 
  ncol = 2, nrow = 3, common.legend = T, align = "v", legend = "bottom",
  hjust = 0, vjust = 1)
pl_combi <- annotate_figure(pl_combi, 
  bottom = text_grob("note: groups with < 20 observations are omitted", 
    hjust = 0, x = unit(5.5, "pt"), y = unit(60, "pt")))
ggsave(paste0(dir_path, "report/bmi_cat_change.png"), dpi = "print", 
  height = 25, width = 30, units = "cm")
ggsave(paste0(dir_path, "report/bmi_cat_change.pdf"), dpi = "print", 
  height = 25, width = 30, units = "cm")
rm(list = ls(pattern = "^pl"))


# -----------------------------------------------------------------------------
# Visualise BMI category share pre-war and at first observation - short version
# -----------------------------------------------------------------------------

# Data for this plot
df_fig <- df_tab

# Convert dataset so that the pre-war, two periods (month_entry) 
    # and all periods combined are stacked on top of each other
df_fig$period <- df_fig$month_entry
x <- df_fig
x$period <- "Overall"
df_fig <- rbind(df_fig, x)
x <- df_fig
x$period <- "Pre-war"
df_fig <- rbind(df_fig, x)
df_fig$period <- factor(df_fig$period, 
  levels = c("Pre-war", "Aug-Sep 2025", "Oct-Nov 2025", "Overall"))

# Select BMI category for different periods
df_fig$bmi_category_fig <- ifelse(df_fig$period == "Pre-war", 
  df_fig$bmi_category_prewar, df_fig$bmi_category_entry)
df_fig$bmi_category_fig <- tolower(df_fig$bmi_category_fig)
df_fig$bmi_category_fig <- factor(df_fig$bmi_category_fig, 
  levels = c("obese", "overweight", "normal", "underweight"),
  labels = c("obese (>= 30.0)", "overweight (25.0 to 29.9)", 
    "normal (18.5 to 24.9)", "underweight (< 18.5)"))
table(df_fig$bmi_category_fig, useNA = "always")

# Remove two missing BMI values
df_fig <- df_fig[complete.cases(df_fig$bmi_category_fig), ]

# Variables of interest
vars <- c("sex", "governorate")
names(vars) <- c("sex", "governorate of residence")

# Produce bar charts for each variable, by period and overall
for (i in vars) {
  
  # data for this plot
  df_i <- df_fig
  
  # variable
  df_i$var_i <- df_i[, i]
  
  # eliminate missing
  df_i <- subset(df_i, var_i != "missing")
  
  # labels with sample size
  df_i$n <- 1
  labs <- aggregate(n~var_i + period, data = df_i, FUN = sum)

  # eliminate from the graph groups with n < 20
  for (j in 1:nrow(labs)) {
    if (labs[j, "n"] < 20) {
      x <- which(df_i$var_i == labs[j, "var_i"] & 
          df_i$period == labs[j, "period"])
      df_i <- df_i[-x, ]
    }
  }
  
  # plot
  pl <- ggplot(data = df_i, aes(x = period, fill = bmi_category_fig)) +
    geom_bar(position = "fill") +
    scale_y_continuous("percent in each BMI category", labels = percent,
      expand = expansion(add = c(0, 0.05))) +
    scale_x_discrete("") +
    lshtm_theme() +
    scale_fill_manual("", values = lshtm_palette$bmi_categories) +
    geom_hline(yintercept = 0, colour = "grey70", linetype = "21") +
    facet_grid(.~var_i) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 30,
      hjust = 1, vjust = 1), plot.margin = margin(20,10,20,10))
  
  # assign plot name
  assign(paste0("pl_", i), pl)
}

# Produce combination plot
x <- grep("pl_", ls(), value = T)
x <- x[x!= "pl_combi"]
pl_combi <- ggarrange(plotlist = sapply(x, get), labels = names(sort(vars)), 
  ncol = 1, nrow = 2, common.legend = T, align = "h", legend = "bottom",
  hjust = 0, vjust = 1)
pl_combi <- annotate_figure(pl_combi, 
  bottom = text_grob("note: groups with < 20 observations are omitted", 
    hjust = 0, x = unit(5.5, "pt"), y = unit(60, "pt")))
ggsave(paste0(dir_path, "report/bmi_cat_change_short.png"), dpi = "print", 
  height = 25, width = 20, units = "cm")
ggsave(paste0(dir_path, "report/bmi_cat_change_short.pdf"), dpi = "print", 
  height = 25, width = 20, units = "cm")
rm(list = ls(pattern = "^pl"))


# -----------------------------------------------------------------------------
# Visualise participation over time
# -----------------------------------------------------------------------------

# Dataset for this figure
df_fig <- df_main

# Range of first observation
df_fig$date_entry <- as.Date(df_fig$date_entry)
range(df_fig$date_entry)

# Range of any observation
df_fig$record_date <- as.Date(df_fig$record_date)
range(df_fig$record_date)

# Code observations into new and repeat
df_fig$record_number_cat <- df_fig$record_number
df_fig[which(df_fig$record_number_cat >= 3), "record_number_cat"] <- 
  "third or more"
df_fig[which(df_fig$record_number_cat == 1), "record_number_cat"] <- "first"
df_fig[which(df_fig$record_number_cat == 2), "record_number_cat"] <- "second"
df_fig$record_number_cat <- as.character(df_fig$record_number_cat)
df_fig$record_number_cat <- factor(df_fig$record_number_cat, 
  levels = c("first", "second", "third or more"))
table(df_fig$record_number_cat, useNA = "always")

# Histogram of record dates, by record number category
pl <- ggplot(df_fig, aes(x = record_date, group = record_number_cat,
  fill = record_number_cat)) +
  geom_histogram(stat = "count", colour = "black") +
  scale_x_date("observation date (2025)", breaks = "week", 
    date_labels = "%d %b") +
  scale_y_continuous("number of observations", expand = expansion(add=c(0,5))) +
  scale_fill_manual("observation number", 
    values = lshtm_palette$record_number_cat) +
  lshtm_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(paste0(dir_path, "report/date_histogram.png"), dpi = "print", 
  height = 12, width = 25, units = "cm")
ggsave(paste0(dir_path, "report/date_histogram.pdf"), dpi = "print", 
  height = 12, width = 25, units = "cm")


# # -----------------------------------------------------------------------------
# # Analyse influence of different features on percent BMI change at 1st obs.
# # -----------------------------------------------------------------------------
# 
# # Data for this analysis
# df_gbm <- df_tab
# 
# # Create a numeric relative date variable
# df_gbm$date_entry <- as.Date(df_gbm$date_entry)
# df_gbm$day_entry <- df_gbm$date_entry - min(df_gbm$date_entry)
# df_gbm$day_entry <- as.numeric(df_gbm$day_entry)
# 
# # Compute relative BMI change
# df_gbm$bmi_change <- (df_gbm$bmi_entry - df_gbm$bmi_prewar) / df_gbm$bmi_entry
# 
# # Select features
# vars <- c("bmi_prewar", "day_entry", "sex", "age_years", 
#   "governorate", "role_cat", "children")
# 
# # Reduce dataset to complete observations
# df_gbm <- df_gbm[, c("bmi_change", vars)]
# for (i in vars) {df_gbm <- df_gbm[which(df_gbm[, i] != "missing"), ]}
# for (i in c("sex", "governorate", "role_cat", "children")) {
#   df_gbm[, i] <- as.numeric(df_gbm[, i])
# }
# df_gbm <- na.omit(df_gbm)
# 
# # Identify optimal combination of shrinkage parameter and number of trees
# fm <- as.formula(paste0("bmi_change ~", paste(vars, collapse = "+")))
# caretGrid <- expand.grid(interaction.depth = c(1, 3, 5), n.trees = (0:50)*50,
#   shrinkage = c(0.1, 0.05, 0.01, 0.005, 0.001), n.minobsinnode = c(5, 10, 20))
# x <- caret::train(fm, data = df_gbm, distribution = "gaussian", method = "gbm",
#   metric = "RMSE", trControl = trainControl(method = "cv", number = 5),
#   tuneGrid = caretGrid, verbose = F)
# print(x)
# 
# # Fit gradient-boosted model
# m_try <- gbm(fm, data = df_gbm, n.trees = 500, interaction.depth = 1,
#   shrinkage = 0.01, n.minobsinnode = 20)
# summary(m_try)
# 
# # Visualise predictions vs observations
# df_gbm$predicted <- predict(m_try)
# x <- range(c(df_gbm$predicted, df_gbm$bmi_change))
# ggplot(df_gbm, aes(x = bmi_change, y = predicted)) +
#   geom_point(fill = lshtm_palette$lshtm_generic, alpha = 0.75) +
#   scale_x_continuous("observed BMI change", labels = percent, limits = x) +
#   scale_y_continuous("predicted BMI change", labels = percent, limits = x) +
#   lshtm_theme() +
#   geom_abline(slope = 1)
# 
# # Collect influence statistics
# x <- summary(m_try)
# colnames(x) <- c("pred", "importance")
# x$pred <- gsub("_num", "", x$pred)
# x$importance <- x$importance/max(x$importance)
# x <- x[order(-x$importance), ]
# 
# 

# -----------------------------------------------------------------------------
# Model BMI change over time among people with >= 2 observations
# -----------------------------------------------------------------------------

# Dataset for this figure
df_gam <- df_main

# Restrict data to children with >= 2 observations
x <- subset(df_gam, ! record_number %in% 1)
x <- unique(x$id)
df_gam <- df_gam[which(df_gam$id %in% x), ]

# Create time variable
df_gam$record_date <- as.Date(df_gam$record_date)
df_gam$day <- as.integer(df_gam$record_date)

# Create baseline BMI normalised score
df_gam$bmi_entry_sc <- df_gam$bmi_entry / max(df_gam$bmi_entry) 

# Check BMI distribution and transform if needed
ggplot(df_gam, aes(x = bmi)) +
  geom_density()
df_gam$bmi_log <- log(df_gam$bmi)

# Categorise age
df_gam$age_cat <- "missing"
df_gam[which(df_gam$age_years < 30), "age_cat"] <- "< 30y"
df_gam[which(df_gam$age_years %in% 30:44), "age_cat"] <- "30 to 44y"
df_gam[which(df_gam$age_years >= 45), "age_cat"] <- ">= 45y"
df_gam$age_cat <- factor(df_gam$age_cat, 
  levels = c("< 30y", "30 to 44y", ">= 45y"))

# Combine governorates
df_gam$governorate_cat <- df_gam$governorate
df_gam[which(df_gam$governorate_cat %in% c("Gaza City", "North Gaza")), 
  "governorate_cat"] <- "Gaza City, North Gaza"
df_gam[which(df_gam$governorate_cat %in% c("Khan Yunis", "Rafah")), 
  "governorate_cat"] <- "Khan Yunis, Rafah"
table(df_gam$governorate_cat, useNA = "always")
df_gam$governorate_cat <- factor(df_gam$governorate_cat, 
  levels = c("Gaza City, North Gaza", "Khan Yunis, Rafah", "Deir Al Balah"))

# Factorise other variables
df_gam$sex <- factor(df_gam$sex, levels = c("Female", "Male"))
df_gam$children <- factor(df_gam$children, levels = c("0", "1", "2", "3+"))

# Compute daily change between sequential observations
df_gam <- df_gam[order(df_gam$id, df_gam$record_number), ]
df_gam$id <- factor(df_gam$id)
x <- by(df_gam[, c("id", "bmi", "record_date", "record_number")], df_gam$id, 
  function(xx) {data.frame(
    id = xx[, "id"],
    record_number = xx[, "record_number"],
    bmi_diff = c(NA, diff(xx[, "bmi"])), 
    date_diff = c(NA, diff(xx[, "record_date"]))
  )  
  }
)
df_diff <- do.call(rbind, x)
df_diff$bmi_daily_change <- df_diff$bmi_diff / df_diff$date_diff

# Plot absolute daily change
pl <- ggplot(df_diff, aes(x = abs(bmi_daily_change))) +
  geom_histogram(fill = lshtm_palette$lshtm_generic, alpha = 0.75, 
    colour = "black") +
  scale_x_continuous("absolute daily change in BMI") +
  scale_y_continuous("number of sequential observations") +
  lshtm_theme()
ggsave(paste0(dir_path, "report/bmi_daily_change.png"), dpi = "print", 
  height = 10, width = 15, units = "cm")
ggsave(paste0(dir_path, "report/bmi_daily_change.pdf"), dpi = "print", 
  height = 10, width = 15, units = "cm")


# Eliminate participants with an implausible daily change between observations
    # adopt an absolute cutoff of 1.0
x <- subset(df_diff, abs(bmi_daily_change) >= 1)
x <- unique(x$id)
df_gam <- subset(df_gam, ! id %in% x)

# Fit generalised additive growth model, just to check that it fits well
m_try <- mgcv::gam(bmi_log ~ s(day, bs = "bs") + s(bmi_prewar, bs = "bs") + 
  s(id, bs = "re"), data = df_gam, family = "gaussian")
summary(m_try)
mgcv::gam.check(m_try)

# Predict values
pred_frame <- unique(df_gam[, c("bmi_prewar", "age_years", "sex", 
  "governorate_cat", "children", "id", "age_cat")])
pred_frame <- merge(data.frame(day = min(df_gam$day):max(df_gam$day)),
  pred_frame)

x <- predict(m_try, se.fit = T, newdata = pred_frame)
pred_frame$pred <- exp(x[[1]])
pred_frame$pred_lci <- exp(x[[1]] - 1.96*x[[2]])
pred_frame$pred_uci <- exp(x[[1]] + 1.96*x[[2]])
pred_frame$date <- as.Date(pred_frame$day)

# Compute median and 80% centiles of predictions
centiles <- aggregate(pred ~ date, data = pred_frame, 
  FUN = function(xx) {quantile(xx, c(0.50, 0.10, 0.90))})
centiles[, 2:4] <- unlist(centiles[, 2])
colnames(centiles) <- c("date", "median", "centile_10", "centile_90")

# Plot data and model predictions
pl <- ggplot() +
  geom_line(data = df_gam, aes(x = record_date, y = bmi, group = id),
    linetype = "11", colour = "grey30") +
  geom_point(data = df_gam, aes(x = record_date, y = bmi, group = id), 
    shape = 22, colour = "grey30") +
  lshtm_theme() +
  scale_x_date("date (2025)", breaks = "week", date_labels = "%d %b") +
  scale_y_continuous("Body Mass Index") +
  geom_line(data = centiles, aes(x = date, y = median), linewidth = 1,
    colour = lshtm_palette$lshtm_generic, linetype = "31") +
  geom_ribbon(data = centiles, aes(x = date, ymin = centile_10, 
    ymax = centile_90), alpha = 0.20, outline.type = "both",
    fill = lshtm_palette$lshtm_generic, colour = lshtm_palette$lshtm_generic) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
ggsave(paste0(dir_path, "report/bmi_evolution_overall.png"), dpi = "print", 
  height = 15, width = 20, units = "cm")
ggsave(paste0(dir_path, "report/bmi_evolution_overall.pdf"), dpi = "print", 
  height = 15, width = 20, units = "cm")

# Model and plot by variable of interest
vars <- c("age_cat", "sex", "governorate_cat", "children")
vars_labs <- c("age", "sex", "governorate", "number of children dependents")
for (i in vars) {

  # assign variable
  df_gam$var_i <- df_gam[, i]
  pred_frame$var_i <- pred_frame[, i]

  # fit growth model and predict for each stratum/category
  pred_frame <- pred_frame[order(pred_frame$var_i), ]
  for (j in levels(df_gam$var_i)) {
    m_j <- mgcv::gam(bmi_log ~ s(day, bs = "tp") + s(bmi_prewar, bs = "tp") + 
      s(id, bs = "re"), data = subset(df_gam, var_i == j), family = "gaussian")
    pred_frame[which(pred_frame$var_i == j), "pred"] <- exp(predict(m_j, 
      newdata = pred_frame[which(pred_frame$var_i == j), ]))
  }
  
  # compute median and 80% centiles of predictions over data span
  centiles_i <- aggregate(pred ~ var_i + date, data = pred_frame, 
    FUN = function(xx) {quantile(xx, c(0.50, 0.10, 0.90))})
  centiles_i[, 3:5] <- unlist(centiles_i[, 3])
  colnames(centiles_i) <- c("var_i", "date", "median", "centile_10", 
    "centile_90")
  for (j in levels(df_gam$var_i)) {
    x <- df_gam[which(df_gam$var_i == j), "record_date"]
    centiles_i[which(centiles_i$var_i == j & (centiles_i$date < min(x) |
        centiles_i$date > max(x))), c("median", "centile_10", "centile_90")] <- 
      NA
  }  
  
  # plot data and model predictions
  pl_i <- ggplot() +
    geom_line(data = df_gam, aes(x = record_date, y = bmi, group = id, 
      colour = var_i), linetype = "11") +
    geom_point(data = df_gam, aes(x = record_date, y = bmi, group = id,
      colour = var_i), shape = 22) +
    lshtm_theme() +
    scale_x_date("date (2025)", breaks = "month", date_labels = "%b") +
    scale_y_continuous("Body Mass Index") +
    geom_line(data = centiles_i, aes(x = date, y = median, colour = var_i), 
      linewidth = 1, linetype = "31") +
    geom_ribbon(data = centiles_i, aes(x = date, ymin = centile_10, 
      ymax = centile_90, colour = var_i, fill = var_i), alpha = 0.20, 
      outline.type = "both") +
    scale_colour_manual(i, values = other_palette) +
    scale_fill_manual(i, values = other_palette) +
    facet_wrap(.~var_i, nrow = 1) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
      legend.position = "none", plot.margin = margin(20,10,20,10))
  
  # assign plot name
  assign(paste0("pl_", i), pl_i)
}

# Produce combination plot
pl_list <- sapply(paste0("pl_", vars), get)
pl_combi <- ggarrange(plotlist = pl_list, ncol = 2, nrow = 2, 
  labels = vars_labs, hjust = 0)
ggsave(paste0(dir_path, "report/bmi_evolution_by_factor.png"), dpi = "print", 
  height = 20, width = 30, units = "cm")
ggsave(paste0(dir_path, "report/bmi_evolution_by_factor.pdf"), dpi = "print", 
  height = 20, width = 30, units = "cm")


# -----------------------------------------------------------------------------
# Assess selection bias: compare participants with 1 versus >= 2 observations
# -----------------------------------------------------------------------------

# Prepare output table
vars <- c("n_obs", "age_cat", "sex", "bmi_prewar", "bmi_entry")
names(vars) <- c("number of observations", "age", "sex", "BMI pre-war", 
  "BMI at first observation")
tab2 <- matrix(NA, nrow = 12, ncol = 4)
tab2 <- as.data.frame(tab2)
colnames(tab2) <- c("Variable", "Participants with a single observation", 
  "Participants with >= 2 observations", "p-value")
tab2$Variable <- c("number of observations", "age", 
  paste0("  ", levels(df_tab$age_cat)), "sex", paste0("  ", levels(df_tab$sex)),
  "mean BMI pre-war", "mean BMI at first observation")

# Classify participants in terms of whether they have >= 2 observations
df_tab$type <- ifelse(df_tab$n_obs_cat == "1", "single observation",
  "multiple observations")
df_tab$type <- factor(df_tab$type, levels = c("single observation",
  "multiple observations"))
table(df_tab$type, useNA = "always")

# Fill in table
tab2[1, 2:3] <- table(df_tab$type)
tab2[3:6, 2:3] <- table(df_tab$age_cat, df_tab$type)
x <- stats::chisq.test(df_tab$age_cat, df_tab$type)
tab2[2, 4] <- x$p.value
tab2[8:10, 2:3] <- table(df_tab$sex, df_tab$type)
x <- stats::chisq.test(df_tab$sex, df_tab$type)
tab2[7, 4] <- x$p.value
tab2[11, 2:3] <- as.vector(by(df_tab$bmi_prewar, df_tab$type, 
  function(xx) {mean(xx, na.rm = T)}))
x <- stats::t.test(
  df_tab[which(df_tab$type == "single observation"), "bmi_prewar"],
  df_tab[which(df_tab$type == "multiple observations"), "bmi_prewar"]
)
tab2[11, 4] <- x$p.value
tab2[12, 2:3] <- as.vector(by(df_tab$bmi_entry, df_tab$type, 
  function(xx) {mean(xx, na.rm = T)}))
x <- stats::t.test(
  df_tab[which(df_tab$type == "single observation"), "bmi_entry"],
  df_tab[which(df_tab$type == "multiple observations"), "bmi_entry"]
)
tab2[12, 4] <- x$p.value

# Format and save table
tab2[11:12, 2:3] <- apply(tab2[11:12, 2:3], c(1,2), round, 1)
tab2[1:10, 2:3] <- formatC(apply(tab2[1:10, 2:3], c(1,2), as.integer), 
  digits = 0)
tab2[, 4] <- formatC(round(tab2[, 4], 3), 3)
write.csv(tab2, paste0(dir_path, "report/tab2.csv"), row.names = F)
x <- flextable(tab2)
flextable::save_as_docx(x, path = paste0(dir_path, "report/tab2.docx"))

  
# -----------------------------------------------------------------------------
# Assess selection bias: compare participants with Hamad et al. 2020 survey
# -----------------------------------------------------------------------------

# Read aggregate dataset from 2020 Hamad et al. survey
    # (https://doi.org/10.1038/s41371-022-00783-w)
hamad <- read.csv(paste0(dir_path, "data/raw/gaza_survey2020_kcal_bmi_agg.csv"))

# Estimate BMI for age groups 40-49 and 50-59 in Hamad survey, by sex
hamad$bmi <- hamad$weight / hamad$height^2

# Estimate BMI (pre-war) for the same groups in our sample
df_tab$age_cat2 <- NA
df_tab[which(df_tab$age_years >= 40 & df_tab$age_years < 50), "age_cat2"] <-
  "40 to 49y"
df_tab[which(df_tab$age_years >= 50 & df_tab$age_years < 60), "age_cat2"] <-
  "50 to 59y"
x <- aggregate(bmi_prewar ~ age_cat2 + sex, data = df_tab, FUN = mean)
x <- x[order(x$age_cat2), ]

# Comparison table
tab3 <- data.frame(
  age = c("40 to 49y", "40 to 49y", "50 to 59y", "50 to 59y"),
  sex = c("female", "male", "female", "male"),
  hamad = hamad[1:4, "bmi"],
  this_sample = x$bmi_prewar
)
for (i in c("hamad","this_sample")) {
  tab3[, i] <- round(tab3[, i], 1)
}
colnames(tab3) <- c("age", "sex", "mean BMI (Hamad et al., 2022)",
  "mean pre-war BMI (this study)")
write.csv(tab3, paste0(dir_path, "report/tab3.csv"), row.names = F)
x <- flextable(tab3)
flextable::save_as_docx(x, path = paste0(dir_path, "report/tab3.docx"))
  
  
  