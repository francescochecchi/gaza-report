# =============================================================================
# Visualise data for paper
# =============================================================================


# -----------------------------------------------------------------------------
# Load packages and read dataset
# -----------------------------------------------------------------------------

set.seed(20260612)
pacman::p_load(flextable, ggpubr, ggplot2, here, scales, tidyverse, viridis)

# Colour-blind palette for graphing
palette_gen <- viridis(16)

# Read in processed data
data_processed <- read.csv(here::here("data", "processed", "participants.csv"))

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
      color="#01454f", fill="#01454f", size=1.5, linetype="solid"
    ),
    legend.position = "bottom",
    legend.title = element_text(colour = "#01454f", face = "bold"),
    legend.text = element_text(colour = "#01454f")
  )
}
theme_set(lshtm_theme())

lshtm_palette <- list(
  bmi_categories = c(
    "Obese" = "#01454f",
    "Overweight" = "#007457",
    "Normal" = "#709b28",
    "Underweight" = "#ffa600"),
  lshtm_generic = "#01454f"
)



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
write.csv(tab1, here("report", "tab1.csv"), row.names = F)
x <- flextable(tab1)
flextable::save_as_docx(x, path = here("report", "tab1.docx"))


# -----------------------------------------------------------------------------
# Visualise weight and BMI change from pre-war to first observation
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

# Produce boxplots for each variable, by period and overall
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
  labs$weight_change <- 0.28
  
  # eliminate from the graph groups with n < 5
  for (j in 1:nrow(labs)) {
    if (labs[j, "n"] < 5) {
      x <- which(df_i$var_i == labs[j, "var_i"] & 
          df_i$period == labs[j, "period"])
      df_i <- df_i[-x, ]
    }
  }
  # labs <- labs[which(labs$var_i %in% unique(df_i$var_i) &
  #     labs$period %in% unique(df_i$period)), ]
  
  # plot
  pl <- ggplot(data = df_i, aes(y = weight_change, x = period, 
    colour = period, fill = period)) +
    geom_violin(linewidth = 1.0, trim = F, quantiles = 0.50,
      alpha = 0.50, quantile.linetype = "11") +
    scale_y_continuous("percent weight change", labels = percent,
      limits = c(-0.52, 0.32), breaks = seq(-0.50, 0.30, 0.10)) +
    scale_x_discrete("period") +
    scale_colour_manual("period", values = c("#007457", "#709b28", "#01454f")) +
    scale_fill_manual("period", values = c("#007457", "#709b28", "#01454f")) +
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
ggsave(here("report", "weight_change.png"), dpi = "print", height = 25, 
  width = 30, units = "cm")
ggsave(here("report", "weight_change.pdf"), dpi = "print", height = 25, 
  width = 30, units = "cm")



# -----------------------------------------------------------------------------
# Visualise participation over time
# -----------------------------------------------------------------------------

# Range of first observation
range(df$date_entry)



