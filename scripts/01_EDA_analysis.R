# ==============================================================================
# UK Air Quality Analysis - Research Question 1 (EDA)
# How did daily concentrations of PM₂.₅, PM₁₀, NO₂, and O₃ vary across 
# major UK cities during and after COVID-19 pandemic (2020-2024)?
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP: Load Required Packages
# ------------------------------------------------------------------------------

#install.packages(c("tidyverse", "lubridate", "ggplot2", "patchwork"))

library(tidyverse)    # Data manipulation and visualization
library(lubridate)    # Date handling
library(ggplot2)      # Plotting (also loaded via tidyverse, but explicit here)
library(patchwork)    # Combining multiple plots

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))

# Create processed_data directory if it doesn't exist
dir.create("processed_data", showWarnings = FALSE)

# Option to save plots to files (set to TRUE to speed up execution)
save_plots_to_file <- TRUE  # Change to FALSE to display in RStudio

# Create output directory for plots if saving
if (save_plots_to_file) {
  dir.create("plots", showWarnings = FALSE)
  cat("Plots will be saved to 'plots/' directory\n")
}

# ------------------------------------------------------------------------------
# 2. DATA IMPORT AND CLEANING
# ------------------------------------------------------------------------------

# NOTE: Skip first 10 rows (3 header + 7 metadata rows) to get to column headers
raw_data <- read_csv("uk_air_quality_data.csv", skip = 10, show_col_types = FALSE)

# View structure
glimpse(raw_data)

# Clean and reshape the data
##  Rename first column to Date and convert it into a proper date format
air_quality <- raw_data %>%
  rename(Date = 1) %>%  
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  # Select only pollutant value columns (exclude status/flag columns)
  select(
    Date,
    # Edinburgh
    Edin_O3   = 2, Edin_NO2  = 4, Edin_SO2  = 6, Edin_CO   = 8, 
    Edin_PM10 = 10, Edin_PM25 = 12,
    
    # London
    Lon_O3   = 14, Lon_NO2  = 16, Lon_SO2  = 18, Lon_CO   = 20,
    Lon_PM10 = 22, Lon_PM25 = 24,
    
    # Manchester
    Man_O3   = 26, Man_NO2  = 28, Man_SO2  = 30, 
    Man_PM10 = 32, Man_PM25 = 34,
    
    # Sheffield
    Shef_O3  = 36, Shef_NO2 = 38, 
    Shef_PM10 = 40, Shef_PM25 = 42
  ) %>%
  # Convert all pollutant columns to numeric (handles "No data" as NA)
  mutate(across(-Date, ~as.numeric(as.character(.)))) %>%
  # Filter out rows with invalid dates
  filter(!is.na(Date))

glimpse(air_quality)
colSums(is.na(air_quality))


# Create long format for easier analysis
air_quality_long <- air_quality %>%
  pivot_longer(
    cols = -Date,
    names_to = c("City", "Pollutant"),
    names_sep = "_",
    values_to = "Concentration"
  ) %>%
  # Clean city names
  mutate(
    City = case_when(
      City == "Edin" ~ "Edinburgh",
      City == "Lon" ~ "London",
      City == "Man" ~ "Manchester",
      City == "Shef" ~ "Sheffield"
    ),
    # Clean pollutant names
    Pollutant = case_when(
      Pollutant == "PM25" ~ "PM2.5",
      Pollutant == "PM10" ~ "PM10",
      Pollutant == "NO2" ~ "NO2",
      Pollutant == "O3" ~ "O3",
      Pollutant == "SO2" ~ "SO2",
      Pollutant == "CO" ~ "CO"
    )
  ) %>%
  # Add temporal variables
  mutate(
    Year = year(Date),
    Month = month(Date),
    YearMonth = floor_date(Date, "month"),
    # Define pandemic period: 2020-2021 (Pandemic) vs 2022-2024 (Post-pandemic)
    Period = case_when(
      Year >= 2020 & Year <= 2021 ~ "Pandemic",
      Year >= 2022 ~ "Post-Pandemic"
    ),
    Period = factor(Period, levels = c("Pandemic", "Post-Pandemic"))
  )



# Summary statistics (BEFORE removing NAs)
cat("\n=== DATA SUMMARY ===\n")
cat("Date range:", min(air_quality_long$Date, na.rm = TRUE), "to", max(air_quality_long$Date, na.rm = TRUE), "\n")
cat("Total rows in long format:", nrow(air_quality_long), "\n")
cat("Cities:", paste(unique(air_quality_long$City), collapse = ", "), "\n")
cat("Pollutants:", paste(unique(air_quality_long$Pollutant), collapse = ", "), "\n\n")
colSums(is.na(air_quality_long))

# Check data completeness by city and pollutant (BEFORE removing NAs)
cat("=== DATA COMPLETENESS CHECK ===\n")
data_completeness <- air_quality_long %>%
  group_by(City, Pollutant) %>%
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(Concentration)),
    pct_complete = round((1 - n_missing/n_obs) * 100, 2),
    .groups = "drop"
  )
print(data_completeness)

# Now remove rows with missing concentration values for analysis
air_quality_long <- air_quality_long %>%
  filter(!is.na(Concentration))

cat("\nRows after removing NAs:", nrow(air_quality_long), "\n")

# ------------------------------------------------------------------------------
# STEP 1.1: DAILY TIME-SERIES PLOTS
# ------------------------------------------------------------------------------

cat("\n=== STEP 1.1: Daily Time-Series Plots ===\n")

# Focus on key pollutants for RQ1
key_pollutants <- c("PM2.5", "PM10", "NO2", "O3")
length(key_pollutants)

# Check row counts for each pollutant
total_rows <- 0

# Total Number of rows
for (poll in key_pollutants) {
  n_rows <- nrow(air_quality_long %>% filter(Pollutant == poll))
  cat(poll, ":", n_rows, "rows\n")
}
cat("----------------------------\n")
cat("Total :", nrow(air_quality_long %>% filter(Pollutant %in% key_pollutants)), "rows\n")

# Create daily time series for each pollutant
for (poll in key_pollutants) {
  cat("\nGenerating plot for", poll, "...\n")
  
  p <- air_quality_long %>%
    filter(Pollutant == poll) %>%
    ggplot(aes(x = Date, y = Concentration, color = City)) +
    geom_line(alpha = 0.6, linewidth = 0.5) +  
    geom_vline(xintercept = as.Date("2022-01-01"), 
               linetype = "dashed", color = "blue", linewidth = 1) +
    facet_wrap(~City, ncol = 1, scales = "free_y") +
    labs(
      title = paste("Daily", poll, "Concentrations Across UK Cities (2020-2024)"),
      subtitle = "Blue line: Post-pandemic period begins (2022)",
      x = "Date",
      y = paste(poll, "Concentration (μg/m³)"),
      color = "City"
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold")
    )
  
  if (save_plots_to_file) {
    # Save to file (much faster than displaying)
    ggsave(
      filename = paste0("plots/daily_timeseries_", poll, ".png"),
      plot = p,
      width = 12,
      height = 10,
      dpi = 300
    )
    cat("  Saved: plots/daily_timeseries_", poll, ".png\n", sep = "")
  } else {
    # Display in RStudio (slower)
    print(p)
    cat("  Plot for", poll, "displayed.\n")
  }
}

# ------------------------------------------------------------------------------
# STEP 1.2: MONTHLY TRENDS (CLEANER VIEW)
# ------------------------------------------------------------------------------

cat("\n=== STEP 1.2: Monthly Trend Analysis ===\n")


# Calculate monthly averages
monthly_data <- air_quality_long %>%
  filter(Pollutant %in% key_pollutants) %>%
  group_by(City, Pollutant, YearMonth) %>%
  summarise(
    Monthly_Mean = mean(Concentration, na.rm = TRUE),
    Monthly_SD = sd(Concentration, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

# Plot monthly trends for each pollutant
for (poll in key_pollutants) {
  cat("\nGenerating monthly plot for", poll, "...\n")
  
  p <- monthly_data %>%
    filter(Pollutant == poll) %>%
    ggplot(aes(x = YearMonth, y = Monthly_Mean, color = City, group = City)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, alpha = 0.6) +
    geom_vline(xintercept = as.Date("2022-01-01"), 
               linetype = "dashed", color = "blue", linewidth = 1) +
    labs(
      title = paste("Monthly Average", poll, "Concentrations"),
      subtitle = "Blue line: Post-pandemic period begins (2022)",
      x = "Month",
      y = paste("Monthly Mean", poll, "(μg/m³)"),
      color = "City"
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    scale_color_brewer(palette = "Set1") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Display in RStudio 
  print(p)
  
  # Also save to file
  if (save_plots_to_file) {
    ggsave(
      filename = paste0("plots/monthly_trend_", poll, ".png"),
      plot = p,
      width = 12,
      height = 6,
      dpi = 300
    )
    cat("  Saved: plots/monthly_trend_", poll, ".png\n", sep = "")
  }
  
  cat("  Monthly plot for", poll, "completed.\n")
}

# ------------------------------------------------------------------------------
# STEP 1.3: PANDEMIC VS POST-PANDEMIC COMPARISON
# ------------------------------------------------------------------------------

cat("\n=== STEP 1.3: Pandemic vs Post-Pandemic Comparison ===\n")

# Filter for Pandemic (2020-2021) and Post-Pandemic (2022-2024) periods
comparison_data <- air_quality_long %>%
  filter(
    Pollutant %in% key_pollutants,
    !is.na(Period)  # This will include both periods
  )

print(nrow(comparison_data))

# Statistical summary
period_summary <- comparison_data %>%
  group_by(City, Pollutant, Period) %>%
  summarise(
    Mean = mean(Concentration, na.rm = TRUE),
    Median = median(Concentration, na.rm = TRUE),
    SD = sd(Concentration, na.rm = TRUE),
    Q25 = quantile(Concentration, 0.25, na.rm = TRUE),
    Q75 = quantile(Concentration, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(period_summary)
write_csv(period_summary, "processed_data/period_summary.csv")


# Boxplots comparing periods
# Boxplots are reasonably fast, so we display AND save them
for (poll in key_pollutants) {
  cat("\nGenerating boxplot for", poll, "...\n")
  
  p <- comparison_data %>%
    filter(Pollutant == poll) %>%
    ggplot(aes(x = City, y = Concentration, fill = Period)) +
    geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +  # Smaller outliers for speed
    labs(
      title = paste(poll, "Concentrations: Pandemic vs Post-Pandemic"),
      x = "City",
      y = paste(poll, "Concentration (μg/m³)"),
      fill = "Period"
    ) +
    scale_fill_manual(values = c("Pandemic" = "#E69F00", "Post-Pandemic" = "#56B4E9")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Display in RStudio
  print(p)
  
  # Also save to file
  if (save_plots_to_file) {
    ggsave(
      filename = paste0("plots/boxplot_comparison_", poll, ".png"),
      plot = p,
      width = 10,
      height = 6,
      dpi = 300
    )
    cat("  Saved: plots/boxplot_comparison_", poll, ".png\n", sep = "")
  }
  
  cat("  Boxplot for", poll, "completed.\n")
}

# Perform t-tests for each city and pollutant
cat("\n=== Statistical Tests (Pandemic vs Post-Pandemic) ===\n")
test_results <- comparison_data %>%
  group_by(City, Pollutant) %>%
  summarise(
    t_test = list(t.test(
      Concentration[Period == "Pandemic"],
      Concentration[Period == "Post-Pandemic"]
    )),
    .groups = "drop"
  ) %>%
  mutate(
    p_value = map_dbl(t_test, ~.$p.value),
    mean_pandemic = map_dbl(t_test, ~.$estimate[1]),
    mean_post = map_dbl(t_test, ~.$estimate[2]),
    difference = mean_post - mean_pandemic,
    significant = ifelse(p_value < 0.05, "Yes", "No")
  ) %>%
  select(-t_test)

print(test_results)
write_csv(test_results, "processed_data/test_results.csv")

# ------------------------------------------------------------------------------
# STEP 1.4: POLLUTANT RELATIONSHIPS (FOR MODELLING)
# ------------------------------------------------------------------------------

cat("\n=== STEP 1.4: Pollutant Relationships ===\n")

# Create wide format for correlation analysis
pollutant_wide <- air_quality_long %>%
  filter(Pollutant %in% key_pollutants) %>%
  select(Date, City, Pollutant, Concentration, Period) %>%
  pivot_wider(
    names_from = Pollutant,
    values_from = Concentration
  )
colSums(is.na(pollutant_wide))

cat("\n--- Correlation Matrices by City and Period in console ---\n")
for (city in unique(pollutant_wide$City)) {
  for (period in c("Pandemic", "Post-Pandemic")) {

    city_period_data <- pollutant_wide %>%
      filter(City == city, Period == period) %>%
      select(all_of(key_pollutants)) %>%
      drop_na()

    if (nrow(city_period_data) > 30) {
      cat("\n", city, "-", period, "(n =", nrow(city_period_data), "):\n")
      cor_matrix <- cor(city_period_data, use = "complete.obs")
      print(round(cor_matrix, 3))
    } else {
      cat("\n", city, "-", period, ": skipped (n =", nrow(city_period_data), ")\n")
    }
  }
}


# Scatterplot: PM2.5 vs NO2
cat("\nGenerating scatterplot: PM2.5 vs NO2...\n")
p1 <- pollutant_wide %>%
  ggplot(aes(x = NO2, y = PM2.5, color = City)) +
  geom_point(alpha = 0.3, size = 0.8, na.rm = TRUE) +  # Smaller points for speed
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, na.rm = TRUE) +
  facet_wrap(~City, scales = "free") +
  labs(
    title = "PM2.5 vs NO2 Relationship",
    x = "NO2 Concentration (μg/m³)",
    y = "PM2.5 Concentration (μg/m³)"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

print(p1)

# Scatterplot: PM2.5 vs PM10
cat("Generating scatterplot: PM2.5 vs PM10...\n")
p2 <- pollutant_wide %>%
  ggplot(aes(x = PM10, y = PM2.5, color = City)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~City, scales = "free") +
  labs(
    title = "PM2.5 vs PM10 Relationship",
    x = "PM10 Concentration (μg/m³)",
    y = "PM2.5 Concentration (μg/m³)"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

print(p2)

# Scatterplot: PM2.5 vs O3
cat("Generating scatterplot: PM2.5 vs O3...\n")
p3 <- pollutant_wide %>%
  ggplot(aes(x = O3, y = PM2.5, color = City)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~City, scales = "free") +
  labs(
    title = "PM2.5 vs O3 Relationship",
    x = "O3 Concentration (μg/m³)",
    y = "PM2.5 Concentration (μg/m³)"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

print(p3)

# Combined correlation plot
cat("Creating combined correlation plot...\n")
combined_plot <- (p1 / p2 / p3) +
  plot_annotation(
    title = "Pollutant Relationships for Predictive Modelling",
    subtitle = "Linear relationships with 95% confidence intervals"
  )
print(combined_plot)

# Save scatterplots to file
if (save_plots_to_file) {
  ggsave(
    filename = "plots/scatterplot_PM25_vs_NO2.png",
    plot = p1,
    width = 12,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = "plots/scatterplot_PM25_vs_PM10.png",
    plot = p2,
    width = 12,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = "plots/scatterplot_PM25_vs_O3.png",
    plot = p3,
    width = 12,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = "plots/scatterplot_combined.png",
    plot = combined_plot,
    width = 12,
    height = 14,
    dpi = 300
  )
  cat("Scatterplots saved to plots/ directory\n")
}

# ------------------------------------------------------------------------------
# SUMMARY STATISTICS TABLE
# ------------------------------------------------------------------------------

cat("\n=== OVERALL SUMMARY STATISTICS PER CITY AND POLLUTANT ===\n")

overall_summary <- air_quality_long %>%
  filter(Pollutant %in% key_pollutants) %>%
  group_by(City, Pollutant) %>%
  summarise(
    N_obs = n(),
    Mean = mean(Concentration, na.rm = TRUE),
    Median = median(Concentration, na.rm = TRUE),
    SD = sd(Concentration, na.rm = TRUE),
    Min = min(Concentration, na.rm = TRUE),
    Max = max(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

print(overall_summary)

# ------------------------------------------------------------------------------
# SAVE PROCESSED DATA FOR FUTURE ANALYSIS (RQ2 & RQ3)
# ------------------------------------------------------------------------------

# Save clean datasets
write_csv(air_quality_long, "processed_data/air_quality_clean_long.csv")
write_csv(pollutant_wide, "processed_data/air_quality_clean_wide.csv")
write_csv(monthly_data, "processed_data/air_quality_monthly.csv")

cat("\n=== Analysis Complete ===\n")
cat("Clean datasets saved:\n")
cat("  - processed_data/air_quality_clean_long.csv\n")
cat("  - processed_data/air_quality_clean_wide.csv\n")
cat("  - processed_data/air_quality_monthly.csv\n")