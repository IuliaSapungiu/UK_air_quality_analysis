# ==============================================================================
# UK Air Quality Analysis - Research Questions 2 & 3
# RQ2: Simple Linear Regression (NO₂ → PM₂.₅)
# RQ3: Multiple Linear Regression (NO₂ + PM₁₀ + O₃ → PM₂.₅)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP: Load Required Packages
# ------------------------------------------------------------------------------

install.packages("corrplot")

library(tidyverse)
library(corrplot)

set.seed(123)  # For reproducibility
# Plot control
SAVE_PLOTS <- TRUE  # Change to FALSE if you only want to see plots in RStudio


cat("==============================================================================\n")
cat("REGRESSION ANALYSIS: RQ2 & RQ3\n")
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------------------------

cat("Loading data...\n")
data <- read_csv("processed_data/air_quality_clean_wide.csv", show_col_types = FALSE)

# Prepare data
regression_data <- data %>%
  select(Date, City, PM2.5, NO2, PM10, O3) %>%
  drop_na(PM2.5, NO2, PM10, O3) %>%
  mutate(
    Year = year(Date),
    Period = case_when(
      Year >= 2020 & Year <= 2021 ~ "Pandemic",
      Year >= 2022 ~ "Post-Pandemic"
    ),
    Period = factor(Period, levels = c("Pandemic", "Post-Pandemic")),
    City = factor(City, levels = c("Edinburgh", "London", "Manchester", "Sheffield"))
  ) %>%
  filter(!is.na(Period))

cat("Total observations:", nrow(regression_data), "\n\n")

# ------------------------------------------------------------------------------
# 3. TRAIN/TEST SPLIT (80/20)
# ------------------------------------------------------------------------------

cat("Creating train/test split (80/20)...\n")

# Split by city to keep balanced
train_data <- regression_data %>%
  group_by(City) %>%
  slice_sample(prop = 0.8) %>%
  ungroup()

test_data <- regression_data %>%
  filter(!Date %in% train_data$Date)

levels(train_data$Period)

cat("Training:", nrow(train_data), "| Test:", nrow(test_data), "\n\n")

# ------------------------------------------------------------------------------
# 4. EXPLORE TRAINING DATA
# ------------------------------------------------------------------------------

cat("=== EXPLORATORY ANALYSIS (Training Data) ===\n\n")

# Correlation matrix
cor_train <- train_data %>%
  select(PM2.5, NO2, PM10, O3) %>%
  cor(use = "complete.obs")

cat("Correlation Matrix:\n")
print(round(cor_train, 3))
cat("\n")

# Save correlation matrix
write_csv(as.data.frame(cor_train) %>% 
            rownames_to_column("Variable"), 
          "processed_data/correlation_matrix.csv")

# Visualize correlation matrix
corrplot(cor_train, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black")
if (SAVE_PLOTS) {
  png("plots/correlation_matrix.png", width = 8, height = 8, units = "in", res = 300)
  corrplot(cor_train, method = "color", type = "upper", 
           addCoef.col = "black", tl.col = "black")
  dev.off()
}

# Scatterplots
p1 <- train_data %>%
  ggplot(aes(x = NO2, y = PM2.5)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~City) +
  labs(title = "PM2.5 vs NO2")
print(p1)
if (SAVE_PLOTS) ggsave("plots/PM25_vs_NO2.png", p1, width = 10, height = 6, dpi = 300)

p2 <- train_data %>%
  ggplot(aes(x = PM10, y = PM2.5)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~City) +
  labs(title = "PM2.5 vs PM10")
print(p2)
if (SAVE_PLOTS) ggsave("plots/PM25_vs_PM10.png", p2, width = 10, height = 6, dpi = 300)

p3 <- train_data %>%
  ggplot(aes(x = O3, y = PM2.5)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~City) +
  labs(title = "PM2.5 vs O3")
print(p3)
if (SAVE_PLOTS) ggsave("plots/PM25_vs_O3.png", p3, width = 10, height = 6, dpi = 300)

# Period comparison
p4 <- train_data %>%
  ggplot(aes(x = Period, y = PM2.5, fill = Period)) +
  geom_boxplot() +
  facet_wrap(~City) +
  labs(title = "Pandemic vs Post-Pandemic")
print(p4)
if (SAVE_PLOTS) ggsave("plots/period_comparison.png", p4, width = 10, height = 6, dpi = 300)

# ==============================================================================
# RQ2: SIMPLE LINEAR REGRESSION BY CITY × PERIOD (PM2.5 ~ NO2)
# ==============================================================================

cat("\n=== RQ2: Simple Linear Regression by City × Period ===\n")
cat("Fitting separate models for each City-Period combination\n\n")

# Store results for all city-period combinations
rq2_results <- data.frame()

# Loop through each city and period
for (city in levels(train_data$City)) {
  for (period in levels(train_data$Period)) {
    
    # Subset data for this city-period combination
    train_subset <- train_data %>% filter(City == city, Period == period)
    test_subset <- test_data %>% filter(City == city, Period == period)
    
    # Skip if insufficient training data
    if (nrow(train_subset) < 30) {
      cat("Skipping", city, "-", period, "(n =", nrow(train_subset), ")\n")
      next
    }
    
    # Fit simple linear regression: PM2.5 ~ NO2
    model <- lm(PM2.5 ~ NO2, data = train_subset)
    
    # Training metrics
    train_pred <- predict(model, newdata = train_subset)
    train_r2 <- summary(model)$r.squared
    train_rmse <- sqrt(mean((train_subset$PM2.5 - train_pred)^2))
    
    # Test metrics (if sufficient test data)
    if (nrow(test_subset) > 1) {
      test_pred <- predict(model, newdata = test_subset)
      
      # Check for zero variance (avoids correlation issues)
      if (sd(test_subset$PM2.5) > 0 && sd(test_pred) > 0) {
        test_r2 <- cor(test_subset$PM2.5, test_pred)^2
      } else {
        test_r2 <- NA
      }
      
      test_rmse <- sqrt(mean((test_subset$PM2.5 - test_pred)^2))
    } else {
      test_pred <- NULL
      test_r2 <- NA
      test_rmse <- NA
    }
    
    # Extract coefficients
    intercept <- coef(model)[1]
    slope_no2 <- coef(model)[2]
    
    # Print summary
    cat(sprintf("%-15s | %-15s | Slope: %6.4f | Train R²: %.3f | Test R²: %.3f\n",
                city, period, slope_no2, train_r2, test_r2))
    
    # Store results
    rq2_results <- rbind(rq2_results, data.frame(
      City = as.character(city),
      Period = as.character(period),
      Intercept = intercept,
      Slope_NO2 = slope_no2,
      Train_R2 = train_r2,
      Train_RMSE = train_rmse,
      Test_R2 = test_r2,
      Test_RMSE = test_rmse,
      n_train = nrow(train_subset),
      n_test = nrow(test_subset)
    ))
  }
}

# ------------------------------------------------------------------------------
# Save Results and Create Summary Visualizations
# ------------------------------------------------------------------------------

cat("\n=== SAVING RQ2 RESULTS ===\n")

# Save summary table
write_csv(rq2_results, "processed_data/RQ2_city_period_summary.csv")
cat("✓ Saved: processed_data/RQ2_city_period_summary.csv\n\n")

# Print summary table
cat("--- RQ2 Summary Table ---\n")
print(rq2_results)

# Slope Comparison
if (nrow(rq2_results) > 0) {
  p_slopes <- rq2_results %>%
    ggplot(aes(x = City, y = Slope_NO2, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = round(Slope_NO2, 3)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("Pandemic" = "steelblue", "Post-Pandemic" = "red")) +
    labs(
      title = "RQ2: NO₂ Slope Comparison by City and Period",
      subtitle = "PM₂.₅ change per 1 μg/m³ increase in NO₂",
      y = "Regression Slope (μg/m³ PM₂.₅ per μg/m³ NO₂)",
      x = "City",
      fill = "Period"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  print(p_slopes)
  if (SAVE_PLOTS) {
    ggsave("plots/RQ2_slope_comparison.png", p_slopes, 
           width = 10, height = 6, dpi = 300)
    cat("✓ Saved: plots/RQ2_slope_comparison.png\n")
  }
  
  # R² Comparison
  p_r2 <- rq2_results %>%
    ggplot(aes(x = City, y = Test_R2, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = round(Test_R2, 3)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("Pandemic" = "steelblue", "Post-Pandemic" = "red")) +
    labs(
      title = "RQ2: Model Performance by City and Period",
      subtitle = "Test R² indicates how well NO₂ predicts PM₂.₅",
      y = "Test R²",
      x = "City",
      fill = "Period"
    ) +
    ylim(0, 1) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  print(p_r2)
  if (SAVE_PLOTS) {
    ggsave("plots/RQ2_r2_comparison.png", p_r2, 
           width = 10, height = 6, dpi = 300)
    cat("✓ Saved: plots/RQ2_r2_comparison.png\n")
  }
}

cat("\n=== RQ2 ANALYSIS COMPLETE ===\n")

# ==============================================================================
# RQ3: MULTIPLE LINEAR REGRESSION BY CITY × PERIOD (PM2.5 ~ NO2 + PM10 + O3)
# ==============================================================================

cat("\n=== RQ3: Multiple Linear Regression by City × Period ===\n")
cat("Fitting separate models for each City-Period combination\n\n")

# Store results for all city-period combinations
rq3_results <- data.frame()

# Loop through each city and period
for (city in levels(train_data$City)) {
  for (period in levels(train_data$Period)) {
    
    # Subset data for this city-period combination
    train_subset <- train_data %>% filter(City == city, Period == period)
    test_subset <- test_data %>% filter(City == city, Period == period)
    
    # Skip if insufficient training data
    if (nrow(train_subset) < 30) {
      cat("Skipping", city, "-", period, "(n =", nrow(train_subset), ")\n")
      next
    }
    
    # Fit multiple linear regression: PM2.5 ~ NO2 + PM10 + O3
    model <- lm(PM2.5 ~ NO2 + PM10 + O3, data = train_subset)
    
    # Training metrics
    train_pred <- predict(model, newdata = train_subset)
    train_r2 <- summary(model)$r.squared
    train_adj_r2 <- summary(model)$adj.r.squared
    train_rmse <- sqrt(mean((train_subset$PM2.5 - train_pred)^2))
    
    # Test metrics (if sufficient test data)
    if (nrow(test_subset) > 1) {
      test_pred <- predict(model, newdata = test_subset)
      
      # Check for zero variance
      if (sd(test_subset$PM2.5) > 0 && sd(test_pred) > 0) {
        test_r2 <- cor(test_subset$PM2.5, test_pred)^2
      } else {
        test_r2 <- NA
      }
      
      test_rmse <- sqrt(mean((test_subset$PM2.5 - test_pred)^2))
    } else {
      test_pred <- NULL
      test_r2 <- NA
      test_rmse <- NA
    }
    
    # Extract coefficients
    coefs <- coef(model)
    
    # Print summary
    cat(sprintf("%-15s | %-15s | Train R²: %.3f | Adj R²: %.3f | Test R²: %.3f\n",
                city, period, train_r2, train_adj_r2, test_r2))
    
    # VIF check for multicollinearity
    vif_no2 <- tryCatch({
      1/(1 - summary(lm(NO2 ~ PM10 + O3, data = train_subset))$r.squared)
    }, error = function(e) NA)
    
    vif_pm10 <- tryCatch({
      1/(1 - summary(lm(PM10 ~ NO2 + O3, data = train_subset))$r.squared)
    }, error = function(e) NA)
    
    vif_o3 <- tryCatch({
      1/(1 - summary(lm(O3 ~ NO2 + PM10, data = train_subset))$r.squared)
    }, error = function(e) NA)
    
    # Store results
    rq3_results <- rbind(rq3_results, data.frame(
      City = as.character(city),
      Period = as.character(period),
      Intercept = coefs[1],
      Coef_NO2 = coefs[2],
      Coef_PM10 = coefs[3],
      Coef_O3 = coefs[4],
      Train_R2 = train_r2,
      Train_Adj_R2 = train_adj_r2,
      Train_RMSE = train_rmse,
      Test_R2 = test_r2,
      Test_RMSE = test_rmse,
      VIF_NO2 = vif_no2,
      VIF_PM10 = vif_pm10,
      VIF_O3 = vif_o3,
      n_train = nrow(train_subset),
      n_test = nrow(test_subset)
    ))
  }
}

# ------------------------------------------------------------------------------
# Save Results and Create Summary Visualizations
# ------------------------------------------------------------------------------

cat("\n=== SAVING RQ3 RESULTS ===\n")

# Save summary table
write_csv(rq3_results, "processed_data/RQ3_city_period_summary.csv")
cat("✓ Saved: processed_data/RQ3_city_period_summary.csv\n\n")

# Print summary table
cat("--- RQ3 Summary Table ---\n")
print(rq3_results)

# Summary visualizations
if (nrow(rq3_results) > 0) {
  
  # R² comparison
  p_r2_rq3 <- rq3_results %>%
    ggplot(aes(x = City, y = Test_R2, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = round(Test_R2, 3)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("Pandemic" = "#0072B2", "Post-Pandemic" = "#D55E00")) +
    labs(
      title = "RQ3: Multiple Regression Model Performance by City and Period",
      subtitle = "Test R² for PM₂.₅ ~ NO₂ + PM₁₀ + O₃",
      y = "Test R²",
      x = "City",
      fill = "Period"
    ) +
    ylim(0, 1) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  print(p_r2_rq3)
  if (SAVE_PLOTS) {
    ggsave("plots/RQ3_r2_comparison.png", p_r2_rq3, 
           width = 10, height = 6, dpi = 300)
    cat("✓ Saved: plots/RQ3_r2_comparison.png\n")
  }
  
  # VIF summary (check multicollinearity)
  vif_summary <- rq3_results %>%
    select(City, Period, VIF_NO2, VIF_PM10, VIF_O3) %>%
    pivot_longer(cols = starts_with("VIF"), names_to = "Predictor", values_to = "VIF") %>%
    mutate(Predictor = str_remove(Predictor, "VIF_"))
  
  p_vif <- vif_summary %>%
    ggplot(aes(x = City, y = VIF, fill = Predictor)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_hline(yintercept = 10, linetype = "dashed", color = "darkred", linewidth = 1) +
    facet_wrap(~Period) +
    scale_fill_manual(values = c("NO2" = "#E69F00", "PM10" = "#56B4E9", "O3" = "#009E73")) +
    labs(
      title = "RQ3: Variance Inflation Factor (VIF) Check Across All Models",
      subtitle = "VIF < 5 indicates no multicollinearity concerns | VIF > 10 indicates severe multicollinearity",
      y = "VIF Value",
      x = "City",
      fill = "Predictor"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
  
  print(p_vif)
  if (SAVE_PLOTS) {
    ggsave("plots/RQ3_vif_summary.png", p_vif, 
           width = 12, height = 6, dpi = 300)
    cat("✓ Saved: plots/RQ3_vif_summary.png\n")
  }
  
  print(p_coef)
  if (SAVE_PLOTS) {
    cat("✓ Saved: plots/RQ3_coefficient_comparison.png\n")
  }
}

cat("\n=== RQ3 ANALYSIS COMPLETE ===\n")

# ==============================================================================
# MODEL COMPARISON: RQ2 vs RQ3
# ==============================================================================

cat("\n=== MODEL COMPARISON: RQ2 (Simple) vs RQ3 (Multiple) ===\n\n")

# Merge RQ2 and RQ3 results for comparison
comparison <- rq2_results %>%
  select(City, Period, RQ2_Train_R2 = Train_R2, RQ2_Test_R2 = Test_R2, 
         RQ2_Test_RMSE = Test_RMSE) %>%
  left_join(
    rq3_results %>%
      select(City, Period, RQ3_Train_R2 = Train_R2, RQ3_Test_R2 = Test_R2,
             RQ3_Test_RMSE = Test_RMSE),
    by = c("City", "Period")
  ) %>%
  mutate(
    R2_Improvement = RQ3_Test_R2 - RQ2_Test_R2,
    RMSE_Improvement = RQ2_Test_RMSE - RQ3_Test_RMSE,
    Pct_Improvement = (R2_Improvement / RQ2_Test_R2) * 100
  )

# Print comparison table
print(comparison)

# Save comparison
write_csv(comparison, "processed_data/RQ2_vs_RQ3_comparison.csv")
cat("\n✓ Saved: processed_data/RQ2_vs_RQ3_comparison.csv\n")

# Visualization: R² improvement
p_comparison_r2 <- comparison %>%
  select(City, Period, RQ2 = RQ2_Test_R2, RQ3 = RQ3_Test_R2) %>%
  pivot_longer(cols = c(RQ2, RQ3), names_to = "Model", values_to = "Test_R2") %>%
  ggplot(aes(x = City, y = Test_R2, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Period) +
  geom_text(aes(label = round(Test_R2, 3)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Model Comparison: RQ2 (Simple) vs RQ3 (Multiple)",
    subtitle = "Test R² by City and Period",
    y = "Test R²",
    x = "City"
  ) +
  scale_fill_manual(values = c("RQ2" = "#E69F00", "RQ3" = "#56B4E9")) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_comparison_r2)
if (SAVE_PLOTS) ggsave("plots/RQ2_vs_RQ3_comparison_R2.png", p_comparison_r2, 
                       width = 12, height = 6, dpi = 300)

# Visualization: RMSE improvement
p_comparison_rmse <- comparison %>%
  select(City, Period, RQ2 = RQ2_Test_RMSE, RQ3 = RQ3_Test_RMSE) %>%
  pivot_longer(cols = c(RQ2, RQ3), names_to = "Model", values_to = "Test_RMSE") %>%
  ggplot(aes(x = City, y = Test_RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Period) +
  geom_text(aes(label = round(Test_RMSE, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Model Comparison: RQ2 (Simple) vs RQ3 (Multiple)",
    subtitle = "Test RMSE by City and Period (lower is better)",
    y = "Test RMSE (μg/m³)",
    x = "City"
  ) +
  scale_fill_manual(values = c("RQ2" = "#E69F00", "RQ3" = "#56B4E9")) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_comparison_rmse)
if (SAVE_PLOTS) ggsave("plots/RQ2_vs_RQ3_comparison_RMSE.png", p_comparison_rmse,
                       width = 12, height = 6, dpi = 300)

# Summary statistics
cat("\n=== Summary: Model Improvement ===\n")
cat("Average R² improvement:", round(mean(comparison$R2_Improvement, na.rm = TRUE), 4), "\n")
cat("Average % improvement:", round(mean(comparison$Pct_Improvement, na.rm = TRUE), 1), "%\n")
cat("Average RMSE reduction:", round(mean(comparison$RMSE_Improvement, na.rm = TRUE), 2), "μg/m³\n")

cat("\n==============================================================================\n")
cat("Analysis Complete!\n")
cat("==============================================================================\n")