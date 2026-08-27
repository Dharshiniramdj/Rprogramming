################################################################################
# CASE STUDY 1: STUDENT ACADEMIC PERFORMANCE ANALYSIS
# ------------------------------------------------------------------------------
# Dataset (simulated, 500 students): Student_ID, Gender, Attendance_Percentage,
# Assignment_Marks, Internal_Marks, Study_Hours_Per_Week, Final_Marks
#
# Goals:
#   1. Visualize distributions (histograms, box plots, bar charts, scatter,
#      line charts) using ggplot2
#   2. Simple linear regression: Study_Hours_Per_Week -> Final_Marks
#   3. Multiple linear regression: Attendance + Assignment + Internal + Study
#      Hours -> Final_Marks
#   4. Interpret coefficients, R-squared, significance, residuals
#   5. Recommendations to improve student performance
#
# NOTE: If you have your own real dataset (e.g. "students.csv"), replace the
# data-generation block below with:
#     students <- read.csv("students.csv")
# and skip straight to the "EXPLORATORY DATA ANALYSIS" section.
################################################################################

# ---- 0. SETUP: PACKAGES ------------------------------------------------------

required_packages <- c("ggplot2", "dplyr", "car", "corrplot", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages, repos = "https://cran.r-project.org")

library(ggplot2)
library(dplyr)
library(car)        # for VIF (multicollinearity check)
library(corrplot)   # for correlation heatmap
library(gridExtra)  # to arrange multiple ggplots on one page

# Create an output folder for saved plots
if (!dir.exists("plots")) dir.create("plots")


# ---- 1. SIMULATE THE DATASET (500 students) ---------------------------------
# Replace this block with read.csv("your_file.csv") if you have real data.

set.seed(42)
n <- 500

Student_ID <- paste0("S", sprintf("%04d", 1:n))
Gender <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48))

# Study hours: realistic right-skewed-ish distribution, 0 to ~25 hrs/week
Study_Hours_Per_Week <- round(pmax(0, rnorm(n, mean = 10, sd = 4)), 1)

# Attendance: correlated loosely with study hours, bounded 40-100%
Attendance_Percentage <- round(pmin(100, pmax(40,
                                              60 + 1.2 * Study_Hours_Per_Week + rnorm(n, 0, 8))), 1)

# Assignment marks (out of 100): influenced by study hours + noise
Assignment_Marks <- round(pmin(100, pmax(0,
                                         30 + 2.5 * Study_Hours_Per_Week + rnorm(n, 0, 10))), 1)

# Internal marks (out of 50): influenced by attendance + study hours
Internal_Marks <- round(pmin(50, pmax(0,
                                      10 + 0.25 * Attendance_Percentage + 0.6 * Study_Hours_Per_Week +
                                        rnorm(n, 0, 5))), 1)

# Final marks (out of 100): the outcome we want to explain/predict.
# Built as a function of all predictors + irreducible noise, so the
# regression models below have genuine signal to recover.
Final_Marks <- round(pmin(100, pmax(0,
                                    10 +
                                      0.25 * Attendance_Percentage +
                                      0.30 * Assignment_Marks +
                                      0.50 * Internal_Marks +
                                      0.80 * Study_Hours_Per_Week +
                                      rnorm(n, 0, 6))), 1)

students <- data.frame(
  Student_ID, Gender, Attendance_Percentage, Assignment_Marks,
  Internal_Marks, Study_Hours_Per_Week, Final_Marks,
  stringsAsFactors = FALSE
)

# Quick look
str(students)
summary(students)
head(students)


# ---- 2. EXPLORATORY DATA ANALYSIS (EDA) -------------------------------------

# 2.1 Histogram: Distribution of Final Marks
p1 <- ggplot(students, aes(x = Final_Marks)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Final Marks", x = "Final Marks", y = "Number of Students") +
  theme_minimal()
print(p1)
ggsave("plots/01_hist_final_marks.png", p1, width = 7, height = 5)

# 2.2 Histogram: Distribution of Study Hours
p2 <- ggplot(students, aes(x = Study_Hours_Per_Week)) +
  geom_histogram(binwidth = 2, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Weekly Study Hours", x = "Study Hours/Week", y = "Count") +
  theme_minimal()
print(p2)
ggsave("plots/02_hist_study_hours.png", p2, width = 7, height = 5)

# 2.3 Box plot: Final Marks by Gender
p3 <- ggplot(students, aes(x = Gender, y = Final_Marks, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Final Marks Distribution by Gender", x = "Gender", y = "Final Marks") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3)
ggsave("plots/03_box_finalmarks_gender.png", p3, width = 7, height = 5)

# 2.4 Box plot: Attendance by Gender
p4 <- ggplot(students, aes(x = Gender, y = Attendance_Percentage, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Attendance Distribution by Gender", x = "Gender", y = "Attendance (%)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p4)
ggsave("plots/04_box_attendance_gender.png", p4, width = 7, height = 5)

# 2.5 Bar chart: Average Final Marks by Gender
avg_by_gender <- students %>%
  group_by(Gender) %>%
  summarise(Avg_Final_Marks = mean(Final_Marks), .groups = "drop")

p5 <- ggplot(avg_by_gender, aes(x = Gender, y = Avg_Final_Marks, fill = Gender)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(Avg_Final_Marks, 1)), vjust = -0.5) +
  labs(title = "Average Final Marks by Gender", x = "Gender", y = "Average Final Marks") +
  theme_minimal() +
  theme(legend.position = "none")
print(p5)
ggsave("plots/05_bar_avg_marks_gender.png", p5, width = 7, height = 5)

# 2.6 Bar chart: Performance category counts (categorize final marks into bands)
students <- students %>%
  mutate(Performance_Band = case_when(
    Final_Marks >= 85 ~ "Excellent (85-100)",
    Final_Marks >= 70 ~ "Good (70-84)",
    Final_Marks >= 50 ~ "Average (50-69)",
    TRUE              ~ "Needs Improvement (<50)"
  ))
students$Performance_Band <- factor(students$Performance_Band,
                                    levels = c("Needs Improvement (<50)", "Average (50-69)", "Good (70-84)", "Excellent (85-100)"))

p6 <- ggplot(students, aes(x = Performance_Band, fill = Performance_Band)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Number of Students per Performance Band", x = "Performance Band", y = "Number of Students") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))
print(p6)
ggsave("plots/06_bar_performance_band.png", p6, width = 8, height = 5)

# 2.7 Scatter plot: Study Hours vs Final Marks (with trend line)
p7 <- ggplot(students, aes(x = Study_Hours_Per_Week, y = Final_Marks)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Study Hours vs Final Marks", x = "Study Hours per Week", y = "Final Marks") +
  theme_minimal()
print(p7)
ggsave("plots/07_scatter_studyhours_finalmarks.png", p7, width = 7, height = 5)

# 2.8 Scatter plot: Attendance vs Final Marks
p8 <- ggplot(students, aes(x = Attendance_Percentage, y = Final_Marks)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Attendance vs Final Marks", x = "Attendance (%)", y = "Final Marks") +
  theme_minimal()
print(p8)
ggsave("plots/08_scatter_attendance_finalmarks.png", p8, width = 7, height = 5)

# 2.9 Line chart: Average Final Marks across binned Study Hours
# (a "line chart" view of the trend — bin study hours, plot average marks per bin)
students <- students %>%
  mutate(Study_Hours_Bin = cut(Study_Hours_Per_Week,
                               breaks = seq(0, 25, by = 2.5), include.lowest = TRUE))

trend_data <- students %>%
  group_by(Study_Hours_Bin) %>%
  summarise(Avg_Final_Marks = mean(Final_Marks), n = n(), .groups = "drop") %>%
  filter(!is.na(Study_Hours_Bin))

p9 <- ggplot(trend_data, aes(x = Study_Hours_Bin, y = Avg_Final_Marks, group = 1)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_point(color = "purple", size = 2) +
  labs(title = "Trend: Average Final Marks across Study-Hour Ranges",
       x = "Study Hours per Week (binned)", y = "Average Final Marks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p9)
ggsave("plots/09_line_trend_studyhours.png", p9, width = 8, height = 5)

# 2.10 Correlation heatmap among numeric variables
numeric_vars <- students %>%
  select(Attendance_Percentage, Assignment_Marks, Internal_Marks,
         Study_Hours_Per_Week, Final_Marks)
corr_matrix <- cor(numeric_vars)
png("plots/10_correlation_heatmap.png", width = 700, height = 700)
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix",
         mar = c(0,0,2,0))
dev.off()
print(round(corr_matrix, 2))


# ---- 3. SIMPLE LINEAR REGRESSION --------------------------------------------
# Question: Does Study_Hours_Per_Week significantly predict Final_Marks?

simple_model <- lm(Final_Marks ~ Study_Hours_Per_Week, data = students)
summary(simple_model)

cat("\n--- SIMPLE LINEAR REGRESSION INTERPRETATION ---\n")
cat("Intercept: expected Final Marks when Study Hours = 0.\n")
cat("Slope (Study_Hours_Per_Week coefficient): expected change in Final Marks\n")
cat("for each additional hour of weekly study, holding nothing else constant.\n")
cat("Check the p-value on the slope (< 0.05 => statistically significant predictor)\n")
cat("and R-squared (proportion of variance in Final Marks explained by study hours alone).\n\n")

# Residual diagnostics for the simple model
png("plots/11_simple_model_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(simple_model)
dev.off()
par(mfrow = c(1,1))


# ---- 4. MULTIPLE LINEAR REGRESSION -------------------------------------------
# Predictors: Attendance_Percentage, Assignment_Marks, Internal_Marks,
#             Study_Hours_Per_Week  ->  Final_Marks

multiple_model <- lm(Final_Marks ~ Attendance_Percentage + Assignment_Marks +
                       Internal_Marks + Study_Hours_Per_Week, data = students)
summary(multiple_model)

cat("\n--- MULTIPLE LINEAR REGRESSION INTERPRETATION ---\n")
cat("Each coefficient = expected change in Final Marks for a one-unit increase\n")
cat("in that predictor, HOLDING ALL OTHER PREDICTORS CONSTANT.\n")
cat("Compare Multiple R-squared vs Adjusted R-squared: Adjusted R-squared penalizes\n")
cat("adding predictors that don't genuinely improve fit, so it's the fairer metric\n")
cat("when comparing the simple vs multiple model.\n")
cat("Check each predictor's p-value (Pr(>|t|)) to see which are statistically\n")
cat("significant (commonly, p < 0.05) contributors to Final Marks.\n\n")

# Compare simple vs multiple model fit
cat("Simple model R-squared:  ", round(summary(simple_model)$r.squared, 3), "\n")
cat("Multiple model R-squared:", round(summary(multiple_model)$r.squared, 3), "\n")
cat("Multiple model Adjusted R-squared:", round(summary(multiple_model)$adj.r.squared, 3), "\n\n")

# ANOVA comparison: does adding the extra predictors significantly improve the model?
anova_comparison <- anova(simple_model, multiple_model)
print(anova_comparison)

# Multicollinearity check (Variance Inflation Factor)
cat("\n--- VARIANCE INFLATION FACTORS (multicollinearity check) ---\n")
print(vif(multiple_model))
cat("Rule of thumb: VIF > 5 (some use >10) suggests problematic multicollinearity\n")
cat("between that predictor and the others.\n\n")

# Residual diagnostics for the multiple model
png("plots/12_multiple_model_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(multiple_model)
dev.off()
par(mfrow = c(1,1))

# Standardized coefficients (to compare relative predictor strength on a common scale)
students_scaled <- students %>%
  mutate(across(c(Attendance_Percentage, Assignment_Marks, Internal_Marks,
                  Study_Hours_Per_Week, Final_Marks), ~ as.numeric(scale(.))))
standardized_model <- lm(Final_Marks ~ Attendance_Percentage + Assignment_Marks +
                           Internal_Marks + Study_Hours_Per_Week, data = students_scaled)
cat("\n--- STANDARDIZED COEFFICIENTS (relative importance, unitless) ---\n")
print(round(coef(standardized_model), 3))
cat("Larger absolute standardized coefficient = stronger relative influence on Final Marks.\n\n")

# Predicted vs Actual plot for the multiple regression model
students$Predicted_Final_Marks <- predict(multiple_model)

p10 <- ggplot(students, aes(x = Predicted_Final_Marks, y = Final_Marks)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Multiple Regression: Predicted vs Actual Final Marks",
       x = "Predicted Final Marks", y = "Actual Final Marks") +
  theme_minimal()
print(p10)
ggsave("plots/13_predicted_vs_actual.png", p10, width = 7, height = 5)


# ---- 5. SUMMARY TABLE OF KEY MODEL RESULTS ----------------------------------

cat("\n==================== MODEL SUMMARY ====================\n")
cat(sprintf("%-25s %10s %10s\n", "Predictor", "Coefficient", "p-value"))
mm_summary <- summary(multiple_model)$coefficients
for (i in 1:nrow(mm_summary)) {
  cat(sprintf("%-25s %10.3f %10.4f\n",
              rownames(mm_summary)[i], mm_summary[i,1], mm_summary[i,4]))
}
cat("=========================================================\n\n")


# ---- 6. RECOMMENDATIONS (printed as comments / console output) -------------
cat("
--- RECOMMENDATIONS TO IMPROVE STUDENT PERFORMANCE ---
(Derived from the regression results above -- re-check actual coefficient
signs/magnitudes/p-values once run, as they depend on your real data.)

1. If Study_Hours_Per_Week has a significant positive coefficient in both
   models: encourage structured study-time programs, study-hall hours, or
   time-management workshops, since additional weekly study time is
   associated with meaningfully higher final marks.

2. If Attendance_Percentage is a significant positive predictor: strengthen
   attendance-tracking and early-intervention outreach for students whose
   attendance drops below a threshold (e.g. 75%), since attendance appears
   to compound with study habits to affect outcomes.

3. If Internal_Marks / Assignment_Marks carry large coefficients: these
   continuous-assessment components are strong leading indicators of final
   performance -- consider using them as an early-warning system to flag
   at-risk students mid-semester, well before the final exam.

4. Use the Performance_Band bar chart to identify what fraction of the
   cohort is in the 'Needs Improvement' band, and target remedial support
   (tutoring, peer mentoring) specifically at that group.

5. If VIF values are high for any predictor pair (e.g. Attendance and
   Study Hours), be cautious interpreting their individual coefficients in
   isolation -- their effects are somewhat entangled, and combined
   interventions (e.g. programs that raise both together) may be more
   effective than targeting either alone.

6. Compare Adjusted R-squared here against future semesters' data to check
   whether interventions are improving how well these factors explain
   (and presumably improve) final performance over time.
")

cat("\nAll plots have been saved to the 'plots/' folder in your working directory.\n")
cat("Run getwd() to see where that is on your machine.\n")