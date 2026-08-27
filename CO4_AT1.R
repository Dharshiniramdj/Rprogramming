# ---- Store marks in a matrix ----
students <- c("Amit", "Bala", "Chitra", "Deepa", "Esha",
              "Farah", "Gopal", "Hema", "Irfan", "Jyoti")

marks <- matrix(c(78, 82, 91, 55, 67, 88, 45, 72, 95, 60,   # Mathematics
                  65, 74, 80, 60, 72, 91, 50, 68, 85, 58,  # Physics
                  70, 79, 85, 62, 75, 84, 48, 71, 90, 65), # Chemistry
                nrow = 10, ncol = 3,
                dimnames = list(students, c("Mathematics", "Physics", "Chemistry")))

print(marks)

# ---- Subject-wise averages ----
subject_avg <- colMeans(marks)
cat("\nSubject-wise averages:\n")
print(round(subject_avg, 2))

# ---- Highest and lowest scorer per subject ----
cat("\nHighest and lowest scorers:\n")
for (subj in colnames(marks)) {
  top_student <- rownames(marks)[which.max(marks[, subj])]
  low_student <- rownames(marks)[which.min(marks[, subj])]
  cat(sprintf("%-12s -> Highest: %-8s (%d)  |  Lowest: %-8s (%d)\n",
              subj, top_student, max(marks[, subj]),
              low_student, min(marks[, subj])))
}

# ---- Students scoring above the overall average ----
overall_avg <- mean(marks)
student_totals <- rowMeans(marks)
above_avg <- names(student_totals[student_totals > overall_avg])

cat(sprintf("\nOverall average (all subjects, all students): %.2f\n", overall_avg))
cat("Students scoring above overall average (by their own mean):\n")
print(above_avg)
subject_avg <- colMeans(marks)

barplot(subject_avg,
        col = "#2a78d6",
        main = "Subject-wise Average Marks",
        ylab = "Average mark",
        ylim = c(0, 100))

library(plotly)

plot_ly(
  z = t(marks), x = students, y = colnames(marks),
  type = "heatmap", colorscale = "Blues",
  hovertemplate = "%{x} - %{y}: %{z}<extra></extra>"
) %>%
  layout(title = "Marks Heatmap: Students x Subjects")

library(plotly)

plot_ly(
  type = 'scatterpolar', mode = 'lines+markers', fill = 'none'
) %>%
  add_trace(r = c(95,85,90,95), theta = c('Mathematics','Physics','Chemistry','Mathematics'), name = 'Irfan') %>%
  add_trace(r = c(91,80,85,91), theta = c('Mathematics','Physics','Chemistry','Mathematics'), name = 'Chitra') %>%
  add_trace(r = c(88,91,84,88), theta = c('Mathematics','Physics','Chemistry','Mathematics'), name = 'Farah') %>%
  layout(polar = list(radialaxis = list(visible = TRUE, range = c(0,100))),
         title = "Top 3 Students Across Subjects")

student_totals <- rowMeans(marks)
overall_avg <- mean(marks)
colors <- ifelse(student_totals > overall_avg, "#2a78d6", "gray70")

barplot(student_totals,
        col = colors,
        main = "Each Student vs Overall Average",
        ylab = "Average mark",
        las = 2)
abline(h = overall_avg, col = "#eb6834", lwd = 2, lty = 2)
legend("topright", legend = "Overall average", col = "#eb6834", lty = 2, lwd = 2, bty = "n")