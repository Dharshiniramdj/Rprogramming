student1 <- data.frame(
  Student_ID = c(101,102,103,104,105),
  Name = c(
    "Anitha","Bala","Charan","Divya","Ganesh"
  ),
  Department = c(
    "CSE","IT","CSE","ECE","IT"
  )
)

student2 <- data.frame(
  Student_ID = c(101,102,103,104,106),
  CGPA = c(8.7,7.8,9.1,8.2,7.5),
  Attendance = c(92,85,95,88,78)
)

# 1. Display both data frames
print(student1)
print(student2)

# 2. Inner merge
inner_data <- merge(
  student1,
  student2,
  by = "Student_ID"
)

print(inner_data)

# 3. Left merge
left_data <- merge(
  student1,
  student2,
  by = "Student_ID",
  all.x = TRUE
)

print(left_data)

# 4. Right merge
right_data <- merge(
  student1,
  student2,
  by = "Student_ID",
  all.y = TRUE
)

print(right_data)

# 5. Full merge
full_data <- merge(
  student1,
  student2,
  by = "Student_ID",
  all = TRUE
)

print(full_data)

# 6. IDs present only in first data frame
only_first <- setdiff(
  student1$Student_ID,
  student2$Student_ID
)

print(only_first)

# 7. IDs present only in second data frame
only_second <- setdiff(
  student2$Student_ID,
  student1$Student_ID
)

print(only_second)

# 8. Display missing values
print(is.na(full_data))

# 9. Replace missing CGPA and Attendance
full_data$CGPA[
  is.na(full_data$CGPA)
] <- 0

full_data$Attendance[
  is.na(full_data$Attendance)
] <- 0

print(full_data)

# 10. Complete academic report
academic_report <- merge(
  student1,
  student2,
  by = "Student_ID",
  all.x = TRUE
)

print(academic_report)