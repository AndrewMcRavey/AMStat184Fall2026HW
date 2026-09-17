
## Homework 1



## Problem 1: Objects, data structures, and subsetting


student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section    <- c("A", "B", "A", "B", "A", "B")
quiz1      <- c(82, 91, 76, 88, 95, 69)
quiz2      <- c(85, 89, 80, 92, 94, 74)
passed     <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

## Part A: Building Data Structures 

# Factor with explicit level order A, B
section <- factor(section, levels = c("A", "B"))

# Data frame with all five vectors
students <- data.frame(
  student_id = student_id,
  section    = section,
  quiz1      = quiz1,
  quiz2      = quiz2,
  passed     = passed
)

# Matrix: students as rows, quizzes as columns
score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = length(student_id),
  ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

# List: course info, scores, and named cutoffs vector
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)


# Inspect
typeof(quiz1)
# [1] "double"
class(quiz1)
# [1] "numeric"
length(quiz1)
# [1] 6

typeof(section)
# [1] "integer"
class(section)
# [1] "factor"
length(section)
# [1] 6

typeof(score_matrix)
# [1] "double"
class(score_matrix)
# [1] "matrix" "array"
dim(score_matrix)
# [1] 6 2

typeof(course_record)
# [1] "list"
class(course_record)
# [1] "list"
length(course_record)
# [1] 3

str(students)
# 'data.frame':	6 obs. of  5 variables:
#  $ student_id: chr  "S01" "S02" "S03" "S04" ...
#  $ section   : Factor w/ 2 levels "A","B": 1 2 1 2 1 2
#  $ quiz1     : num  82 91 76 88 95 69
#  $ quiz2     : num  85 89 80 92 94 74
#  $ passed    : logi  TRUE TRUE TRUE TRUE TRUE FALSE

dim(students)
# [1] 6 5


# A vector holds one type of data in a single dimension.
# A factor is a special vector for categorical data, storing values as
# integer codes with associated levels.
# A matrix is a 2D, homogeneous collection of values.
# A list can hold elements of different types and lengths, including other
# lists or data frames.
# A data frame is a special list where each element (column) is a vector
# of the same length.



## Part B: Subsetting

# Student S04's second quiz score from score_matrix
score_matrix["S04", "quiz2"]
# [1] 92

# First two rows, preserving matrix dimensions (drop = FALSE)
score_matrix[1:2, , drop = FALSE]
#     quiz1 quiz2
# S01    82    85
# S02    91    89

# Extracting "course" from course_record three ways
course_record["course"]    # [  -> returns a sub-list containing "course"
# [1] "R Programming"
course_record[["course"]]  # [[ -> returns the element itself
# [1] "R Programming"
course_record$course       # $  -> same as [[, uses elements name
# [1] "R Programming"



## Part C: Vectorized calculations

# Row-wise mean of the two quizzes, added to students
students$average <- rowMeans(cbind(students$quiz1, students$quiz2))

# TRUE when average >= 90
students$excellent <- students$average >= 90

# Section A students with average >= 80
sectionA_high <- students[students$section == "A" & students$average >= 80, ]

# Only student_id, section, average for that subset
sectionA_high_subset <- sectionA_high[, c("student_id", "section", "average")]
sectionA_high_subset
#   student_id section average
# 1        S01       A    83.5
# 5        S05       A    94.5

# Named numeric vector of all student averages
student_averages <- setNames(students$average, students$student_id)
student_averages
#  S01  S02  S03  S04  S05  S06
# 83.5 90.0 78.0 90.0 94.5 71.5



## Problem 2: Importing, cleaning, and summarizing data


csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

## Part A: Importing data 

measurements <- read.csv(text = csv_text, na.strings = c("", "NA"),
                         stringsAsFactors = FALSE)

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# Count of missing values per column
colSums(is.na(measurements))

# Complete-case data frame
measurements_complete <- measurements[complete.cases(measurements), ]

# Sample IDs removed by the complete-case filter
removed_ids <- setdiff(measurements$sample_id, measurements_complete$sample_id)
removed_ids
# [1] "M02" "M03" "M06"

# x == NA is not a valid missing-value test because NA represents an
# unknown value, so it cannot be compared as it is the lack of a variable 
# entirely, x == NA is always not true or false

## Part B: Transform, filter, and summarize 

# Convert site and status to factors and report levels
measurements$site   <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
# [1] "East"  "North" "South"
levels(measurements$status)
# [1] "ok"     "review"

# Fahrenheit conversion
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

# ph_below_7, preserving NA 
measurements$ph_below_7 <- ifelse(is.na(measurements$ph), NA, measurements$ph < 7)

# Complete observations from North or South with status "ok"
ns_ok <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
]

ns_ok_subset <- ns_ok[, c("sample_id", "site", "temp_c", "temp_f", "ph")]
ns_ok_subset
#   sample_id  site temp_c temp_f  ph
# 1       M01 North   18.2  64.76 7.1
# 7       M07 North   17.8  64.04 6.9
# 8       M08 South   21.2  70.16 7.2

# Overall mean temperature 
mean_temp_overall <- mean(measurements$temp_c, na.rm = TRUE)
mean_temp_overall
# [1] 20.35714

# South-site mean temperature 
mean_temp_south <- mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)
mean_temp_south
# [1] 20.46667

## Part C: Matrix operations 

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise_product <- A * B
matrix_product <- A %*% B


dim(elementwise_product)  # 2 x 2
# [1] 2 2
dim(matrix_product)       # 2 x 2
# [1] 2 2

elementwise_product
#      [,1] [,2]
# [1,]    5   21
# [2,]   12   32

matrix_product
#      [,1] [,2]
# [1,]    5   21
# [2,]   12   32

# A * B multiplies corresponding elements of A and B position-by-position
# producing a result of the same shape. Whereas A %*% B performs true matrix 
# multiplication where each entry in the result is the sum of products of a 
# row of A and a column of B and doesnt always reult in the same 
# vector dimensions



## Problem 3: Functions



student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)


## Part A: Write a grading function

grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
# [1] NA
grade_one(90)
# [1] "A"
grade_one(80)
# [1] "B"
grade_one(85)
# [1] "B"
grade_one(74)
# [1] "C"

## Part B: Applying functions with a loop

grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades
# P01 P02 P03 P04 P05 P06 P07 P08
# "A" "B"  NA "D" "C" "B" "F" "A"

# During the loop, i represents the current index (position) being
# processed, from 1 up to length(scores), used to access scores[i] and
# store results into grades[i] on each iteration.

## Part C-1: Function behavior 

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  result <- c(
    total   = length(x),
    missing = sum(is.na(x)),
    mean    = round(mean(x, na.rm = na.rm), digits),
    sd      = round(sd(x, na.rm = na.rm), digits),
    min     = round(min(x, na.rm = na.rm), digits),
    max     = round(max(x, na.rm = na.rm), digits)
  )
  return(result)
}

# Call with defaults
summarize_scores(scores)
#   total missing    mean      sd     min     max
#     8.0     1.0    79.4    13.3    59.0    95.0

# Call with named arguments
summarize_scores(x = scores, na.rm = TRUE, digits = 2)
#   total missing    mean      sd     min     max
#    8.00    1.00   79.43   13.28   59.00   95.00

## Part C-2: Function behavior 

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}
  
plot_scores(scores, type = "b", pch = 19,
            xlab = "Position", ylab = "Score", main = "Student Scores")

# Prediction/explanation: plot_scores(scores, ...) plots score values on
# the y-axis against their position (1 through 8) on the x-axis. The "..."
# lets any additional plotting arguments (type, pch, xlab, ylab, main,
# etc.) pass straight through to the underlying plot() call, so the caller
# can customize the appearance without plot_scores() needing to know about
# every possible plotting parameter in advance.position 3 does not appear
# on the plot because its value is NA.

