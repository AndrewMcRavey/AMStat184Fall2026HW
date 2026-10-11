library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

# Problem 1
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# Part A
sales_clean <- sales_raw |>
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      .default = "small"
    )
  )
sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

# Part B
sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )
invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
invalid_codes

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number"),
                       cols_remove = FALSE) |>
  select(sale_id, code_clean, channel, number)
valid_code_parts

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
sale_tags
# separate_wider_delim() splits one string column into several new columns,
# while separate_longer_delim() splits it into several new rows.

# Part C
customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (known_n == 0) NA_real_ else large_n / known_n,
    .by = customer_id
  ) |>
  arrange(customer_id)
customer_summary
# C1's large_share = 0/1: the numerator is large_n = 0 (the only known net
# amount, 10, is below 20) and the denominator is known_n = 1 (sale 3 has a
# missing net amount and is excluded).

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )
levels(sales_factors$item_factor)
sales_factors |> count(item_factor)
levels(sales_factors$item_group)
sales_factors |> count(item_group)
# Changing level order (fct_infreq) only reorders the same four categories and
# leaves their counts unchanged; combining categories (fct_lump_n) merges
# eraser and notebook into "Other", reducing the levels from 4 to 3.

# Problem 2 
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Part A
reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )
reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount)
# coalesce() uses the backup only when the primary is NA. A recorded primary
# value, including 0, is a real measurement and not missing, so it is kept

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))
reports_complete

c(original_rows = sum(reports_complete$recorded_row),
  added_rows = sum(!reports_complete$recorded_row),
  missing_before = sum(is.na(reports_clean$amount)),
  missing_after = sum(is.na(reports_complete$amount)))
# 6 original rows, 6 added rows. Missing amounts: 1 before, 7 after.
# Explicit missing: C3 round 1 (row exists, amount is NA).
# Implicit missing: C1 round 2 (no row exists before completion).

# Part B
sum(duplicated(customers$customer_id))
sum(is.na(customers$customer_id))
reports_clean |> count(customer_id, round) |> filter(n > 1)
# No duplicate or missing customer IDs, and no repeated customer_id-round
# combination in reports_clean.

customers_marked <- customers |> mutate(customer_found = TRUE)
reports_joined <- reports_clean |>
  left_join(customers_marked, by = join_by(customer_id),
            relationship = "many-to-one", na_matches = "never") |>
  mutate(customer_found = replace_na(customer_found, FALSE))
reports_joined |>
  select(customer_id, round, amount, region, customer_found)

reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)
customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)
# Only in reports_clean: C4. Only in customers: C5.
# C3 has customer_found = TRUE (it is in customers, its region is just unknown);
# C4 has customer_found = FALSE (it has no customer record at all). A missing
# region alone cannot distinguish these two cases.

# Part C
customers_bad <- tibble(
  customer_id = c("C1", "C1", "C2", "C3", "C5"),
  region = c("North", "East", "South", NA, "West")
)
reports_duplicated <- reports_clean |>
  left_join(customers_bad, by = join_by(customer_id),
            relationship = "many-to-many")
c(rows_before = nrow(reports_clean), rows_after = nrow(reports_duplicated))
c(total_before = sum(reports_clean$amount, na.rm = TRUE),
  total_after = sum(reports_duplicated$amount, na.rm = TRUE))
reports_duplicated |> count(customer_id, round) |> filter(n > 1)
# Rows go from 6 to 8 and the total from 75 to 100. The repeated reports are
# C1 round 1 (10) and C1 round 3 (15): each matches two C1 rows in
# customers_bad, so they are counted twice (75 + 10 + 15 = 100).

try(reports_clean |>
      left_join(customers_bad, by = join_by(customer_id),
                relationship = "many-to-one"))
# With "many-to-one", dplyr stops with an error because C1 matches more than
# one row in customers_bad. Allowing "many-to-many" does not fix the
# conflicting information (C1 is both North and East); it only suppresses the
# check, so the duplicate must be resolved in customers_bad before joining.

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, values_from = amount, names_prefix = "round_")
reports_wide

reports_wide |>
  pivot_longer(starts_with("round_"), names_to = "round",
               values_to = "amount", values_drop_na = TRUE) |>
  nrow()
# Predicted: 5 rows (12 cells minus 7 NA). Dropping missing values does not
# recover the unknown amounts: they were never observed, so the rows are just
# removed, and the difference between the explicit NA and the added rows is lost.

# Problem 3
# Part A
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x); ny <- length(y)
  mx <- mean(x); my <- mean(y)
  vx <- var(x); vy <- var(y)
  if (equal_var) {
    method <- "Pooled"
    sp2 <- ((nx - 1) * vx + (ny - 1) * vy) / (nx + ny - 2)
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    df <- nx + ny - 2
  } else {
    method <- "Welch"
    se <- sqrt(vx / nx + vy / ny)
    df <- (vx / nx + vy / ny)^2 /
      ((vx / nx)^2 / (nx - 1) + (vy / ny)^2 / (ny - 1))
  }
  t_stat <- (mx - my) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  list(method = method, t_stat = t_stat, df = df,
       p_value = p_value, reject = p_value < alpha)
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

mine_welch <- two_sample_t(x_check, y_check)
mine_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)
mine_welch
mine_pooled

ref_welch <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)
abs(c(mine_welch$t_stat - unname(ref_welch$statistic),
      mine_welch$df - unname(ref_welch$parameter),
      mine_welch$p_value - ref_welch$p.value)) < 1e-8
abs(c(mine_pooled$t_stat - unname(ref_pooled$statistic),
      mine_pooled$df - unname(ref_pooled$parameter),
      mine_pooled$p_value - ref_pooled$p.value)) < 1e-8
# For both Welch and pooled, t_stat, df, and p_value differ from t.test() by
# less than 1e-8 (all TRUE above).

# Part B
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05
scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)
set.seed(2026)

methods <- c("Welch", "Pooled")
equal_vs <- c(FALSE, TRUE)
n_rows <- nrow(scenarios) * B * length(methods)
res_scenario <- character(n_rows)
res_replicate <- integer(n_rows)
res_method <- character(n_rows)
res_t <- numeric(n_rows)
res_df <- numeric(n_rows)
res_p <- numeric(n_rows)
res_reject <- logical(n_rows)

row <- 0
for (s in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    for (m in seq_along(methods)) {
      out <- two_sample_t(x, y, equal_var = equal_vs[m], alpha = alpha)
      row <- row + 1
      res_scenario[row] <- scenarios$scenario[s]
      res_replicate[row] <- b
      res_method[row] <- out$method
      res_t[row] <- out$t_stat
      res_df[row] <- out$df
      res_p[row] <- out$p_value
      res_reject[row] <- out$reject
    }
  }
}

simulation_results <- data.frame(
  scenario = res_scenario, replicate = res_replicate, method = res_method,
  t_stat = res_t, df = res_df, p_value = res_p, reject = res_reject
)
head(simulation_results, 6)
nrow(simulation_results)

simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  mutate(across(c(mean_t, sd_t, rejection_rate), \(v) round(v, 3)))
simulation_summary

# report:
# Under equal variances both tests reject close to 0.05 (Welch 0.044, pooled 
# 0.038), but with unequal variances only Welch stays near it (0.061) while 
# the pooled test inflates Type I error to 0.139. When the means differ, 
# rejection rates are power: both tests are about 0.96 in equal_shift, but 
# power drops with unequal variances (Welch 0.282, pooled 0.497). The pooled 
# test's higher power in unequal_shift is not a real advantage, because it also 
# rejects too often when H0 is true, so a test must control Type I error near 
# alpha before its power can be trusted. With B = 1000 the Monte Carlo standard 
# error of a rate near 0.05 is about 0.007, so 0.038 vs 0.044 vs 0.061 is within
# random variation, but 0.139 is not.
