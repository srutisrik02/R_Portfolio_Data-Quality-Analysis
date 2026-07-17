# Simulated SurveyCTO Data Quality Monitoring
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
set.seed(42)
n <- 200 
survey_data <- data.frame(
submission_id     = paste0("DRG-", sprintf("%04d", 1:n)),
enumerator_id     = sample(paste0("ENUM-", c("01","02","03","04","05")),
n, replace = TRUE),
route_id          = sample(paste0("ROUTE-", c("10A","17C","33B","41D","56E")),
n, replace = TRUE),
interview_date    = sample(seq(as.Date("2026-07-01"),
as.Date("2026-07-05"), by = "day"),
n, replace = TRUE),
duration_min      = c(round(runif(170, 8, 15)), # normal
round(runif(20,  1,  3)),  # suspiciously fast
round(runif(10, 45, 90))), # suspiciously slow
respondent_gender = sample(c("Woman", "Gender-diverse", "Man"),
n, replace = TRUE, prob = c(0.70, 0.10, 0.20)),
respondent_age    = c(sample(18:65, 185, replace = TRUE),
rep(NA, 15)),  # 15 missing ages
safety_perception = c(sample(1:5, 180, replace = TRUE), rep(NA, 20)),
bystander_comfort = sample(1:5, n, replace = TRUE),
harassment_exp    = sample(c("Yes","No"), n, replace = TRUE,
prob = c(0.45, 0.55)),
harassment_type   = sample(c("Verbal","Physical","Both","None"), n,
replace = TRUE),
gps_lat           = c(rnorm(185, mean = 12.9716, sd = 0.01),  # Bengaluru
rnorm(15,  mean = 13.5, sd = 0.5)),     # off-location
gps_lon           = c(rnorm(185, mean = 77.5946, sd = 0.01),
rnorm(15,  mean = 77.0, sd = 0.5))
)
survey_data <- survey_data[sample(nrow(survey_data)), ] %>%
arrange(interview_date, enumerator_id)
glimpse(survey_data)

# 1. Data Quality Checks
missing_data <- survey_data |>
summarise(across(everything(), ~ sum(is.na(.)))) |>
pivot_longer(everything(),
names_to = "variable",
values_to = "missing_count") |>
mutate(
total = nrow(survey_data),
missing_pct = round(missing_count / total * 100, 1),
flag = ifelse(missing_pct > 10, "FLAG", "OK")
) |>
arrange(desc(missing_pct))
print(missing_data)
duration_flags <- survey_data |>
mutate(
duration_flag = case_when(
duration_min < 5  ~ "TOO SHORT — possible fabrication",
duration_min > 30 ~ "TOO LONG — possible interruption or error",
TRUE              ~ "OK"
)
) |>
filter(duration_flag != "OK") |>
select(submission_id, enumerator_id, route_id,
interview_date, duration_min, duration_flag)
cat("Duration anomalies flagged:", nrow(duration_flags), "submissions")
print(duration_flags)
duration_flags <- survey_data |>
mutate(
duration_flag = case_when(
duration_min < 5  ~ "SHORT",
duration_min > 30 ~ "LONG",
TRUE              ~ "OK"
)
) |>
filter(duration_flag != "OK") |>
select(submission_id, enumerator_id, route_id,
interview_date, duration_min, duration_flag)
cat("Duration anomalies flagged:", nrow(duration_flags), "submissions")
print(duration_flags)
consistency_flags <- survey_data |>
mutate(
consistency_flag = case_when(
harassment_exp == "Yes" & harassment_type == "None" ~
"Harassment reported but type = None",
harassment_exp == "No"  & harassment_type != "None" ~
"No harassment reported but type filled",
TRUE ~ "OK"
)
) |>
filter(consistency_flag != "OK") |>
select(submission_id, enumerator_id, harassment_exp,
harassment_type, consistency_flag)
cat("Consistency issues flagged:", nrow(consistency_flags), "submissions")
print(consistency_flags)
gps_flags <- survey_data |>
mutate(
gps_flag = case_when(
abs(gps_lat - 12.9716) > 0.05 |
abs(gps_lon - 77.5946) > 0.05 ~ "Coordinates outside expected area",
TRUE ~ "OK"
)
) |>
filter(gps_flag != "OK") |>
select(submission_id, enumerator_id, route_id,
gps_lat, gps_lon, gps_flag)
cat("GPS anomalies flagged:", nrow(gps_flags), "submissions")
print(gps_flags)
#GPS coordinates of central Bangalore is 12.9716 degrees N and 77.5946 degrees E. Subracting these coordinates from the coordinates recorded in the submissions gives us an approx. distance at which the survey was conducted from the centre of Bangalore. A 5 km radius is set as the limit for the purpose of this project.

# 2. Enumerator Performance
enumerator_summary <- survey_data |>
mutate(
duration_issue = duration_min < 5 | duration_min > 30,
consistency_issue = (harassment_exp == "Yes" & harassment_type == "None") |
(harassment_exp == "No"  & harassment_type != "None"),
gps_issue = abs(gps_lat - 12.9716) > 0.05 |
abs(gps_lon - 77.5946) > 0.05,
any_flag = duration_issue | consistency_issue | gps_issue
) |>
group_by(enumerator_id) %>%
summarise(
total_submissions   = n(),
avg_duration_min    = round(mean(duration_min, na.rm = TRUE), 1),
duration_flags      = sum(duration_issue, na.rm = TRUE),
consistency_flags   = sum(consistency_issue, na.rm = TRUE),
gps_flags           = sum(gps_issue, na.rm = TRUE),
total_flags         = sum(any_flag, na.rm = TRUE),
flag_rate_pct       = round(total_flags / total_submissions * 100, 1),
missing_age_count   = sum(is.na(respondent_age)),
.groups = "drop"
) |>
arrange(desc(flag_rate_pct))
cat("Enumerator Performance Summary")
print(enumerator_summary, width = Inf)
daily_summary <- survey_data %>%
group_by(interview_date, enumerator_id) %>%
summarise(
submissions = n(),
avg_duration = round(mean(duration_min, na.rm = TRUE), 1),
flags = sum(
(duration_min < 5 | duration_min > 30) |
(harassment_exp == "Yes" & harassment_type == "None") |
(harassment_exp == "No"  & harassment_type != "None") |
(abs(gps_lat - 12.9716) > 0.05 | abs(gps_lon - 77.5946) > 0.05),
na.rm = TRUE
),
.groups = "drop"
)
cat("Daily Submissions by Enumerator")
print(daily_summary, width = Inf)
# 3. Data Visualisation
p1 <- ggplot(enumerator_summary,
aes(x = fct_reorder(enumerator_id, flag_rate_pct),
y = flag_rate_pct,
fill = flag_rate_pct > 55)) +
geom_col(width = 0.6) +
geom_text(aes(label = paste0(flag_rate_pct, "%")),
hjust = -0.2, size = 4, fontface = "bold") +
scale_fill_manual(values = c("TRUE" = "#C0392B", "FALSE" = "#2471A3"),
guide = "none") +
coord_flip() +
scale_y_continuous(limits = c(0, 80)) +
labs(
title = "Data Quality Flag Rate by Enumerator",
subtitle = "Submissions with at least one quality issue (duration, consistency, or GPS)",
x = NULL,
y = "Flag rate (%)",
caption = "Simulated field data | July 2026"
) +
theme_minimal(base_size = 13) +
theme(
plot.title = element_text(face = "bold"),
plot.subtitle = element_text(color = "#555555"),
panel.grid.major.y = element_blank()
)
print(p1)

flag_breakdown <- enumerator_summary %>%
select(enumerator_id, duration_flags, consistency_flags, gps_flags) %>%
pivot_longer(
cols = c(duration_flags, consistency_flags, gps_flags),
names_to = "flag_type",
values_to = "count"
) %>%
mutate(flag_type = recode(flag_type,
"duration_flags"     = "Duration anomaly",
"consistency_flags"  = "Consistency error",
"gps_flags"          = "GPS anomaly"
))
p2 <- ggplot(flag_breakdown,
aes(x = enumerator_id, y = count, fill = flag_type)) +
geom_col(position = "dodge", width = 0.7) +
scale_fill_manual(values = c(
"Duration anomaly"   = "#C0392B",
"Consistency error"  = "#E67E22",
"GPS anomaly"        = "#2471A3"
)) +
labs(
title = "Quality Flag Types by Enumerator",
subtitle = "Consistency errors are the dominant issue across all enumerators",
x = NULL,
y = "Number of flagged submissions",
fill = "Flag type",
caption = "Simulated field data | July 2026"
) +
theme_minimal(base_size = 13) +
theme(
plot.title = element_text(face = "bold"),
plot.subtitle = element_text(color = "#555555"),
panel.grid.major.x = element_blank(),
legend.position = "bottom"
)
print(p2)

daily_totals <- daily_summary %>%
group_by(interview_date) %>%
summarise(
total_submissions = sum(submissions),
total_flags = sum(flags),
flag_rate = round(total_flags / total_submissions * 100, 1),
.groups = "drop"
)
p3 <- ggplot(daily_totals, aes(x = interview_date)) +
geom_col(aes(y = total_submissions), fill = "#AED6F1", width = 0.6) +
geom_line(aes(y = total_flags), color = "#C0392B",
linewidth = 1.2, group = 1) +
geom_point(aes(y = total_flags), color = "#C0392B", size = 3) +
geom_text(aes(y = total_flags,
label = paste0(flag_rate, "%")),
vjust = -0.8, size = 3.5, color = "#C0392B", fontface = "bold") +
labs(
title = "Daily Submissions and Quality Flags — Field Week 1",
subtitle = "Bars = total submissions | Red line = flagged submissions | % = flag rate",
x = "Interview date",
y = "Count",
caption = "Simulated field data | July 2026"
) +
scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
theme_minimal(base_size = 13) +
theme(
plot.title = element_text(face = "bold"),
plot.subtitle = element_text(color = "#555555"),
panel.grid.major.x = element_blank()
)
print(p3)

flag_total <- survey_data |>
  summarise(
  )
flag_total <- survey_data |>
  summarise(
    duration_anomaly = sum( duration_min < 5 | duration_min > 30,
                            na.rm = TRUE),
    consistency_error = sum((harassment_exp == "Yes" & harassment_type == "None") |
                              (harassment_exp == "No"  & harassment_type != "None"),
                            na.rm = TRUE ),
    gps_anomaly = sum(abs(gps_lat - 12.9716) > 0.05 | abs(gps_lon - 77.5946) > 0.05,
                      na.rm = TRUE),
    missing_values = sum(is.na(respondent_age) |
                           is.na(safety_perception),
                         na.rm = TRUE)
  ) |>
  pivot_longer(everything(),
               names_to = "flag_type",
               values_to = "count") |>
  mutate(
    total_flags = sum(count),
    percentage = round(count/total_flags*100,1),
    label = paste0(percentage,"%\n(n=",count,")")
  )
print(flag_total)
p4 <- ggplot(flag_total, aes(x = "", y = percentage, fill = flag_type)) +
  geom_col(width = 1, color = "white", linewidth = 0.8) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c(
    duration_anomaly  = "#C0392B",
    consistency_error = "#E67E22",
    gps_anomaly       = "#2471A3",
    missing_values      = "#7D3C98")) +
  labs(
    title    = "Data Quality Flag Breakdown — Field Week 1",
    subtitle = "Consistency errors account for the largest share of quality issues",
    fill     = "Flag type",
    caption  = "Simulated field data | July 2026"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "#555555", hjust = 0.5),
    legend.position = "bottom"
  )
print(p4)

ggsave("crt_p1_flag_rate_by_enumerator.png",
plot = p1, width = 8, height = 4, dpi = 300, bg = "white")
ggsave("crt_p2_flag_types_by_enumerator.png",
plot = p2, width = 8, height = 5, dpi = 300, bg = "white")
ggsave("crt_p3_daily_submissions_flags.png",
plot = p3, width = 8, height = 4, dpi = 300, bg = "white")
ggsave("crt_p4_flag_type_breakdown.png",
plot = p4, width = 7, height = 6, dpi = 300, bg = "white")
getwd()

