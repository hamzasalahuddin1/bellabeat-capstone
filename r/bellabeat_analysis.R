# =============================================
# BELLABEAT CAPSTONE ANALYSIS
# =============================================

# Step 1 : Set working directory
setwd("~/Desktop/projects/bellabeat-capstone/data/raw")

# Step 2 : Load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(scales)

# Step 3 : Load data
daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day      <- read.csv("sleepDay_merged.csv")
hourly_steps   <- read.csv("hourlySteps_merged.csv")
weight_log     <- read.csv("weightLogInfo_merged.csv")

# Step 4 : Verify
nrow(daily_activity)
nrow(sleep_day)
nrow(hourly_steps)
nrow(weight_log)

# Fix date formats
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, 
                                       format = "%m/%d/%Y")

sleep_day$SleepDay <- as.POSIXct(sleep_day$SleepDay, 
                                 format = "%m/%d/%Y %I:%M:%S %p")

hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour, 
                                        format = "%m/%d/%Y %I:%M:%S %p")

# Add day of week column
daily_activity$day_of_week <- weekdays(daily_activity$ActivityDate)

# Add hour of day column
hourly_steps$hour_of_day <- as.integer(format(hourly_steps$ActivityHour, "%H"))

# Remove zero activity days
daily_activity_clean <- daily_activity %>%
  filter(TotalSteps > 0 & Calories > 0)

# Remove duplicate sleep rows
sleep_day_clean <- sleep_day %>%
  distinct()

# Add activity category
daily_activity_clean <- daily_activity_clean %>%
  mutate(activity_category = case_when(
    TotalSteps < 5000 ~ "Sedentary",
    TotalSteps >= 5000 & TotalSteps < 7500 ~ "Lightly Active",
    TotalSteps >= 7500 & TotalSteps < 10000 ~ "Fairly Active",
    TotalSteps >= 10000 ~ "Very Active"
  ))

# Add hours asleep column
sleep_day_clean <- sleep_day_clean %>%
  mutate(hours_asleep = TotalMinutesAsleep / 60,
         hours_in_bed = TotalTimeInBed / 60)

# Verify cleaning
nrow(daily_activity_clean)
nrow(sleep_day_clean)

# Overall summary statistics
summary_stats <- daily_activity_clean %>%
  summarise(
    total_users = n_distinct(Id),
    total_days = n(),
    avg_steps = round(mean(TotalSteps), 0),
    median_steps = round(median(TotalSteps), 0),
    avg_calories = round(mean(Calories), 0),
    avg_sedentary_hours = round(mean(SedentaryMinutes) / 60, 1),
    avg_very_active_min = round(mean(VeryActiveMinutes), 0),
    pct_below_10k = round(mean(TotalSteps < 10000) * 100, 1)
  )

print(summary_stats)

# Sleep summary
sleep_summary <- sleep_day_clean %>%
  summarise(
    sleep_users = n_distinct(Id),
    avg_hours_asleep = round(mean(hours_asleep), 2),
    pct_under_7h = round(mean(hours_asleep < 7) * 100, 1),
    avg_wasted_min = round(mean(TotalTimeInBed - TotalMinutesAsleep), 0)
  )

print(sleep_summary)

# Create visualizations folder
dir.create("~/Desktop/projects/bellabeat-capstone/visualizations", 
           showWarnings = FALSE)

daily_activity_clean$StepCategory <- ifelse(
  daily_activity_clean$TotalSteps < 10000,
  "Below 10k",
  "Above 10k"
)

#-----------------------------------------
# Distribution of Daily Steps
#-----------------------------------------


ggplot(daily_activity_clean, aes(x = TotalSteps, fill = StepCategory)) +
  geom_histogram(binwidth = 1000, color = "white", alpha = 0.85) +
  geom_vline(xintercept = 10000, color = "#1A3A5C", 
             linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(daily_activity_clean$TotalSteps),
             color = "#444444", linetype = "solid", linewidth = 1) +
  annotate("text", x = 11000, y = 80, 
           label = "WHO Goal: 10,000", 
           color = "#1A3A5C", size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Below 10k" = "#E76F51", 
                               "Above 10k" = "#2A9D8F")) +
  labs(
    title = "Distribution of Daily Steps",
    subtitle = "67.9% of days fall below the WHO recommended 10,000 steps",
    x = "Total Daily Steps",
    y = "Number of Days",
    
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"))


ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/distribution_of_daily_steps.png",
       width = 8, height = 5.5, dpi = 300)


#-----------------------------------------
# Average Daily Steps by Day of Week
#-----------------------------------------

# Order days correctly
day_order <- c("Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday", "Sunday")

daily_activity_clean$day_of_week <- factor(daily_activity_clean$day_of_week, 
                                           levels = day_order)

# Calculate average steps by day
steps_by_day <- daily_activity_clean %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = round(mean(TotalSteps), 0))

# Find most active day
most_active <- steps_by_day$day_of_week[which.max(steps_by_day$avg_steps)]

# Create chart
ggplot(steps_by_day, aes(x = day_of_week, y = avg_steps, 
                         fill = day_of_week == most_active)) +
  geom_col(color = "white", width = 0.65) +
  scale_fill_manual(values = c("TRUE" = "#E76F51", "FALSE" = "#2A9D8F")) +
  geom_text(aes(label = comma(avg_steps)), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    title = "Average Daily Steps by Day of Week",
    subtitle = paste("Most active day:", most_active),
    x = "Day of Week",
    y = "Average Steps",
   
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", color = "#1A3A5C")) +
  scale_y_continuous(labels = comma)

ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/02_steps_by_day.png",
       width = 10, height = 5.5, dpi = 300)



# ---------------------------------------------
# Average Steps by Hour of Day
# ---------------------------------------------

# Average steps by hour
hourly_avg <- hourly_steps %>%
  group_by(hour_of_day) %>%
  summarise(avg_steps = round(mean(StepTotal), 0))

# Create hour labels
hour_labels <- c("12am","1am","2am","3am","4am","5am",
                 "6am","7am","8am","9am","10am","11am",
                 "12pm","1pm","2pm","3pm","4pm","5pm",
                 "6pm","7pm","8pm","9pm","10pm","11pm")

# Find peak hour
peak_hour <- hourly_avg$hour_of_day[which.max(hourly_avg$avg_steps)]

ggplot(hourly_avg, aes(x = hour_of_day, y = avg_steps,
                       fill = hour_of_day == peak_hour)) +
  geom_col(color = "white", width = 0.8) +
  scale_fill_manual(values = c("TRUE" = "#E76F51", "FALSE" = "#2A9D8F")) +
  scale_x_continuous(breaks = 0:23, labels = hour_labels) +
  labs(
    title = "Average Steps by Hour of Day",
    subtitle = "Peak activity in the evening after work hours",
    x = "Hour of Day",
    y = "Average Steps",
  
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold", color = "#1A3A5C"))

ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/03_hourly_steps.png",
       width = 13, height = 5.5, dpi = 300)



# ---------------------------------------------
# Calculate correlation (r and p-value)
# ---------------------------------------------
cor_test <- cor.test(daily_activity_clean$TotalSteps, 
                     daily_activity_clean$Calories)

r_value <- cor_test$estimate
p_value <- cor_test$p.value

# Create label
label_text <- paste0("r = ", round(r_value, 2), 
                     ", p = ", signif(p_value, 3))

# ---------------------------------------------
# Steps vs Calories Correlation
# ---------------------------------------------
p4 <- ggplot(daily_activity_clean, 
             aes(x = TotalSteps, y = Calories, 
                 color = activity_category)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "#1A3A5C", se = TRUE) +
  
  # Add correlation label
  annotate("text", 
           x = max(daily_activity_clean$TotalSteps) * 0.6, 
           y = max(daily_activity_clean$Calories) * 0.95,
           label = label_text,
           hjust = 0, size = 4, fontface = "bold",
           color = "#1A3A5C") +
  
  scale_color_manual(values = c(
    "Sedentary"      = "#AED6F1",
    "Lightly Active" = "#2A9D8F",
    "Fairly Active"  = "#E9C46A",
    "Very Active"    = "#1A3A5C"
  )) +
  labs(
    title = "Daily Steps vs Calories Burned",
    subtitle = "Positive relationship between physical activity and calories burned",
    x = "Total Daily Steps",
    y = "Calories Burned",
    color = "Activity Level",
  
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C")) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

# Show plot
p4

# Save plot
ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/04_steps_vs_calories.png",
       p4, width = 9, height = 6, dpi = 300)

# ---------------------------------------------
# Distribution of Daily Sleep Hours
# ---------------------------------------------

avg_sleep <- mean(sleep_day_clean$hours_asleep)
pct_under7 <- round(mean(sleep_day_clean$hours_asleep < 7) * 100, 1)

ggplot(sleep_day_clean, aes(x = hours_asleep)) +
  geom_histogram(binwidth = 0.5, fill = "#7D3C98", 
                 color = "white", alpha = 0.85) +
  geom_vline(xintercept = 7, color = "#E76F51", 
             linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 9, color = "#E76F51", 
             linetype = "dashed", linewidth = 1) +
  annotate("rect", xmin = 7, xmax = 9, ymin = 0, ymax = Inf,
           fill = "#E76F51", alpha = 0.1) +
  annotate("text", x = 7.2, y = 60, 
           label = "Recommended\n7 to 9 hours",
           color = "#E76F51", size = 3.5, fontface = "bold") +
  labs(
    title = "Distribution of Daily Sleep Hours",
    subtitle = paste0(pct_under7, "% of nights fall below the recommended 7 hours"),
    x = "Hours Asleep",
    y = "Number of Records",
  
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"))

ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/05_sleep_distribution.png",
       width = 9, height = 5.5, dpi = 300)



# ---------------------------------------------
# Time in Bed vs Time Asleep
# ---------------------------------------------

# Prepare data
sleep_day_clean <- sleep_day_clean %>%
  mutate(
    hours_asleep = TotalMinutesAsleep / 60,
    hours_in_bed = TotalTimeInBed / 60,
    time_awake = hours_in_bed - hours_asleep
  )

# Calculate average awake time
avg_awake <- mean(sleep_day_clean$time_awake)
avg_label <- paste0("Avg. ", round(avg_awake * 60), 
                    " min/night\nin bed but awake")

# Create plot
p_sleep <- ggplot(sleep_day_clean, 
                  aes(x = hours_in_bed, y = hours_asleep)) +
  
  geom_point(color = "#2A9D8F", alpha = 0.6) +
  
  # Perfect sleep line
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "gray40") +
  
  # Annotation
  annotate("label",
           x = 3,
           y = max(sleep_day_clean$hours_asleep) * 0.95,
           label = avg_label,
           fill = "#FDECEC",
           color = "#E76F51",
           label.size = 0.5) +
  
  labs(
    title = "Time in Bed vs Time Asleep",
    subtitle = "Gap between time in bed and actual sleep duration",
    x = "Hours in Bed",
    y = "Hours Asleep",
   
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#1A3A5C")
  )

# Show plot
p_sleep

# Save plot
ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/05_sleep_efficiency.png",
       p_sleep, width = 9, height = 6, dpi = 300)


# ---------------------------------------------
# User Segmentation
# ---------------------------------------------

# Create user segments based on average steps
user_segments <- daily_activity_clean %>%
  group_by(Id) %>%
  summarise(avg_steps = mean(TotalSteps)) %>%
  mutate(segment = case_when(
    avg_steps < 5000 ~ "Sedentary",
    avg_steps >= 5000 & avg_steps < 7500 ~ "Lightly Active",
    avg_steps >= 7500 & avg_steps < 10000 ~ "Fairly Active",
    avg_steps >= 10000 ~ "Very Active"
  ))

# Count segments
segment_counts <- user_segments %>%
  count(segment) %>%
  mutate(pct = round(n / sum(n) * 100, 0),
         label = paste0(segment, "\n", pct, "%"))

# Create donut chart
ggplot(segment_counts, 
       aes(x = 2, y = n, fill = segment)) +
  geom_col(color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c(
    "Sedentary"      = "#AED6F1",
    "Lightly Active" = "#2A9D8F",
    "Fairly Active"  = "#E9C46A",
    "Very Active"    = "#1A3A5C"
  )) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, fontface = "bold", color = "white") +
  labs(
    title = "User Segmentation by Activity Level",
    subtitle = "Based on average daily steps",
   
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C",
                                  hjust = 0.5),
        legend.position = "none")

ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/06_user_segments.png",
       width = 7, height = 7, dpi = 300)


# ---------------------------------------------
# Daily Activity Minutes Breakdown
# ---------------------------------------------


# Activity Minutes Breakdown
avg_mins <- data.frame(
  category = c("Very Active", "Fairly Active", 
               "Lightly Active", "Sedentary"),
  minutes = c(
    mean(daily_activity_clean$VeryActiveMinutes),
    mean(daily_activity_clean$FairlyActiveMinutes),
    mean(daily_activity_clean$LightlyActiveMinutes),
    mean(daily_activity_clean$SedentaryMinutes)
  )
)

avg_mins$hours <- round(avg_mins$minutes / 60, 1)
avg_mins$label <- paste0(round(avg_mins$minutes, 0), 
                         " min (", avg_mins$hours, "h)")

avg_mins$category <- factor(avg_mins$category, 
                            levels = c("Very Active", 
                                       "Fairly Active",
                                       "Lightly Active", 
                                       "Sedentary"))

ggplot(avg_mins, aes(x = category, y = minutes, fill = category)) +
  geom_col(color = "white", width = 0.55) +
  coord_flip() +
  geom_text(aes(label = label), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Sedentary"      = "#D5D8DC",
    "Lightly Active" = "#A8DADC",
    "Fairly Active"  = "#2A9D8F",
    "Very Active"    = "#1A3A5C"
  )) +
  labs(
    title = "Average Daily Activity Minutes Breakdown",
    subtitle = "Users spend the vast majority of their day sedentary",
    x = NULL,
    y = "Average Minutes per Day",
   
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", color = "#1A3A5C"),
    plot.subtitle = element_text(color = "#666666")
  ) +
  scale_y_continuous(limits = c(0, 1100))

ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/09_activity_minutes.png",
       width = 10, height = 5.5, dpi = 300)



# ---------------------------------------------
# Weight Tracking Adoption
# ---------------------------------------------


# Weight Tracking Adoption
total_users <- n_distinct(daily_activity_clean$Id)
weight_users <- n_distinct(weight_log$Id)
non_weight_users <- total_users - weight_users

weight_adoption <- data.frame(
  category = c("Tracked Weight", "Did Not Track"),
  users = c(weight_users, non_weight_users)
)

weight_adoption$pct <- round(weight_adoption$users / total_users * 100, 0)
weight_adoption$label <- paste0(weight_adoption$users, 
                                "\n(", weight_adoption$pct, "%)")

ggplot(weight_adoption, aes(x = category, y = users, fill = category)) +
  geom_col(color = "white", width = 0.5) +
  geom_text(aes(label = label),
            vjust = -0.3, size = 4.5, 
            fontface = "bold", color = "#1A3A5C") +
  scale_fill_manual(values = c(
    "Tracked Weight"  = "#2A9D8F",
    "Did Not Track"   = "#D5D8DC"
  )) +
  labs(
    title = "Weight Tracking Adoption",
    subtitle = paste0("Only ", 
                      round(weight_users/total_users*100, 0), 
                      "% of users logged weight data, a significant engagement gap"),
    x = NULL,
    y = "Number of Users",

  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", color = "#1A3A5C"),
    plot.subtitle = element_text(color = "#666666")
  ) +
  scale_y_continuous(limits = c(0, total_users * 1.25))

ggsave("~/Desktop/projects/bellabeat-capstone/visualizations/10_weight_tracking.png",
       width = 8, height = 5.5, dpi = 300)
