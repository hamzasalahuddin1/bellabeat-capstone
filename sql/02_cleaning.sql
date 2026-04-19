
-- ================================================

-- BELLABEAT CAPSTONE : PHASE 3 DATA CLEANING
-- Analyst : Hamza Salahuddin

-- ================================================

-- Cleaning Query 1 : Create clean daily activity table
-- Purpose : Remove zero activity days, add derived columns
-- Result  : 396 rows, 34 users

CREATE OR REPLACE TABLE fitbit.dailyActivity_clean AS
SELECT
  CAST(Id AS STRING) AS user_id,
  ActivityDate AS activity_date,
  FORMAT_DATE('%A', ActivityDate) AS day_of_week,
  TotalSteps AS total_steps,
  TotalDistance AS total_distance_km,
  VeryActiveMinutes AS very_active_min,
  FairlyActiveMinutes AS fairly_active_min,
  LightlyActiveMinutes AS lightly_active_min,
  SedentaryMinutes AS sedentary_min,
  Calories AS calories_burned,
  CASE
    WHEN TotalSteps < 5000 THEN 'Sedentary'
    WHEN TotalSteps BETWEEN 5000 AND 7499 THEN 'Lightly Active'
    WHEN TotalSteps BETWEEN 7500 AND 9999 THEN 'Fairly Active'
    WHEN TotalSteps >= 10000 THEN 'Very Active'
  END AS activity_category
FROM fitbit.dailyActivity_merged
WHERE TotalSteps > 0
  AND Calories > 0;

-- Verify clean daily activity table
SELECT
  COUNT(*) AS total_rows,
  COUNT(DISTINCT user_id) AS total_users,
  MIN(activity_date) AS earliest_date,
  MAX(activity_date) AS latest_date
FROM fitbit.dailyActivity_clean;

-- Cleaning Query 2 : Create clean sleep table
-- Purpose : Remove 3 duplicates, convert dates, add hours columns
-- Result  : 410 rows, 24 users

CREATE OR REPLACE TABLE fitbit.sleepDay_clean AS
SELECT DISTINCT
  CAST(Id AS STRING) AS user_id,
  PARSE_DATETIME('%m/%d/%Y %I:%M:%S %p', SleepDay) AS sleep_datetime,
  DATE(PARSE_DATETIME('%m/%d/%Y %I:%M:%S %p', SleepDay)) AS sleep_date,
  TotalSleepRecords AS total_sleep_records,
  TotalMinutesAsleep AS total_minutes_asleep,
  TotalTimeInBed AS total_time_in_bed,
  ROUND(TotalMinutesAsleep / 60.0, 2) AS hours_asleep,
  ROUND(TotalTimeInBed / 60.0, 2) AS hours_in_bed,
  ROUND((TotalTimeInBed - TotalMinutesAsleep) / 60.0, 2) AS wasted_bed_hours
FROM fitbit.sleepDay_merged;

-- Verify clean sleep table
SELECT
  COUNT(*) AS total_rows,
  COUNT(DISTINCT user_id) AS total_users
FROM fitbit.sleepDay_clean;

-- Cleaning Query 3 : Create clean hourly steps table
-- Purpose : Convert datetime, extract hour of day
-- Result  : 24,084 rows, 34 users

CREATE OR REPLACE TABLE fitbit.hourlySteps_clean AS
SELECT
  CAST(Id AS STRING) AS user_id,
  PARSE_DATETIME('%m/%d/%Y %I:%M:%S %p', ActivityHour) AS activity_datetime,
  EXTRACT(HOUR FROM PARSE_DATETIME('%m/%d/%Y %I:%M:%S %p',
    ActivityHour)) AS hour_of_day,
  StepTotal AS steps
FROM fitbit.hourlySteps_merged;

-- Verify clean hourly steps table
SELECT
  COUNT(*) AS total_rows,
  COUNT(DISTINCT user_id) AS total_users
FROM fitbit.hourlySteps_clean;

-- Cleaning Query 4 : Merge activity and sleep tables
-- Purpose : Combined analysis of activity and sleep patterns
-- Result  : 11 rows, 11 users

CREATE OR REPLACE TABLE fitbit.activity_sleep_merged AS
SELECT
  a.user_id,
  a.activity_date,
  a.day_of_week,
  a.total_steps,
  a.very_active_min,
  a.fairly_active_min,
  a.lightly_active_min,
  a.sedentary_min,
  a.calories_burned,
  a.activity_category,
  s.hours_asleep,
  s.hours_in_bed,
  s.wasted_bed_hours
FROM fitbit.dailyActivity_clean a
INNER JOIN fitbit.sleepDay_clean s
ON a.user_id = s.user_id
AND a.activity_date = s.sleep_date;

-- Verify merged table
SELECT
  COUNT(*) AS total_rows,
  COUNT(DISTINCT user_id) AS total_users
FROM fitbit.activity_sleep_merged;

-- User summary with activity classification
CREATE OR REPLACE TABLE fitbit.user_summary AS
SELECT
  user_id,
  ROUND(AVG(total_steps), 0) AS avg_daily_steps,
  ROUND(AVG(calories_burned), 0) AS avg_calories,
  ROUND(AVG(very_active_min), 1) AS avg_very_active_min,
  ROUND(AVG(sedentary_min), 1) AS avg_sedentary_min,
  COUNT(*) AS days_tracked,
  CASE
    WHEN AVG(total_steps) < 5000 THEN 'Sedentary'
    WHEN AVG(total_steps) BETWEEN 5000 AND 7499 THEN 'Lightly Active'
    WHEN AVG(total_steps) BETWEEN 7500 AND 9999 THEN 'Fairly Active'
    WHEN AVG(total_steps) >= 10000 THEN 'Very Active'
  END AS activity_category
FROM fitbit.dailyActivity_clean
GROUP BY user_id;