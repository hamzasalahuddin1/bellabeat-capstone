
-- ================================================

-- BELLABEAT CAPSTONE : PHASE 3 DATA EXPLORATION
-- Analyst : Hamza Salahuddin

-- ================================================

-- Query 1 : Row counts and date range
-- Purpose : Confirm dataset size and collection timeframe
-- Result  : 457 rows, 35 users, Mar 12 to Apr 12 2016
SELECT
  COUNT(*) AS total_rows,
  COUNT(DISTINCT Id) AS unique_users,
  MIN(ActivityDate) AS earliest_date,
  MAX(ActivityDate) AS latest_date
FROM fitbit.dailyActivity_merged;

-- Query 2a : Unique users in daily activity
-- Purpose : Understand feature participation across tables
-- Result  : 35 users
SELECT COUNT(DISTINCT Id) AS daily_users
FROM fitbit.dailyActivity_merged;

-- Query 2b : Unique users in sleep data
-- Result : 24 users
SELECT COUNT(DISTINCT Id) AS sleep_users
FROM fitbit.sleepDay_merged;

-- Query 2c : Unique users in hourly steps
-- Result : 34 users
SELECT COUNT(DISTINCT Id) AS hourly_users
FROM fitbit.hourlySteps_merged;

-- Query 2d : Unique users in weight log
-- Result : 11 users
SELECT COUNT(DISTINCT Id) AS weight_users
FROM fitbit.weightLogInfo_merged;

-- Key insight : Passive tracking has near full adoption
-- Active logging drops to as low as 31 percent

-- Query 3 : Preview raw data
-- Purpose : Understand column names, data types and values
SELECT *
FROM fitbit.dailyActivity_merged
LIMIT 10;

-- Query 4 : Zero activity days
-- Purpose : Find days where device was likely not worn
-- Result  : 61 rows with zero steps or zero calories
SELECT
  COUNT(*) AS zero_activity_days
FROM fitbit.dailyActivity_merged
WHERE TotalSteps = 0
  OR Calories = 0;

-- Query 5 : Duplicate check in daily activity
-- Purpose : Find repeated rows for same user and date
-- Result  : No duplicates found
SELECT
  Id,
  ActivityDate,
  COUNT(*) AS duplicate_count
FROM fitbit.dailyActivity_merged
GROUP BY Id, ActivityDate
HAVING COUNT(*) > 1;

-- Query 6 : Duplicate check in sleep data
-- Purpose : Find repeated sleep records for same user and date
-- Result  : 3 duplicate rows found
SELECT
  Id,
  SleepDay,
  COUNT(*) AS duplicate_count
FROM fitbit.sleepDay_merged
GROUP BY Id, SleepDay
HAVING COUNT(*) > 1;

-- Query 7 : NULL values check
-- Purpose : Find missing values in key columns
-- Result  : Zero NULLs found in all key columns
SELECT
  COUNTIF(Id IS NULL) AS null_id,
  COUNTIF(ActivityDate IS NULL) AS null_date,
  COUNTIF(TotalSteps IS NULL) AS null_steps,
  COUNTIF(Calories IS NULL) AS null_calories,
  COUNTIF(SedentaryMinutes IS NULL) AS null_sedentary
FROM fitbit.dailyActivity_merged;