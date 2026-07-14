-- Purpose: Summarize gender, age at first observation, and total follow-up.
-- Parameters: @cdm_database_schema is the schema containing the OMOP CDM tables.

WITH observation_bounds AS (
  SELECT
    person_id,
    MIN(observation_period_start_date) AS first_observation_start,
    MAX(observation_period_end_date) AS last_observation_end
  FROM @cdm_database_schema.observation_period
  GROUP BY person_id
),
person_characteristics AS (
  SELECT
    person.person_id,
    COALESCE(gender_concept.concept_name, 'Unknown') AS gender_category,
    CASE
      WHEN observation_bounds.first_observation_start IS NULL
        OR person.year_of_birth IS NULL THEN NULL
      ELSE YEAR(observation_bounds.first_observation_start) - person.year_of_birth
    END AS age_at_first_observation,
    CASE
      WHEN observation_bounds.first_observation_start IS NULL
        OR observation_bounds.last_observation_end IS NULL
        OR observation_bounds.last_observation_end
          < observation_bounds.first_observation_start THEN NULL
      ELSE DATEDIFF(
        day,
        observation_bounds.first_observation_start,
        observation_bounds.last_observation_end
      ) + 1
    END AS follow_up_days
  FROM @cdm_database_schema.person person
  LEFT JOIN observation_bounds
    ON person.person_id = observation_bounds.person_id
  LEFT JOIN @cdm_database_schema.concept gender_concept
    ON person.gender_concept_id = gender_concept.concept_id
),
categorized_people AS (
  SELECT
    person_id,
    gender_category,
    CASE
      WHEN age_at_first_observation IS NULL THEN 'Unknown'
      WHEN age_at_first_observation < 18 THEN '<18'
      WHEN age_at_first_observation < 35 THEN '18-34'
      WHEN age_at_first_observation < 50 THEN '35-49'
      WHEN age_at_first_observation < 65 THEN '50-64'
      WHEN age_at_first_observation < 75 THEN '65-74'
      ELSE '75+'
    END AS age_group,
    CASE
      WHEN follow_up_days IS NULL THEN 'Unknown'
      WHEN follow_up_days < 180 THEN '<180 days'
      WHEN follow_up_days < 365 THEN '180-364 days'
      WHEN follow_up_days < 730 THEN '365-729 days'
      ELSE '730+ days'
    END AS follow_up_group
  FROM person_characteristics
),
summaries AS (
  SELECT
    'gender' AS metric,
    gender_category AS category,
    COUNT(*) AS person_count,
    CASE
      WHEN UPPER(gender_category) = 'MALE' THEN 1
      WHEN UPPER(gender_category) = 'FEMALE' THEN 2
      WHEN gender_category = 'Unknown' THEN 99
      ELSE 3
    END AS category_order,
    1 AS metric_order
  FROM categorized_people
  GROUP BY gender_category

  UNION ALL

  SELECT
    'age_group' AS metric,
    age_group AS category,
    COUNT(*) AS person_count,
    CASE age_group
      WHEN '<18' THEN 1
      WHEN '18-34' THEN 2
      WHEN '35-49' THEN 3
      WHEN '50-64' THEN 4
      WHEN '65-74' THEN 5
      WHEN '75+' THEN 6
      ELSE 7
    END AS category_order,
    2 AS metric_order
  FROM categorized_people
  GROUP BY age_group

  UNION ALL

  SELECT
    'follow_up_group' AS metric,
    follow_up_group AS category,
    COUNT(*) AS person_count,
    CASE follow_up_group
      WHEN '<180 days' THEN 1
      WHEN '180-364 days' THEN 2
      WHEN '365-729 days' THEN 3
      WHEN '730+ days' THEN 4
      ELSE 5
    END AS category_order,
    3 AS metric_order
  FROM categorized_people
  GROUP BY follow_up_group
)
SELECT
  metric,
  category,
  person_count,
  category_order
FROM summaries
ORDER BY metric_order, category_order, category;
