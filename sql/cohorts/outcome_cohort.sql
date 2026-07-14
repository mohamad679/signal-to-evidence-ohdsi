-- Outcome occurrences are retained individually for later risk-window assignment.
CREATE TEMPORARY TABLE @table_name AS
SELECT
  @cohort_definition_id AS cohort_definition_id,
  condition_occurrence.person_id AS subject_id,
  condition_occurrence.condition_start_date AS cohort_start_date,
  condition_occurrence.condition_start_date AS cohort_end_date
FROM @database_schema.condition_occurrence condition_occurrence
WHERE condition_occurrence.condition_concept_id = @outcome_concept_id
  AND EXISTS (
    SELECT 1
    FROM @database_schema.observation_period observation
    WHERE observation.person_id = condition_occurrence.person_id
      AND condition_occurrence.condition_start_date >=
        observation.observation_period_start_date
      AND condition_occurrence.condition_start_date <=
        observation.observation_period_end_date
  );
