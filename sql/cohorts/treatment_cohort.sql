-- Index date: earliest qualifying target or comparator exposure per person.
-- Washout: no target or comparator exposure during days -@washout_days through -1.
-- Risk window: cohort end is day @risk_window_end_days, capped at observation end.
CREATE TEMPORARY TABLE @table_name AS
WITH study_exposures AS (
  SELECT
    drug_exposure.drug_exposure_id,
    drug_exposure.person_id,
    drug_exposure.drug_concept_id AS treatment_concept_id,
    drug_exposure.drug_exposure_start_date
  FROM @database_schema.drug_exposure drug_exposure
  WHERE drug_exposure.drug_concept_id IN (
    @target_concept_id,
    @comparator_concept_id
  )
),
study_exposure_dates AS (
  SELECT DISTINCT
    study_exposures.person_id,
    study_exposures.drug_exposure_start_date
  FROM study_exposures
),
exposure_history AS (
  SELECT
    study_exposure_dates.person_id,
    study_exposure_dates.drug_exposure_start_date,
    LAG(study_exposure_dates.drug_exposure_start_date) OVER (
      PARTITION BY study_exposure_dates.person_id
      ORDER BY study_exposure_dates.drug_exposure_start_date ASC
    ) AS previous_exposure_date
  FROM study_exposure_dates
),
first_outcomes AS (
  SELECT
    condition_occurrence.person_id,
    MIN(condition_occurrence.condition_start_date) AS first_outcome_date
  FROM @database_schema.condition_occurrence condition_occurrence
  WHERE condition_occurrence.condition_concept_id = @outcome_concept_id
  GROUP BY condition_occurrence.person_id
),
qualifying_exposure_observation_periods AS (
  SELECT
    exposure.drug_exposure_id,
    exposure.person_id,
    exposure.treatment_concept_id,
    exposure.drug_exposure_start_date,
    observation.observation_period_id,
    ROW_NUMBER() OVER (
      PARTITION BY exposure.drug_exposure_id
      ORDER BY
        observation.observation_period_start_date DESC,
        observation.observation_period_id ASC
    ) AS observation_period_number
  FROM study_exposures exposure
  INNER JOIN @database_schema.observation_period observation
    ON exposure.person_id = observation.person_id
    AND exposure.drug_exposure_start_date >=
      observation.observation_period_start_date
    AND exposure.drug_exposure_start_date <=
      observation.observation_period_end_date
  WHERE DATEDIFF(
      day,
      observation.observation_period_start_date,
      exposure.drug_exposure_start_date
    ) >= @prior_observation_days
),
resolved_exposures AS (
  SELECT
    qualifying_exposure_observation_periods.drug_exposure_id,
    qualifying_exposure_observation_periods.person_id,
    qualifying_exposure_observation_periods.treatment_concept_id,
    qualifying_exposure_observation_periods.drug_exposure_start_date,
    qualifying_exposure_observation_periods.observation_period_id
  FROM qualifying_exposure_observation_periods
  WHERE qualifying_exposure_observation_periods.observation_period_number = 1
),
eligible_exposures AS (
  SELECT
    exposure.drug_exposure_id,
    exposure.person_id,
    exposure.treatment_concept_id,
    exposure.drug_exposure_start_date,
    exposure.observation_period_id
  FROM resolved_exposures exposure
  INNER JOIN exposure_history history
    ON exposure.person_id = history.person_id
    AND exposure.drug_exposure_start_date =
      history.drug_exposure_start_date
  LEFT JOIN first_outcomes first_outcome
    ON exposure.person_id = first_outcome.person_id
  WHERE (
      history.previous_exposure_date IS NULL
      OR history.previous_exposure_date < DATEADD(
        day,
        -@washout_days,
        exposure.drug_exposure_start_date
      )
    )
    AND (
      first_outcome.first_outcome_date IS NULL
      OR first_outcome.first_outcome_date >
        exposure.drug_exposure_start_date
    )
),
ranked_treatment_entries AS (
  SELECT
    eligible_exposures.drug_exposure_id,
    eligible_exposures.person_id,
    eligible_exposures.treatment_concept_id,
    eligible_exposures.drug_exposure_start_date,
    eligible_exposures.observation_period_id,
    ROW_NUMBER() OVER (
      PARTITION BY eligible_exposures.person_id
      ORDER BY
        eligible_exposures.drug_exposure_start_date ASC,
        eligible_exposures.drug_exposure_id ASC
    ) AS treatment_entry_number
  FROM eligible_exposures
),
selected_treatment_entries AS (
  SELECT
    ranked_treatment_entries.person_id,
    ranked_treatment_entries.treatment_concept_id,
    ranked_treatment_entries.drug_exposure_start_date,
    ranked_treatment_entries.observation_period_id
  FROM ranked_treatment_entries
  WHERE ranked_treatment_entries.treatment_entry_number = 1
)
SELECT
  @cohort_definition_id AS cohort_definition_id,
  selected_treatment_entries.person_id AS subject_id,
  selected_treatment_entries.drug_exposure_start_date AS cohort_start_date,
  CASE
    WHEN DATEADD(
      day,
      @risk_window_end_days,
      selected_treatment_entries.drug_exposure_start_date
    ) < selected_observation.observation_period_end_date
      THEN DATEADD(
        day,
        @risk_window_end_days,
        selected_treatment_entries.drug_exposure_start_date
      )
    ELSE selected_observation.observation_period_end_date
  END AS cohort_end_date
FROM selected_treatment_entries
INNER JOIN @database_schema.observation_period selected_observation
  ON selected_treatment_entries.observation_period_id =
    selected_observation.observation_period_id
WHERE selected_treatment_entries.treatment_concept_id = @treatment_concept_id;
