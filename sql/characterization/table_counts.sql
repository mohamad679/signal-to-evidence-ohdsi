-- Purpose: Count records and distinct people in core OMOP CDM tables.
-- Parameters: @cdm_database_schema is the schema containing the OMOP CDM tables.

SELECT
  table_name,
  record_count,
  person_count
FROM (
  SELECT
    'person' AS table_name,
    COUNT(*) AS record_count,
    COUNT(*) AS person_count,
    1 AS table_order
  FROM @cdm_database_schema.person

  UNION ALL

  SELECT
    'observation_period' AS table_name,
    COUNT(*) AS record_count,
    COUNT(DISTINCT person_id) AS person_count,
    2 AS table_order
  FROM @cdm_database_schema.observation_period

  UNION ALL

  SELECT
    'drug_exposure' AS table_name,
    COUNT(*) AS record_count,
    COUNT(DISTINCT person_id) AS person_count,
    3 AS table_order
  FROM @cdm_database_schema.drug_exposure

  UNION ALL

  SELECT
    'condition_occurrence' AS table_name,
    COUNT(*) AS record_count,
    COUNT(DISTINCT person_id) AS person_count,
    4 AS table_order
  FROM @cdm_database_schema.condition_occurrence

  UNION ALL

  SELECT
    'visit_occurrence' AS table_name,
    COUNT(*) AS record_count,
    COUNT(DISTINCT person_id) AS person_count,
    5 AS table_order
  FROM @cdm_database_schema.visit_occurrence

  UNION ALL

  SELECT
    'measurement' AS table_name,
    COUNT(*) AS record_count,
    COUNT(DISTINCT person_id) AS person_count,
    6 AS table_order
  FROM @cdm_database_schema.measurement
) table_counts
ORDER BY table_order;
