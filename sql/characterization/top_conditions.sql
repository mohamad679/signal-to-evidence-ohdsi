-- Purpose: Identify the 20 most frequent non-zero condition concepts.
-- Parameters: @cdm_database_schema is the schema containing the OMOP CDM tables.

SELECT TOP 20
  condition_occurrence.condition_concept_id AS concept_id,
  concept.concept_name AS concept_name,
  COUNT(*) AS occurrence_count,
  COUNT(DISTINCT condition_occurrence.person_id) AS affected_person_count
FROM @cdm_database_schema.condition_occurrence condition_occurrence
INNER JOIN @cdm_database_schema.concept concept
  ON condition_occurrence.condition_concept_id = concept.concept_id
WHERE condition_occurrence.condition_concept_id <> 0
  AND concept.concept_name IS NOT NULL
GROUP BY
  condition_occurrence.condition_concept_id,
  concept.concept_name
ORDER BY
  occurrence_count DESC,
  affected_person_count DESC,
  concept_id ASC;
