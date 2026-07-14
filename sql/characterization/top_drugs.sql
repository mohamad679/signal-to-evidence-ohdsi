-- Purpose: Identify the 20 most frequent non-zero drug concepts.
-- Parameters: @cdm_database_schema is the schema containing the OMOP CDM tables.

SELECT TOP 20
  drug_exposure.drug_concept_id AS concept_id,
  concept.concept_name AS concept_name,
  COUNT(*) AS exposure_count,
  COUNT(DISTINCT drug_exposure.person_id) AS exposed_person_count
FROM @cdm_database_schema.drug_exposure drug_exposure
INNER JOIN @cdm_database_schema.concept concept
  ON drug_exposure.drug_concept_id = concept.concept_id
WHERE drug_exposure.drug_concept_id <> 0
  AND concept.concept_name IS NOT NULL
GROUP BY
  drug_exposure.drug_concept_id,
  concept.concept_name
ORDER BY
  exposure_count DESC,
  exposed_person_count DESC,
  concept_id ASC;
