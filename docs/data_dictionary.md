# Data dictionary

## Feasibility matrix

`results/tables/feasibility_matrix.csv` contains one aggregate row for each unordered pair of
candidate drug concepts and each candidate outcome concept. It contains no person identifiers or
dates. Eunomia is synthetic/sample demonstration data; these results are methodological and are
not clinical evidence.

Each entry gives the CSV type, meaning, and whether the field is an engineering feasibility
measure.

- `target_concept_id`
  - Type: numeric whole number.
  - Meaning: OMOP concept ID for the first drug in the deterministically ordered pair.
  - Engineering feasibility measure: no; descriptor only.
- `target_concept_name`
  - Type: character.
  - Meaning: OMOP concept name for the first drug.
  - Engineering feasibility measure: no; descriptor only.
- `comparator_concept_id`
  - Type: numeric whole number.
  - Meaning: OMOP concept ID for the second drug in the deterministically ordered pair.
  - Engineering feasibility measure: no; descriptor only.
- `comparator_concept_name`
  - Type: character.
  - Meaning: OMOP concept name for the second drug.
  - Engineering feasibility measure: no; descriptor only.
- `outcome_concept_id`
  - Type: numeric whole number.
  - Meaning: OMOP condition concept ID evaluated as the outcome.
  - Engineering feasibility measure: no; descriptor only.
- `outcome_concept_name`
  - Type: character.
  - Meaning: OMOP concept name for the evaluated condition outcome.
  - Engineering feasibility measure: no; descriptor only.
- `target_subject_count`
  - Type: numeric whole number.
  - Meaning: eligible people assigned to the target arm.
  - Engineering feasibility measure: yes; aggregate arm-size measure.
- `comparator_subject_count`
  - Type: numeric whole number.
  - Meaning: eligible people assigned to the comparator arm.
  - Engineering feasibility measure: yes; aggregate arm-size measure.
- `target_outcome_count`
  - Type: numeric whole number.
  - Meaning: target-arm people with a qualifying risk-window outcome.
  - Engineering feasibility measure: yes; aggregate event-count measure.
- `comparator_outcome_count`
  - Type: numeric whole number.
  - Meaning: comparator-arm people with a qualifying risk-window outcome.
  - Engineering feasibility measure: yes; aggregate event-count measure.
- `median_prior_observation_days`
  - Type: numeric.
  - Meaning: median prior-observation days across both arms; missing if both are empty.
  - Engineering feasibility measure: yes; aggregate observable-history measure.
- `total_outcome_count`
  - Type: numeric whole number.
  - Meaning: sum of target and comparator outcome counts.
  - Engineering feasibility measure: yes; aggregate event-count measure.
- `feasible`
  - Type: logical.
  - Meaning: whether all configured arm-size and outcome-count thresholds were met.
  - Engineering feasibility measure: yes; engineering screening flag.
- `feasibility_reason`
  - Type: character.
  - Meaning: passed-threshold statement or deterministic list of failed thresholds.
  - Engineering feasibility measure: yes; engineering screening explanation.

The `feasible` flag does not imply clinical validity, scientific preference, a treatment effect,
or a causal interpretation. It only indicates that prespecified engineering thresholds were met
in demonstration data. Candidate combinations are not ranked by significance or effect size.
