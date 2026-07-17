# Signal to Evidence OHDSI Demonstration

This repository is a reproducible methodological OHDSI/OMOP demonstration
using Eunomia sample data. It implements a prespecified observational-study
workflow from feasibility assessment through cohort construction, covariate
adjustment, outcome estimation, subgroup and sensitivity analyses, aggregate
reporting, and continuous integration.

## Scope and interpretation

This project is not clinical evidence, regulatory evidence, or a drug-safety
conclusion. Eunomia is sample demonstration data and does not represent a
clinical population. The analysis does not make a causal claim and must not be
interpreted as evidence of benefit, harm, equivalence, safety, or
effectiveness.

## Frozen study design

- Target exposure: Celecoxib
- Comparator exposure: Diclofenac
- Outcome: Gastrointestinal hemorrhage
- Primary risk window: Days 1 through 30 after index
- Estimand: Average treatment effect in the treated
- Adjustment: 1:1 propensity-score matching with preference-score trimming
- Effect measure: Odds ratio
- Uncertainty: Matched-set cluster-robust 95% confidence interval

## Reproducibility

The R environment is recorded in `renv.lock`. The CI runner supports:

```bash
Rscript scripts/10_ci_validate.R --mode=parse
Rscript scripts/10_ci_validate.R --mode=lint
Rscript scripts/10_ci_validate.R --mode=test
Rscript scripts/10_ci_validate.R --mode=pipeline
Rscript scripts/10_ci_validate.R --mode=security
Rscript scripts/10_ci_validate.R --mode=report
```

The frozen pipeline consists of the numbered scripts `01` through `09`.
The reporting contract contains exactly 15 aggregate CSV tables. Figures are
embedded in the rendered Quarto HTML reports:

- `_site/reports/index.html`
- `_site/reports/executive_summary.html`

## Privacy and security

Raw and derived databases are excluded from version control. CI validates
aggregate-only reporting, disclosure-safety rules, tracked-path restrictions,
secret patterns, read-only workflow permissions, action references, and
rendered-report publication scope.

## Citation

Citation metadata are provided in `CITATION.cff`.

## License

This project is distributed under the MIT License.
