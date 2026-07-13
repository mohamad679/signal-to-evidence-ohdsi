# AGENTS.md

## Project purpose

This repository implements a reproducible methodological demonstration of
an OHDSI/OMOP new-user active-comparator drug-safety study using Eunomia
sample data.

The repository is a research portfolio project. It must not claim clinical
validity or causal proof.

## Technology

- Implementation language: R
- Query language: SQL
- Documentation and configuration formats may include Markdown, YAML, JSON, Quarto, and BibTeX.
- Database interface: DatabaseConnector
- Sample OMOP data: Eunomia
- OHDSI analysis: CohortMethod where feasible
- Reporting: Quarto
- Dependency management: renv
- Tests: testthat
- Linting: lintr
- Formatting: styler
- CI: GitHub Actions

Do not introduce Python, JavaScript, Shiny, Docker, or additional implementation frameworks.
Only R and SQL may be used for analytical implementation.

## Research constraints

- Treat Eunomia as synthetic or sample demonstration data.
- Never describe results as clinical evidence.
- Never make causal claims or describe estimates as causal effects.
- Stating design assumptions does not convert an observational association into a causal claim.
- Use the phrase:
  "Adjusted observational association under the stated design assumptions."
- Do not select target, comparator, or outcome based on statistical significance.
- Do not use covariates measured on or after the index date.
- Do not suppress null, unstable, or negative findings.
- Report cohort sizes, event counts, overlap, balance, confidence intervals,
  and limitations.
- Preserve the prespecified protocol.
- Record post-freeze protocol changes in docs/protocol_deviations.md.

## Data governance

- Never commit database files.
- Never commit row-level person data.
- Never commit credentials, tokens, .Renviron, .env, or secrets.
- Write generated data only under ignored directories.
- Commit aggregate tables only when disclosure-safe.
- Use deterministic random seeds where randomness is required.

## R standards

- Use snake_case for objects and functions.
- Prefer small pure functions.
- Avoid global assignment.
- Never use setwd().
- Use here::here() for project paths.
- Use explicit package namespaces in reusable functions.
- Validate function inputs.
- Return named objects.
- Add roxygen-style comments to non-trivial functions.
- Keep lines below 100 characters.
- Do not silence warnings without justification.

## SQL standards

- Use uppercase SQL keywords.
- Use snake_case aliases.
- Never use SELECT * in production queries.
- Parameterize concept IDs, dates, and schema names.
- Add comments describing index date, washout, and risk windows.
- Ensure one row per person per cohort entry where required.
- Avoid post-index information leakage.
- Use SqlRender for parameter rendering and dialect translation.

## Testing

Every new reusable function requires tests for:

- normal inputs;
- empty inputs;
- missing columns;
- invalid parameters;
- duplicated person and index-date records;
- date-window boundaries.

## Git workflow

- Never work directly on main.
- Use one issue per logical task.
- Use one branch per issue.
- Keep commits atomic.
- Never force-push main.
- Do not commit until tests and linting pass.
- Do not push or merge without explicit user approval.

## Required checks before task completion

Run:

1. Rscript scripts/run_tests.R
2. Rscript scripts/run_lint.R
3. Rscript scripts/check_project.R
4. quarto render

Report:

- files changed;
- commands executed;
- tests passed or failed;
- unresolved assumptions;
- risks and limitations.

## Forbidden actions

- Do not delete unrelated files.
- Do not rewrite the entire repository for a local problem.
- Do not add dependencies without explaining why.
- Do not fabricate concept IDs or clinical interpretations.
- Do not hide failing tests.
- Do not alter the study protocol after examining results without recording
  the change as a protocol deviation.
- Do not use danger-full-access.
- Do not bypass approvals or sandbox restrictions.

## Codex execution restrictions

- Codex may inspect and edit files only inside the current workspace.
- Codex must never create Git commits.
- Codex must never push branches or tags.
- Codex must never merge pull requests.
- Codex must never modify the main branch directly.
- Codex must leave all commit, push, tag, release, and merge actions to the user.
