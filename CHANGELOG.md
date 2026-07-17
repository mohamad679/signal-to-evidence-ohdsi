# Changelog

All notable changes to this project are documented in this file.

## [0.1.0] - 2026-07-17

### Added

- Automated parse, lint, complete-test, reporting, and CI validation.
- Full nine-stage reproducibility validation for the frozen study workflow.
- Aggregate artifact immutability and embedded-figure validation.
- Security and privacy gates for tracked paths, secret patterns, workflow
  permissions, and disclosure-safe aggregate reporting.
- Release documentation and citation metadata.

### Changed

- CI uses read-only repository permissions and does not persist checkout
  credentials.
- CI job timeout accommodates the complete reproducibility pipeline.
- Reporting reflects the actual artifact model: 15 aggregate CSV tables and
  figures embedded in Quarto HTML reports.

### Security

- Person-level reporting fields and private database paths are prohibited.
- Published CI artifacts are limited to the two aggregate HTML reports.
