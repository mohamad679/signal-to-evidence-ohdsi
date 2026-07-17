# Release Checklist

## Local validation

- Run parse validation.
- Run lint validation.
- Run the complete test suite.
- Run security and privacy validation.
- Render and validate both Quarto reports.
- Confirm the exact changed-path set.
- Confirm the staged diff contains no private or generated artifacts.

## Publication

- Create one exact-scope release commit.
- Push `feat/ci-release`.
- Verify the remote commit SHA.
- Wait for the `CI` workflow associated with the pushed commit.
- Create and push `v0.1.0` only after remote CI succeeds.
- Confirm the working tree and index are clean.
