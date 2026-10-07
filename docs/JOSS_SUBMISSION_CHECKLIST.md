# JOSS submission checklist

This repository is organized so that JOSS compiles `paper/paper.md`. The longer
technical article remains available under `docs/extended-article/` for readers,
but it is not the JOSS submission manuscript.

## Completed in the repository

- [x] A concise JOSS paper with the required summary, statement of need, state
  of the field, software design, research impact, AI disclosure,
  acknowledgements, and references.
- [x] A permissive software license and machine-readable citation metadata.
- [x] A reproducible R environment in `renv.lock`.
- [x] Automated statistical tests and an application startup smoke test.
- [x] GitHub Actions workflows for the R tests and JOSS paper compilation.
- [x] User, statistical-method, support, contribution, and governance
  documentation.
- [x] A clearly separated extended article and its figures.
- [x] Example-data provenance and licensing notes.

## Author actions before submission

- [ ] Read and approve every line of `paper/paper.md`, especially the research
  impact and AI usage statements.
- [ ] Confirm the title, author order, affiliations, and ORCIDs with every
  co-author; add missing ORCIDs if available.
- [ ] Confirm that the repository's public development history meets the JOSS
  review criteria and that the research-impact statement accurately describes
  real use of the software.
- [ ] Confirm that ESS11 Edition 3.0 is the exact source of the included
  Belgian extract and that its adapted-data attribution is accurate.
- [ ] Push these changes and verify that both GitHub Actions workflows pass in
  the public repository.
- [ ] Check that the repository is public and that installation and the example
  workflow work from a fresh clone.
- [ ] Disclose the separate extended article to the JOSS editor if it has been
  published, submitted, or posted elsewhere.

## After successful review

- [ ] Create the version requested by the editor as a numbered software
  release, archive it with Zenodo or another accepted archive, and provide the
  version number and archived DOI in the JOSS review issue.
