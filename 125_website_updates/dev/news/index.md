# Changelog

## admiralneuro (development version)

### Documentation

- Added a new vignette “Explore ADaM Templates” to the “Get Started”
  menu. It displays the
  [admiralneuro](https://pharmaverse.github.io/admiralneuro/) templates.
  ([\#126](https://github.com/pharmaverse/admiralneuro/issues/126))

- A night mode option has been added to the {admiralneuro} website. This
  can be be toggled using the sun/moon icon in the top right corner of
  the navigation bar. Tooltips have also been set up for each of the
  icons on the navigation bar.
  ([\#125](https://github.com/pharmaverse/admiralneuro/issues/125))

### Various

Developer Notes

- Update to roxygen2 8.0.0.
  ([\#135](https://github.com/pharmaverse/admiralneuro/issues/135))

- Creating the website for pull requests was enabled. Add “\[create
  website\]” to the pull request title to trigger website creation.
  ([\#125](https://github.com/pharmaverse/admiralneuro/issues/125))

- Fixed links in the vignettes and added the Slack invitation link to
  `.lycheeignore` for a successful CI/CD Links/Validate Links workflow
  run. ([\#131](https://github.com/pharmaverse/admiralneuro/issues/131))

- Clickable links to personal websites or GitHub profiles were added in
  the home page sidebar for every {admiralneuro}
  author.([\#135](https://github.com/pharmaverse/admiralneuro/issues/135))

- Updated the required version of {admiraldev} to 1.5.0 to use the
  custom roclets from admiraldev (i.e. roxygen\_\* functions) instead of
  admiral.
  ([\#135](https://github.com/pharmaverse/admiralneuro/issues/135))

## admiralneuro 0.2.1

CRAN release: 2026-02-04

### Updates of Existing Functions

- Improved test coverage in
  [`compute_centiloid()`](https:/pharmaverse.github.io/admiralneuro/125_website_updates/dev/reference/compute_centiloid.md)
  function when invalid tracer combination is provided. The function
  also now only accepts positive SUVR values.
  ([\#106](https://github.com/pharmaverse/admiralneuro/issues/106))

Developer Notes

- Aligned the templates’ save directory with
  [pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/)
  requirements.
  ([\#91](https://github.com/pharmaverse/admiralneuro/issues/91))

### New features

- The function
  [`compute_upsit_percentile()`](https:/pharmaverse.github.io/admiralneuro/125_website_updates/dev/reference/compute_upsit_percentile.md)
  calculates percentiles based on age, sex, and UPSIT scores. (# 95)

### Documentation

- Vignette “Creating ADNV”.
  ([\#114](https://github.com/pharmaverse/admiralneuro/issues/114))

### Template programs

- ADNV template scripts `ad_adnv.R` which creates a Nervous System
  Analysis Dataset.
  ([\#113](https://github.com/pharmaverse/admiralneuro/issues/113))

### Various

- Moved test SDTM datasets `dm_neuro`, `nv_neuro`, `suppnv_neuro` and
  `ag_neuro` from
  [admiralneuro](https://pharmaverse.github.io/admiralneuro/) to
  [pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/).
  ([\#92](https://github.com/pharmaverse/admiralneuro/issues/92))

- Updated the kapa.ai `data-modal-search-placeholder` to “Ask me a
  question about {admiralneuro} or the {admiral} ecosystem…”
  ([\#119](https://github.com/pharmaverse/admiralneuro/issues/119))

## admiralneuro 0.1.0

CRAN release: 2025-09-14

- Initial package release mainly focused on Alzheimer’s disease
  specifically for the PET scan analysis.

### Functions

- The function
  [`compute_centiloid()`](https:/pharmaverse.github.io/admiralneuro/125_website_updates/dev/reference/compute_centiloid.md)
  computes centiloid values based on Amyloid PET tracer, SUVR value and
  pipeline, and reference region.
  ([\#21](https://github.com/pharmaverse/admiralneuro/issues/21))

### Template programs

- ADAPET template scripts `ad_adapet.R` which creates an Amyloid PET
  Scan Analysis Dataset.
  ([\#25](https://github.com/pharmaverse/admiralneuro/issues/25))
- ADTPET template scripts `ad_adtpet.R` which creates a Tau PET Scan
  Analysis Dataset.
  ([\#25](https://github.com/pharmaverse/admiralneuro/issues/25))

### Documentation

- Vignette “Creating ADAPET and ADTPET”.
  ([\#19](https://github.com/pharmaverse/admiralneuro/issues/19))
