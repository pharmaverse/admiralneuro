# Changelog

## admiralneuro 0.2.1.9000

### Updates of Existing Functions

- Improved test coverage in
  [`compute_centiloid()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_centiloid.md)
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
  [`compute_upsit_percentile()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_upsit_percentile.md)
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
  [`compute_centiloid()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_centiloid.md)
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
