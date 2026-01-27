# Changelog

## admiralneuro (development version)

### New features

### Documentation

### Template programs

### Various

- Moved test SDTM datasets `dm_neuro`, `nv_neuro`, `suppnv_neuro` and
  `ag_neuro` from
  [admiralneuro](https://pharmaverse.github.io/admiralneuro/) to
  [pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/).
  ([\#92](https://github.com/pharmaverse/admiralneuro/issues/92))

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
