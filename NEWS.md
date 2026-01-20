# admiralneuro (development version)

## New features

## Documentation

## Template programs

## Various

- Moved test SDTM datasets `dm_neuro`, `nv_neuro`, `suppnv_neuro` and `ag_neuro` from `{admiralneuro}` to `{pharmaversesdtm}`. (#92)
- Updated the kapa.ai `data-modal-search-placeholder` to "Ask me a question about {admiralneuro} or the {admiral} ecosystem..." (#119)

# admiralneuro 0.1.0

- Initial package release mainly focused on Alzheimer’s disease specifically for the PET scan analysis.

## Functions

- The function `compute_centiloid()` computes centiloid values based on Amyloid PET tracer, SUVR value and pipeline, and reference region. (#21)

## Template programs

- ADAPET template scripts `ad_adapet.R` which creates an Amyloid PET Scan Analysis Dataset. (#25)
- ADTPET template scripts `ad_adtpet.R` which creates a Tau PET Scan Analysis Dataset. (#25)

## Documentation

- Vignette "Creating ADAPET and ADTPET". (#19)
