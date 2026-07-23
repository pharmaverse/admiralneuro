# admiralneuro (development version)

## Documentation

- Added a new vignette "Explore ADaM Templates" to the "Get Started" menu.
It displays the `{admiralneuro}` templates. (#126)

- A night mode option has been added to the {admiralneuro} website. This can be be toggled using the sun/moon icon in the top right corner of the navigation bar. Tooltips have also been set up for each of the icons on the navigation bar. (#125)

## Various

<details>
<summary>Developer Notes</summary>

- Creating the website for pull requests was enabled. Add "[create website]" to
the pull request title to trigger website creation. (#125)

- Fixed links in the vignettes and added the Slack invitation link to `.lycheeignore` for a successful CI/CD Links/Validate Links workflow run. (#131)

- Clickable links to personal websites or GitHub profiles were added in the home page sidebar for every {admiralneuro} author. Up-versioned `{admiraldev}` to 1.5.0, as the custom `{admiral}` roclets (i.e., `roxygen_*()` functions) have now been moved to `{admiraldev}`. Update to roxygen2 8.0.0. (#135)

</details>

# admiralneuro 0.2.1

## Updates of Existing Functions

- Improved test coverage in `compute_centiloid()` function when invalid tracer combination is provided. The 
function also now only accepts positive SUVR values. (#106)

<details>
<summary>Developer Notes</summary>

- Aligned the templates' save directory with `{pharmaverseadam}` requirements. (#91)

</details>

## New features

- The function `compute_upsit_percentile()` calculates percentiles based on age, sex, and UPSIT scores. (# 95)

## Documentation

- Vignette "Creating ADNV". (#114)

## Template programs

- ADNV template scripts `ad_adnv.R` which creates a Nervous System Analysis Dataset. (#113)

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
