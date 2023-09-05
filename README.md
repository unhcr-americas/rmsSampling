
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {rmsSampling}

<!-- badges: start -->
<!-- badges: end -->

The goal of {rmsSampling} is to ease the implementation of the RMS
Sampling guidance

## A guided workflow

User are guided through a workflow:

- Select the country so that the last population statistics aggregated
  by group are pulled

  - RAS - aka REF+ASY+OIP, Refugees, Asylum-seeker and other in Need of
    International Protection - “Pillar-1-Refugee”

  - STA: Stateless Persons – “Pillar-2-Stateless”

  - IDP: Internally displaced Persons - “Pillar-4-IDP”

  - REP (RET+ROC): Returned Refugees & IDPs - “Pillar-3-Reintegration”

  - OOC: Other of Concerns

Then for each group a specific tab will be available with a series of
conditional questions to confirm wether one the 7 potential sampling
method can apply:

- Flag population groups with less than 5,000 individuals, to confirm if
  there is really budget to implement a sampling strategy

- Request user to indicate if a reliable registration group list is
  available for each population group

- Check size of the country to confirm if the implementation area is
  small (allowing for **Simple Random Sampling without stratification**)

- Check if disaggregation by geographic location are available within
  the countries (allowing for **Simple Random Sampling within
  strata**)  

- Check breakdown by country of Origin to confirm if potential
  stratification by country of orgin would make sense for Refugee and
  Asylum Seekers? (allowing for **multiple Stage Cluster Sampling**)

- Check if budget and time is available to conduct a list exercise
  (allowing for either **Probability Proportion to Size** or **Adaptive
  Cluster Sampling**)

- Check if the population tend to gather to a certain location on a
  specific day time? (allowing for a **Location Time Sampling**)

- Check if the community if well connected and is there is adequate time
  to conduct a formative survey to identify the sees and prepare
  coupons? (allowing for a **Respondent Driven Sampling**)

- ask for expected margin of errors, level of confidence and sample
  power.

As an output they can generate a report to document the appropriate
sampling approach by country.

## Installation

You can install the development version of rmsSampling like so:

``` r
install.packages("pak")
pak::pkg_install("edouard-legoupil/msSampling")  
```
