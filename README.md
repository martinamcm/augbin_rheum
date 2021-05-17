
## augbin\_rheum

## [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues)

## Description

R package for analysing composite responder endpoints in rheumatology
with up to two continuous and one binary components.

## Getting Started

Install from Github using
`devtools::install_github("martinamcm/augbin_rheum")`

Implementation as a Shiny app with further documentation on
functionality and examples available at
[AugBin](https://github.com/martinamcm/AugBin)

## Details

Function `augbinrheum` is used to conduct the analysis with the
following arguments:

  - `Data` Data with columns ordered as follows: patient ID, treatment
    arm, continuous outcome(s), binary outcome followed by the baseline
    continuous measure(s)
  - `cts` Number of continuous outcomes in the composite which can be 1
    or 2. Defaults to 1.  
  - `bin` Number of binary outcomes in the composite which can be 1 or
    0. Defaults to 1.
  - `dichot` Responder threshold in each of the continuous outcomes. For
    one continuous outcome input as ‘d1’ and for two continuous outcomes
    input as ‘c(d1,d2)’

## References

McMenamin M, Grayling MJ, Berglind A, Wason JMS. Increasing power in the
analysis of responder endpoints in rheumatology: a software tutorial.
*medRxiv*. 2020. doi:
[10.1101/2020.07.28.20163378](https://www.medrxiv.org/content/10.1101/2020.07.28.20163378v1)

McMenamin M, Barrett JK, Berglind A, Wason JM. Employing a latent
variable framework to improve efficiency in composite endpoint analysis.
*Statistical Methods in Medical Research*. 2021;30(3):702-716. doi:
[10.1177/0962280220970986](https://doi.org/10.1177/0962280220970986)
