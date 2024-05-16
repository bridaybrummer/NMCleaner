
<!-- README.md is generated from README.Rmd. Please edit that file -->

## NMCleaner

The {NMCleaner} package provides some useful, everyday tools for
epidemiologists using data from the Notifiable Medical Conditiosn-
Surveillance System. The {NMCleaner} package, crucially provides
Provincial Epidemiologists and other stakehodlers to clean and analyse
the documents in a standard way in addition for a substantial amount of
work done, behind-the-scenes.

The package is maintained by the NMC epidemiologist and we encourage you
to contact them [NMC Epidemiologist](mailto:brinb@nicd.ac.za)

## Installation

You can install {NMCleaner} from github with:

``` r
remotes::install_github("bridaybrummer/gtsummary")
```

or

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("bridaybrummer/NMCleaner", force = TRUE)
```

The first time installation may take up to 5 minutes as the package has
many dependancies to make way for implementing Epitable and Epicurve
functions in the future. If the installation takes more time than that,
and you are on NICD wifi, consider changing to the guest wifi, or your
own hotspot as the NICD wifi has a firewall that may block the
installation.

If you are a first time R user, consider installing R and RStudio from
the following links:

- [R](https://cran.r-project.org/)

If you previously tried to learn R, ensure you uninstall and version of
it from your device.

## History, Use, and direction

The {NMCleaner} package wasbased off of the cleaning **.do** files
compiled by Mabore Morifi and Lehlohonolo Chandu; As such, the main
cleaning fucntion is called\[`stata2script()`\].

The aim of the package is three-fold, 1) to make the transition to R
easy for a busy epidemiologist. 2) To standardise cleaning practices
across the provincial epidemiology team and users of NMC data. 3)
Provide a repository of useufl country characteristics such as
popualtion data and shape files.

This package is intended to encourage Epidemiologists to transition from
using Stata to R, therefore we provide a set of common functions that
are used. We also encourage users to review the code in
[github](https://github.com/bridaybrummer/NMCleaner). If you have common
workflows that you would like to see implemented in **R**, please
contact the NMC Epidemiologist.

The package will constantly be in development, cleaning practis will
seldom change without stakholder consultation. However, we intend to
provide a large range of helper functions to creat epicurve, epitables
and other common outputs for sitrep or academic outputs.

## Examples

### Bind .xlsx files from the NMC linelist export

Use
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
to summarize a data frame.

### Clean the NMC linelist
