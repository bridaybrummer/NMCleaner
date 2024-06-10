
## NMCleaner

The {NMCleaner} package provides some useful, everyday tools for
epidemiologists using data from the Notifiable Medical
Conditions-Surveillance System.

The package is maintained by the NMC epidemiologist and we encourage you
to contact them [NMC Epidemiologist](mailto:brinb@nicd.ac.za)

## Installation

You can install {NMCleaner} from github with:

``` r
if(!require("devtools")) install.packages("devtools")

devtools::install_github("bridaybrummer/NMCleaner", force = TRUE)
```

First time installation may take up to 5 minutes as the package has many
dependancies to make way for implementing Epitable and Epicurve
functions in the future.

*If the installation takes more time than that*, and you are on NICD
wifi, consider changing to the guest wifi, or your own hotspot as the
NICD wifi has a firewall that may block the installation.

If you are a first time R user, consider installing R and RStudio from
the following links:

- [R](https://cran.r-project.org/)

If you previously tried to learn R, ensure you uninstall and version of
it from your device.

## History, Use, and direction

The {NMCleaner} package is based off of the cleaning **.do** files
compiled by Mabore Morifi and Lehlohonolo Chandu; As such, the main
cleaning function is called \[`stata2script()`\].

The aim of the package is three-fold, 1) to make the transition to R
easy for a busy epidemiologist. 2) To standardise cleaning practices
across the provincial epidemiology team and users of NMC data. 3)
Provide a repository of useful country characteristics such as
population data and shape files.

This package is intended to encourage Epidemiologists to transition from
using Stata to R, therefore we provide a set of common functions that
are used. We also encourage users to review the code in
[github](https://github.com/bridaybrummer/NMCleaner), and if you have
common workflows that you would like to see implemented in **R**, please
contact the NMC Epidemiologist.

The package will constantly be in development, cleaning practices will
seldom change without stakeholder consultation. However, we intend to
provide a large range of helper functions to create epicurve, epitables
and other common outputs for sitrep or academic outputs.

## Examples

### Bind .xlsx files from the NMC linelist export

Use \[`bind_linelist()`\] to produce a \[\`data.frame’\] from the
directory that contains the .xlsx files downlaoded form the NMC.

``` r
#library(NMCleaner)
#dirty_dataframe <- bind_linelist("path/to/linelist/folder")
```

Use \[`stata2script()`\] to produce the cleaned data. The return object
is a list of other datafrmes which shows the duplicates that were
removed.

``` r
#clean<- stata2script(dirty_dataframe)
# the clean object also returns the list of notificatiosn that were taken out because of duplication
#clean_df<- clean$data_dup33
```

### You could use a function that leaverages gtsummary for a quick table

``` r

#clean_df<- clean$data_dup33%>%

#clean_df %>%  at_a_glance()
```

# Future Plans

We would like to add more helper functions to the package, such as
`epicurve()` and `epitable()` functions. We would also like to add a
function that can produce a sitrep from the cleaned data.
