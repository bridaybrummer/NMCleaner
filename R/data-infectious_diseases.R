#' A dataframe of the NMC conditions that may be modelled.
#'
#' The dataset contains infromation on relevant paramters to model
#' specific conditions.
#'
#' A consensus on incubation periods,generation times, and other parameters,
#' should be taken from literature and subject matter experts.
#' A review could also be conducted based on changing epidemiology.
#' These can be important metrics to capture by field epidemiologitst.
#'
#' THis dataset still needs to be imporved by:
#' The distribution of these paramerter should also be estimated and
#' stored as a list in the df based on the EpiNow2 package framework.
#'
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{condition}{the condition as it relates to condition_df(character)}
#'   \item{mean_incubation_period}{The incubation period (integer)}
#'   \item{Rt}{Gender of the individual (factor with levels "Male" and "Female")}
#'   \item{generation_time}{Randomly generated age values between 20 and 40 (integer)}
#'   \item{refs}{Randomly generated age values between 20 and 40 (integer)}
#' }
#' @examples
#' sample_data
#'
#' # View the first few rows
#' head(condition_df)
#'
#' # Summarize the data
#' summary(condition_df)
#'
#' @source Generated for demonstration purposes
"condition_df"

###
infectious_diseases <-
  tibble::tibble(
    condition = c("Cholera", "Measles"),
    mean_incubation_period = c(4, 2),
    Rt = c(2.5, 3.5),
    generation_time = c(10, 12),
    refs = c("WHO", "CDC")
  )

  save(infectious_diseases, file = "data/infectious_diseases.rda")

usethis::use_data(infectious_diseases, overwrite = TRUE)
