#' tolbutamide data - chapter 15-1, p260
#'
#' Age-specific comparison of death from all causes for Tolbutamide and placebo
#' treatment groups, University Group Diabetes Program (1970)
#'
#'  @format A dataframe with 409 observations and 3 variables
#'  \describe{
#'    \item{tolbutamide}{Given Tolbutamide (1) or placebo (0)}
#'    \item{dead}{Died (1) or surviving (0)}
#'    \item{age}{Less than 55 (<55) or 55 and over (ge55)}
#'  }
"tolbutamide"

#' ebola data
#'
#' Person-time data for death from ebola. Data from
#' https://datacompass.lshtm.ac.uk/599/
#'  Marks, M. Learning Clinical Epidemiology with R. (Project). London School of Hygiene & Tropical Medicine, London, United Kingdom.
#'
#' @format A dataframe
#' \describe{
#'    \item{id}{Unique patient identifier}
#'    \item{age}{Age in years}
#'    \item{age_group}{Grouped age}
#'    \item{sex}{Sex}
#'    \item{disease_onset}{Date of onset (date class)}
#'    \item{disease_ended}{Date of death or recovery (date class)}
#'    \item{days_at_risk}{Number of days at risk of death}
#'    \item{status}{Died or recovered (string)}
#'    \item{transmission}{How ebola was transmitted}
#'    \item{died}{Integer 1 = died, 0 = survived}
#' }
"ebola"
