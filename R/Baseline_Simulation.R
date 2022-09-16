#Baseline Non interactive communication of dyads.
# Authors:
#                       ## Variables ##
# n: Number of observations before filtering for inhibited calls.
#
#                       ## Latency ##
# mu_latency: The average wait time between consecutive calls for each individual.
# sd_latency: The uncertainty regarding the latency between calls.
#
#                   ## Call Duration ##
# mu_duration: What is the average call duration of our callers.
# sd_duration: What is the uncertainty in the average call duration.



#' @title overview_tab
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @importFrom dplyr "%>%"




Baseline_Simulation <- function(n, mu_latency, sd_latency, mu_duration, sd_duration){
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be numeric and between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be numeric and between 1:Infinity")
  if ( any( mu_duration%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_duration should be numeric and between 1:Infinity")
  if ( any( sd_duration%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_duration should be numeric and between 1:Infinity")

  IOI_1 <- data.frame(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, ID = 1) # The neighboring individual is freely vocalizing at its tempo
  IOI_2 <- data.frame(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, ID = 2) # The neighboring individual is freely vocalizing at its tempo

  for (i in 1:n){
    if (i == 1){
      IOI_1$Onset[i] = IOI_1$Interval[i]
      IOI_2$Onset[i] = IOI_2$Interval[i]

      IOI_1$Offset[i] = IOI_1$Interval[i] + IOI_1$Duration[i]
      IOI_2$Offset[i] = IOI_2$Interval[i] + IOI_2$Duration[i]
    }
    else{
      IOI_1$Onset[i] = IOI_1$Onset[i-1] + IOI_1$Duration[i-1] + IOI_1$Interval[i]
      IOI_2$Onset[i] = IOI_2$Onset[i-1] + IOI_2$Duration[i-1] + IOI_2$Interval[i]

      IOI_1$Offset[i] = IOI_1$Onset[i] + IOI_1$Duration[i]
      IOI_2$Offset[i] = IOI_2$Onset[i] + IOI_2$Duration[i]

    }
  }

  IOI <- rbind(IOI_1, IOI_2) %>%
    arrange(Onset) %>%
    mutate(Latency = Onset - lag(Offset)) %>%
    mutate(CallNr = 1)

  return(IOI)
}


