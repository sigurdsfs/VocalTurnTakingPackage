##Activation Functions##

### Arousal Model: ###

## Neigh is the initiator, focal is the second individual, who, in the arousal model follows the neighbor
# IF THE NEIGHBOR VOCALIZES, THE FOCAL VOCALIZES WITH AN INTERVAL OF 0-PREV NEIGH INTERVAL
# THEN IT WAITS
# WE HAVE NO DURATION HERE, IT DOESN*T MATTER FOR THE MODEL, BUT WE MIGHT ADD IT.


# Authors:
#                       ## Variables ##
# n: Number of observations before filtering for inhibited calls.
#
#                       ## Latency ##
# mu_latency: The average wait time between consecutive calls for each individual.
# sd_latency: The uncertainty regarding the latency between calls.

#' @title Generative model of Arousal based turn-taking.
#'
#' @description Simulates dyadic vocal turn taking timings based on the arousal model.
#' This model includes a stochastic element. The probability of Interlocutor B  calling increases as the number of calls heard from
#' interlocutor B increases. We have modelled the relationship with a sigmoid activation function.
#' Calls have no duration.
#'
#' @param n Number of Observations
#' @param mu_latency Average latency between consecutive calls per caller.
#' @param sd_latency Uncertainty around the average latency between calls.

#'
#' @return A data frame object that contains our dyadic interlocutors
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%"


Arousal_Simulation_Stochastic <- function(n, mu_latency, sd_latency){
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be numeric and between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be numeric and between 1:Infinity")

  #Initiator
  IOI_neigh <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Onset = NA, caller = "Initiator") # The neighboring individual is freely vocalizing at its tempo

  for (i in 1:n) {
    if (i == 1) {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
    } else {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Onset[i - 1]
    }
  }


  #Receiver
  IOI_focal <- tibble(Interval = rep(NA, n), Onset = NA, caller = "Responder") # The focal individual is yet undetermined

  n_since_call <- 1
  for (i in 1:n){
    prob_of_responding <- sigmoid(n_since_call - 3)
    Talk_boolean <- rbinom(1,1, prob = prob_of_responding)

    if (Talk_boolean == 1){
      IOI_focal$Interval[i] <- runif(1, 0, IOI_neigh$Interval[i])
      IOI_focal$Onset[i] <- IOI_neigh$Onset[i] + IOI_focal$Interval[i]
      n_since_call <- 1
    }
    else{
      IOI_focal$Interval[i] <- NA
      IOI_focal$Onset[i] <- NA
      n_since_call <- n_since_call + 1
    }
  }
  IOI <- rbind(IOI_focal, IOI_neigh) %>%
    filter(Onset != "NA") %>%
    arrange(Onset) %>%
    rename(Latency = Interval) %>%
    mutate(ID = caller) %>%
    mutate(CallNr = 1)
  IOI$Latency[1] <- NA

  return(IOI)
}

