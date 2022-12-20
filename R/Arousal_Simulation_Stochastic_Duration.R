### Arousal Model: ###

## Neigh is the initiator, focal is the second individual, who, in the arousal model follows the neighbor
# IF THE NEIGHBOR VOCALIZES, THE FOCAL VOCALIZES WITH AN INTERVAL OF 0-PREV NEIGH INTERVAL
# THEN IT WAITS
# WE HAVE NO DURATION HERE, IT DOESN*T MATTER FOR THE MODEL, BUT WE MIGHT ADD IT.


#' @title Generative model of arousal based turn-taking with duration.
#'
#' @description Simulates dyadic vocal turn taking timings based on the arousal model.
#' This model includes a stochastic element. The probability of Interlocutor B  calling increases as the number of calls heard from
#' interlocutor B increases. We have modelled the relationship with a sigmoid activation function.
#' Calls have duration & and the duration of the responder depends on the duration of the initiator's call.
#'
#' @param n Number of Observations
#' @param mu_latency Average latency between consecutive calls per caller.
#' @param sd_latency Uncertainty around the average latency between calls.
#' @param mu_duration Average duration of initiator's call.
#' @param sd_duration Uncertainty in call duration of the initiator.
#'
#' @return A data frame object that contains our dyadic interlocutors
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%"



Arousal_Simulation_Stochastic_duration <- function(n, mu_latency, sd_latency, mu_duration, sd_duration){
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be numeric and between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be numeric and between 1:Infinity")

  #Initiator
  IOI_neigh <- dplyr::tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Onset = NA, Offset = NA, Duration = abs(rnorm(n = n, mean = mu_duration, sd = sd_duration)) , caller = "Initiator")

  # The neighboring individual is freely vocalizing at its tempo
  for (i in 1:n) {
    if (i == 1) {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
      IOI_neigh$Offset[i] = IOI_neigh$Onset[i] + IOI_neigh$Duration[i]
    } else {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Offset[i - 1]
      IOI_neigh$Offset[i]= IOI_neigh$Onset[i] + IOI_neigh$Duration[i]
    }
  }

  #Receiver
  IOI_focal <- tibble(Interval = rep(NA, n), Onset = NA,Offset = NA, Duration = NA, caller = "Responder") # The focal individual is yet undetermined

  n_since_call <- 1
  for (i in 1:n){
    prob_of_responding <- sigmoid(n_since_call - 2)
    Talk_boolean <- rbinom(1,1, prob = prob_of_responding)

    if (Talk_boolean == 1){
      IOI_focal$Interval[i] <- runif(1, 0, IOI_neigh$Interval[i])
      IOI_focal$Onset[i] <- IOI_neigh$Offset[i] + IOI_focal$Interval[i]
      #The duration of the response is gonne be dependent on duration of the call it is responding to.
      IOI_focal$Duration[i] <- abs(rnorm(1, mean = IOI_neigh$Duration[i], sd = IOI_neigh$Duration[i]/10))
      IOI_focal$Offset[i] <- IOI_focal$Onset[i] + IOI_focal$Duration[i]
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
    mutate(Latency = Interval) %>%
    mutate(ID = caller) %>%
    mutate(Latency = Onset - lag(Offset))

  IOI <- IOI %>%
    mutate(CallNr = seq(nrow(IOI)))

  IOI$Latency[1] <- NA

  return(IOI)
}

SimulatedData <- Arousal_Simulation_Stochastic_duration(n = 50,
                                       mu_latency = 200,
                                       sd_latency = 50,
                                       mu_duration = 100,
                                       sd_duration = 20)

ArousalData <- SimulatedData %>%
  mutate(ID = as.factor(ID)) %>%
  mutate(NeighbourInterval = lag(Interval)) #create a neighbor interval column

ggplot(data = subset(ArousalData, ID == "Responder")) + # subset to focal individual
  geom_point(aes(x = NeighbourInterval, y = Latency, color = ID)) +
  scale_color_manual(values=viridis(n = 3)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()
