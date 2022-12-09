#' @title Phase-Delay Simulation
#'
#' @description Simulates dyadic vocal turn taking based on the Phase-Delay model.
#' In this interaction, if a
#' neighbour’s signal is presented just before the focal male’s signal,
#' the signal will not be interrupted, but his next signal will be
#' advanced with respect to the previous focal signal onset
#'
#'
#'
#' @param n Number of Observations
#' @param b modulates changes in phase-correction N(T,bT)
#' @param cutoff the cutoff threshhold for when a person are eligible for phase correction.
#' @param mu_latency Average latency between consecutive calls per caller.
#' @param sd_latency Uncertainty around the average latency between calls.
#' @param mu_duration Average duration of each call.
#' @param sd_duration Uncertainty in average call duration.
#'
#' @return A data frame object that contains our dyadic interlocutors
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%"


### NOTE TO SELF #####
# We could possibly filter out all Overlapping calls = 1 which are not eligible for phase-correction. (I.e. their calls would be inhibited)

Phase_delay_simulation <- function(n,b, latency_mean, latency_sd, duration_mean, duration_sd, cutoff){
  require(dplyr)

  IOI_1 <- dplyr::tibble(Interval = rnorm(n, mean = latency_mean , sd = latency_sd),
                         Duration = abs(rnorm(n, mean = duration_mean,  sd = duration_sd)),
                         Onset = NA,
                         Offset = NA,
                         ID = 1,
                         CallNr = 1:n) %>%
    dplyr::mutate(Onset = Interval, Offset = Onset + Duration)


  IOI_2 <- dplyr::tibble(Interval = rnorm(n, mean = latency_mean , sd = latency_sd),
                         Duration = abs(rnorm(n, mean = duration_mean, sd = duration_sd)),
                         Onset = NA,
                         Offset = NA,
                         ID = 2,
                         CallNr = 1:n) %>%
    dplyr::mutate(Onset = Interval, Offset = Onset + Duration)

  for (i in 2:n) {
    IOI_1$Onset[i] = IOI_1$Offset[i - 1] + IOI_1$Interval[i]
    IOI_1$Offset[i] = IOI_1$Onset[i] + IOI_1$Duration[i]

    IOI_2$Onset[i] = IOI_2$Offset[i - 1] + IOI_2$Interval[i]
    IOI_2$Offset[i] = IOI_2$Onset[i] + IOI_2$Duration[i]

  }

  IOI_combined <- rbind(IOI_1, IOI_2) %>%
    dplyr::arrange(Onset) %>%
    dplyr::mutate(Offset_Latency = Onset - dplyr::lag(Offset, default = 0),
                  Onset_Latency = Onset - dplyr::lag(Onset , default = 0)) %>%
    dplyr::mutate(Overlapping = ifelse(Offset_Latency <= 0 , 1 ,0))


  # NOW LOOP THROUGH FIND ALL PLACES WHERE ONSET LATENCY IS LOW BUT OFFSET LATENCY IS NEGATIVE. (THIS MEANS OVERLAP BUT STARTED CLOSELY TO EACHOTHER)
  # LET THE BIRD SING AT TIME = T BUT ADJUST THEIR NEXT RESPONSE TIMING AT (T+1)

  for (i in 1:nrow(IOI_combined)) {
    if (IOI_combined$Overlapping[i] == 0 & IOI_combined$Onset_Latency[i] < cutoff) { #Check if persons call are overlapping and elgible for phase correction.

      Call_to_adjust <- which(IOI_combined$ID == IOI_combined$ID[i] & IOI_combined$CallNr == IOI_combined$CallNr[i] + 1) #Find the next call of the person who are going to phase correct.

      Phase_correction <-  rnorm(1, mean = IOI_combined$Onset_Latency[i], sd = b * IOI_combined$Onset_Latency[i]) # Phase correction

      #### Updating timing with the new phase correction for the call in question. ####

      #Adjust interval
      IOI_combined$Interval[Call_to_adjust] <- IOI_combined$Interval[Call_to_adjust] - Phase_correction
      #Adjust Onset
      IOI_combined$Onset[Call_to_adjust] <- IOI_combined$Onset[Call_to_adjust] - Phase_correction
      #Adjust Offset
      IOI_combined$Offset[Call_to_adjust] <- IOI_combined$Onset[Call_to_adjust] + IOI_combined$Duration[Call_to_adjust]



      ### Adjust the remaining calls for the phase correction ###

      for (ii in min(nrow(IOI_combined), 1 + Call_to_adjust):nrow(IOI_combined)) {
        #On-, Offset
        IOI_combined$Onset[ii] <- IOI_combined$Offset[ii - 1] + IOI_combined$Interval[ii]
        IOI_combined$Offset[ii] <- IOI_combined$Onset[ii] + IOI_combined$Duration[ii]
        #On, Offset Latencies
        IOI_combined$Onset_Latency[ii] <- IOI_combined$Onset[ii] - IOI_combined$Onset[ii - 1]
        IOI_combined$Offset_Latency[ii] <- IOI_combined$Offset[ii] - IOI_combined$Offset[ii - 1]

        IOI_combined$Overlapping[ii] <- ifelse(IOI_combined$Offset_Latency[ii] <= 0 , 1, 0)

      }
    }
  }
  return(IOI_combined)

}

ok <- Phase_delay_simulation(n = 150, b = 1/10, latency_mean = 5, latency_sd = 2, duration_mean = 15, duration_sd = 4, cutoff = 4)



360 * 1/ok$Duration * ok$Onset_Latency




