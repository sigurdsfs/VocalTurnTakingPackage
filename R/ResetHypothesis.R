

#' @title Reset Hypothesis Testing
#' @description
#'    We test whether empirical data could have been generated under the reset hypothesis.
#'
#' @param df The data frame we wish to test.
#'
#' @return Summary of the test output.
#'
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%"
#'
ResetHypothesis <- function(data){

  if (!"Group" %in% colnames(data) ){ #If we only have 1 group
    data <- data %>%
      arrange(CallNr, Onset) %>%
      mutate(Interindividual = 0)
  }

  else{  #Multiple groups
    data <- data %>%
      arrange(Group, CallNr, Onset) %>%
      mutate(Interindividual = 0)
  }
  for(i in 2:nrow(data)){
    data[i,"Interindividual"] = ifelse(data$ID[i] == data$ID[i-1], 0, 1) #Check if inter- or intra-timing
  }
  data <- data %>%
    mutate(Interindividual = as.factor(Interindividual))

  #plotting
  plot <- data %>%
    filter(Latency != "NA") %>%
    ggplot(aes(x = Latency, color = Interindividual)) + geom_density() + ggtitle("Density Plot of Latencies: Reset Hypothesis")
  print(plot)

  #Testing
  df_Inter <- data %>%
    filter(Latency != "NA" & Interindividual == 1)

  df_Intra <- data %>%
    filter(Latency != "NA" & Interindividual == 0)

  test <- ks.test(df_Inter$Latency, df_Intra$Latency)
  print(test)
  return(data)
}
