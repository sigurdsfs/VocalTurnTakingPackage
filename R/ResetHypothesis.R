

#' @title Reset Hypothesis Testing
#' @description
#'    We test whether empirical data could have been generated under the reset hypothesis.
#'    The function is not written to be adaptive so the data frame column names must match that
#'    of the ones used in the function.
#'    ID %cat: There must be an ID column with has a unique ID for each interlocutor.
#'    CallNr %disc: This column should keep track of the sequence of calls. 1.st call 2nd call so forth.
#'    Onset %numeric: When an interlocutor starts their call.
#'    Offset %numeric: When an interlocutor stops their call.
#'
#' @param df The data frame we wish to test. Must contain columns ID, CallNr, Onset & Offset.
#' See the description for further information.
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
