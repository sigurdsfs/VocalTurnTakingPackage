########### Inhibiton Hypothesis ############

#' @title Inhibition Hypothesis Testing
#'
#' @description
#'    We test whether empirical data could have been generated under the inhibition hypothesis.
#'
#' @param df The data frame we wish to test.
#'
#' @return Summary of the test output.
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%"

InhibitionHypothesis <- function(df){
  i = 1
  #If Groups Do Not Exist
  if ("Group" %in% colnames(df) == FALSE){
    print("No Groups")
    for (id in unique(df$ID)){

      for (b in (unique(df$CallNr[df$ID == id]))){

        #Whale1
        whale1 <- subset(df,ID == id & CallNr == b)

        if (length(unique(whale1$CallNr))== 1){
          #Whale 2
          whale2_INFO <- df %>%
            group_by(ID) %>%
            slice_sample()%>%
            filter(ID != id) %>%
            ungroup() %>%
            sample_n(1)
        }
        else{
          #Whale 2
          whale2_INFO <- df %>%
            group_by(ID, CallNr) %>%
            slice_sample()%>%
            filter(ID != id & CallNr != b) %>%
            ungroup() %>%
            sample_n(1)
        }

        whale2 <- subset(df, ID == whale2_INFO$ID & CallNr == whale2_INFO$CallNr)

        df_inhib2 <- rbind(whale1,whale2) %>%
          filter(Latency > 0.1) %>%
          arrange(CallNr, Onset) %>%
          mutate(Group = i) %>%
          mutate(Latency = Onset - lag(Offset, 1))

        #Combine
        if (i == 1) df_inhib_final2 <- df_inhib2
        else df_inhib_final2 <- rbind(df_inhib2, df_inhib_final2)

        i = i +1
      }
    }
    df <- df %>% #We need to have Group collumn to bind simulated data with original data
      mutate(Group = 1)
  }
  #If Groups Exist
  else{
    print("Groups Exist")
    for (g in unique(df$Group)) {

      for (id in unique(df$ID[df$Group == g])){

        for (b in (unique(df$CallNr[df$Group == g & df$ID == id]))){

          #Whale1
          whale1 <- subset(df, Group == g & ID == id & CallNr == b)

          if (length(unique(whale1$CallNr))== 1){
            #Whale 2
            whale2_INFO <- df %>%
              group_by(ID, Group) %>%
              slice_sample()%>%
              filter(Group != g & ID != id) %>%
              ungroup() %>%
              sample_n(1)
          }
          else{
            #Whale 2
            whale2_INFO <- df %>%
              group_by(ID, Group, CallNr) %>%
              slice_sample()%>%
              filter(Group != g & ID != id & CallNr != b) %>%
              ungroup() %>%
              sample_n(1)
          }

          whale2 <- subset(df, Group == whale2_INFO$Group & ID == whale2_INFO$ID & CallNr == whale2_INFO$CallNr)

          df_inhib2 <- rbind(whale1,whale2) %>%
            filter(Latency > 0.1) %>%
            arrange(Group, CallNr, Onset) %>%
            mutate(Group = i) %>%
            mutate(Latency = Onset - lag(Offset, 1))



          #Combine
          if (i == 1) df_inhib_final2 <- df_inhib2
          else df_inhib_final2 <- rbind(df_inhib2, df_inhib_final2)

          i = i + 1
        }
      }
    }
  }
  #Combine the Shuffled and real data
  df_inhib_final2$Type <-  "Simulated"
  df$Type <-  "Real"

  df_inhib_combined2 <- rbind(df_inhib_final2, df)

  #Plot
  Plot_inhib2 <- df_inhib_combined2 %>%
    ggplot(aes(Latency, color = Type)) + geom_density() + ggtitle("Density Plot of Latencies: Inihibiton Hypothesis")
  print(Plot_inhib2)
  #Test
  Test <- ks.test(df_inhib_final2$Latency, df$Latency)
  print(Test)

  return(df_inhib_combined2)
}

