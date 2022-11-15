
#' @title Rotate Data
#'
#' @description
#'    We create rotated data sets for each caller. First we add a random n of seconds to the time series at the start.
#'    We use the shifted time series which are now longer than the bout and we cut and paste them together.
#'
#'
#' @param d The data frame we wish to rotate.
#' @param threshold Threshold for cutoff
#' @param delay Delay duration of the snippet being cut.
#'
#' @return A data frame containing the rotated time series.
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%"
#'
RotateData <- function(d, threshold, delay) {

  # We create rotated data sets.
  # for each whale but the first we add a random n of seconds to the time series of starting points
  # we take the shifted time series which are now longer than the original bout series.
  # we cut and paste the sticking out bit to the beginning

  for (i in unique(d$bout)){

    ## Let's reset the timer within each bout
    BoutReset <- d$StartTime[d$bout==i][1]
    d$BoutStartTime[d$bout==i] <- d$StartTime[d$bout==i] - BoutReset
    d$BoutEndTime[d$bout==i] <- d$EndTime[d$bout==i] - BoutReset

    ## identify all callers but the first
    Callers <- unique(d$WhaleID[d$bout==i])
    RotatedCallers <- Callers[-1]
    RotatedDelays <- runif(length(RotatedCallers), min= 1, max= delay)

    ## Last call
    LastCall <- nrow(d[d$bout==i,])
    BoutEnd <- d$BoutEndTime[d$bout==i][LastCall]

    if (length(Callers)>1){

      for (n in seq(length(RotatedCallers))){

        rc <- RotatedCallers[n]

        d$BoutStartTime[d$bout==i & d$WhaleID == rc] <- d$BoutStartTime[d$bout==i & d$WhaleID == rc] + RotatedDelays[n]
        d$BoutEndTime[d$bout==i & d$WhaleID == rc] <- d$BoutEndTime[d$bout==i & d$WhaleID == rc] + RotatedDelays[n]

        d$BoutStartTime[d$bout==i & d$WhaleID == rc & d$BoutStartTime > BoutEnd] <- d$BoutStartTime[d$bout==i & d$WhaleID == rc & d$BoutStartTime > BoutEnd] - BoutEnd

        d$BoutEndTime[d$bout==i & d$WhaleID == rc & d$BoutEndTime > BoutEnd] <- d$BoutStartTime[d$bout==i & d$WhaleID == rc & d$BoutEndTime > BoutEnd] + d$Duration[d$bout==i & d$WhaleID == rc & d$BoutEndTime > BoutEnd ]

      }

    }

  }

  d <-d[order(d$bout, d$BoutStartTime),]

  d <- d %>% mutate(
    BoutLatency = BoutStartTime - lag(BoutEndTime, 1),
  )
  d$BoutLatency[d$BoutStartTime == 0]<-NA

  d$Type <- "Rotated"

  d <- d[order(d$StartTime),]
  d <- d %>%
    mutate(
      Latency = StartTime - lag(EndTime, 1),
    )



  return(d)
}
