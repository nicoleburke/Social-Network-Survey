#### clean_qualtrics function
#### Social Network Survey package
#### 7.23.2019
#### script to run the qualtrics output through to prepare data

clean_qualtrics <- function(qualtricsoutput) {
  qualtricsoutput <- qualtricsoutput[order(qualtricsoutput$Participant....for.the.researcher.),]
  # hide this line of code for now - it will mess up the logic
  #qualtricsoutput <- qualtricsoutput[colSums(!is.na(qualtricsoutput)) > 0]
  # this replaces NAs with 9999
  qualtricsoutput[is.na(qualtricsoutput)] <- 9999
  library(tidyverse)
  newdataframe <- qualtricsoutput %>%
    rename(SubjectID = Participant....for.the.researcher.)
  return(newdataframe)
}
