#### relationship_perline function

#### Social Network Survey package
#### 7.23.2019
#### script to make a dataframe with each relationships per line

# create a function that separates out the dataframe
relationship_perline <- function(qualtricsoutput) {
  library(tidyverse)
  newdataframe <- qualtricsoutput %>%
    group_by(SubjectID) %>%
    summarize(Nodes = Please.select.all.the.people.who.your.child.sees.in.a.regular.week...Selected.Choice) %>%
    mutate(Nodes = strsplit(as.character(Nodes), ",")) %>%
    unnest(Nodes)
  newdataframe$Gender <- NA
  newdataframe$Race <- NA
  newdataframe$Age
  newdataframe$Language
  return(newdataframe)
}


