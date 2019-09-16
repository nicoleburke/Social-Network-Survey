#### lang_perrelationship function

#### Social Network Survey
#### 9.16.2019
#### language information per relationship

language_perrelationship <- function(qualtricsoutput, relationshipdf) {
  # a vector with the subjectIDs from the Qualtrics Survey
  subjids <- unique(qualtricsoutput$SubjectID)
  # get the index in the newdataframe you are creating per subject
  for (s in 1:length(subjids)) {
    print("subjectID")
    print(s)
    print(subjids[s])
    # create a vector of index per subject of the values in the newdataframe
    subjindex <- grep(subjids[s], relationshipdf$SubjectID)
    print("subjindex")
    print(subjindex)
    # Where the function should start searching per subject in the newdataframe
    starti <- subjindex[1]
    # print(starti)
    # Where the function should stop searching per subject in the newdataframe
    endi <- subjindex[length(subjindex)]
    for (i in starti:endi) {
      ############################################## PARENT 1
      if (relationshipdf$Nodes[i] == "Parent 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Parent 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## PARENT 2
      if (relationshipdf$Nodes[i] == "Parent 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Parent 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## PARENT 4
      if (relationshipdf$Nodes[i] == "Parent 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Parent 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## PARENT 4
      if (relationshipdf$Nodes[i] == "Parent 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Parent 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 1
      if (relationshipdf$Nodes[i] == "Sibling 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 2
      if (relationshipdf$Nodes[i] == "Sibling 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 4
      if (relationshipdf$Nodes[i] == "Sibling 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 4
      if (relationshipdf$Nodes[i] == "Sibling 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 5
      if (relationshipdf$Nodes[i] == "Sibling 5") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 5") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 6
      if (relationshipdf$Nodes[i] == "Sibling 6") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 6") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 7
      if (relationshipdf$Nodes[i] == "Sibling 7") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 7") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 8
      if (relationshipdf$Nodes[i] == "Sibling 8") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 8") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 9
      if (relationshipdf$Nodes[i] == "Sibling 9") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 9") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## SIBLING 10
      if (relationshipdf$Nodes[i] == "Sibling 10") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Sibling 10") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 1
      if (relationshipdf$Nodes[i] == "Friend 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 2
      if (relationshipdf$Nodes[i] == "Friend 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 3
      if (relationshipdf$Nodes[i] == "Friend 3") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 3") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 4
      if (relationshipdf$Nodes[i] == "Friend 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 5
      if (relationshipdf$Nodes[i] == "Friend 5") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 5") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 6
      if (relationshipdf$Nodes[i] == "Friend 6") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 6") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 7
      if (relationshipdf$Nodes[i] == "Friend 7") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 7") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 8
      if (relationshipdf$Nodes[i] == "Friend 8") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 8") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 9
      if (relationshipdf$Nodes[i] == "Friend 9") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 9") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 10
      if (relationshipdf$Nodes[i] == "Friend 10") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 10") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## TEACHER 1
      if (relationshipdf$Nodes[i] == "Teacher 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Teacher 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## TEACHER 2
      if (relationshipdf$Nodes[i] == "Teacher 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Teacher 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## TEACHER 3
      if (relationshipdf$Nodes[i] == "Teacher 3") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Teacher 3") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## TEACHER 4
      if (relationshipdf$Nodes[i] == "Teacher 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Teacher 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## TEACHER 5
      if (relationshipdf$Nodes[i] == "Teacher 5") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Teacher 5") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## DAYCARE CLASS
      if (relationshipdf$Nodes[i] == "Daycare Class") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Daycare Class") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 1 PARENT 1
      if (relationshipdf$Nodes[i] == "Friend 1 Parent 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 1 Parent 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 1 PARENT 2
      if (relationshipdf$Nodes[i] == "Friend 1 Parent 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 1 Parent 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 2 PARENT 1
      if (relationshipdf$Nodes[i] == "Friend 2 Parent 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 2 Parent 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## FRIEND 2 PARENT 2
      if (relationshipdf$Nodes[i] == "Friend 2 Parent 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Friend 2 Parent 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 1 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 1 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 1 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 2 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 2 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 2 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 3 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 3 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 3 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 4 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 4 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 4 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 5 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 5 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 5 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 6 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 6 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 6 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 7 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 7 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 7 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 8 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 8 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 8 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 9 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 9 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 9 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 10 TEACHER
      if (relationshipdf$Nodes[i] == "Extracurricular Class 10 Teacher") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 10 Teacher") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 1 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 1 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 1 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 2 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 2 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 2 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 3 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 3 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 3 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 4 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 4 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 4 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 5 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 5 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 5 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 6 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 6 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 6 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 7 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 7 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 7 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 8 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 8 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 8 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 9 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 9 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 9 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTRACIR CLASS 10 KIDS
      if (relationshipdf$Nodes[i] == "Extracurricular Class 10 Kids") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Extracurricular Class 10 Kids") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## DAYCARE ASSISTANT 1
      if (relationshipdf$Nodes[i] == "Daycare Assistant 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Daycare Assistant 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## DAYCARE ASSISTANT 2
      if (relationshipdf$Nodes[i] == "Daycare Assistant 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Daycare Assistant 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## DAYCARE ASSISTANT 3
      if (relationshipdf$Nodes[i] == "Daycare Assistant 3") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Daycare Assistant 3") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 1
      if (relationshipdf$Nodes[i] == "ExtendedFam 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 2
      if (relationshipdf$Nodes[i] == "ExtendedFam 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 3
      if (relationshipdf$Nodes[i] == "ExtendedFam 3") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam3") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 4
      if (relationshipdf$Nodes[i] == "ExtendedFam 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 5
      if (relationshipdf$Nodes[i] == "ExtendedFam 5") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam5") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 6
      if (relationshipdf$Nodes[i] == "ExtendedFam 6") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam6") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 7
      if (relationshipdf$Nodes[i] == "ExtendedFam 7") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam7") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 8
      if (relationshipdf$Nodes[i] == "ExtendedFam 8") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam8") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 9
      if (relationshipdf$Nodes[i] == "ExtendedFam 9") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam9") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 10
      if (relationshipdf$Nodes[i] == "ExtendedFam 10") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam10") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 11
      if (relationshipdf$Nodes[i] == "ExtendedFam 11") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam11") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 12
      if (relationshipdf$Nodes[i] == "ExtendedFam 12") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam12") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 13
      if (relationshipdf$Nodes[i] == "ExtendedFam 13") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam13") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 14
      if (relationshipdf$Nodes[i] == "ExtendedFam 14") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam14") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## EXTENDED FAM 15
      if (relationshipdf$Nodes[i] == "ExtendedFam 15") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "ExtendedFam15") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## GRANDPARENT 1
      if (relationshipdf$Nodes[i] == "Grandparent 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Grandparent 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## GRANDPARENT 2
      if (relationshipdf$Nodes[i] == "Grandparent 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Grandparent 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## GRANDPARENT 3
      if (relationshipdf$Nodes[i] == "Grandparent 3") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Grandparent 3") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## GRANDPARENT 4
      if (relationshipdf$Nodes[i] == "Grandparent 4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Grandparent 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## GRANDPARENT 5
      if (relationshipdf$Nodes[i] == "Grandparent 5") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Grandparent 5") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## NANNY/BABYSITTER 1
      if (relationshipdf$Nodes[i] == "Nanny/Babysitter 1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Nanny/Babysitter 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## NANNY/BABYSITTER 2
      if (relationshipdf$Nodes[i] == "Nanny/Babysitter 2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Nanny/Babysitter 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## OTHER 1
      if (relationshipdf$Nodes[i] == "Other1") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Other 1") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## OTHER 2
      if (relationshipdf$Nodes[i] == "Other2") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Other 2") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## OTHER 3
      if (relationshipdf$Nodes[i] == "Other3") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Other 3") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
      ############################################## OTHER 4
      if (relationshipdf$Nodes[i] == "Other4") {
        print("s")
        print(s)
        # 's' will specify what row to look for in the qualtricsoupt, we need the column index
        indexcol <- grep("Please.answer.the.following.questions.for.this.person.people...Selected.Choice", colnames(qualtricsoutput))
        for (x in 1:length(indexcol)) {
          if (as.character(qualtricsoutput[s,indexcol[x]]) == "Other 4") {
            # get the value in qualdricsoutput
            lang <- qualtricsoutput[s,indexcol[x]+89]
            # needs to be a value
            lang <- as.character(lang)
            # if data is blank, print "MISSING"
            if (lang == "") {
              relationshipdf[i,6] <- "MISSING"
            }
            # otherwise, print value
            else {
              relationshipdf[i,6] <- lang
            }
          }
        }
      }
    }
  }
  return(relationshipdf)
}
