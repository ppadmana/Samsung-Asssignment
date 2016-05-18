# Store the feature labels column of the features data frame as a vector.
features_vec1 <- as.vector(features$V2)

# Change the column names of the X_train data frame into the corresponding feature names using the colnames() function.
colnames(X_train) <- features_vec1
colnames(X_test) <- features_vec1

# Combine X_test and X_train into a single data frame using full_join() from dplyr.
X_tt2 <- full_join(X_test, X_train, by = features_vec1)

# Combine the y_test and y_train data frames.
Y_tt1 <- bind_rows(y_test, y_train)

# Combine the subject_test and subject_train data frames.
subject_tt <- bind_rows(subject_test, subject_train)

# Renaming the activity and subject columns.
colnames(subject_tt)[1] <- "Subject_Number"
colnames(Y_tt1)[1] <- "Activity_Number"

# Combine the subject and activity number column to the X_tt2 data frame.
X_tt3 <- bind_cols(Y_tt1, X_tt2)
X_tt4 <- bind_cols(subject_tt, X_tt3)

# Label the activity number column with their corresponding activity names.
function(x){if(x == 1){print("WALKING")} else if(x == 2)print("WALKING_UPSTAIRS") else if(x == 3)print("WALKING_DOWNSTAIRS") else if(x == 4)print("SITTING") else if(x == 5){print("STANDING")} else print("LAYING")}
X_tt5 <- mutate(X_tt4, Activity_Label = sapply(X_tt4$Activity_Number, renameR) )
# Make the activity label column the third column.
X_tt6 <- X_tt5[, c(1,2,564,3:563)]

# Selecting colums containing "mean" and "std".
X_tt7 <- select(X_tt6, contains("mean"))
X_tt8 <- select(X_tt7, -contains("angle"))
X_tt9 <- select(X_tt8, -contains("meanFreq"))
X_tt10 <- select(X_tt6, contains("std"))
X_tt11 <- bind_cols(X_tt9, X_tt10)

# The cleaned up data set !
X_tt12 <- bind_cols(Y_tt1, X_tt11)
X_tt13 <- bind_cols(subject_tt, X_tt12)
X_tt14 <- mutate(X_tt13, Activity_Label = sapply(X_tt13$Activity_Number, renameR) )
X_tt15 <- X_tt14[, c(1,2,69,3:68)]

# The final task !
X_tt16 <- X_tt15 %>%
  + group_by(Subject_Number, Activity_Number) %>%
  + select(-Subject_Number) %>%
  + select(-contains("Activity")) %>%
  + summarise_each(funs(mean))

# Adding back the Activity_Label column.
X_tt17 <- mutate(X_tt16, Activity_Label = sapply(X_tt16$Activity_Number, renameR) )
X_tt18 <- X_tt17[, c(1, 2, 69, 3:68)]
