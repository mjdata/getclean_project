# run_analysis.R
# Data Getting and Collecting Course Project
# This R script reads the raw datasets, cleans the data, creates tidy data
# sets and writes a tidy data sets.

run_analysis <- function() {

    library("data.table")
    library(readr)
    library(plyr)
    library(dplyr)
    library(stringr)
    library(reshape2)
    library(tidyr)

# 1. Reading the training and test data sets into R
    dataDir <- "./data"
    options(stringsAsFactors = F)

    fpath <- file.path(dataDir, "activity_labels.txt")
    activityLabel <- read.table(fpath)[,2]

    fpath <- file.path(dataDir, "features.txt")
    feature_all <- read.table(fpath)[,2]

    fpath <- file.path(dataDir, "train", "X_train.txt")
    train_data <- read_table(fpath, col_names = FALSE)

    fpath <- file.path(dataDir, "train", "Y_train.txt")
    train_activity <- read.table(fpath)

    fpath <- file.path(dataDir, "train", "subject_train.txt")
    train_subject <- read.table(fpath)


    fpath <- file.path(dataDir, "test", "X_test.txt")
    test_data <- read_table(fpath, col_names = FALSE)

    fpath <- file.path(dataDir, "test", "Y_test.txt")
    test_activity <- read.table(fpath)


    fpath <- file.path(dataDir, "test", "subject_test.txt")
    test_subject <- read.table(fpath)

# 2. Extracting only the measurements on the mean and standard deviation for each
# measurement.

    ind <- which(str_detect(feature_all, "(mean|std)\\(\\)") == TRUE)
    feature <- feature_all[ind]
    train_extract <- train_data[, ind]
    test_extract <- test_data[, ind]

# 3. Merging the training and the test sets to create one data set:

    train <- bind_cols(list(train_activity, train_subject, train_extract))
    names(train)[1:2] = c("activity","subject")

    test <- bind_cols(list(test_activity, test_subject, test_extract))
    names(test)[1:2] = c("activity","subject")

    bigTidy <- bind_rows(train, test)

# 4. Naming the activities in the data set useing descriptive activity names

    bigTidy$activity <- str_to_lower(activityLabel[bigTidy$activity])

# 5. Labeling the data sets with modified descriptive variable names.

    itt <- which(str_sub(feature, 1, 1) == "t")
    iff <- which(str_sub(feature, 1, 1) == "f")
    str_sub(feature[itt], 1, 1) <- "time."
    str_sub(feature[iff], 1, 1) <- "freq."
    featureModified <- feature %>%
        str_replace("-mean\\(\\)", ".mean") %>%
        str_replace("-std\\(\\)", ".std") %>%
        str_replace(".mean-X", ".x.mean") %>%
        str_replace(".mean-Y", ".y.mean") %>%
        str_replace(".mean-Z", ".z.mean") %>%
        str_replace(".std-X", ".x.std") %>%
        str_replace(".std-Y", ".y.std") %>%
        str_replace(".std-Z", ".z.std") %>%
        str_replace("BodyBody", "body.") %>%
        str_replace("Body", "body.") %>%
        str_replace("Gravity", "gravity.") %>%
        str_replace("Jerk", ".jerk") %>%
        str_replace("Mag", ".magnitude") %>%
        str_replace("Acc", "accelerat") %>%
        str_replace("Gyro", "gyroscope")

    featureModified <- str_to_lower(featureModified)

    names(bigTidy)[3:68] <- featureModified

# 6. Melting the bigTidy resulted in a long form, using reshape2::melt()

    longBig <- melt(bigTidy, id = c("activity", "subject"))

# Creating a second tidy data set with the average of each variable
# for each activity and each subject.

    tidy_wide <- dcast(longBig, activity + subject ~ variable, mean)

# 7. Labeling the new tidy data set with the new variable names, which describe
# that each row of the values represents the average value of each original
# variable for each activity and each subject.

    names(tidy_wide)[1:2] <- c("activity", "subject")
    names(tidy_wide)[3:68] <- str_c(featureModified, ".average")

# Alternatively, a long form of the tidy data sets can be created:
#    tidy_long <- melt(tidy_wide, id = c("activity", "subject"))
# or tidy_long <- tidy_wide %>% gather(variable, value, -activity, -subject )

# 8. Writing the tidy_wide data set using write.table() with row.names = FALSE

    tidyPath <- file.path("tidy_wide.txt")
    write.table(tidy_wide, tidyPath, row.names = FALSE)

    tidyPath
}
# Run the analysis with run_analysis()
# The raw datasets are in "./data" under my working directory.

# The tidy data set can be read into R using read.table with header=TRUE
#tidy <- read.table("tidy_wide.txt", header = TRUE)
#dim(tidy)
##[1] 180  68


