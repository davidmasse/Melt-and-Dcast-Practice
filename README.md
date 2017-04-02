The script run_analysis.R uses packages reshape2 and dplyr.  The first three lines of code download the zipped data, unzips them, and then sets the working directory to the unzipped folder. 

The script patches together and cleans up real-world data from text files to produce two tidy dataframes, the second a summary of the first.  After running the script, the summary dataframe can be read back in from the textfile, "output.txt":

data <- read.table("output.txt", header = TRUE)

Thirty subjects performed six different activities while their smartphones' accelerometers recorded data.  The data were randomly split between a test group (30%) and a training group (70%).  For each group we have three separate text files containing the same number of rows in the same order: 1) identifiers (integers 1 to 30) for the subjects,  2) identifiers (integers 1 to 6) for the activities, and 3) numerical measurements of 561 "features" per observation.

The script first reads in all six files containing data, using cbind to pull together the three files in each group and then rbind to bring together the two groups. As loaded, the column names are meaningless, so we use the provided features list, changing the second column from factors to strings.  Grep then searches these strings for those containing "mean()" or "std()" and returns a vector of indices of where these matches (66 of them) are in the features list. There are other features that contain some kind of mean such as meanFreq, but this is dependent on frequencies from a Fourrier transform - not the simple mean mentioned in the question. 

The big dataframe with all the data has two more columns (563) than there are features (561) because we added subject and activity identifiers.  We also want to select for these, so the index vector is modified by adding  1 and 2 in the first two positions and increasing the remaining indices by 2.  The "slimmed" dataframe therefore has only 68 columns selected by the vector just constructed.

The next few lines read in the integers assigned to each activity.  Since this contains a column with numbers corresponding to a column of numbers in the slimmed dataframe, we merge them on this id.  (There will be a warning message about duplicate labels, but all of these are removed or replaced later, so inore it.)  We are left with a data frame with activities both named and numbered.  This would not be tidy (more than one column per variable), so we remove the (first) column with the activity numbers, leaving only the descriptive activity names. The remaing data are tidy with a range of measurements - in columns - per observation of a subject and activity.

Next we give descriptive variable names to all the columns.  We start by renaming all the columns with "activity" "subject" and the rest of the features with mean() or std().  We then move the "subject" column to the left of "activity" (simply by convention putting the id for the participant on the far left). We then use a series of substitutions ("Time" for "t" at the beginning and so on) to make various aspects of the names more clear (as described in the codebook).  The resulting dataframe, "tidy," is tidy because all observations are still in separate rows while now the columns each represent one variable, and the overall table contains all the data about a particular aspect of the experiment.

In order to produce a summary (by mean) of all combinations of subjects and activities, we first melt the dataframe keeping only these first two columns as ids.  We then dcast the molten dataframe with a formula specifying that we want values for each variable to be averaged, grouped by subject and activity.  This dataframe is then written to a text file "output.txt" which was separately turned in as part of this assingment.   The summary data frame is also tidy because every row can be thought of as an observation of a particular subject/activity combination (180 of them) showing the mean value of 66 different variables corresponding to each.

The optional code at the bottom just takes the rows in the tidy dataframe associated with subject numeber 1 and activity "LAYING".  It then checks whether the mean of the "TimeBodyAccMeanX" column for just these rows matches the summary value in the third column of the first row of the summary "output" dataframe.