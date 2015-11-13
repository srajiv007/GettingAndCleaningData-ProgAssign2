# GettingAndCleaningData-ProgAssign2
Programming Assignment 2 (Getting &amp; Cleaning Data)

> Files in this Repository

- run_analysis.R

- HAR Tidy Dataset.txt

> Code description

- read y_ dataset and join with activity labels
- combine activity labeled dataset with subject table
- read x_ dataset and filter for mean()/std() columns
- combine y_ dataset and x_ dataset
- repeat above for both test & train datasets & form single dataset
- summarize final table by (subject, activity-label)
- write summary table to txt file
