# Import data.
ffm <- read.csv("data5.csv", sep = "\t")

# Remove columns that will not be used.
ffm <- ffm[,!(names(ffm) %in% c("source", "country", "age", "race", "hand", "engnat"))]

# Initialize vectors to extract the personality trait data.
# The traits are the last 50 columns of the ffm data frame with each trait consisting of 10 consecutive columns.
traits.columns <- (ncol(ffm)-49):ncol(ffm)
e.columns <- traits.columns[1:10]
n.columns <- traits.columns[11:20]
a.columns <- traits.columns[21:30]
c.columns <- traits.columns[31:40]
o.columns <- traits.columns[41:50]

# Test for missing values in the questionnaire answer fields.
ffm[apply(ffm[,traits.columns], 1, function(x) {any(x == 0)}),]

# The only missing values are restricted to row 19065, where the entire
# questionnaire data is missing, so that row will be deleted.
ffm <- ffm[-19065,]

# Test for missing values in the gender field.
table(ffm$gender)

# There are 24 missing gender values.
# They will be imputed using Hot Deck Imputation.
library("HotDeckImputation")
ffm <- as.data.frame(impute.SEQ_HD(as.matrix(ffm), initialvalues = 1, navalues = 0, modifyinplace = TRUE))

# Since only Male and Female gender will be classified, remove all genders labelled as Other.
ffm <- ffm[ffm$gender != 3,]

dim(ffm)

# The final dataset contains 19615 observations and 51 variables.