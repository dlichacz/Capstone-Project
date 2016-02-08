# Create factors for the gender column.
gender.factors <- c("Male", "Female")
ffm$gender <- factor(ffm$gender, labels = gender.factors)

# Calculate the total score for each personality trait using the formulas from personality-testing.info 
# and store them in a data frame.
ffm.totals <- as.data.frame(cbind(gender = ffm$gender,
                                  E = 20 + ffm$E1 - ffm$E2 + ffm$E3 - ffm$E4 + ffm$E5 - ffm$E6 + ffm$E7 - ffm$E8 + ffm$E9 - ffm$E10,
                                  A = 14 - ffm$A1 + ffm$A2 - ffm$A3 + ffm$A4 - ffm$A5 + ffm$A6 - ffm$A7 + ffm$A8 + ffm$A9 + ffm$A10,
                                  C = 14 + ffm$C1 - ffm$C2 + ffm$C3 - ffm$C4 + ffm$C5 - ffm$C6 + ffm$C7 - ffm$C8 + ffm$C9 + ffm$C10,
                                  N = 38 - ffm$N1 + ffm$N2 - ffm$N3 + ffm$N4 - ffm$N5 - ffm$N6 - ffm$N7 - ffm$N8 - ffm$N9 - ffm$N10,
                                  O = 8 + ffm$O1 - ffm$O2 + ffm$O3 - ffm$O4 + ffm$O5 - ffm$O6 + ffm$O7 + ffm$O8 + ffm$O9 + ffm$O10))

ffm.totals$gender <- factor(ffm.totals$gender, labels = gender.factors)
# Create a vector of the traits column names for future use.
traits.names <- colnames(ffm.totals[,-1])

# Since some of the test questions are phrased positively with respect to a given traits and others
# are phrased negatively, the negative questions will be transformed to allow for direct comparison.

# Create vectors for each trait where the ith element of the vector is 1 if question i is positive
# and -1 if it is negative.
e.signs <- c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1)
n.signs <- c(-1, 1, -1, 1, -1, -1, -1, -1, -1, -1)
a.signs <- c(-1, 1, -1, 1, -1, 1, -1, 1, 1, 1)
c.signs <- c(1, -1, 1, -1, 1, -1, 1, -1, 1, 1)
o.signs <- c(1, -1, 1, -1, 1, -1, 1, 1, 1, 1)
signs <- c(e.signs, n.signs, a.signs, c.signs, o.signs)

# Create a data frame with only the test answers.
ffm.traits <- ffm[,traits.columns]

# If the question is negative, reverse the order of the Likert scale.
for (i in 1:ncol(ffm.traits)) {
  if (signs[i] == -1) {
    ffm.traits[,i] <- 6 - ffm.traits[,i]
  }
}

# Recreate the full data frame using the transformed data and drop temporary data frame.
ffm <- cbind(gender=ffm$gender, ffm.traits)
rm(ffm.traits)

# Calculate the percentage of Extreme answers (1 or 5), Moderate answers (2 or 4) and Neutral answers (3) 
# given by each respondent and store the results in a data frame.
ffm.likert <- as.data.frame(cbind(gender = factor(ffm$gender, labels = gender.factors),
                                  Extreme = apply(ffm[,traits.columns],1, function(x) (sum(x == 1) + sum(x == 5))/50),
                                  Moderate = apply(ffm[,traits.columns],1, function(x) (sum(x == 2) + sum(x == 4))/50),
                                  Neutral = apply(ffm[,traits.columns],1, function(x) sum(x == 3)/50)))

ffm.likert$gender <- factor(ffm.likert$gender, labels = gender.factors)
likert.names <- colnames(ffm.likert[,-1])
