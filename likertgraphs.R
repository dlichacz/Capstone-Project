# Create Likert graphs for similarites among test responses.

require(likert)

# Create vector containing the labels for the Likert values.
likert.labels <- c("Strongly Disagree","Disagree", "Neutral","Agree","Strongly Agree")

# Plot stacked Likert graphs for all 50 responses as well as for each of the five traits.
plot(likert(data.frame(lapply(ffm[,rev(traits.columns)], factor, levels=1:5, labels=likert.labels))), 
     main = "All Questions", ylab = "Question")
plot(likert(data.frame(lapply(ffm[,rev(e.columns)], factor, levels=1:5, labels=likert.labels))), 
     main = "Extrovertism", ylab = "Question")
plot(likert(data.frame(lapply(ffm[,rev(n.columns)], factor, levels=1:5, labels=likert.labels))), 
     main = "Neuroticism", ylab = "Question")
plot(likert(data.frame(lapply(ffm[,rev(a.columns)], factor, levels=1:5, labels=likert.labels))), 
     main = "Agreeableness", ylab = "Question")
plot(likert(data.frame(lapply(ffm[,rev(c.columns)], factor, levels=1:5, labels=likert.labels))), 
     main = "Conscientiousness", ylab = "Question")
plot(likert(data.frame(lapply(ffm[,rev(o.columns)], factor, levels=1:5, labels=likert.labels))), 
     main = "Openness to Experience", ylab = "Question")