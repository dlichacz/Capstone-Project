# Create side-by-side bar plots to examine the differences in the mean score for each trait by gender.
traits.means <- matrix(c(mean(ffm.totals[ffm.totals$gender == "Male", "E"]), mean(ffm.totals[ffm.totals$gender == "Female", "E"]),
                         mean(ffm.totals[ffm.totals$gender == "Male", "A"]), mean(ffm.totals[ffm.totals$gender == "Female", "A"]),
                         mean(ffm.totals[ffm.totals$gender == "Male", "C"]), mean(ffm.totals[ffm.totals$gender == "Female", "C"]),
                         mean(ffm.totals[ffm.totals$gender == "Male", "N"]), mean(ffm.totals[ffm.totals$gender == "Female", "N"]),
                         mean(ffm.totals[ffm.totals$gender == "Male", "O"]), mean(ffm.totals[ffm.totals$gender == "Female", "O"])),
                         nrow=2, ncol=5)

barplot(traits.means, beside=T, col=c("blue","red"), names.arg=traits.names, main="Trait means by gender")
legend("topleft", gender.factors, pch=15, col=c("blue","red"), bty="n")

# Create side-by-side bar plots to examine the differences in the mean score for each Likert answer type by gender.
types.means <- matrix(c(mean(ffm.likert[ffm.likert$gender == "Male", "Extreme"]), mean(ffm.likert[ffm.totals$gender == "Female", "Extreme"]),
                        mean(ffm.likert[ffm.likert$gender == "Male", "Moderate"]), mean(ffm.likert[ffm.totals$gender == "Female", "Moderate"]),
                        mean(ffm.likert[ffm.likert$gender == "Male", "Neutral"]), mean(ffm.likert[ffm.totals$gender == "Female", "Neutral"])),                
                      nrow=2, ncol=3)

barplot(types.means, beside=T, col=c("blue","red"), names.arg=likert.names, main="Likert answer type means by gender")
legend("topright", gender.factors, pch=15, col=c("blue","red"), bty="n")

# Create density plots with normal overlays to look for normal behavior in the test scores.
# Also perform an F-test to test for equality in the variances in male and female populations.
for (trait in traits.names) {
  trait.male <- ffm.totals[ffm.totals$gender == "Male", trait]
  trait.female <- ffm.totals[ffm.totals$gender == "Female", trait]
  m.male <- mean(trait.male)
  m.female <- mean(trait.female)
  sd.male <- sd(trait.male)
  sd.female <- sd(trait.female)
  par(mfrow=c(1,2))
  plot(density(trait.male), col="red", main=paste("Density for", trait, "- Male"), ylim=c(0, 0.065))
  lines(dnorm(0:40, m=m.male, sd=sd.male), col="blue") 
  plot(density(trait.female), col="red", main=paste("Density for", trait, "- Female"), ylim=c(0, 0.065))
  lines(dnorm(0:40, m=m.female, sd=sd.female), col="blue")
  
  print(paste("F-test p-value for", trait, "is", var.test(trait.male, trait.female, alternative = "t")$p.value))
}

# Create density plots with normal overlays to look for normal behavior in the Likert answer pattern.
# Also perform an F-test to test for equality in the variances in male and female populations.
for (type in likert.names) {
  type.male <- ffm.likert[ffm.likert$gender == "Male", type]
  type.female <- ffm.likert[ffm.likert$gender == "Female", type]
  m.male <- mean(type.male)
  m.female <- mean(type.female)
  sd.male <- sd(type.male)
  sd.female <- sd(type.female)
  par(mfrow=c(1,2))
  plot(density(type.male), col="red", main=paste("Density for", type, "- Male"))
  lines(seq(0,1,length=100), dnorm(seq(0,1,length=100), m=m.male, sd=sd.male), col="blue") 
  plot(density(type.female), col="red", main=paste("Density for", type, "- Female"))
  lines(seq(0,1,length=100),dnorm(seq(0,1,length=100), m=m.female, sd=sd.female), col="blue") 
  
  print(paste("F-test p-value for", type, "is", var.test(type.male, type.female, alternative = "t")$p.value))
}

# While the density plots indicate normality, many of the F-tests indicate unequal variances 
# in the two populations.  As a result, Welch's t-test will be performed to test for equality
# in the means of the two populations.
for (trait in traits.names) {
  trait.male <- ffm.totals[ffm.totals$gender == "Male", trait]
  trait.female <- ffm.totals[ffm.totals$gender == "Female", trait]
  mean.test <- t.test(trait.male, trait.female)
  print (paste("Welch t-test p-value for", trait, "is", mean.test$p.value))
}

for (type in likert.names) {
  type.male <- ffm.likert[ffm.likert$gender == "Male", type]
  type.female <- ffm.likert[ffm.likert$gender == "Female", type]
  mean.test <- t.test(type.male, type.female)
  print (paste("Welch t-test p-value for", type, "is", mean.test$p.value))
}