# Written by: Emil Bach; 25-02-2020; Version 1.1
# Version 1.1: Added functionality ot check for multiple p-values else return p.value. In addition,
# added replace to replace p.values == 0 to .Machine$double.xmin
# Dependecies: stats, base
# Function to perform Fisher's combined probability test on a set of p-values.
# In its basic form, it is used to combine the results from several independent tests bearing upon 
# the same overall hypothesis (H0).
# Fisher's method combines extreme value probabilities from each test, commonly known as "p-values", 
# into one test statistic (X2) using the formula: 
#   X2 = -2*sum(ln(pi))
# X2 has a chi-squared distribution with 2k degrees of freedom.
# The function first calculates the X2 value and then looks up the combined p-value within the X2 distribution.

#Start of function code
fishers_method <- function(p.values){
  if(length(p.values > 1)){
    p.values <- replace(p.values, 0, .Machine$double.xmin)
    pchisq(-2*sum(log(p.values)), df = length(p.values)*2, lower.tail = F)
  } else  {
    p.values
  }
}
