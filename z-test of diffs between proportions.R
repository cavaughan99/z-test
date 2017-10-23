##create function to conduct a one-sample z-test of significant differences between the 
##observed sample value and expected population value

##p = expected population value, s = observed sample value, n = sample size, z = z-statistic
##create function to compute z-score
ztest.prop <- function(p, s, n) {
  sd <- sqrt((p * (1 - p))/n)
  z <- (s - p)/sd
  return(z)
}

##create function to compute p-value for 2-tailed test from z-score
ztest.pval <- function(p, s, n) {
  sd <- sqrt((p * (1 - p))/n)
  z <- (s - p)/sd
  zpval <- (pnorm(-1*(abs(z))))*2
  return(zpval)
}


##sum over numbers of users of different racial/ethnic groups to generate a total 
##number of users in each target area category for sample size inputs

##note that sample size is the same for every racial/ethnic category because it is the total
##number of users of the target area category across all racial/ethnic categories

totpau = 12883 + 10186 + 4654 + 1139
totspo = 9736 + 9119 + 4288 + 1066
totexer = 2134 + 1640 + 1371 + 495
totdgpk = 43 + 745 + 20 + 44
totplay = 3614 + 4642 + 1830 + 853

print(totpau)
print(totspo)
print(totexer)
print(totdgpk)
print(totplay)
##LATINOS


##input pop values, sample values, and ns for all 5 target area categories
plat <- c(.26, .27, .30, .27, .26)
slat <- c(.45, .40, .38, .5, .33)
nlat <- c(totpau, totspo, totexer, totdgpk, totplay)

##output z-scores for Latinos in all 5 target area categories
ztest.prop(plat, slat, nlat)

##output p-values for 2-tailed tests 
ztest.pval(plat, slat, nlat)

##WHITES

##input pop value, sample value, and n for all 5 target area categories
pwhite <- c(.43, .42, .37, .46, .43)
swhite <- c(.35, .38, .29, .87, .42)
nwhite <- c(totpau, totspo, totexer, totdgpk, totplay)

##output z-scores for whites in all 5 target area categories
ztest.prop(pwhite, swhite, nwhite)

##output p-values for 2-tailed test
ztest.pval(pwhite, swhite, nwhite)

##BLACKS

##input pop values, sample values, and ns for all 5 target area categories
pblack <- c(.19, .19, .20, .10, .19)
sblack <- c(.16, .18, .24, .02, .17)
nblack <- c(totpau, totspo, totexer, totdgpk, totplay)

##output z-scores for blacks in all 5 target area categories
ztest.prop(pblack, sblack, nblack)

##output p-values for 2-tailed tests
ztest.pval(pblack, sblack, nblack)

##OTHERS

##input pop values, sample values, and ns for all 5 target area categories
pother <- c(.12, .12, .12, .16, .12)
sother <- c(.04, .04, .09, .05, .08)
nother <- c(totpau, totspo, totexer, totdgpk, totplay)

##output z-scores for other racial/ethnic categories
ztest.prop(pother, sother, nother)

##output p-values for 2-tailed tests
ztest.pval(pother, sother, nother)

## Standard deviation. Compute the standard deviation (s) of the sampling distribution.
##s = sqrt[ P * ( 1 - P ) / n ]

##where P is the hypothesized value of population proportion in the null hypothesis, and n is the sample size.

##Test statistic. The test statistic is a z-score (z) defined by the following equation.
##z = (p - P) / s

##where P is the hypothesized value of population proportion in the null hypothesis, 
##p is the sample proportion, and s is the standard deviation of the sampling distribution.