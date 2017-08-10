########################################
### Example
########################################

source("CIplot.R")

####################
# CIplot.htest
####################

set.seed(1234)
x <- rnorm(10, 10, 2); y <- rnorm(10, 8, 2)
res <- t.test(x, y)
CIplot(res)

x <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
res <- fisher.test(x)
CIplot(res, log = TRUE)


####################
# CIplot.OddsRatio
# glm(formula, family = binomial)
####################

library(MASS)
data(birthwt)

x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
         family = binomial)
CIplot(x, las = 1)


####################
# CIplot.Tukey
# userfriendlyscience
# Tukey, Games-Howell
####################

library(userfriendlyscience)
x <- posthocTGH(warpbreaks$breaks, warpbreaks$tension)
CIplot(x, las = 1)


