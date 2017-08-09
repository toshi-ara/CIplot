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

dat <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
res <- fisher.test(dat)
CIplot(res, log = TRUE)


####################
# CIplot.OddsRatio
# glm(formula, family = binomial)
####################

library(MASS)
data(birthwt)

GLM1 <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
            family = binomial)
CIplot(GLM1, las = 1)

## res <- ORci(GLM1, conf.level = 0.95)
## CIplot(res, las = 1)

####################
# CIplot.Tukey
# userfriendlyscience
# Tukey, Games-Howell
####################

library(userfriendlyscience)
res <- posthocTGH(warpbreaks$breaks, warpbreaks$tension)
CIplot(res, las = 1)


