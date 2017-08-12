#' Calculate odds ratios and their confidence intervals
#' from \code{glm} object
#'
#' @import MASS
#'
#' @export
#'
#' @param x \code{glm} object (logistic regression only!).
#' @param conf.level the confidence interval. Default is 0.95.
#' @return an object class \code{ORci} with three columns.
#'   \describe{
#'    \item{OR}{odds ratio}
#'    \item{lwr}{lower conficence intarval}
#'    \item{upr}{upper conficence intarval}
#'   }
#'
#' @examples
#' require(graphics)
#' require(MASS)
#' data(birthwt)
#' x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
#'          family = binomial)
#' OR1 <- ORci(x)
#' CIplot(OR1, las = 1)

ORci <-
    function(x, conf.level = 0.95)
{
    est <- coefficients(x)
    ci <- confint(x, level = conf.level)

    OR <- exp(cbind(est, ci)[-1,])
    colnames(OR) <- c("OR", "lwr", "upr")

    attr(OR, "conf.level") <- conf.level
    attr(OR, "class") <- "ORci"
    return(OR)
}

