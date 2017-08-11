#' Plot Confidential Interval
#'
#' A function to plot confidential interval for
#' such as \code{htest}, \code{glm}
#' (logistic regression only!)
#' and \code{posthocTGH} (\pkg{userfriendlyscience}) objects.
#'
#' @importFrom stats coefficients confint
#' @importFrom graphics abline arrows axis box plot points
#'
#' @export
#'
#' @param x an object: \code{htest},
#'          \code{glm} (logistic regression only!)
#'          or \code{posthocTGH} (\pkg{userfriendlyscience}).
#' @param xlog (logical) if \code{log} is \code{TRUE},
#'             the x axis is drawn logarithmically.
#'             Default is \code{FALSE}.
#' @param xlab a title for the plot.
#' @param xlim the x limits (x1, x2) of the plot.
#' @param yname If \code{yname} is \code{TRUE},
#'              the name of comparison between groups are shown.
#' @param las  numeric in {0,1,2,3}; the style of axis labels.
#'             Default is 0. see also \code{par}.
#' @param pch plotting 'character', i.e., symbol to use.
#' @param pcol color code or name of the points.
#' @param pcolbg background (fill) color for the open plot symbols
#'               given by 'pch = 21:25'.
#' @param pcex character (or symbol) expansion of points.
#' @param cilty line types of conficence intervals.
#' @param cilwd line width of conficence intervals.
#' @param cicol color code or name of conficence intervals.
#' @param v the x-value(s) for vertical line.
#' @param vlty line types of vertical line.
#' @param vlwd line width of vertical line.
#' @param vcol color code or name of vertical line.
#' @param main  a main title for the plot.
#' @param \dots other options for x-axis.
#'
#' @note \code{CIplot} was made based on \code{plot.TukeyHSD}.
#'  \preformatted{
#'  #  File src/library/stats/R/TukeyHSD.R
#'  #  Part of the R package, https://www.R-project.org
#'  #
#'  #  Copyright (C) 2000-2001  Douglas M. Bates
#'  #  Copyright (C) 2002-2015  The R Core Team
#' }
#'
#' @seealso \code{plot}, \code{axis}, \code{points}, \code{par}.
#'
#' @examples
#' require(graphics)
#'
#' ## 'htest' objects
#' set.seed(1234)
#' x <- rnorm(10, 10, 2); y <- rnorm(10, 8, 2)
#' res <- t.test(x, y)
#' CIplot(res)
#'
#' x <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
#' res <- fisher.test(x)
#' CIplot(res, xlog = TRUE)
#'
#' ## 'glm' object: logistic regression only!
#' ## odds ratio
#' require(MASS)
#' data(birthwt)
#' x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
#'          family = binomial)
#' CIplot(x, las = 1)
#'
#' ## 'posthocTGH' object
#' ## Tukey or Games-Howell methos
#' if (require(userfriendlyscience)) {
#'     x <- posthocTGH(warpbreaks$breaks, warpbreaks$tension)
#'     CIplot(x, las = 1)
#' }
#'
#' @keywords plot
#' 
CIplot <-
    function(x, ...) UseMethod("CIplot")

