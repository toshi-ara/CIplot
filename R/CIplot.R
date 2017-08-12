#' Plot Confidential Interval
#'
#' A function to plot confidential interval for
#' such as \code{htest}, \code{TukeyHSD},
#' \code{glht} (\pkg{multcomp}),
#' \code{glm} (logistic regression only!)
#' and \code{posthocTGH} (\pkg{userfriendlyscience}) objects.
#'
#' @importFrom stats coefficients confint
#' @importFrom graphics abline arrows axis box plot points
#'
#' @export
#'
#' @param x an object: \code{htest}, \code{TukeyHSD},
#'          \code{glht} (\pkg{multcomp}),
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
#' ##### 'htest' objects
#' ## t test
#' set.seed(1234)
#' a <- rnorm(10, 10, 2); b <- rnorm(10, 8, 2)
#' x <- t.test(a, b)
#' CIplot(x)
#'
#' ## binomial test
#' x <- binom.test(5, 20)
#' CIplot(x, xlim = c(0, 1))
#'
#' ## Fisher's exact test
#' x <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
#' res <- fisher.test(x)
#' CIplot(res, xlog = TRUE)
#'
#' ##### 'TukeyHSD' objects
#' ## Tukey test
#' aov1 <- aov(breaks ~ tension + wool, data = warpbreaks)
#' x <- TukeyHSD(aov1)
#'
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow = c(1, 2))
#' CIplot(x, las = 1)
#' par(oldpar)
#'
#' ## example of line type and color
#' aov1 <- aov(breaks ~ tension, data = warpbreaks)
#' x <- TukeyHSD(aov1)
#' CIplot(x, las = 1,
#'        pcol = 2:4, pcolbg = 2:4, cicol = 2:4,
#'        vlty = 1, vcol = "gray")
#'
#' ##### 'glht' objects
#' ## Tukey test
#' require(multcomp)
#' aov1 <- aov(breaks ~ tension, data = warpbreaks)
#' x <- glht(aov1, linfct = mcp(tension = "Tukey"))
#' CIplot(x, las = 1)
#'
#' ## Dunnett test
#' x <- glht(aov1, linfct = mcp(tension = "Dunnett"))
#' CIplot(x, las = 1)
#'
#' ##### 'glm' object: logistic regression only!
#' ## odds ratio
#' require(MASS)
#' data(birthwt)
#' x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
#'          family = binomial)
#' CIplot(x, las = 1)
#'
#' ##### 'posthocTGH' object
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

