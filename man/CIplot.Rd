\name{CIplot}
\alias{CIplot}
\alias{CIplot.htest}
\alias{CIplot.glm}
\alias{CIplot.ORci}
\alias{CIplot.Tukey}
\alias{CIplot.posthocTGH}
\title{Plot Confidential Interval}
\description{
    A function to plot confidential interval for
    such as '\code{htest}', '\code{glm}'
    (logistic regression only!)
    and '\code{posthocTGH}' (\pkg{userfriendlyscience}) objects.
}
\usage{
CIplot(x, ...)
\method{CIplot}{htest}(x,
       log = FALSE,
       xlim = NULL, xlab = NULL, main = NULL,
       pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
       cilty = 1, cilwd = 1, cicol = "black",
       v = NULL, vlty = 2, vlwd = 1,  vcol = "black",
       ...)
\method{CIplot}{glm}(x,
       conf.level = 0.95,
       xlim = NULL, xlab = "Odds Ratio",
       main = NULL,
       pch = 21, pcol = "black",
       pcolbg = "white", pcex = 1, cilty = 1, cilwd = 1,
       cicol = "black", vlty = 2, vlwd = 1,  vcol = "black",
       las = NULL,
       ...)
\method{CIplot}{ORci}(x,
       xlim = NULL, xlab = "Odds Ratio", main = NULL,
       pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
       cilty = 1, cilwd = 1, cicol = "black",
       vlty = 2, vlwd = 1,  vcol = "black",
       las = NULL,
       ...)
\method{CIplot}{posthocTGH}(x,
       xlim = NULL, xlab = "Differences in mean", main = NULL,
       pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
       cilty = 1, cilwd = 1, cicol = "black",
       vlty = 2, vlwd = 1,  vcol = "black",
       las = NULL,
       ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    an object: '\code{htest}', '\code{glm}' (logistic regression only!)
    or '\code{posthocTGH}' (\pkg{userfriendlyscience}).
}
\item{log}{
  '\code{htest}' object only.
  (logical) if \code{log} is \code{TRUE},
  the x axis is drawn logarithmically.
  Default is \code{FALSE}.
}
\item{xlim}{
  the x limits (x1, x2) of the plot.
}
\item{xlab}{
  a title for the plot.
}
\item{main}{
  a main title for the plot.
}
\item{pch}{
  plotting 'character', i.e., symbol to use.
}
\item{pcol}{
  color code or name of the points.
}
\item{pcolbg}{
  background (fill) color for the open plot symbols
  given by 'pch = 21:25'.
}
\item{pcex}{
  character (or symbol) expansion of points.
}
\item{cilty}{
  line types of conficence intervals.
}
\item{cilwd}{
  line width of conficence intervals.
}
\item{cicol}{
  color code or name of conficence intervals.
}
\item{v}{
  '\code{htest}' object only.
  the x-value(s) for vertical line.
}
\item{vlty}{
  line types of vertical line.
}
\item{vlwd}{
  line width of vertical line.
}
\item{vcol}{
  color code or name of vertical line.
}
\item{conf.level}{
  '\code{glm}' object only.
  the confidence interval. Default is 0.95.
  see also \code{\link{ORci}}.
}
\item{las}{
  numeric in {0,1,2,3}; the style of axis labels.
  see also \code{par}.
}
\item{\dots}{
  other options for x-axis.
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
%% \value{
%% %%  ~Describe the value returned
%% %%  If it is a LIST, use
%% %%  \item{comp1 }{Description of 'comp1'}
%% %%  \item{comp2 }{Description of 'comp2'}
%% %% ...
%% }
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
\packageAuthor{CIplot}
}
\note{
  \code{CIplot} was made based on \code{plot.TukeyHSD}.

  \preformatted{
    #  File src/library/stats/R/TukeyHSD.R
    #  Part of the R package, https://www.R-project.org
    #
    #  Copyright (C) 2000-2001  Douglas M. Bates
    #  Copyright (C) 2002-2015  The R Core Team
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{plot}, \code{axis}, \code{points}, \code{par}.
}
\examples{
require(graphics)

## 'htest' objects
set.seed(1234)
x <- rnorm(10, 10, 2); y <- rnorm(10, 8, 2)
res <- t.test(x, y)
CIplot(res)

x <- matrix(c(10, 7, 8, 9), 2, 2, byrow = TRUE)
res <- fisher.test(x)
CIplot(res, log = TRUE)

## 'glm' object: logistic regression only!
## odds ratio
require(MASS)
data(birthwt)
x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
         family = binomial)
CIplot(x, las = 1)

## 'posthocTGH' object
## Tukey or Games-Howell methos
if (require(userfriendlyscience)) {
    x <- posthocTGH(warpbreaks$breaks, warpbreaks$tension)
    CIplot(x, las = 1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }% use one of  RShowDoc("KEYWORDS")
\keyword{ htest }% __ONLY ONE__ keyword per line
\keyword{ glm }% __ONLY ONE__ keyword per line
