\name{print.ORci}
\alias{print.ORci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Print Methods for Odds Ratios and their Confidence Intervals
  of \code{ORci} object
}
\description{
  Print odds ratios and their confidence intervals
  of \code{ORci} object.
}
\usage{
\method{print}{ORci}(x, ...)
}
\arguments{
  \item{x}{
    '\code{ORci}' object. see alse \code{\link{ORci}}.
  }
  \item{\dots}{
    other options for print such as \code{digits}.
  }
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
%% \value{
%% }
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
\packageAuthor{CIplot}
}
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{glm}, \code{\link{ORci}}.
}
\examples{
require(MASS)
data(birthwt)
x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
         family = binomial)

OR1 <- ORci(x)
print(OR1, digits = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glm }% use one of  RShowDoc("KEYWORDS")
\keyword{ odds ratio }% __ONLY ONE__ keyword per line
\keyword{ print }% __ONLY ONE__ keyword per line
