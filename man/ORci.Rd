\name{ORci}
\alias{ORci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate odds ratios and their confidence intervals
  from '\code{glm}' object
}
\description{
  Return odds ratios and their confidence intervals
  from '\code{glm}' object (logistic regression only!)
  as '\code{ORci}' object.
}
\usage{
ORci(x, conf.level = 0.95)
}
\arguments{
  \item{x}{
    '\code{glm}' object (logistic regression only!).
  }
  \item{conf.level}{
    the confidence interval. Default is 0.95.
  }
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
  an object class '\code{ORci}' with three columns.
  \item{OR}{odds ratio}
  \item{lwr}{lower conficence intarval}
  \item{upr}{upper conficence intarval}
}
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
  \code{glm}.
}
\examples{
require(graphics)
require(MASS)
data(birthwt)
x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
         family = binomial)
OR1 <- ORci(x)
CIplot(OR1, las = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ glm }% use one of  RShowDoc("KEYWORDS")
\keyword{ odds ratio }% __ONLY ONE__ keyword per line
\keyword{ plot }% __ONLY ONE__ keyword per line
