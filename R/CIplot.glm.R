#' @rdname CIplot
#' @include CIplot.htest.R
#'
#' @method CIplot glm
#' @export
#'
#' @param conf.level \code{glm} object only.
#'                   the confidence interval. Default is 0.95.
#'                   see also \code{\link{ORci}}.
#'
#' @keywords plot
#' @keywords glm
#'
CIplot.glm <-
    function(x,
             conf.level = 0.95,
             xlog = TRUE, xlim = NULL, xlab = "Odds Ratio",
             yname = TRUE, las = 0,
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = 1,
             v = 1, vlty = 2, vlwd = 1,  vcol = 1,
             main = NULL,
             ...)
{
    ci <- ORci(x, conf.level = conf.level)
    CIplot.ORci(ci,
                xlog = xlog, xlim = xlim, xlab = xlab,
                yname = yname, las = las,
                pch = pch, pcol = pcol, pcolbg = pcolbg, pcex = pcex,
                cilty = cilty, cilwd = cilwd, cicol = cicol,
                v = v, vlty = vlty, vlwd = vlwd, vcol = vcol,
                main = main,
                ...)
}

