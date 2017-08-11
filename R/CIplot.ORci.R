#' @rdname CIplot
#' @aliases CIplot.ORci
#'
#' @method CIplot ORci
#' @export
#'
#' @import MASS
#'
#' @keywords plot
#' @keywords ORci
#'
CIplot.ORci <-
    function(x,
             xlog = TRUE, xlim = NULL, xlab = "Odds Ratio",
             yname = TRUE, las = 0,
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = 1,
             v = 1, vlty = 2, vlwd = 1,  vcol = 1,
             main = NULL,
             ...)
{
    xi <- x
    yvals <- nrow(xi):1L
    conf.level <- attr(x, "conf.level")

    if (is.null(xlim)) xlim <- c(min(x), max(x))

    if (is.null(main)) {
        main <- paste0(format(100 * conf.level, digits = 2L),
                       "% confidence interval")
    }

    CIplot.common(xi,
                  xlog = xlog, xlim = xlim, xlab = xlab,
                  yname = yname, las = las,
                  pch = pch, pcol = pcol, pcolbg = pcolbg, pcex = pcex,
                  conf.level = conf.level,
                  cilty = cilty, cilwd = cilwd, cicol = cicol,
                  v = v, vlty = vlty, vlwd = vlwd,  vcol = vcol,
                  main = NULL,
                  ...)
}

