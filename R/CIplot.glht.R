#' @rdname CIplot
#' @include CIplot.TukeyHSD.R
#'
#' @aliases CIplot.glht
#'
#' @import multcomp
#'
#' @method CIplot glht
#' @export
#'
#' @keywords plot
#' @keywords glht
#'
CIplot.glht <-
    function(x,
             xlog = FALSE, xlim = NULL, xlab = "Differences in mean",
             yname = TRUE, las = 0,
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = 1,
             v = 0, vlty = 2, vlwd = 1,  vcol = 1,
             main = NULL,
             ...)
{
    xi <- confint(x)$confint
    conf.level <- attr(xi, "conf.level")
    if (is.matrix(xi)) xi <- as.data.frame(xi)
    if (is.null(xlim)) xlim <- c(min(xi[,1:3]), max(xi[,1:3]))

    CIplot.common(xi,
                  xlog = xlog, xlim = xlim,
                  xlab = "Differences in mean",
                  yname = yname, las = las,
                  pch = pch, pcol = pcol, pcolbg = pcolbg, pcex = pcex,
                  cilty = cilty, cilwd = cilwd, cicol = cicol,
                  v = v, vlty = vlty, vlwd = vlwd, vcol = vcol,
                  main = main,
                  ...)
}

