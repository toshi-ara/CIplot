#' @rdname CIplot
#' @aliases CIplot.posthocTGH CIplot.Tukey
#'
#' @method CIplot posthocTGH
#' @export
#'
#' @keywords plot
#' @keywords posthocTGH
#'
CIplot.posthocTGH <-
    function(x,
             xlog = FALSE, xlim = NULL, xlab = "Differences in mean",
             yname = TRUE, las = 0,
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = 1,
             v = 0, vlty = 2, vlwd = 1,  vcol = 1,
             main = NULL,
             ...)
{
    ci <- CI.posthocTGH(x)
    CIplot.Tukey(ci,
                 xlog = xlog, xlim = xlim, xlab = xlab,
                 yname = yname, las = las,
                 pch = pch, pcol = pcol, pcolbg = pcolbg, pcex = pcex,
                 cilty = cilty, cilwd = cilwd, cicol = cicol,
                 v = v, vlty = vlty, vlwd = vlwd, vcol = vcol,
                 main = main,
                 ...)
}

CIplot.Tukey <-
    function(x,
             xlog = FALSE, xlim = NULL, xlab = "Differences in mean",
             yname = TRUE, las = 0,
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = 1,
             v = 0, vlty = 2, vlwd = 1,  vcol = 1,
             main = NULL,
             ...)
{
    xi <- x
    yvals <- nrow(xi):1L
    conf.level <- attr(x, "conf.level")

    if (is.null(xlim)) xlim <- c(min(x[,1:3]), max(x[,1:3]))

    CIplot.common(xi,
                  xlog = xlog, xlim = xlim, xlab = xlab,
                  yname = yname, las = las,
                  pch = pch, pcol = pcol, pcolbg = pcolbg, pcex = pcex,
                  conf.level = conf.level,
                  cilty = cilty, cilwd = cilwd, cicol = cicol,
                  v = v, vlty = vlty, vlwd = vlwd,  vcol = vcol,
                  main = main,
                  ...)
}

