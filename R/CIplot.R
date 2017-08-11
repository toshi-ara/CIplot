CIplot <-
    function(x, ...) UseMethod("CIplot")

## x: data.frame(estimation, lwr, upr)
##    atr(x, "conf.level")
## requires: v
CIplot.common <-
    function(x,
             xlog = FALSE, xlim = NULL, xlab = NULL,             ## x-axis
             yname = TRUE, las = 0,                              ## y-axis
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,     ## points
             conf.level = 0.95, cilty = 1, cilwd = 1, cicol = 1, ## conf.int
             v, vlty = 2, vlwd = 1,  vcol = 1,                   ## vertical
             main = NULL,                                        ## Title
             ...)
{
    if (is.null(main)) {
        main <- paste0(format(100 * conf.level, digits = 2L),
                       "% confidence interval")
    }

    xi <- x[,1:3]         ## estimation, lwr, upr
    yvals <- nrow(xi):1L

    if (is.null(xlim)) xlim <- c(min(xi), max(xi))

    plot(c(xi[,"lwr"], xi[,"upr"]), rep.int(yvals, 2L),
         type = "n", axes = FALSE,
         xlab = xlab, ylab = "",
         xlim = xlim, log = ifelse(xlog, "x", ""),
         ylim = c(0.5, nrow(xi) + 0.5),
         main = main)

    axis(1, ...)
    if (yname) {
        axis(2, at = nrow(xi):1,
             labels = dimnames(xi)[[1L]], las = las)
    }

    arrows(xi[, "lwr"], yvals, xi[, "upr"], yvals,
           code = 3, angle = 90, length = 0.15,
           lty = cilty, lwd = cilwd, col = cicol)
    points(xi[, 1], yvals, pch = pch, cex = pcex,
           col = pcol, bg = pcolbg)
    abline(v = v, lty = vlty, lwd = vlwd, col = vcol)
    box()
}

