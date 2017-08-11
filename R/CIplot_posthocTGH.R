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

    ## plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2L),
    ##      type = "n", axes = FALSE,
    ##      xlab = xlab, ylab = "",
    ##      xlim = xlim,
    ##      ylim = c(0.5, nrow(xi) + 0.5),
    ##      main = main)

    ## axis(1, ...)
    ## axis(2, at = nrow(xi):1,
    ##      labels = dimnames(xi)[[1L]], las = las)

    ## abline(h = yvals, lty = 1, lwd = 1, col = "lightgray")
    ## abline(v = 0, lty = vlty, lwd = vlwd, col = vcol)

    ## arrows(xi[, "lwr"], yvals, xi[, "upr"], yvals,
    ##        code = 3, angle = 90, length = 0.15,
    ##        lty = cilty, lwd = cilwd, col = cicol)
    ## points(xi[, 1], yvals, pch = pch, cex = pcex,
    ##        col = pcol, bg = pcolbg)

    ## box()
}

