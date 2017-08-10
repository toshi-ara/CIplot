CIplot <-
    function(x, ...) UseMethod("CIplot")

CIplot.htest <-
    function(x,
             log = FALSE,
             xlim = NULL, xlab = NULL, main = NULL,
             pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = "black",
             v = NULL, vlty = 2, vlwd = 1,  vcol = "black",
             ...)
{
    est <- x$estimate
    ci <- x$conf.int
    if (is.null(ci)) {
        warning("No information about confidential interval\n")
        return(invisible(NULL))
    }

    if (length(est) == 2L) est <- est[1] - est[2]

    if (is.null(v)) {
        if (is.null(x$null.value)) {
            v <- ifelse(log, 1, 0)
        } else {
            v <- x$null.value
        }
    }

    if (is.null(xlim)) {
        xlim <- c(min(v, ci[1]), max(v, ci[2]))
    }

    if (is.null(main)) {
        main <- paste0(format(100 * attr(ci, "conf.level"), digits = 2L),
                       "% confidence interval")
    }

    if (is.null(xlab)) {
        xlab <- attr(x$null.value, "names")
        if (is.null(xlab)) {
            xlab <- ifelse(log, "ratio", "difference")
        }
    }

    plot(ci, rep(1, 2), type = "n", axes = FALSE,
         xlim = xlim,
         xlab = xlab, ylab = "", yaxt = "n",
         main = main,
         log = ifelse(log, "x", ""),
         ...)
    axis(1, ...)

    arrows(ci[1], 1, ci[2], 1, angle = 90, code = 3,
           lty = cilty, lwd = cilwd, col = cicol)

    points(est, 1, pch = pch, cex = pcex,
           col = pcol, bg = pcolbg)
    abline(v = v, lty = vlty, lwd = vlwd, col = vcol)

    box()
}

