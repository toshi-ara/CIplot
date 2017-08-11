CIplot.htest <-
    function(x,
             xlog = FALSE, xlim = NULL, xlab = NULL,
             yname = FALSE, las = 0,
             pch = 21, pcol = 1, pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = 1,
             v = NULL, vlty = 2, vlwd = 1,  vcol = 1,
             main = NULL,
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
        xlim <<- c(min(v, ci[1]), max(v, ci[2]))
    }

    if (is.null(xlab)) {
        xlab <- attr(x$null.value, "names")
        if (is.null(xlab)) {
            xlab <- ifelse(log, "ratio", "difference")
        }
    }

    tmp <- data.frame(estimate = est, lwr = ci[1], upr = ci[2])
    CIplot.common(tmp,
                  xlog = xlog, xlim = xlim, xlab = xlab,
                  yname = yname, las = las,
                  pch = pch, pcol = pcol, pcolbg = pcolbg, pcex = pcex,
                  conf.level = attr(x$conf.int, "conf.level"),
                  cilty = cilty, cilwd = cilwd, cicol = cicol,
                  v = v, vlty = vlty, vlwd = vlwd,  vcol = vcol,
                  main = NULL,
                  ...)
}

