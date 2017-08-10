CIplot.glm <-
    function(x,
             conf.level = 0.95,
             xlim = NULL, xlab = "Odds Ratio",
             main = NULL,
             pch = 21, pcol = "black",
             pcolbg = "white", pcex = 1, cilty = 1, cilwd = 1,
             cicol = "black", vlty = 2, vlwd = 1,  vcol = "black",
             las = NULL,
             ...)
{
    ci <- ORci(x, conf.level = conf.level)
    CIplot.ORci(ci,
                     xlim = NULL, xlab = xlab,
                     main = main, ..., pch = pch, pcol = pcol,
                     pcolbg = pcolbg, pcex = pcex,
                     cilty = cilty, cilwd = cilwd,
                     cicol = cicol, vlty = vlty, vlwd = vlwd,
                     vcol = vcol,
                     las = las,
                     ...)
}

ORci <-
    function(x, conf.level = 0.95)
{
    est <- coefficients(x)
    ci <- confint(x, level = conf.level)

    OR <- exp(cbind(est, ci)[-1,])
    colnames(OR) <- c("OR", "lwr", "upr")

    attr(OR, "conf.level") <- conf.level
    attr(OR, "class") <- "ORci"
    return(OR)
}

print.ORci <- function(x, ...)
{
    attr(x, "conf.level") <- NULL
    attr(x, "class") <- NULL
    print(x, ...)
}

CIplot.ORci <-
    function(x,
             xlim = NULL, xlab = "Odds Ratio", main = NULL,
             pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = "black",
             vlty = 2, vlwd = 1,  vcol = "black",
             las = NULL,
             ...)
{
    xi <- x
    yvals <- nrow(xi):1L

    if (is.null(xlim)) xlim <- c(min(x), max(x))

    if (is.null(main)) {
        main <- paste0(format(100 * attr(x, "conf.level"), digits = 2L),
                       "% confidence interval")
    }

    plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2L),
         type = "n", axes = FALSE,
         xlab = xlab, ylab = "",
         xlim = xlim, log = "x",
         ylim = c(0.5, nrow(xi) + 0.5),
         main = main)

    axis(1, ...)
    axis(2, at = nrow(xi):1,
         labels = dimnames(xi)[[1L]], las = las)

    abline(h = yvals, lty = 1, lwd = 1, col = "lightgray")
    abline(v = 1, lty = vlty, lwd = vlwd, col = vcol)

    arrows(xi[, "lwr"], yvals, xi[, "upr"], yvals,
           code = 3, angle = 90, length = 0.15,
           lty = cilty, lwd = cilwd, col = cicol)
    points(xi[, 1], yvals, pch = pch, cex = pcex,
           col = pcol, bg = pcolbg)

    box()
}

