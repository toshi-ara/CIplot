CIplot <- function(x, ...) UseMethod("CIplot")

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
    ci <- ORci(x)
    CIplot.OddsRatio(ci,
                     conf.level = conf.level,
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
    attr(OR, "class") <- "OddsRatio"
    return(OR)
}

CIplot.OddsRatio <-
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


CIplot.posthocTGH <-
    function(x,
             xlim = NULL, xlab = "Differences in mean", main = NULL,
             pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = "black",
             vlty = 2, vlwd = 1,  vcol = "black",
             las = NULL,
             ...)
{
    ci <- CI.posthocTGH(x)
    CIplot.Tukey(ci,
                 xlim = xlim, xlab = xlab, main = main,
                 pch = pch, pcol = pcol, pcolbg = pcolbg, pcex = pcex,
                 cilty = cilty, cilwd = cilwd, cicol = cicol,
                 vlty = vlty, vlwd = vlwd,  vcol = vcol,
                 las = las,
                 ...)
}

CI.posthocTGH <-
    function(x)
{
  if (x$input$method == 'tukey') {
      tmp <- x$output$tukey;
  }
  else if (x$input$method == 'games-howell') {
      tmp <- x$output$games.howell;
  }

  res <- as.matrix(tmp[, 1:6])
  colnames(res)[2:3] <- c("lwr", "upr")

  attr(res, "conf.level") <- x$input$conf.level
  attr(res, "class") <- "Tukey"
  return(res)
}

CIplot.Tukey <-
    function(x,
             xlim = NULL, xlab = "Differences in mean", main = NULL,
             pch = 21, pcol = "black", pcolbg = "white", pcex = 1,
             cilty = 1, cilwd = 1, cicol = "black",
             vlty = 2, vlwd = 1,  vcol = "black",
             las = NULL,
             ...)
{
    xi <- x
    yvals <- nrow(xi):1L

    if (is.null(xlim)) xlim <- c(min(x[,1:3]), max(x[,1:3]))

    if (is.null(main)) {
        main <- paste0(format(100 * attr(x, "conf.level"), digits = 2L),
                       "% confidence interval")
    }

    plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2L),
         type = "n", axes = FALSE,
         xlab = xlab, ylab = "",
         xlim = xlim,
         ylim = c(0.5, nrow(xi) + 0.5),
         main = main)

    axis(1, ...)
    axis(2, at = nrow(xi):1,
         labels = dimnames(xi)[[1L]], las = las)

    abline(h = yvals, lty = 1, lwd = 1, col = "lightgray")
    abline(v = 0, lty = vlty, lwd = vlwd, col = vcol)

    arrows(xi[, "lwr"], yvals, xi[, "upr"], yvals,
           code = 3, angle = 90, length = 0.15,
           lty = cilty, lwd = cilwd, col = cicol)
    points(xi[, 1], yvals, pch = pch, cex = pcex,
           col = pcol, bg = pcolbg)

    box()
}

