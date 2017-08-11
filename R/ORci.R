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

