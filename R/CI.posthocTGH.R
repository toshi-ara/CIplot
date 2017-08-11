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
