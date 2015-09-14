weibullFitStats <- function(x) {
  obs <- data.frame(raw = x,
                    round = as.numeric(as.character(cut(x,
                                                        breaks = seq(0,max(ceiling(x)+0.5),0.5),
                                                        labels = seq(0.25,max(ceiling(x)+0.25),0.5)))))
  counts <- aggregate(cbind(n = raw) ~ round,
                      data = obs,
                      FUN = function(x) sum(!is.na(x)))
  counts$f <- counts$n / sum(counts$n)
  # do the fit to raw data using an MLE approach
  wfit <- fitdistr(x[!is.na(x)], "weibull")
  # get values at unique levels in x
  counts$pred.weibull <- dweibull(counts$round,
                                  shape=wfit$estimate[[1]], 
                                  scale=wfit$estimate[[2]]) * 0.5
  chisq <- sum((counts$pred.weibull - counts$f)^2/counts$f)
  p.weibull <- ggplot(counts,
                      aes(x = round)) +
    geom_line(aes(y = f),
              colour = "black") + 
    geom_line(aes(y = pred.weibull),
              colour = "red")
  print(p.weibull)
  return(c(shape = wfit$estimate[[1]],
           scale = wfit$estimate[[2]],
           mean = mean(x,na.rm = TRUE),
           prop.above.mean = sum(x>mean(x,na.rm = TRUE))/sum(!is.na(x)),
           chi.sq.stat = chisq))
}