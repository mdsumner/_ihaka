forecastplot <-
function (data, fit, n.ahead = 1, coef = 2, 
          showmean= FALSE, transform = function(x) x, ...)
{
    p = predict(fit, n.ahead = n.ahead)
    pred = p$pred
    pse = p$se
    xlim <- range(time(data), time(p$pred))
    ylim <- range(transform(data),
                  transform(pred - coef * pse),
                  transform(pred + coef * pse))
    plot(transform(data), xlim = xlim, ylim = ylim, ...)
    if (showmean)
        abline(h = transform(mean(data)), lty="dotted")
    if (all(start(lag(pred, 1)) == end(data))) {
      jseg = ts(c(window(data, end(data)),
                  window(pred, end = start(pred))),
                start = end(data), frequency = frequency(data))
      lines(transform(jseg), lty = "11", lwd = 1)
    }
    lines(transform(data))
    # lines(transform(p$pred), lty = "11", lwd = 2)
    # points(transform(p$pred), pch = 20, col = "black")
    lines(transform(p$pred), type = "o", pch = 20)
    lines(transform(p$pred - coef*p$se), lty = "11", lwd = 1)
    lines(transform(p$pred + coef*p$se), lty = "11", lwd = 1)
}
