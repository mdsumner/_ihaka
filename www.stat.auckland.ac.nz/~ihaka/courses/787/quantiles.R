quantilefun =
    function(y)
    approxfun(seq(0, 1, length = length(y)), sort(y),
              yleft = NA, yright = NA)

percentilefun =
    function(y)
    approxfun(sort(y), seq(0, 1, length = length(y)),
              yleft = 0, yright = 1)

qqnorm =
    function(y, pch = 20,
             xlab = "Standard Normal Quantiles",
             ylab = "Sample Quantiles", ...)
    {
        y = sort(na.omit(y))
        n = length(y)
        p = (1:length(y) - .5)/length(y)
        k = .895 / (sqrt(n) * (1 - .01 / sqrt(n) + .85 / n))
        l = suppressWarnings(qnorm(p - k))
        q = qnorm(p)
        u = suppressWarnings(qnorm(p + k))
        plot(q, y, xlim = range(l, q, u, na.rm = TRUE),
             xlab = xlab, ylab = ylab, pch = pch, ...)
        lines(l, y, col = "gray")
        lines(u, y, col = "gray")
    }

qqplot =
    function(x, y, pch = 20,
             xlab = "x Quantiles",
             ylab = "y Quantiles",
             main = NULL, ...)
    {
        x = sort(na.omit(x))
        y = sort(na.omit(y))
        qy = quantilefun(y)
        m = length(x)
        n = length(y)
        N = m + n
        M = m * n / N
        K = 1.36
        p = (1:m - 1)/(m - 1)
        yq = qy(p)
        yl = qy(p - K/sqrt(M))
        yu = qy(p + K/sqrt(M))
        plot(x, yq, pch = pch,
             xlim = range(x),
             ylim = range(yq, yl, yu, na.rm = TRUE),
             xlab = xlab, ylab = ylab, main = main, ...)
        lines(x, yl, col = "gray")
        lines(x, yu, col = "gray")
    }

shiftplot =
    function(x, y, pch = 20,
             xlab = "x Quantiles",
             ylab = "y Quantiles",
             main = NULL, ...)
    {
        x = sort(na.omit(x))
        y = sort(na.omit(y))
        qy = quantilefun(y)
        m = length(x)
        n = length(y)
        N = m + n
        M = m * n / N
        K = 1.36
        p = (1:m - 1)/(m - 1)
        yq = qy(p) - x
        yl = qy(p - K/sqrt(M)) - x
        yu = qy(p + K/sqrt(M)) - x
        plot(x, yq, pch = pch,
             xlim = range(x),
             ylim = range(yq, yl, yu, na.rm = TRUE),
             xlab = xlab, ylab = ylab, main = main, ...)
        lines(x, yl, col = "gray")
        lines(x, yu, col = "gray")
    }
