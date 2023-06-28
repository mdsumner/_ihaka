## ---- General Filter Creation ----

makeFilter =
    function(coef, offset = 0)
    {
        if (!is.numeric(coef) || length(coef) < 1 ||
            !is.numeric(offset) || length(offset) != 1 ||
            offset < 0 || offset > length(coef) - 1 ||
            is.na(offset[1]))
            stop("invalid filter components")
        structure(coef, offset = round(offset[1]), class = "filter")
    }

## ---- Accessor Functions ----

coef.filter =
    function(x) as.vector(x)

lag.filter =
    function(x) 1:length(x) - attr(x, "offset") - 1

## ---- Print and Plot Methods for Filters ----

print.filter =
    function(x, ...)
    print(data.frame(Lag = lag(x), Coeffient = coef(x)), ...)

plot.filter =
    function(f, ...)
    {
	coefs = coef(f)
        plot(lag(f), coefs, type="h",
             ylim = range(coefs, 0))
        abline(h = 0, lty = "13")
    }

## ---- Transfer Functions for Filters ----
## ---- Gain and Phase Functions ----

transferFunction =
    function(f)
    {
        offset = attr(f, "offset")
        nhalf = max(2^ceiling(log2(length(f))), 512)
        n = 2 * nhalf
        x = rep(0, n)
        x[(1:length(f) - offset - 1) %% n + 1] = f
        structure(fft(x)[1:nhalf], low = 0, high = (nhalf - 1)/n,
                  class = c("transferFunction", "fs"))
    }

gain = function(x, ...) UseMethod("gain")
phase = function(x, ...) UseMethod("phase")

gain.filter =
    function(x, angular = FALSE)
    {
        tf = transferFunction(x)
        structure(Mod(as.vector(tf)), low = attr(tf, "low"),
                  high = attr(tf, "high"), class = c("gain", "fs"))
    }

phase.filter =
    function(x, ...)
    {
        tf = transferFunction(x)
        structure(Arg(as.vector(tf)), low = attr(tf, "low"),
              high = attr(tf, "high"), class = c("phase", "fs"))
    }

## ---- Plotting Methods for Gain and Phase ----

frequency.fs =
    function(x, angular = FALSE)
    {
        if (angular) 2 * pi * seq(attr(x, "low"), attr(x, "high"),
                                  length = length(x))
        else seq(attr(x, "low"), attr(x, "high"),
                 length = length(x))
    }

print.fs =
    function(x, ...)
    {
        cat("Low Frequency  = ", attr(x, "low"),
            "\nHigh Frequency = ", attr(x, "high"), "\n", sep = "")
        print(as.vector(x))
    }

print.gain =
    function(x, ...)
    {
        cat("Gain Function\n")
        NextMethod()
    }

print.phase =
    function(x, ...)
    {
        cat("Gain Function\n")
        NextMethod()
    }

frequencyLabel =
    function(angular)
    if (angular) "Angular Frequency" else "Cycles / Unit Time"

plot.gain =
    function(x, angular = FALSE, type = "l",
             xlab = frequencyLabel(angular),
             ylab = "Gain", ...)
    {
        plot(frequency(x, angular), x, type = type,
                   xlab = xlab, ylab = ylab, ...)
    }

lines.gain =
    function(x, angular = FALSE, ...)
    lines(frequency(x, angular), x, ...)

plot.phase =
    function(x, angular = FALSE, ylim = c(-pi, pi),
             pch = 20, xlab = frequencyLabel(angular),
             ylab = "Phase", ...)
    {
        plot(frequency(x, angular), x,
             ylim = ylim, pch = pch, xlab = xlab, ylab = ylab, ...)
    }

points.phase =
    function(x, angular = FALSE, ...)
    points(frequency(x, angular), x, ...)

## ---- Apply a Filter to a Time Series ----

applyFilter =
    function(f, x)
    {
        if (!inherits(f, "filter"))
            stop("not a filter")
        off = attr(f, "offset")
        nx = length(x)
        nf = length(f)
        n = nextn(nx)
        u = rep(0, n)
        v = rep(0, n)
        u[1:nx] = x
        v[(1:nf - off - 1) %% n + 1] = f
        w = (Re(fft(fft(u) * fft(v), inv = T))/n)[1:nx]
        w[(1:(nf - 1) - off - 1) %% nx + 1] = NA
        tsp(w) = tsp(x)
        class(w) = class(x)
        w
    }

## ---- Some Example Filters ----

maFilter =
    function(n)
    {
        nc = round(n)[1]
        if (is.na(nc) || nc < 0)
            stop("invalid filter length")
        if (nc %% 2 == 0)
            makeFilter(c(0.5 / nc, rep(1 / nc, nc - 1), 0.5 / nc),
                       offset = nc %/% 2)
        else
            makeFilter(rep(1 / nc, nc), offset = nc %/% 2)
    }
                                                                                
diffFilter =
    function(season = 1)
    {
        season = round(season)[1]
        if (is.na(season) || season < 1)
            stop("Invalid differencing filter")
        coef = rep(0, season + 1)
        coef[1] = 1
        coef[season + 1] = -1
        makeFilter(coef)
    }

rcFilter =
    function(L)
    {
        L = round((L - 1)/2)
        coef = 1 + cos(pi * -L:L/(L + 1))
        makeFilter(coef / sum(coef), offset = L)
    }

## ---- Spectral Analysis ----
##  Simple Univariate and Bivariate Spectral Analysis

spectrum <-
function(x, y, bw = 0, span)
{
    if (missing(span)) {
      if (bw < 0)
        stop("non-negative bandwidth required")
      span <- floor((length(x) * bw)/2)
    }
    else {
      span <- 2 * floor(span / 2) + 1
      if (span < 0)
        stop("non-negative span required")
      bw <- if (span == 1) 0
        else signif(2 * span / length(x), 3)
    }
    if (missing(y))
      ts.power.spectrum(x - mean(x), bw, span)
    else
      ts.cross.spectrum(x - mean(x), y - mean(y), bw, span)
}


ts.power.spectrum <-
function(x, bw, span)
{
    nx <- length(x)
    n <- nextn(nx)
    n2 <- n%/% 2 + 1
    u <- rep(0,n)
    u[1:nx] <- (x - mean(x))

    ## Periodogram

    Ixx <- Mod(fft(u))^2/(2 * pi * length(x))
    Ixx[1] <- 0

    ## Smooth using fast convolution

    k <- floor(span / 2)
    span <- 2 * k + 1
    df <- 2 * span
    if (k > 0) {
        v <- rep(0, n)
        v[c(1:(k + 1), (n - k + 1):n)] <- 1/(2 * k + 1)
        fxx <- (Re(fft(fft(Ixx) * fft(v), inv = TRUE))/n)[1:n2]
    }
    else {
        fxx <- Ixx[1:n2]
    }
    freq <- (1:n2 - 1)/n
    structure(list(freq = freq, fxx = fxx,
                   bw = bw, span = span, df = df, n = nx),
              class = "ts.spectrum")
}

ts.cross.spectrum <-
function(x, y, bw, span)
{
    if (length(x) != length(y))
      stop("x and y series must have equal lengths")
    nx <- length(x)
    n <- nextn(nx)
    n2 <- n %/% 2 + 1
    u <- rep(0, n)
    v <- rep(0, n)
    u[1:nx] <- (y - mean(y))
    v[1:nx] <- (x - mean(x))
    
    ## Periodograms and cross-periodogram
    
    Iyy <- Mod(fft(u))^2/(2 * pi * length(x))
    Ixx <- Mod(fft(v))^2/(2 * pi * length(x))
    Iyx <- fft(u) * Conj(fft(v))/ (2 * pi * length(x))
    Iyy[1] <- 0
    Ixx[1] <- 0
    
    ## Smoothing
    
    k <- floor(span / 2)
    span <- 2 * k + 1
    df <- 2 * span
    if (k > 0) {
        v[1:n] <- 0
        v[c(1:(k + 1), (n - k + 1):n)] <- 1/(2 * k + 1)
        fyy <- (Re(fft(fft(Iyy) * fft(v), inv = TRUE))/n)[1:n2]
        fxx <- (Re(fft(fft(Ixx) * fft(v), inv = TRUE))/n)[1:n2]
        fyx <- (fft(fft(Iyx) * fft(v), inv = TRUE)/n)[1:n2]
    }
    else {
        fyy <- Iyy[1:n2]
        fxx <- Ixx[1:n2]
        fyx <- Iyx[1:n2]
    }
    
    freq <- (1:n2 - 1)/n
    structure(list(freq = freq, fxx = fxx, fyy = fyy, fyx = fyx,
                   bw = bw, span = span, df = df, n = nx),
              class = "ts.spectrum")
}

fxx <-
function(z)
{
    se <- 10^(1.96 * 0.4342945/sqrt(z$df/2))
    structure(list(frequency = z$freq,
                   estimate = ifelse(z$fxx > 0, z$fxx, NA),
                   name = "fxx",
                   bw = z$bw,
                   span = z$span,
                   df = z$df,
                   lower = z$fxx * se,
                   upper = z$fxx / se),
              class = "est.spectrum")
}

fyy <-
function(z)
{
    if (is.null(z$fyy))
      stop("no cross spectral information")
    se <- 10^(1.96 * 0.4342945/sqrt(z$df/2))
    structure(list(frequency = z$freq,
                   estimate = ifelse(z$fyy > 0, z$fyy, NA),
                   name = "fyy",
                   bw = z$bw,
                   span = z$span,
                   df = z$df,
                   lower = z$fyy * se,
                   upper = z$fyy / se),
              class = "est.spectrum")
}

gain.ts.spectrum <-
function(z)
{
    if (is.null(z$fyy))
      stop("no cross spectral information")
    gain <- Mod(z$fyx/z$fxx)
    rxy2 <- Mod(z$fyx)^2/(z$fyy * z$fxx)
    # Drb: 
    se <- 10^((1.96 * 0.4342945/sqrt(z$df))*sqrt(1/rxy2 - 1))
    structure(list(frequency = z$freq,
                   estimate = ifelse(gain > 0, gain, NA),
                   name = "gain",
                   bw = z$bw,
                   span = z$span,
                   df = z$df,
                   lower = gain / se,
                   upper = gain * se),
              class = "est.gain")
}

phase.ts.spectrum <-
function(z)
{
    if (is.null(z$fyy))
      stop("no cross spectral information")
    phase<- Arg(z$fyx/z$fxx)
    rxy2 <- Mod(z$fyx)^2/(z$fyy * z$fxx)
    se <- (1.96 * 0.4342945/sqrt(z$df)) * sqrt(1/rxy2 - 1)
    structure(list(frequency = z$freq,
                   estimate = phase,
                   name = "phase",
                   bw = z$bw,
                   span = z$span,
                   df = z$df,
                   lower = phase - se,
                   upper = phase + se),
              class = "est.phase")
}

coherence <-
function(z)
{
    if (is.null(z$fyy))
      stop("no cross spectral information")
    rxy2 <- Mod(z$fyx)^2/(z$fyy * z$fxx)
    structure(list(frequency = z$freq,
                   estimate = rxy2,
                   name = "coherence",
                   bw = z$bw,
                   span = z$span,
                   df = z$df,
                   lower = 1 - 0.05^(1/(z$df/2 - 1)),
                   upper = 1 - 0.05^(1/(z$df/2 - 1))),
              class = "est.coherence")
}

residual.spectrum <-
function(z)
{
    if (is.null(z$fyy))
      stop("no cross spectral information")
    se <- 10^(1.96 * 0.4342945/sqrt(z$df/2 - 1))
    fee <- z$fyy - Mod(z$fyx)^2/z$fxx
    structure(list(frequency = z$freq,
                   estimate = ifelse(fee > 0, fee, NA),
                   name = "fee",
                   bw = z$bw,
                   span = z$span,
                   df = z$df,
                   lower = fee * se,
                   upper = fee / se),
              class = "est.spectrum")
}

impulse.response <-
function(z)
{
    if (is.null(z$fyy))
      stop("no cross spectral information")
    A <- z$fyx / z$fxx
    A <- c(A, Conj(rev(A[-c(1, length(A))])))
    n <- length(A)
    coef <- Re(fft(A, inv = TRUE))/n
    coef <- coef[c((n%/%2 + 1):n, 1:(n%/%2))]
    lag <- 1:n - n%/%2 -1
    structure(list(lag = 1:n - n%/%2 -1,
                   estimate = coef,
                   name = "impulse.response"),
              class = "est.impulse.resp")
}

plot.estimate <-
function(z,
         ci = FALSE,
         xlim,
         ylim,
         type = "l",
         pch = 21,
         log = "y",
         main = "",
         xlab = "Frequency",
         ylab = "Estimate",
         sub, ...)
{     
    if (missing(sub))
      sub <- paste("Bandwidth = ", z$bw, ",  ",
                   "Degrees of freedom = ", z$df, sep = "")
    frequency <- z$frequency
    estimate <- z$estimate
    lower <- z$lower
    upper <- z$upper
    if (!missing(xlim)) {
        sel <-  (xlim[1] <= frequency & frequency < xlim[2])
        frequency <- frequency[sel]
        estimate <- estimate[sel]
        lower <- lower[sel]
        upper <- upper[sel]
        xlim <- range(frequency, na.rm = TRUE)
    }
    else xlim <- c(0, 0.5)
    if (missing(ylim)) {
        ylim <- if (ci)
          range(upper, lower, na.rm = TRUE)
        else
          range(estimate, na.rm = TRUE)
    }  
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, log = log)
    if (ci) {
            plot.xy(list(x = frequency, y = lower), type = type,
                    pch = pch, col = "gray")
            plot.xy(list(x = frequency, y = upper), type = type,
                    pch = pch, col = "gray")
            
    }
    plot.xy(list(x = frequency, y = estimate), type = type, pch = pch)
    title(main = main, xlab = xlab, ylab = ylab, sub = sub)
    axis(1)
    axis(2)
    box()
}

plot.est.spectrum <-
function(z, ylab = "Power Spectrum", ...)
  plot.estimate(z, ylab = ylab, ...)

plot.est.gain <-
function(z, ylab = "Gain", ...)
  plot.estimate(z, ylab = ylab, ...)

plot.est.phase <-
function(z, ylab = "Phase",
         log = "", ylim = c(-pi, pi),
         type = "p", pch = 20, ...)
  plot.estimate(z, ylab = ylab, log = log, ylim = ylim,
                type = "p", pch = pch, ...)

plot.est.coherence <-
function(z, ylab = "Coherence", ...)
{
    plot.estimate(z, ylab = ylab,
                  log = "", ylim = c(0, 1),
                  sub =ifelse(z$df > 2,
                    "The horizontal line gives the 95% null point",
                    ""),
                  ...)
    abline(h = 1 - 0.05^(1/(z$df/2 - 1)), lty = "dotted")
}

plot.est.impulse.resp <-
function(z,
         xlim = c(-40,40),
         ylim = range(z$estimate),
         type = "h",
         main = "",
         xlab = "Lag",
         ylab = "Coefficent",
         sub = "",
         ...)
{
    ind <- (xlim[1] < z$lag & z$lag < xlim[2])
    plot(z$lag[ind], z$estimate[ind],
         xlim = xlim, ylim = ylim, type = type,
         main = main, xlab = xlab, ylab = ylab, ...)
}
