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

frequencyLabel =
    function(angular)
    if (angular) "Angular Frequency" else "Cycles / Unit Time"

plot.gain =
    function(x, angular = FALSE, ...)
    {
        plot(frequency(x, angular), x, ..., type = "l",
                   xlab = frequencyLabel(angular),
                   ylab = "Gain")
    }

lines.gain =
    function(x, angular = FALSE, ...)
    lines(frequency(x, angular), x, ...)

plot.phase =
    function(x, angular = FALSE, ...)
    {
        plot(frequency(x, angular), x, ...,
             ylim = c(-pi, pi), pch = 20,
             xlab = frequencyLabel(angular),
             ylab = "Phase")
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
