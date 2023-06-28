##  Simple Dot Charts
##
##  This function produces a variety of styles of dotchart.
##  It is designed to reproduce the figures in Cleveland's
##  "The Elements of Graphing Data", particularly those in
##  in the first edition.
##
##  Arguments:
##
##  x:      A matrix (or vector) containing the values to be
##          plotted.  The values in a single row of x are plotted
##          with different symbols in each row of the dotchart.
##
##  names:  A matrix or vector giving names for the rows of x.
##          When there are multiple panels, several label panels
##          are produced at the right of the plot.  Justification
##          of the panels is controled by the "adj" argument.
##
##  group:  A factor which specifies the grouping of rows of x
##          and names.  Separate plots are produced for each
##          group and these are stacked vertically in the plot.
## 
##  xlim:   Limits for the x axis.  This can be either a vector
##          containg two numeric values, or a list of such vectors.
##          In the case of a list, multiple panels are produced,
##          one for each range.  This panels are juxtaposed
##          horizontally.
##
##  widths: The relative widths of the panels produced when
##          there are multiple x limits.  The default is to
##          make the panel widths proportional to the data
##          ranges.
##
##  adj:    The string justifications to be used in the
##          labelling panels.  0 = left-justified,
##          .5 = centered, 1 = right-justified.  The
##          default value is 1.
##
##  xlab:   The label to be used on the x axis.  By default
##          the label is printed both above and below the
##          plot.  Separate labels can be specified below
##          and above by using the arguments xlab1 and xlab3.
##          A NULL value means the label is omitted.
##
##  at:     A vector of positions for major tickmarks on the
##          x axis.  This is just like the "at" argument for
##          axis().  As with xlab, separate at1 and at3 arguments
##          can be used.
##
##  labels: Labels to printed at the tickmarks specified by "at".
##          the default is to use the values in "at".  Again
##          separate labels1 and labels3 can be used.
##
##  xat:    The positions of extra, small tickmarks on the axes.
##          these are drawn half the length of the major tickmarks.
##          Separate xat1 and xat3 arguments can be used.
##
##  gap:    The size of the gap between the label and dot
##          panels.  The default is .25 cm (produced using the
##          function call lcm(.25).
##
##  cex:    The string expansion factor to be used for all
##          elements of the plot.
##
##  pch:    The plotting symbols to be used for the columns
##          of the x matrix.
##
##  col:    The colour of the plotting symbols.
##
##  bg:     The background fill colour for the plotting symbols.
##
##  lty:    The line-type to be used for the horizonal lines
##          to be drawn within the plots.
##
##  lwd:    The line thickness to be used.
##
##  left:   A logical value which indicates whether the lines
##          should extend just from the left of the plot to
##          the points.  If FALSE, a line is drawn the full
##          width of the plot.  A value of TRUE for left only
##          makes sense for a single x range and single x symbol
##          per line of the plot.

dotchart =
    function(x, names = NULL, group = NULL,
	     xlim = NULL, widths = NULL, adj = 1,
             xlab = NULL, xlab1 = xlab, xlab3 = xlab,
             at = NULL, at1 = at, at3 = at,
             labels = TRUE, labels1 = labels, labels3 = labels,
             xat = NULL, xat1 = xat, xat3 = xat, 
             gap = lcm(.25), cex = 1, pch = 20,
             col = "black", bg = "white",
             lty = "13", lwd = par("lwd"),
             left = length(xlim) == 1 && !ncol(x) > 1)
    {
	## --- Save and reset the par values which ---
	## --- will be changed during plotting. ---

        opar = par(cex = cex, lwd = .5, oma = rep(0, 4),
                   mar = rep(0, 4), mfrow = c(1, 1))
        on.exit(par(opar))
	lwd = par("lwd")
        
        ## --- Check and process the arguments ---
	## --- The data matrix ---

        x = as.matrix(x)
        
        if (is.null(names)) {
            names = rownames(x)
            if (is.null(names))
                names = 1:nrow(x)
        }
        ## --- The names matrix. ---

        names = as.matrix(names)
        if (nrow(names) != nrow(x))
            stop("data/names size mismatch")
        if (!is.null(group) && nrow(x) != length(group))
            stop("data/grouping size mismatch")
        nx = ncol(x)
        nnames = ncol(names)

        ## --- Plotting symbols and colours ---

        pch = reshape(pch, nrow(x), nx)

	## --- The label label string justifications ---

        adj = rep(adj, length = nnames)
        
	## --- The xlim specification (vector or list). ---
        if (is.null(xlim))
            xlim = list(range(x, na.rm = TRUE))
        else if (is.numeric(xlim))
            xlim = list(xlim)
        if (any(sapply(xlim, length) != 2 ||
                sapply(xlim, mode) != "numeric")
            || any(is.na(unlist(xlim))))
            stop("invalid xlim spec.")
        nxlim = length(xlim)

	## --- The relative widths of the data panels ---

        if (is.null(widths))
            widths = 1.06 * sapply(xlim, diff)
        xwidths = rep(widths, length = nxlim)

	## --- If there is a groups specification ---
	## --- split the data accordingly. ---

        if (is.null(group))
            df = list(data.frame(x = I(x),
                                 names = I(names), pch = I(pch)))
        else
            df = split(data.frame(x = I(x),
                                  names = I(names), pch = I(pch)),
                       group)
        ngroup = length(df)
        gsize = sapply(df, function(df) nrow(df$x))

        ## --- Build the layout ---

        l = layout.names(names, gap, cex)
        for(xw in xwidths)
            l = layout.cbind(l, make.layout(1, xw, 1), gap = gap)
                
	## --- Replicate the layout vertically with ---
	## --- heights dependent on the group sizes. ---
        l = layout.rep(l, ngroup, dir = "bt")
        l$heights = rev(gsize)

        ## --- Axes below and above
	mar1 = axis.size(labels1, xlab1)
	mar3 = axis.size(labels3, xlab3)
        line1 = axis.line(labels1, xlab1)
        line3 = axis.line(labels3, xlab3)
        l = layout.axis(l, which = 1, size = llines(mar1, cex = cex),
                        skip1 = 2 * ncol(names))
        l = layout.axis(l, which = 3, size = llines(mar3, cex = cex),
                        skip1 = 2 * ncol(names))
        if (dim(x)[2] > 1)
            l = layout.key(l, which = 3, size = llines(2),
              cex = cex, skip1 = 2 * ncol(names))

        ## --- Surrounding Margins
        l = layout.margins(l, 1:4, size = llines(1.1))

        ## --- Set the Layout
        layout.set(l)
        par(cex = cex)
        if (FALSE) {
            layout.show(max(l$mat))
            return(l)
        }
        
        ## --- Loop over the groups and, within groups, ---
        ## --- loop through the plot elements, drawing ---
	## --- each element in turn. ---

        for(g in 1:ngroup) {
            dfg = df[[g]]

	    ## --- Label panels. ---

            for(i in 1:nnames) {
                dotchart.names(1:gsize[g], dfg$names[,i], adj = adj[i],
                                ylim = c(0, gsize[g]+1))
            }

	    ## --- Data panels. ---

            for(i in 1:nxlim) {
                dotchart.canvas(dfg$x, 1:gsize[g], par("usr")[1],
                                xlim = xlim[[i]],
                                ylim = c(0, gsize[g]+1),
                                left = left,
                                pch = dfg$pch, cex = .9,
                                col = col, bg = bg, lty = lty)

		## -- Draw bottom and top axes only. ---

                if (g == 1) {
                    axis(1, at = at1, labels = labels1, lwd = lwd)
                    if (!is.null(xat1))
                        axis(1, at = xat1, labels = FALSE, tcl = -.25,
                             lwd = lwd)
                }
                if (g == ngroup) {
                    axis(3, at = at3, labels = labels3, lwd = lwd)
                    if (!is.null(xat3))
                           axis(3, at = xat3, labels = FALSE, tcl = -.25,
                                lwd = lwd)
                }

	        ## --- Data panels have boxes ---

                box()
            }

	    ## --- If there are multiple groups, ---
	    ## --- label the rightmost data panel. ---
	    ## --- Should be more customisable. ---

            if (!is.null(names(df)))
                text(xlim[[nx]][2], 1, names(df)[g], adj = c(1, .5))
        }

	## --- Axis labels, above and below ---

        if (!is.null(xlab1))
            plot.axis.title(xlab1, side = 1, line = line1,
                            lines = mar1, cex = 1)
        else plot.new()
        if (!is.null(xlab3))
            plot.axis.title(xlab3, side = 3, line = line3,
                            lines = mar3, cex = 1)
        else plot.new()
        if (dim(x)[2] > 1)
                recordGraphics(
                               plot.key(side = 1, dimnames(x)[[2]],
                                        pch = pch[1,], col = "black",
                                        bg = "white", cex = cex),
                               list = list(), env = environment())
        invisible(l)
    }

##  Multiway Dot Charts
##
##  This function produces multiway dotcharts as detailed in
##  Cleveland's "The Elements of Graphing Data", particularly
##  those in in the first edition.
##
##  Arguments:
##
##  x:      A matrix containing the values to be plotted.
##          a separate plot is produced for each column of
##          x.  The rows of each plot are labelled by the
##          row labels of x and the plots are labelled by
##          the column labels of x.
##
##  nr:     The number of plots in each row of the layout.
##
##  nc:     The number of plots in each column of the layout.
##
##  gap:    The horizontal and vertical gap between plots.
##
##  pch:    The plotting symbol(s) to be used.
##
##  cex:    The magnification factor for text in the plot.
##
##  at:     The position of the major tick marks on the axes.
##          Separate tick positions can be specified on sides
##          1 and 3 of the display by using the at1 and at3
##          arguments.
##
##  labels: The labels to be printed at the positions specified
##          at the positions given in at.  Separate labels may
##          be specified with labels1 and labels3.
##
##  asp:    A fixed aspect ratio for the the plots.
##
##  xlab:   The label for the axes.  Separate labels can be specified
##          for the axes on sides 1 and 3 using the xlab1 and xlab3
##          arguments.

mdotchart =
    function(x, nr = 1, nc = ncol(x), gap = NULL, pch = 19,
             cex = 1, at = NULL, at1 = at, at3 = at,
             labels = NULL, labels1 = labels, labels3 = labels,
             xat = NULL, xat1 = xat, xat3 = xat,
             asp = NA, xlab = NULL,
             xlab1 = xlab, xlab3 = xlab)
    {

        ##  We allow x to be a vector, matrix or three-way
        ##  array.  We transform all these cases into the
        ##  last.

        dims = dim(x)
        if (is.null(x) || length(dims) == 1)
            x = array(x, dims = c(length(x), 1, 1),
              dimnames = list(names(x), deparse(substitute(x)), ""))
        else if (length(dims) == 2)
            x = array(x, dim = c(dims, 1),
              dimnames = c(dimnames(x), ""))
        else if (length(dims) > 3)
            stop("1, 2 or 3-way array required for x")
              
        opar = par(cex = cex, lwd = .5, oma = rep(0, 4),
                   mar = rep(0, 4), mfrow = c(1, 1))
        on.exit(par(opar))
	lwd = par("lwd")

        print(dim(x))
        nobs = nrow(x)
        npanels = ncol(x)
        npch = dim(x)[3]
        obsnames = rownames(x)
        pch = reshape(pch, nobs, npch)
        
        l = layout.trellis(nr = 1, nc = nc, nl = 1, np = npanels,
                           asp = asp, gap = gap)
        n = layout.rbind(
          make.layout(0, widths = 1, height = 1),
          layout.names(rownames(x), cex = cex), widths = 2)
        l = layout.cbind(n, l, heights = 2, gap = lcm(.25))
        l = layout.rep(l, nr, dir = "bt", gap = gap)
        layout.mat(l)[layout.mat(l) > 2 * npanels + npanels %/% nc] = 0
        l = layout.axis(l, 1,
          llines(if (is.null(xlab1)) 2.1 else 4.1, cex = cex),
          skip1 = 2)
        l = layout.axis(l, 3,
          llines(if (is.null(xlab3)) 2.1 else 4.1, cex = cex),
          skip1 = 2)
        if (dim(x)[3] > 1)
            l = layout.key(l, side = 1, width = llines(2),
              cex = cex, skip1 = 2)
        l = layout.margins(l, 1:4)
        layout.set(l)
        par(cex = cex)

        xlim = range(x)
        count = 0
        for(i in 1:nr) {
            if (count == npanels)
                break
            dotchart.names(1:nobs, rownames(x, FALSE), adj = 1,
                           ylim = c(.5, nobs + .5))
            for(j in 1:nc) {
                count = count + 1
                dotchart.canvas(as.matrix(x[,count,]), 1:nobs,
                                xleft = par("usr")[1],
                                left = FALSE, pch = pch, cex = cex,
                                xlim = xlim,
                                ylim = c(.5, nobs + .5))
                if (i == 1) {
                    if (j %% 2 == 1)
                        axis(1, at = at1, labels = labels1)
                    else
                        axis(1, at = at1, labels = FALSE)
                    if (!is.null(xat1))
                        axis(1, at = xat1, labels = FALSE, tcl = -.25,
                             lwd = lwd)                        
                }
    
                plot.new()
                plot.window(xlim = c(0, 1), xaxs = "i",
                            ylim = c(0, 1), yaxs = "i")
                text(.5, .5, dimnames(x)[[2]][count], adj = c(.5, NA))
                plot.window(xlim = xlim, ylim = c(0, 1), yaxs = "i")
                if (i == nr) {
                    if (j %% 2 == 0)
                        axis(3, at = at3, labels = labels3)
                    else
                        axis(3, at = at3, labels = FALSE)
                    if (!is.null(xat1))
                        axis(3, at = xat3, labels = FALSE, tcl = -.25,
                             lwd = lwd)
                }
                box()
            }
        }
        plot.axis.title(xlab1, 1, 3, 4.1, cex = cex)
        plot.axis.title(xlab3, 3, 3, 4.1, cex = cex)
        if (dim(x)[3] > 1)
            plot.key(side = 1, dimnames(x)[[3]], pch = pch[1,], col = "black",
                     bg = "white", cex = cex)
    }

        
## --- Organize the plotting symbol and colours ---
reshape =
    function(x, n, p) {
        if (is.matrix(x)) {
            k1 = 1:nrow(x)
            k2 = 1:ncol(x)
            x = x[rep(k1, length = n), rep(k2, length = p)]
        }
        else {
            if (length(x) == 1)
                x = matrix(x, n, p)
            else if (length(x) == p)
                x = matrix(x, n, p, byrow = TRUE)
            else if (length(x) == n)
                x = matrix(x, n, p)
            else
                stop("invalid symbol attribute dimensions")
        }
    }

dotchart.names =
    function(y, names, adj = 1,
             xlim = c(0, 1),
             ylim = range(y) + c(-1, +1))
    {
        plot.new()
        plot.window(xlim = xlim, xaxs = "i",
                    ylim = ylim, yaxs = "i")
        text(adj, y, names, adj = c(adj, .5), xpd = NA)
    }

dotchart.canvas =
    function(x, y, xleft,
             xlim = range(x),
             ylim = range(y) + c(-1, +1),
             left = TRUE,
             pch = 20, cex = 1, col = "black", bg = "white", lty = "13")
    {
        # pch = rep(pch, length = ncol(x))
        col = rep(col, length = ncol(x))
        bg = rep(bg, length = ncol(x))
        plot.new()
        plot.window(xlim = xlim,
                    ylim = ylim, yaxs = "i")
        if (!is.na(left)) {
            if (left) {
                segments(xleft, y, x, y, lty = "13")
            }
            else
                abline(h = y, lty = lty)
        }
        for(j in 1:ncol(x))
            points(x[,j], y, pch = pch[,j], col = col[j],
                   bg = bg[j], cex = cex)
        box()
    }
