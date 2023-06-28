## An S3 Layout Class

assert =
    function(expr) if (!expr) stop("failed assertion")

##  Layout Constructor Function

make.layout =
    function(mat, widths, heights, respect = FALSE) {
        mat = as.matrix(mat)
        assert(ncol(mat) == length(widths))
        assert(nrow(mat) == length(heights))
        structure(list(mat = mat, widths = widths,
                       heights = heights, respect = respect),
                  class = "layout")
    }

##  Accessor Functions

layout.mat = function(l) l$mat
layout.widths = function(l) l$widths
layout.heights = function(l) l$heights
layout.respect = function(l) l$respect
"layout.mat<-" = function(l, value) { l$mat = value; l }
dim.layout = function(l) dim(l$mat)
                    

## An imperial version of lcm()

lin =
    function(x)
    lcm(x * 2.54)

##  A function useful for computing the height of n lines of text
##  in a form suitable for inclusion in a layout height or width
##  specification.

llines =
    function(n, cex = par("cex"))
    lcm(2.54 * n * par("csi") * cex)

##  A function to increment the values in a layout matrix
add.to.mat =
    function(mat, amount)
    ifelse(mat == 0, mat, mat + amount)

##  A function to compute the maximum width of strings in a vector
##  when drawn at the current cex value.

strwcm =
    function(labels, cex = par("cex"))
    lcm(2.54 * max(strwidth(labels, "inch", cex)))

##  Set a layout

layout.set =
    function(l)
    graphics::layout(layout.mat(l), layout.widths(l),
                     layout.heights(l), layout.respect(l))
  
##  layout.margins: Add margins to a layout
##
##  Arguments:
##
##  l     - the layout to which margins are to be added.
##  which - the sides the margins are to be added to.
##  size  - the size of margins (values are recycled if necessary).
##
##  Description
##
##  The function works by bordering the layout matrix with zeros
##  and updating the "widths" and "heights" of the layout.

layout.margins =
    function(l, which = 1:4, size = lcm(1)) {
        size = rep(size, length = length(which))
        m = layout.mat(l)
        h = layout.heights(l)
        w = layout.widths(l)
        r = layout.respect(l)
        for(i in seq(along = which)) {
            if (which[i] == 1) {
                m = rbind(m, 0)
                h = c(h, size[i])
            }
            else if (which[i] == 2) {
                m = cbind(0, m)
                w = c(size[i], w)
            }
            else if (which[i] == 3) {
                m = rbind(0, m)
                h = c(size[i], h)
            }
            else if (which[i] == 4) {
                m = cbind(m, 0)
                w = c(w, size[i])
            }
        }
        make.layout(mat = m, widths = w, heights = h, respect = r)
    }


##  layout.rep:  Replicate a layout either horizontally or vertically.
##
##  Arguments:
##
##  l   - the layout to be replicated
##  n   - the number of replications
##  gap - the gap between elements.
##  dir - the replication direction.
##      dir = "lr", left to right
##      dir = "rl", right to left
##      dir = "tb", top to bottom
##      dir = "bt", bottom to top
##
##  Note: When gap is non-positive it is set to NULL.
##  This makes the binding operations below a no-op,
##  which simplifies the layout.

layout.rep =
    function(l, n = 1, gap = NULL, dir = "lr")
    {
        m = m0 = layout.mat(l)
        w = w0 = layout.widths(l)
        h = h0 = layout.heights(l)
        r = layout.respect(l)
	mgap = 0
        if (is.null(gap) || gap <= 0) {
	    gap = NULL
	    mgap = NULL
        }
        if (dir == "lr") {
            for (i in seq(n)[-1]) {
                m = cbind(m, mgap, ifelse(m0 == 0, m0, m0 + max(m)))
                w = c(w, gap, w0)
            }
	}
	else if (dir == "rl") {
            for (i in seq(n)[-1]) {
                m = cbind(ifelse(m0 == 0, m0, m0 + max(m)), mgap, m)
                w = c(w0, gap, w)
            }
	}
	else if (dir == "tb") {
            for (i in seq(n)[-1]) {
                m = rbind(m, mgap, ifelse(m0 == 0, m0, m0 + max(m)))
                h = c(h, gap, h0)
            }
	}
	else if (dir == "bt") {
            for (i in seq(n)[-1]) {
                m = rbind(ifelse(m0 == 0, m0, m0 + max(m)), mgap, m)
                h = c(h0, gap, h)
            }
        }
        make.layout(m, w, h, r)
    }

layout.cbind =
    function(layout1, layout2, heights = 1, gap = NULL)
    {
        assert(inherits(layout1, "layout"))
        assert(inherits(layout2, "layout"))
        assert(nrow(layout1) == nrow(layout2))
        if (is.null(gap)) {
            make.layout(cbind(layout.mat(layout1),
                              add.to.mat(layout.mat(layout2),
                                         max(layout.mat(layout1)))),
                        c(layout.widths(layout1),
                          layout.widths(layout2)),
                        if (heights == 1) layout.heights(layout1)
                        else layout.heights(layout2),
                        if (heights == 1) layout.respect(layout1)
                        else layout.respect(layout2))
        }
        else {
            make.layout(cbind(layout.mat(layout1),
                              0,
                              add.to.mat(layout.mat(layout2),
                                         max(layout.mat(layout1)))),
                        c(layout.widths(layout1), gap,
                          layout.widths(layout2)),
                        if (heights == 1) layout.heights(layout1)
                        else layout.heights(layout2),
                        if (heights == 1) layout.respect(layout1)
                        else layout.respect(layout2))
        }
    }

## Optionally specify which layout contributes the widths
layout.rbind =
    function(layout1, layout2, widths = 1, gap = NULL)
    {
        assert(inherits(layout1, "layout"))
        assert(inherits(layout2, "layout"))
        assert(ncol(layout1) == ncol(layout2))
        if (is.null(gap)) {
            make.layout(rbind(layout.mat(layout1),
                              add.to.mat(layout.mat(layout2),
                                         max(layout.mat(layout1)))),
                        if (widths == 1) layout.widths(layout1)
                        else layout.widths(layout2),
                        c(layout.heights(layout1),
                          layout.heights(layout2)),
                        if (widths == 1) layout.respect(layout1)
                        else layout.respect(layout2))
        }
        else {
            make.layout(rbind(layout.mat(layout1),
                              0,
                              add.to.mat(layout.mat(layout2),
                                         max(layout.mat(layout1)))),
                        if (widths == 1) layout.widths(layout1)
                        else layout.widths(layout2),
                        c(layout.heights(layout1), gap,
                          layout.heights(layout2)),
                        if (widths == 1) layout.respect(layout1)
                        else layout.respect(layout2))
        }
    }

layout.row =
    function(widths, height, dir = "lr", gap = NULL, respect = FALSE)
    {
        l = make.layout(1, widths[1], height, respect)
        if (dir == "lr")
            for(w in widths[-1])
                l = layout.cbind(l, make.layout(1, w, height), gap = gap)
        else if (dir == "rl")
            for(w in widths[-1])
                l = layout.cbind(make.layout(1, w, height), l, gap = gap)
        else stop("invalid direction")
        l
    }

layout.col =
    function(width, heights, dir = "bt", gap = NULL, respect = FALSE)
    {
        l = make.layout(1, width, heights[1], respect)
        if (dir == "bt")
            for(h in heights[-1])
                l = layout.rbind(l, make.layout(1, width, h), gap = gap)
        else if(dir == "tb")
            for(h in heights[-1])
                l = layout.rbind(make.layout(1, width, h), l, gap = gap)
        else stop("invalid direction")
        l
    }

##  layout.names: Layout for Names Panels
##
##  Description:
##
##  Create a series of vertical label panels, each corresponding
##  to a column of "names".  The panels are separated by the specified
##  gap(s).  The panels are suitable for text drawn with size "cex".
##

layout.names =
    function(names, gaps = 0, cex = 1)
    {
        names = as.matrix(names)
        gaps = rep(gaps, length = ncol(names) - 1)
        for(i in 1:ncol(names)) {
            if (i == 1) {
                mat = matrix(1)
                wid = strwcm(names[,1], cex = cex)
            }
            else {
                mat = cbind(mat, 0, max(mat) + 1)
                wid = c(wid, gaps[i-1], strwcm(names[,i], cex = cex))
            }
        }
        make.layout(mat, wid, 1)
    }

##  layout.trellis: Trellis-Like Layouts
##
##  Arguments:
##
##  nl  The number of label strips at the top of the plot
##  nr  The number of rows in the layout
##  nc  The number of columns in the layout
##  np  The number of panels in the layout
##      Can be less than nr * nc
##
##  Description
##
##  This function creates a trellis like layout.  Within a panel,
##  the label strips are drawn first, top to bottom, followed by
##  the plotting region.  The panels are drawn in the Trellis order:
##  left to right, bottom to top.
##
##  The layout is "naked," so axes, axis labels and keys must be
##  added.

layout.trellis =
    function(nr, nc, nl, np = nr * nc, asp = NA, gap = NULL)
    {
        if (is.finite(asp)) {
            unit.h = abs(asp)
            unit.w = 1
            respect = TRUE
        }
        else {
            unit.h = 1
            unit.w = 1
            respect = FALSE
        }
        l = make.layout(matrix((nl + 1):1),
                        widths = unit.w,
                        heights = c(llines(rep(1.1, nl)), unit.h),
                        respect = respect)
        l = layout.rep(
                layout.rep(l, nc, dir = "lr", gap = gap),
                    nr, dir = "bt", gap = gap)
        layout.mat(l)[layout.mat(l) > (nl + 1) * np] = 0
        l
    }

##  layout.axis: Add margin space for an axis
##
##  Arguments:
##
##  l      - the layout which axes are to be added to.
##  which  - the side the axis will appear on.
##  size  - will the tickmarks be labelled (rotation)
##  skip1 - the rows of the layout at the bottom or
##          columns at the left to skip
##  skip2 - the rows of the layout at the top or
##          columns at the right to skip
##
##  Description:
##
##  Four margins are added to the layout, by bordering the
##  layout matrix with zeros.  The widths of the margins is
##  zero unless set to something else by the "which" and
##  size arguments.

layout.axis =
    function(l, which, size, skip1 = 0, skip2 = 0) {
        l = layout.margins(l, which = which[1], size = size)
        nr = nrow(l$mat)
        nc = ncol(l$mat)
        if (which == 1)
            layout.mat(l)[nr, (1 + skip1):(nc - skip2)] =
                max(layout.mat(l)) + 1
        else if (which == 2)
            layout.mat(l)[(nr - skip1):(1 + skip2), 1] =
                max(layout.mat(l)) + 1
        else if (which == 3)
            layout.mat(l)[1, (1 + skip1):(nc - skip2)] =
                max(layout.mat(l)) + 1
        else if (which == 4)
            layout.mat(l)[(nr - skip1):(1 + skip2), nc] =
                max(layout.mat(l)) + 1
        l
    }

##  Compute the height of an axis area,
##  given the labels and axis title.

axis.size =
    function(labels, title)
    {
        labels = (!is.logical(labels) || labels)
        title = !is.null(title)
	if (labels && title)
	    4.1
	else if (labels || title)
            2.1
        else
            1.1
    }

##  Return the line that an axis title will occupy,
##  given the labels and axis title.
axis.line =
    function(labels, title)
    {
        labels = (!is.logical(labels) || labels)
        title = !is.null(title)
	if (labels)
	    3
        else
            1
    }

##  Plot an axis title.

plot.axis.title =
    function(title, side = 1, line = 3, lines = 4.1, cex = 1)
    {
        plot.new()
        if (side == 1) {
            plot.window(xlim = c(0, 1), ylim = c(lines, 0),
                        xaxs = "i", yaxs = "i")
            text(0.5, line + .5, title, adj = c(.5, NA))            
        }
        else if (side == 3) {
            plot.window(xlim = c(0, 1), ylim = c(0, lines),
                        xaxs = "i", yaxs = "i")
            text(0.5, line + .5, title, adj = c(.5, NA))            
        }

    }

##  layout.key: Add space for a key to a layout.
##
##  Arguments:
##
##  l      - the layout to be added to
##  side   - the side on which the key is to be added
##  height - the height of the key (sides 1 & 3)
##  width  - the width of the key
##  skip1  - the number of rows/columns to skip at the top/left
##           of the layout.
##  skip2  - the number of rows/columns to skip at the bottom/right
##           of the layout.
##
##  Description:
##
##  The variables skip1 and skip2 make it possible to place
##  the key inside the areas given over to axes and labelling.
##  The default values of skip1 and skip2 assume that
##  layout.axes and layout.labels have been called to set up
##  margins for axes and axis labels.
##
##  Currently just a basic placeholder.

layout.key =
    function(l, which = 3,  size = llines(2), cex = 1,
             skip1 = 0, skip2 = 0)
    {
        l = layout.margins(l, which = which[1], size = size)
        nr = nrow(l$mat)
        nc = ncol(l$mat)
        if (which == 1)
            layout.mat(l)[nr, (1 + skip1):(nc - skip2)] =
                max(layout.mat(l)) + 1
        else if (which == 2)
            layout.mat(l)[(nr - skip1):(1 + skip2), 1] =
                max(layout.mat(l)) + 1
        else if (which == 3)
            layout.mat(l)[1, (1 + skip1):(nc - skip2)] =
                max(layout.mat(l)) + 1
        else if (which == 4)
            layout.mat(l)[(nr - skip1):(1 + skip2), nc] =
                max(layout.mat(l)) + 1
        l
    }

plot.key =
    function(side = 1, labels, pch, col, bg, cex = 1)
    {
        ##  Change to per-string width ...
        plot.new()
        plot.window(xlim = c(0, 1), xaxs = "i",
                    ylim = c(0, 1), yaxs = "i")
        wid = max(strwidth(labels, "inch", cex))
        gap1 = .25 / 2.54
        gap2 = 1.0 / 2.54
        nlab = length(labels)
        xpos = 1:nlab/(nlab+1)
        for(i in 1:length(labels)) {
            xpt = xpos[i] - xinch((gap1 + wid/2))
            points(xpt, .5, pch = pch[i], cex = .9)
            xpt = xpt + xinch(gap1)
            text(xpt, .5, labels[i], adj = c(0, NA))
        }
    }
