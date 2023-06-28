#  This function plots a vector of R colours
#  as a horizontal colour ramp.

cramp =
function(colours) {
  n = length(colours)
  plot.new()
  plot.window(xlim = c(0,n),
              ylim = c(0,1))
  rect(1:n - 1, 0, 1:n, 1, col = colours, border = NA)
}

