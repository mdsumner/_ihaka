## source("layout-utilities.R")
## source("dotchart1.R")
## source("data.R")

casemap = toupper
casemap = function(x) x

dotchart(rev(animalSpeed * 8/5),
         names = casemap(names(rev(animalSpeed))),
         xlim = list(c(-.2, 2.2), c(12, 115)),
         widths = c(1, 3), at = c(0, 2, seq(25, 100, by = 25)),
         xat = c(1, seq(12, 112.5, by = 25)), labels3 = FALSE,
         xlab1 = "Speed (km/hr)", cex = .75, pch = 19)

with(jg, dotchart(area,
                  names = cbind(paste(casemap(journal), ":", sep = ""),
                    casemap(subject)),
                  adj = c(1, 0),
                  group = factor(casemap(science),
                    levels = casemap(c("Social", "Mathematical", "Natural"))),
                  xlab = casemap("Graph Area (Fraction of Total)"),
                  at = seq(0, .3, by = .1),
                  labels = c("0.0", "0.10", "0.20", "0.30"),
                  xat = seq(.05, .25, by = .1), pch = 19,
                  cex = .8))

dotchart(taxes,
	 names = casemap(names(taxes)),
         xlim = range(taxes, 900),
         xlab1 = casemap("Per Capita Taxes, 1980 (Dollars)"),
         at = seq(300, 900, by = 200),
         xat = seq(400, 800, by = 200),
         cex = .75, pch = 19)

im2 = immigrants[,c(1,4)]
dotchart(im2, cex = .8, pch = c(21, 19), bg = "white",
         xlab1 = "Percent", labels3 = FALSE)

mdotchart(immigrants, asp = 1, cex = .8, at = c(0, 20, 40),
          xlab1 = "Percent", xat = c(10, 30), gap = lcm(.1))

mdotchart(t(immigrants), nr = 3, nc = 2,  asp = 1/2, cex = .8,
          at = c(0, 20, 40), xat = c(10, 30),
          gap = lcm(.1), xlab = "Percent")
        

