`RidgeogramPlot` <-
function(ridgeogram,pval=0.05,ptitle="Ridgeogram",high.col="red",low.col="blue")
{
    if (!is.numeric(ridgeogram$high) || !is.numeric(ridgeogram$low)) {
        cat("RidgeogramPlot: Something wrong with ridgeogram\n")
        return()
    }
    slen = length(ridgeogram$sequence)
    i.p <- log10(ridgeogram$high)
    i.n <- log10(ridgeogram$low)
    min.p <- min(i.p)
    min.n <- min(i.n)
    lalfa <- log10(pval)
    high.pall <- colorRampPalette(c(high.col, "white"))(20)
    low.pall <- colorRampPalette(c(low.col, "white"))(20)
    def.par <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2, 1, 3, 4, 0), 3, 2, byrow = TRUE), widths = c(2,
        lcm(1.5)), heights = c(2, 2, 1))
    par(mar = c(0, 4, 2, 2))
    plot(c(1, nrow(i.p)), y = c(1, ncol(i.p) + 1), t = "n", ylab = "windowsize",
        yaxt = "n", xaxt = "n", bty = "n", main = ptitle, xaxs = "i",
        yaxs = "i")

    if (min.p < lalfa) {
        image(x = 1:nrow(i.p), y = 1:ncol(i.p), z = i.p, zlim = c(min.p,
            lalfa), add = TRUE, col = high.pall)
    }

    if (min.n < lalfa) {
        image(x = 1:nrow(i.n), y = 1:ncol(i.n), z = i.n, zlim = c(min.n,
            lalfa), add = TRUE, col = low.pall)
    }
    if (!is.null(ridgeogram$range)) {
        wr <- ridgeogram$range
    } else {
        wr <- c(1,slen);
    }
    if (!is.null(ridgeogram$pos)) {
        pr <- c(min(ridgeogram$pos),max(ridgeogram$pos))
    } else {
        pr <- c(1,slen)
    }

    axis(2, at = (pretty(wr, 5)-min(wr))/(max(wr)-min(wr))*ncol(i.p)+1, labels = pretty(wr, 5))
    axis(1, at = (pretty(pr, 5)-min(pr))/(max(pr)-min(pr))*nrow(i.p)+1, labels = pretty(pr, 5))
    if (!ridgeogram$circular) {
        tymn = -wr[1]/(wr[2]-wr[1])*ncol(i.p)+1;
        tymx = ((max(pr)-min(pr))-wr[1])/(wr[2]-wr[1])*ncol(i.p)+1;
        lines(c(1, nrow(i.n), (nrow(i.n) + 1)/2, 1),   c(tymn, tymn, tymx,tymn))
    }

    par(mar = c(2, 2.5, 2, 1), mgp = c(2, 1, 0))
    rg <- pretty(c(min.p, lalfa), 10)
    m = matrix(rg, nrow = 1, ncol = length(rg))
    image(m, xaxt = "n", yaxt = "n", ylab = "p-val higher", col = high.pall)
    axis(2, label = as.list(signif(10^rg, 2)), at = seq(0, 1,
        by = (1/(length(rg) - 1))))
    rg <- pretty(c(min.n, lalfa), 10)
    m = matrix(rg, nrow = 1, ncol = length(rg))
    image(m, xaxt = "n", yaxt = "n", ylab = "p-val lower", col = low.pall)
    axis(2, label = as.list(signif(10^rg, 2)), at = seq(0, 1,
        by = (1/(length(rg) - 1))))
    par(mar = c(2, 4, 1, 2))
    if (!is.null(ridgeogram$pos)) {
        plot(ridgeogram$pos,ridgeogram$sequence, xlab = "", ylab = "", t = "l",bty = "u", xaxs = "i", yaxs = "r", xaxt = "n")
    } else {
        plot(ridgeogram$sequence, xlab = "", ylab = "", t = "l", bty = "u", xaxs = "i", yaxs = "r", xaxt = "n")
    }
    par(def.par)
}

