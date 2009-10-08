`SigWin` <- function (s, window.size, pos=NULL, antiRidge = FALSE, adjust.method = "BH", circular = FALSE,resolution=400)
{
    RankMovingMedian <- function(rs, winsize, circular) {
        slen = length(rs)
        if (circular)
            .C("mmedian_c", r = as.integer(rs - 1), len = as.integer(slen),
                wsize = as.integer(winsize), res = as.integer(array(1:slen)),
                PACKAGE = "SigWinR")$res + 1
        else .C("mmedian", r = as.integer(rs - 1), len = as.integer(slen),
            wsize = as.integer(winsize), res = as.integer(array(1:(slen -
                winsize + 1))), PACKAGE = "SigWinR")$res + 1
    }
    if (!is.null(pos))
    {
        rg=range(pos)
        rseq = rank(s);
        slen = length(s);
        if (circular) {
                res <- .C("pos_pmedian_c",as.integer(rseq),as.double(pos),as.double(rg),as.integer(slen),as.double(window.size),as.integer(resolution),pvalr=as.double(1:resolution),pvala=as.double(1:resolution),PACKAGE="SigWinR");
        } else {
                res <- .C("pos_pmedian",as.integer(rseq),as.double(pos),as.double(rg),as.integer(slen),as.double(window.size),as.integer(resolution),pvalr=as.double(1:resolution),pvala=as.double(1:resolution),PACKAGE="SigWinR");
        }
        if (antiRidge) {
                res = p.adjust(res$pvala,method = adjust.method);
        } else {
                res = p.adjust(res$pvalr,method = adjust.method);
        }
    } else {
        slen = length(s)
        if (antiRidge) {
                rs <- rank(s, ties.method = "max")
        } else {
                rs <- rank(s, ties.method = "min")
        }
        st = round((window.size - 1)/2)
        range <- (st + 1):(slen - st)
        constant = log(2 * st + 1) - log(slen) - lchoose(slen - 1, 2 * st)
        ptab <- c(cumsum(exp(constant + lchoose(range - 1, st) + lchoose(slen - range, st))), rep(1, st))
        mm <- RankMovingMedian(rs, 2 * st + 1, circular)
        if (antiRidge) { idx.vals <- mm - st } else { idx.vals <- slen + 1 - mm - st }
        res = p.adjust(ptab[idx.vals], method = adjust.method)
    }
    res;
}

