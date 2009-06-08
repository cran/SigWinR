`Ridgeogram` <-
function(s,adjust.method="BH",circular=FALSE)
{
        RankMovingMedian <- function(rs,winsize,circular) {
                slen = length(rs)
		if (circular) 
		.C("mmedian_c",r = as.integer(rs-1), len = as.integer(slen),wsize = as.integer(winsize), res = as.integer(array(1:slen)), PACKAGE = "SigWinR")$res+1 
		else
                .C("mmedian",r=as.integer(rs-1),len=as.integer(slen),wsize=as.integer(winsize),res=as.integer(array(1:(slen-winsize+1))),PACKAGE="SigWinR")$res+1
        }
        slen = length(s)
        rs <- rank(s,ties.method="max");
	mdf <- rank(s[order(s)],ties.method="min"); 
        imgp <- matrix(2,ncol=slen/2,nrow=slen)
        imgn <- matrix(2,ncol=slen/2,nrow=slen)
        for (st in 1:(slen/2-1)) {
                prange <- (st+1):(slen-st)
		if (circular) { irange = 1:slen } else {irange <- prange }
                constant = log(2*st+1)-log(slen)-lchoose(slen-1,2*st)
                ptab <- c(cumsum(exp(constant+lchoose(prange-1,st)+lchoose(slen-prange,st))),rep(1,st));
                mm <- RankMovingMedian(rs,2*st+1,circular);
                imgp[irange,st] <- p.adjust(ptab[slen+1-mdf[mm]-st],method = adjust.method)
                imgn[irange,st] <- p.adjust(ptab[mm-st],method = adjust.method)
        }
        list(high=imgp,low=imgn,sequence=s,circular=circular)
}

