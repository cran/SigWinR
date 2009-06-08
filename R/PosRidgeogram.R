`PosRidgeogram` <- 
function(seq,pos,resolution=400,winrange=c(0,max(pos)-min(pos)),adjust.method="BH",circular=FALSE)
{
        if (length(seq) != length(pos))
        {
                cat("PosRidgeogram: lenght of data and location sequences differ\n")
                return()
        }
	if ((length(winrange) <2) || (min(winrange) < 0)) {
                cat("PosRidgeogram: error in winrange definition \n")
                return()
	}
        rg = range(pos)
        rseq = rank(seq,ties.method="first");
        slen = length(seq);
        resx = resolution;
        resy = resolution;
        imgp <- matrix(2, ncol = resy, nrow = resx)
        imgn <- matrix(2, ncol = resy, nrow = resx)
        cnt = 1;
        for ( w in seq(winrange[1],winrange[2],length.out=resy)) {
                if (circular) {
                res <- .C("pos_pmedian_c",as.integer(rseq),as.double(pos),as.double(rg),as.integer(slen),w, as.integer(resx),pvalr=as.double(1:resx),pvala=as.double(1:resx),PACKAGE = "SigWinR");
                } else {
                res <- .C("pos_pmedian",as.integer(rseq),as.double(pos),as.double(rg),as.integer(slen),w, as.integer(resx),pvalr=as.double(1:resx),pvala=as.double(1:resx),PACKAGE = "SigWinR");
                }
                imgp[,cnt] <- p.adjust(res$pvalr,method=adjust.method);
                imgn[,cnt] <- p.adjust(res$pvala,method=adjust.method);
                cnt <- cnt+1;
        }
        list(high = imgp, low = imgn, sequence = seq, position = pos, range = winrange, circular = circular)
}

