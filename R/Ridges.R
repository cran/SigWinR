`Ridges` <-
function(s,window.size,pos=NULL,pval=0.05,antiRidge=FALSE,adjust.method="BH",min.sup=length(window.size),resolution=400)
{

        if (missing(s) | !is.numeric(s))
        {
                cat("Ridges: I need numeric input\n");
                return();
        }
        if (missing(window.size))
        {
                cat("Ridges: no window.size \n");
                return();

        }
        if (!is.numeric(window.size))
        {
                cat("Ridges: window.size must be numeric\n");
                return();
        }
        if  (is.null(pos) && min(window.size) <3)
        {
                cat("Ridges: window.size values for sequences without position must be >= 3\n");
                return();
        }
        if  (is.null(pos) && max(window.size) > length(s))
        {
                cat("Ridges: window.size values for sequences without position must be smaller than sequence length\n");
                return();
        }
        if  (!is.numeric(pval) | (pval <= 0) | (pval > 1))
        {
                cat("Ridges: pval must be number between 0 and 1\n");
                return();
        }
	if (is.null(pos)) {
		spos = seq(1,length(s));
	} else {
		spos = seq(min(pos),max(pos),length.out=resolution);
	}
        getRidge <- function(s) {
                inr <- FALSE;
                l = list(r.start=NULL, r.end=NULL);
                for (i in 1:length(s)) {
                        if (s[i] & !inr) { rstart = spos[i]; inr <- TRUE;}
                        if (!s[i] & inr)  { l$r.start= c(l$r.start,rstart); l$r.end = c(l$r.end,spos[i-1]);  inr<- FALSE }
                }
                if (inr) {
                        l$r.start= c(l$r.start,rstart); l$r.end = c(l$r.end,spos[length(spos)]);
                }
                as.data.frame(l);
        }

        pvalues <- sapply(window.size, function(w) { SigWin(s,w,pos=pos,antiRidge=antiRidge,adjust.method=adjust.method,circular=FALSE,resolution=resolution) <= pval},simplify=FALSE)
	if (is.null(pos)) {
        	tarr <- array(FALSE,dim=c(length(window.size),length(s)));
        	for (i in 1:length(window.size)) {
			st = (window.size[i]-1)/2;
			tarr[i,] <- c(rep(FALSE,st),pvalues[[i]],rep(FALSE,st))
		}
	} else {
        	tarr <- array(FALSE,dim=c(length(window.size),resolution));
        	for (i in 1:length(window.size)) {
			tarr[i,] <- pvalues[[i]]
		}
		
	}
        getRidge(apply(tarr,2,sum) >= min.sup);
}

