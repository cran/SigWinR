`RidgePlot` <-
function(ridges,x,y,ptitle="Ridges",labels=NULL,col=NULL,...)
{
        slen = length(x)
	if (missing(y)) 
	{
		y <- x;
		x <- seq(1:slen);
	}
	if (missing(col)) 
		col <- rep("#8888FF",length(ridges));
        plot(x,y,t="n",main=ptitle,...)
	for (i in 1:length(ridges)) {	
	        r <- ridges[[i]] 
		if (!is.null(r$r.start)) {
       	     		rect(r$r.start,min(y[y>-1e300]),r$r.end,max(y[y<1e300]),col=col[i],border=NA)
		}
	}
        lines(x,y)
	if (!missing(labels))
	        legend("topright",labels,fill=col);
}

