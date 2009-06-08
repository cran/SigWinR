SubRidgeogram <- function (rg,start,stop) 
{
	if (!is.null(rg$position)) 
	{
		sspos = rg$position[rg$position > start & rg$position < stop]
		ssseq = rg$sequence[rg$position > start & rg$position < stop]
		sspp <- seq(min(rg$position),max(rg$position),length.out=nrow(rg$high))
		sshigh <- rg$high[(sspp > start) & (sspp< stop),]
		sslow  <- rg$low[(sspp > start) & (sspp< stop),]
		res <- list(high= sshigh,low=sslow, sequence = ssseq, position=sspos,range = rg$range,circular=FALSE)
	} else {
		h = stop-start+1;
		ssseq = rg$sequence[start:stop]
		sshigh <- rg$high[start:stop,1:(h/2)]
		sslow  <- rg$low[start:stop,1:(h/2)]
		for (i in 3:h/2) {
			sshigh[1:i,i] = 2;
			sshigh[(h-i+2):h,i] = 2;
			sslow[1:i,i] = 2;
			sslow[(h-i+2):h,i] = 2;
		}
		res <- list(high= sshigh,low=sslow, sequence = ssseq,circular=FALSE)
	}
}

