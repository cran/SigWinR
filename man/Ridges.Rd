\name{Ridges}
\alias{Ridges}
\title{ Calculate Ridges  }
\description{
  Use SigWin to calculate rigdges given a certain set of window sizes. 
}
\usage{
Ridges(s, window.size, pos=NULL,pval = 0.05, antiRidge = FALSE, 
    adjust.method = "BH", min.sup = length(window.size),resolution=400)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{ input sequence }
  \item{window.size}{ vector of window sizes (only odd windowsizes are considered)  }
  \item{pos}{ position of samples in input sequence }
  \item{pval}{ p-value   }
  \item{antiRidge}{  Look for anti ridges }
  \item{adjust.method}{ Multiple testing adjustment  }
  \item{min.sup}{ The minimum number of window.sizes which have to find a ridge for a certain point to consider it a ridge }
  \item{resolution}{ Resolution of the ridge calculation (for localized SigWin only) }
}
\details{
}
\value{
  A data frame is returned containing a row for each found ridge and 
  the start and end positions in the columns
}
\author{ w.c.deLeeuw@uva.nl }
\note{

}
\seealso{ SigWin,Ridgeogram }
\examples{
	lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),
		rnorm(50,4.0),rnorm(100))
	rp <- Ridges(lseq,seq(45,65,by=2))
	np <- Ridges(lseq,seq(45,65,by=2),antiRidge=TRUE)
	RidgePlot(list(rp,np),lseq,labels=c("ridges","anti-ridges"),
	   col=c("#88FF88","#FF8888"))

	lseq <- c(rnorm(100),rnorm(100,3),rnorm(300),
		rnorm(100,-3),rnorm(200),rnorm(50,4.0),rnorm(100))
	rg <- Ridgeogram(lseq)
	#find 5 window sizes in which lowest p-value occur
	win.neg <- 1+2*order(apply(log(rg$low),2,min))[1:5]
	win.pos <- 1+2*order(apply(log(rg$high),2,min))[1:5]
	rp <- Ridges(lseq,win.pos,pval=0.01)
	rn <- Ridges(lseq,win.neg,antiRidge=TRUE,pval=0.01);
	RidgePlot(list(rp,rn),lseq,ptitle="Most significant ridges",
		labels=c("ridges","anti-ridges"),col=c("#CCFFCC","#FFCCCC"))


}

\keyword{ hplot }
