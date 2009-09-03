
\name{RidgePlot}
\alias{RidgePlot}
\title{ Plot ridges}
\description{ Plot a graph of a sequence with ridges calulated by Ridges
}
\usage{
RidgePlot(ridges,x,y,ptitle="Ridges",labels=NULL,col=NULL,...)
}
\arguments{
  \item{ridges}{ a list of ridges produced by one or more cals to Ridges}
  \item{x}{ an optional vector with the same length as y of sample positions  }
  \item{y}{ the sequence for which the ridges are calculated }
  \item{ptitle}{ The title of the graph}
  \item{labels}{ A vector of the labels for the elements in the ridge list}
  \item{col}{ The colors for rendering the elements of the ridge list}
  \item{...}{ Additional plot parameters}
}
\details{
  An xy-plot will be drawn with the ridges shown as rectangles behind the graph.
}
\value{
}
\references{ SigWin-detector: a Grid-enabled workflow for discovering 
enriched windows of genomic features related to DNA sequences
Marcia A Inda, Marinus F van Batenburg, Marco Roos, Adam SZ Belloum, 
Dmitry Vasunin, Adianto Wibisono, Antoine HC van Kampen, and Timo M Breit
BMC Research Notes 2008; 1:63}
\author{ w.c.deleeuw@uva.nl }
\note{ 
}
\seealso{ Ridges,SigWin}
\examples{
x<-sort(unlist(sapply(1:10,function(i) {runif(round((rnorm(1,0,10))^2),i-1,i)})))
y <- sin(x*2)+rnorm(length(x))
ridges <- list(Ridges(y,1,x,pval=0.05),Ridges(y,1,x,pval=0.05,antiRidge=TRUE))
RidgePlot(ridges,x,y,col=c("#CCFFCC","#FFCCCC"))
lines(seq(0.1,10,by=0.1),sin(2*seq(0.1,10,by=0.1)),col="blue",lw=2)
}

\keyword{ hplot }% __ONLY ONE__ keyword per line
