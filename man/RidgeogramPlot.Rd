\name{RidgeogramPlot}
\alias{RidgeogramPlot}
\title{ Plot a RIDGEOGRAM }
\description{
  Basic RIDGEOGRAM plotter.  
}
\usage{
RidgeogramPlot(ridgeogram, pval=0.05, ptitle = "Ridgeogram", high.col = "red", low.col = "blue",p.range.min)
}
\arguments{
  \item{ridgeogram}{ridgeogram to plot. If not given it will be calculated }
  \item{pval}{ the highes p-value given a color in the graph}
  \item{ptitle}{ title of the graph }
  \item{high.col}{ color used to show positive significant windows  }
  \item{low.col}{ color used to show negative significant windows }
  \item{p.range.min}{lower-bound for p-value color-range. }
}
\details{
	In the plot the ridgeogram is plotted along with the sequence used as input of the graph. 
}
\references{ SigWin-detector: a Grid-enabled workflow for discovering 
enriched windows of genomic features related to DNA sequences
Marcia A Inda, Marinus F van Batenburg, Marco Roos, Adam SZ Belloum, 
Dmitry Vasunin, Adianto Wibisono, Antoine HC van Kampen, and Timo M Breit
BMC Research Notes 2008; 1:63}
\author{ w.c.deleeuw@uva.nl }
\seealso{ Ridgeogram() }
\examples{
RidgeogramPlot(Ridgeogram(0.5*rnorm(124)+cos(seq(0.1,12.4,by=0.1))));
lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
rg <- Ridgeogram(lseq)
RidgeogramPlot(rg,0.05,"Example ridgeogram",high.col="green",low.col="red")
}
\keyword{ hplot }
