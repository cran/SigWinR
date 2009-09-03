\name{SubRidgeogram}
\alias{SubRidgeogram}
\title{ Extract a ridgeogram for a sub-sequences from a larger Ridgeogram}
\description{
Create a ridgeogram for a subsequence from the ridgeogram of a sequence. The result 
is different from that of direct application of Ridgeogram to the subsequence. 
}
\usage{
SubRidgeogram(rg,start,stop)
}
\arguments{
  \item{rg}{ ridgeogram}
  \item{start}{ start of the sub sequence }
  \item{stop}{ end of the sub sequence}
}
\details{
The interpretation of start and stop depends on the existence of position information in the
input ridgeogram. If available then start and stop are interprented as postions; otherwise the 
start and stop are used as indexes in the sequence array.
}
\value{
	A ridgeogram representing the subsequence. 
}
\references{ SigWin-detector: a Grid-enabled workflow for discovering 
enriched windows of genomic features related to DNA sequences
Marcia A Inda, Marinus F van Batenburg, Marco Roos, Adam SZ Belloum, 
Dmitry Vasunin, Adianto Wibisono, Antoine HC van Kampen, and Timo M Breit
BMC Research Notes 2008; 1:63}
\author{ w.c.deleeuw@uva.nl }
\note{ 

}
\seealso{ Ridgeogram,PosRidgeogram,RidgeogramPlot }
\examples{

lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
RidgeogramPlot(SubRidgeogram(Ridgeogram(lseq),325,375),0.5,
	"Example subridgeogram",high.col="green",low.col="red")
}

\keyword{ hplot }% __ONLY ONE__ keyword per line
