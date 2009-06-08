\name{SigWinR-package}
\alias{SigWinR-package}
\alias{SigWinR}
\docType{package}
\title{
	calculate and display significant windows and ridgeograms
}
\description{
R implemenation of SigWin-detector developed be Márcia A Inda. 
}
\details{
\tabular{ll}{
Package: \tab SigWinR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-02-04\cr
License: \tab GPL\cr
}
}
\author{
Wim de Leeuw
<w.c.deleeuw@uva.nl>
}
\references{ Details can be found in: SigWin-detector: a Grid-enabled workflow for discovering
enriched windows of genomic features related to DNA sequences
Márcia A Inda, Marinus F van Batenburg, Marco Roos, Adam SZ Belloum,
Dmitry Vasunin, Adianto Wibisono, Antoine HC van Kampen, and Timo M Breit
BMC Research Notes 2008; 1:63}
\keyword{ package }
\seealso{ RidgeogramPlot,Ridgeogram,SigWin }
\examples{
lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
ridgeogram <- Ridgeogram(lseq)
RidgeogramPlot(ridgeogram,0.05,"Example ridgeogram",high.col="green",low.col="red")
}


