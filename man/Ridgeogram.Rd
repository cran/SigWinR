\name{Ridgeogram}
\alias{Ridgeogram}
\title{ Significant window detector in sequences }
\description{
Create a RIDGEOGRAM for a sequence of data.  The function tests the input
sequence against the null hypothesis that the value in the sequence does
depend on position.  The function produces p-values for the probability 
that the sequence is locally higher or lower than average for all 
possible positions and all odd window-sizes. The p-value is corrected 
for multiple testing using the Benjamini-Hochberg correction.
}
\usage{
Ridgeogram(s,adjust.method="BH",circular=FALSE)
}
\arguments{
  \item{s}{ sequence of numeric data }
  \item{adjust.method}{ method for multiple testing correction (see p.adjust for valid options) }
  \item{circular}{ assume the sequence is circular }
}
\value{
  A list containing 
  \item{high }{ 
Array containing p-values for the probability
that the sequence is locally higher than average within
the given window-size , the rows contain the windowsizes. starting
from 3, the significance for all odd window sizes are
calculated. The columns contain the p-values along the input
sequence for a window size. If circular is FALSE the number of possible windows
decreases with increasing window size and the output is centered within 
the rows. In this way the output can be rendered as an image
(see RidgeogramPlot). }
  \item{low }{Same as \code{pos} for signifcantly lower regions}
}
\references{ SigWin-detector: a Grid-enabled workflow for discovering 
enriched windows of genomic features related to DNA sequences
Marcia A Inda, Marinus F van Batenburg, Marco Roos, Adam SZ Belloum, 
Dmitry Vasunin, Adianto Wibisono, Antoine HC van Kampen, and Timo M Breit
BMC Research Notes 2008; 1:63}
\author{ w.c.deleeuw@uva.nl }
\note{ 
The computation time increases quadratically with the length of the input sequence. 
Sequences up to about 10.000 do not pose a problem but if they are longer you'll 
need patience. Alternatively, reducing the input sequence lenght using averaging or 
median filtering can be considered. 
}
\seealso{ SigWin,RidgeogramPlot }
\examples{

lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
rg <- Ridgeogram(lseq)
RidgeogramPlot(rg,0.5,"Example ridgeogram",high.col="green",low.col="red")
}

\keyword{ hplot }% __ONLY ONE__ keyword per line
