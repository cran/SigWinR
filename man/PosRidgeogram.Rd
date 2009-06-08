\name{PosRidgeogram}
\alias{PosRidgeogram}
\title{ Significant window detector in non regularly sampled sequences }
\description{
Create a RIDGEOGRAM for a sequence of data of non-regularly sampled data.
The function tests the input sequence against the null hypothesis that
the value in the sequence does depend on position. The function produces
p-values for the probability that the sequence is locally higher or
lower than average for a certain position and window-size for a range
of positions and window sizes.  By default the p-value is corrected for
multiple testing using the Benjamini-Hochberg correction.
}
\usage{
PosRidgeogram(seq,pos,resolution=400,winrange=c(0,max(pos)-min(pos)),adjust.method="BH",circular=FALSE)

}
\arguments{
  \item{seq}{sequence of samples}
  \item{pos}{sequence containing location of samples}
  \item{resolution}{resolution of ridgeogram }
  \item{winrange}{range of window sizes to be used}
  \item{adjust.method}{method for multiple testing correction (see p.adjust for valid options) }
  \item{circular}{assume the sequence is circular}
}
\details{
}
\value{
  A list containing 
  \item{high}{ 
Array containing Benjamini-Hochberg corrected p-values for the probability
that the sequence is locally higher than average within the given
window-size , the rows contain the windowsizes. starting from 3, the
significance for all odd window sizes are calculated. The columns contain
the p-values along the input sequence for a window size. If circular is
FALSE the number of possible windows decreases with increasing window
size and the output is centered within the rows. In this way the output
can be rendered as an image (see RidgeogramPlot). }
  \item{low}{Same as \code{high} for signifcantly lower regions.}
  \item{sequence}{The input sequence used.}
  \item{position}{The input location used.}
  \item{range}{The location range used.}
  \item{circular}{Circularity was assumed.}
}
\references{ SigWin-detector: a Grid-enabled workflow for discovering 
enriched windows of genomic features related to DNA sequences
Márcia A Inda, Marinus F van Batenburg, Marco Roos, Adam SZ Belloum, 
Dmitry Vasunin, Adianto Wibisono, Antoine HC van Kampen, and Timo M Breit
BMC Research Notes 2008; 1:63}
\author{ w.c.deleeuw@uva.nl }
\note{ 
The computation time is significantly higher than the Ridgeogram function.
1000 do not pose a problem but if they are longer you'll need patience. }
\seealso{ SigWin,RidgeogramPlot }
\examples{

slen <- 200
pos <- 4*sort(rnorm(slen))
RidgeogramPlot(PosRidgeogram(sin(pos)+rnorm(slen,sd=0.3),pos,circular=TRUE))
}

\keyword{ hplot }% __ONLY ONE__ keyword per line
