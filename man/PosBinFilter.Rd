\name{PosBinFilter}
\alias{PosBinFilter}
\title{ Bin filter }
\description{
Function meant as in input filter to Rigeogram or RigeoPic to generate Rigeograms of 
non-regularly sampled data. Filter a sequence sampled at arbitrary positions to 
generate samples at regular intervals which can serve as input to Rigeogram() or Ridges(). 
}
\usage{
PosBinFilter(x, y, steps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The positions of the samples  }
  \item{y}{ The sample values  }
  \item{steps}{ The number of values in the new sequence }
}
\value{
  The function returns a list of length \code{steps}. The signal is approximated 
over the range of values in x in bins equal length. For each bin the median value 
of the sample values found in the bin is calculated. If no signal values are found 
in the interval the mean value of the bins containing values is inserted. 
}
\author{ Wim de Leeuw <w.c.deLeeuw@uva.nl> }
\note{ 
This one possible way to achieve a regular sampling. More 
elaborate and clever methods might be needed in specific situations. 

}
\seealso{ Ridgeogram()  }
\examples{
	x <- rnorm(300,sd=3)
	plot(PosBinFilter(x,sin(x),50),t="l")
}
\keyword{ hplot }
