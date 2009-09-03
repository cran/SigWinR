\name{SigWin}
\alias{SigWin}
\title{ Significant window detector in sequences }
\description{
Calculate significant local changes in a sequence of data. 
The function tests the input sequence against the null hypothesis that 
the value in the sequence does not depend on position. 
The function produces p-values for the probability that the sequence is 
locally higher or lower than average for all possible positions for a given window size. 
The p-value is corrected for multiple testing using a method of choice. 

Two variants of SigWin are implemented: one for simple ordered sequences
without location information , and a second for a sequence of localized
samples (for example expression measurements at known positions on
a chromosome). 

}
\usage{
SigWin(s,window.size,pos=NULL,antiRidge=FALSE,adjust.method="BH",circular=FALSE,resolution=400)
}
\arguments{
  \item{s}{ sequence of numeric data }
  \item{window.size}{ size of window to use. For non localized (\code{pos=NULL}) sequences the 
  window size is in sequence elements. For localized data the winow size is relative 
  to the location values given in the \code{pos} parameter }
  \item{pos}{ optional sequence of sample location data }
  \item{antiRidge}{ look for positive (antiRidge=FALSE) or negative deviations (antiRidge=TRUE). }
  \item{adjust.method}{ method for multiple testing correction (see p.adjust for valid options). }
  \item{circular}{ Assume the sequence is circular. If set to \code{TRUE} the window can extend 
  over the edges of the sequence into the other ende of the sequence. }
  \item{resolution}{ resolution of SigWin calculation for localized sequences. Value is ignored for calculations without location data  }
}
\details{
}
\value{
A sequence containing the p-values corrected for multiple testing. For
simple non localized sequences the length of the output sequence is the
length of the input sequence plus one minus the window size. For localized
sequences the length of the output is equal to the resolution parameter.
}
\references{ SigWin-detector: a Grid-enabled workflow for discovering 
enriched windows of genomic features related to DNA sequences
Marcia A Inda, Marinus F van Batenburg, Marco Roos, Adam SZ Belloum, 
Dmitry Vasunin, Adianto Wibisono, Antoine HC van Kampen, and Timo M Breit
BMC Research Notes 2008; 1:63}
\author{ w.c.deleeuw@uva.nl }
\note{ 
This function can be used for purposes where Rigeogram is not needed or wanted such as long 
sequences. To generate a ridgeogram the Ridgeogram function is faster than repeated calls to 
SigWin using a range of window sizes. 
}
\seealso{ Ridgeogram }
\examples{

lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
plot((lseq-min(lseq))/(max(lseq)-min(lseq)),t="l")
lines(18:(length(lseq)-17),SigWin(lseq,35),t="l",col="green")
lines(18:(length(lseq)-17),SigWin(lseq,35,antiRidge=TRUE),t="l",col="red")
}

\keyword{ hplot }% __ONLY ONE__ keyword per line
