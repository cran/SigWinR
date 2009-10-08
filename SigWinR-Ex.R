pkgname <- "SigWinR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SigWinR')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("PosBinFilter")
### * PosBinFilter

flush(stderr()); flush(stdout())

### Name: PosBinFilter
### Title: Bin filter
### Aliases: PosBinFilter
### Keywords: hplot

### ** Examples

	x <- rnorm(300,sd=3)
	plot(PosBinFilter(x,sin(x),50),t="l")



cleanEx()
nameEx("PosRidgeogram")
### * PosRidgeogram

flush(stderr()); flush(stdout())

### Name: PosRidgeogram
### Title: Significant window detector in non regularly sampled sequences
### Aliases: PosRidgeogram
### Keywords: hplot

### ** Examples


slen <- 200
pos <- 4*sort(rnorm(slen))
RidgeogramPlot(PosRidgeogram(sin(pos)+rnorm(slen,sd=0.3),pos,circular=TRUE))



cleanEx()
nameEx("RidgePlot")
### * RidgePlot

flush(stderr()); flush(stdout())

### Name: RidgePlot
### Title: Plot ridges
### Aliases: RidgePlot
### Keywords: hplot

### ** Examples

x<-sort(unlist(sapply(1:10,function(i) {runif(round((rnorm(1,0,10))^2),i-1,i)})))
y <- sin(x*2)+rnorm(length(x))
ridges <- list(Ridges(y,1,x,pval=0.05),Ridges(y,1,x,pval=0.05,antiRidge=TRUE))
RidgePlot(ridges,x,y,col=c("#CCFFCC","#FFCCCC"))
lines(seq(0.1,10,by=0.1),sin(2*seq(0.1,10,by=0.1)),col="blue",lw=2)



cleanEx()
nameEx("Ridgeogram")
### * Ridgeogram

flush(stderr()); flush(stdout())

### Name: Ridgeogram
### Title: Significant window detector in sequences
### Aliases: Ridgeogram
### Keywords: hplot

### ** Examples


lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
rg <- Ridgeogram(lseq)
RidgeogramPlot(rg,0.5,"Example ridgeogram",high.col="green",low.col="red")



cleanEx()
nameEx("RidgeogramPlot")
### * RidgeogramPlot

flush(stderr()); flush(stdout())

### Name: RidgeogramPlot
### Title: Plot a RIDGEOGRAM
### Aliases: RidgeogramPlot
### Keywords: hplot

### ** Examples

RidgeogramPlot(Ridgeogram(0.5*rnorm(124)+cos(seq(0.1,12.4,by=0.1))));
lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
rg <- Ridgeogram(lseq)
RidgeogramPlot(rg,0.05,"Example ridgeogram",high.col="green",low.col="red")



cleanEx()
nameEx("Ridges")
### * Ridges

flush(stderr()); flush(stdout())

### Name: Ridges
### Title: Calculate Ridges
### Aliases: Ridges
### Keywords: hplot

### ** Examples

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





cleanEx()
nameEx("SigWin")
### * SigWin

flush(stderr()); flush(stdout())

### Name: SigWin
### Title: Significant window detector in sequences
### Aliases: SigWin
### Keywords: hplot

### ** Examples


lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
plot((lseq-min(lseq))/(max(lseq)-min(lseq)),t="l")
lines(18:(length(lseq)-17),SigWin(lseq,35),t="l",col="green")
lines(18:(length(lseq)-17),SigWin(lseq,35,antiRidge=TRUE),t="l",col="red")



cleanEx()
nameEx("SubRidgeogram")
### * SubRidgeogram

flush(stderr()); flush(stdout())

### Name: SubRidgeogram
### Title: Extract a ridgeogram for a sub-sequences from a larger
###   Ridgeogram
### Aliases: SubRidgeogram
### Keywords: hplot

### ** Examples


lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
RidgeogramPlot(SubRidgeogram(Ridgeogram(lseq),325,375),0.5,
	"Example subridgeogram",high.col="green",low.col="red")



cleanEx()
nameEx("rsigwin-package")
### * rsigwin-package

flush(stderr()); flush(stdout())

### Name: SigWinR-package
### Title: calculate and display significant windows and ridgeograms
### Aliases: SigWinR-package SigWinR
### Keywords: package

### ** Examples

lseq <- c(rnorm(200),rnorm(100,-1),rnorm(50),rnorm(50,4.0),rnorm(100))
ridgeogram <- Ridgeogram(lseq)
RidgeogramPlot(ridgeogram,0.05,"Example ridgeogram",
	high.col="green",low.col="red")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
