\name{ensViz}
\alias{ensViz}
\alias{EnsModel} 
\alias{EnsRegionTrack}
\alias{EnsScanBamParam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utilities for the use of Gviz in displaying gene models derived from Ensembl
}
\description{
Utilities for the use of Gviz in displaying gene models derived from Ensembl
}
\usage{
ensViz(eid, sym = "CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", ideo = TRUE, genome = "hg19", ...)
EnsModel(eid, sym = "CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", 
    ideo = TRUE, genome = "hg19", ...) 
EnsRegionTrack(eid, sym = "CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", 
    ideo = TRUE, genome = "hg19", ...) 
EnsScanBamParam(eid, sym="CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", extra=1e4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eid}{
ENSG Ensembl identifier for a gene; not needed if \code{sym}
is provided}
  \item{sym}{
gene symbol known to the Ensembl data base referenced in 
\code{edbnm}; not needed if \code{eid} is provided
}
  \item{edbnm}{
string naming a package that
provides an instance of \code{\link[ensembldb]{EnsDb-class}}
}
  \item{ideo}{
logical; if TRUE, a \code{\link[Gviz]{IdeogramTrack-class}}
instance will be displayed; this generally requires good internet access
}
  \item{genome}{
UCSC genome tag used for ideogram track collection
}
  \item{extra}{number of bases of padding around the region used to specify the \code{which}
component of the \code{\link[Rsamtools]{ScanBamParam-class}} obtained
with \code{EnsScanBamParam}}
  \item{\dots}{ for \code{EnsRegionTrack}, extra 
   arguments to \code{\link[Gviz]{GeneRegionTrack}}
 }
}
\details{
If \code{ideo==TRUE}, the Gviz caching mechanism is used
for ideogram track retrieval.  First use may be slow depending
on UCSC server load, subsequent calls for a specific chromosome
will be fast.

\code{EnsScanBamParam} helps to limit resources used to import BAM data.
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%%% ~put references to the literature/web site here ~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
ensViz(ideo=FALSE)
if (require(RNAseqData.HNRNPC.bam.chr14)) {
 fs = RNAseqData.HNRNPC.bam.chr14_BAMFILES
 em = EnsModel(sym="HNRNPC")
 em
 parm = EnsScanBamParam(sym="HNRNPC")
 tr1 = AlignmentsTrack( fs[1] , param=parm, type="coverage", name="WT" )
 tr5 = AlignmentsTrack( fs[5] , param=parm, type="coverage", name="KO" )
 emt = EnsRegionTrack(sym="HNRNPC", stacking="dense")
 plotTracks(list(tr1, tr5, emt), sizes=c(1,1,.7), ylim=c(0,200))
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphics }
