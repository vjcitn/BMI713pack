\name{ensViz}
\alias{ensViz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use Gviz to display a gene model derived from Ensembl
}
\description{
Use Gviz to display a gene model derived from Ensembl
}
\usage{
ensViz(eid, sym = "CRISPLD2", edbnm = "EnsDb.Hsapiens.v75", ideo = TRUE, genome = "hg19", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eid}{
ENSG Ensembl identifier for a gene; not needed if \code{sym}
is provided
}
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
instance will be displayed
}
  \item{genome}{
UCSC genome tag used for ideogram track collection
}
  \item{\dots}{
not used
}
}
\details{
If \code{ideo==TRUE}, the Gviz caching mechanism is used
for ideogram track retrieval.  First use may be slow depending
on UCSC server load, subsequent calls for a specific chromosome
will be fast.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphics }
