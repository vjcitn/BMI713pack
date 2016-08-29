\name{getOtoSheet}
\alias{getOtoSheet}
\alias{getOtoClass}
\alias{getOtoExprs}
\alias{getNdSox}
\alias{exprStateTab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
handle information from PMID 24768691, expression patterns in the developing murine otocyst
}
\description{
handle information from PMID 24768691, expression patterns in the developing murine otocyst
}
\usage{
getOtoSheet(shnum = 5)
getOtoClass()
getOtoExprs()
getNdSox()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shnum}{
%%     ~~Describe \code{shnum} here~~
}
}
\details{
These functions simplify acccess to the expression data for
"Reconstruction of the Mouse Otocyst and Early Neuroblast Lineage 
at Single-Cell Resolution" by Durruthy-Durruthy et al., PMID 24768691.

getOtoSheet retrieves the information in a sheet selected from the workbook

getOtoClass creates and returns a data frame with the A1/A2, B1-B6 classifications of
individual cells

getOtoExprs creates and returns a data frame with the per-cell expression of 96 genes,
rows are cells, columns are genes

getNdSox creates a two-column data.frame for two genes by 301 cells for which
Brip1 expression is positive
%%  ~~ If necessary, more details than the description above ~~
}
\value{
getOtoSheet returns a tbl_df instance for use with dplyr

Values of other functions are described above
}
\references{
PMID 24768691
%% ~put references to the literature/web site here ~
}
\author{
VJ Carey <stvjc@channing.harvard.edu>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
sh = getOtoSheet()
sh[1:4,1:4]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }% use one of  RShowDoc("KEYWORDS")
