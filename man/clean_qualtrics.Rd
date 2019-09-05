\name{clean_qualtrics}
\alias{clean_qualtrics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
clean_qualtrics - processes raw Qualtrics output
}
\description{
This function prepares the raw Qualtrics output for data extraction.
It renames the SubjectID column to "SubjectID" and it reorders the subjects.
This function needs to be run first before you extract any data.
}
\usage{
clean_qualtrics(qualtricsoutput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qualtricsoutput}{
This should be a .csv file of the raw qualtricsoutput.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a dataframe. The dataframe is the raw Qualtrics output, but with a renames SubjectID columnhead and the participants in numerical order.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicole Burke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
