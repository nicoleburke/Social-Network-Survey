\name{language_perrelationship}
\alias{language_perrelationship}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts language info for each node
}
\description{
This function extracts the language information of each node for each subject. This function should be used after the relationship dataframe has been created.
}
\usage{
language_perrelationship(qualtricsoutput, relationshipdf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qualtricsoutput}{
This is the Qualtrics dataframe. Make sure to run 'clean_qualtrics' before you run this function.
}
  \item{relationshipdf}{
  this is the new relationship-per-line dataframe. This dataframe is the output from the 'relationship_perline' function.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function will return the relationshipdf with the added language information. Make sure to assign the relationshipdf to this function (see example).
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
relationshipdf <- gender_perrelationship(Qualtricsoupt, relationshipdf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
