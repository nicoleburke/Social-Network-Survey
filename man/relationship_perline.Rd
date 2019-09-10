\name{relationship_perline}
\alias{relationship_perline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
relationship_perline - creates a dataframe of all the nodes per subject in the sample
}
\description{
This function takes the Qualtrics output and creates a dataframe where each node for each subject is a separate line. This function needs to be run right after the 'clean_qualtrics' function.
}
\usage{
relationship_perline(qualtricsoutput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qualtricsoutput}{
This should be a .csv file with the clean Qualtrics output. You have to run 'clean_qualtrics' before you can use this function!
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a dataframe. This is a new dataframe where each line is a unique node per each subject. Subjects will have multiple rows of data (the number of rows equals their network size). The dataframe will have blank columns for Gender, Race, Age, and Language. You need to run the other functions to extract that information.
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
newrelationshipdf <- relationship_perline(cleanqualtricsoutput)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
