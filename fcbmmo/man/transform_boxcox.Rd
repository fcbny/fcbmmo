\name{transform_boxcox}
\alias{transform_boxcox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
transform_boxcox(x, lambda1 = 0, lambda2 = NULL, type = "1-parameter")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{lambda1}{
%%     ~~Describe \code{lambda1} here~~
}
  \item{lambda2}{
%%     ~~Describe \code{lambda2} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (x, lambda1 = 0, lambda2 = NULL, type = "1-parameter") 
{
    if (length(type) != 1) {
        stop("You've provided more than one or zero arguement to parameter 'type'")
    }
    if (type != "1-parameter" & type != "2-parameter") {
        stop("Invalid 'type' parameter. Only '1-parameter' or '2-parameter' accepted")
    }
    if (type == "1-parameter") {
        if (TRUE \%in\% (x <= 0)) {
            stop("Only x > 0 values are valid in 1-parameter boxcox")
        }
        if (!is.null(lambda1)) {
            r <- onePam_boxcox(x, lambda = lambda1)
        }
        else {
            stop("Profile likelihood estimation not implemented yet")
        }
    }
    else if (type == "2-parameter") {
        if (!is.null(lambda1) & !is.null(lambda2)) {
            r <- twoPam_boxcox(x, lambda1 = lambda1, lambda2 = lambda2)
        }
        else {
            stop("Both lambdas must be numeric in 2-parameter boxcox")
        }
    }
    return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
