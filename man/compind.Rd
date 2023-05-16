\name{compind}
\alias{compind}
\title{
Computation of PCA based composite index values
}
\description{
The “compind” function estimates the relative weights of variables using principal component analysis method, and then computes the composite index values by aggregating the weighted variables.
}
\usage{
compind(data, var_p, var_n)
}
\arguments{
  \item{data}{
Multivariate data set comprises positive and negative variables.
}
  \item{var_p}{
A set of positive variables. Example: var_p = c("A", "B", "C", "D", "F", "G", "H", "I", "J"), where "A", "B", "C", "D", "F", "G", "H", "I", "J" are positive variables.
}
  \item{var_n}{
A set of negative variables. Example: var_n = c("E"), where "E" is negative varible.
}
}

\details{
Variables measured in different scales, and having different units are often required to be aggregated for summarizing the phenomenon under study. As the variables may have different degrees of importance with respect to the phenomenon under question, their relative importance need to be evaluated before aggregating them. The present package offers a principal component analysis based method to derive weights of the constituent variables of the phenomenon under question, and thereby combining the weighted variables for a composite index value.
}
\value{
  \item{Index }{Composite index value.}
  \item{Weights }{Principal component analysis based derived weights of the constituent variables. }

}
\references{
Sendhil, R., Jha, A., Kumar, A. and Singh, S. (2018). Extent of vulnerability in wheat producing agro ecologies of India: Tracking from indicators of cross section and multi dimension data. Ecological Indicators, 89(2018): 771--780.

Wu, T. (2021). Quantifying coastal flood vulnerability for climate adaptation policy using principal component analysis. Ecological Indicators, 129(2021): 108006.

}
\seealso{
Data_sample
}
\examples{
data("Data_sample")
compind(Data_sample, var_p=c("A", "B", "C", "D", "F", "G", "H", "I",  "J"), var_n = c("E"))
}
\keyword{compositive index}
