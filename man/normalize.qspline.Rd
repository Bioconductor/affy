\name{normalize.celfile.qspline}
\alias{qspline-normalize}
\alias{normalize.qspline}
\alias{normalize.Cel.container.qspline}
\alias{normalize.Plob.qspline}
\title{Normalize CEL files}
\description{
  normalizes a list of CEL files to each other or to a set of target intensities
}
\usage{
normalize.Cel.container.qspline(listcel, ...)

%normalize.Plob.qspline(plob, ...)

normalize.qspline(x, target = NULL, samples = NULL, 
                  fit.iters = 5, min.offset = 5, 
                  spline.method = "natural", smooth = TRUE,
                  spar = 0, p.min = 0, p.max = 1.0, 
                  incl.ends = TRUE, converge = FALSE, 
                  verbose = TRUE, na.rm = FALSE)
}
\arguments{
  \item{x}{a \code{data.matrix} of intensities}
  \item{listcel}{a \code{Cel.container}}
%  \item{plob}{a \code{Plob}}
  \item{target}{numerical vector of intensity values to normalize to.
                (could be the name for one of the celfiles in 'listcel')}
  \item{samples}{numerical, the number of quantiles to be used for spline.
                 if (0,1], then it is a sampling rate}
  \item{fit.iters}{number of spline interpolations to average}
  \item{min.offset}{minimum span between quantiles (rank difference) for the
                    different fit iterations}
  \item{spline.method}{specifies the type of spline to be used. Possible values are 
                       `"fmm"', `"natural"', and `"periodic"'.}
  \item{smooth}{logical, if `TRUE', smoothing splines are used on the quantiles}
  \item{spar}{smoothing parameter for `splinefun', typically in (0,1].}
  \item{p.min}{minimum percentile for the first quantile}
  \item{p.max}{maximum percentile for the last quantile}
  \item{incl.ends}{include the minimum and maximum values from the normalized
                   and target arrays in the fit}
  \item{converge}{(currently unimplemented)}
  \item{verbose}{logical, if `TRUE' then normalization progress is reported}
  \item{na.rm}{logical, if `TRUE' then handle NA values (by ignoring
    them)}
  \item{...}{Optional parameters to be passed through}
}
\value{
  a list of normalized celfile objects, each of them having the extra list
  element \code{method}.
}
\details{
  This normalization method uses the quantiles from each array and the 
  target to fit a system of cubic splines to normalize the data. The 
  target should be the mean (geometric) or median of each probe but could 
  be another celfile.
}
\author{
  Laurent and Workman C.
}
\references{
Workman C. et al., 2002, A new non-linear normalization method to reduce
variability in DNA microarray experiments, submitted to Genome Biology.
}
%\seealso{
%}
%\examples{
%
%  ## TO DO...  
%}
\keyword{}