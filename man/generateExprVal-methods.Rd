\name{generateExprVal}
\alias{generateExprVal}
\alias{generateExprVal-methods}
\alias{generateExprVal.methods}
\alias{express.summary.stat}
\alias{express.summary.stat-methods}
\alias{express.summary.stat.methods}

\title{Generate an expression value from the probes informations}
\description{
  Generate an expression from the from the probes
}
\usage{
generateExprVal.PPSet.container(pps.container, method, ...)
generateExprVal.methods # vector of names of methods

express.summary.stat(pps.container, method, ...)
express.summary.stat.methods # vector of names of methods
}
\arguments{
  \item{pps.container}{a \code{\link[Biobase]{container-class}} (\code{PPSet.container} for
    now.. .a \code{Plob} in the future ?..)}
  \item{method}{the method used to generate the expression value.}
  \item{...}{other parameters the method might need... (see the
    corresponding methods below...)}
}
\value{
Returns a vector of expression values.
}
\examples{
  ## loading data
  data(listcel)
  data(CDF.example)

  affyid <- "A28102_at"
  pps <- get.PPSet(affyid, CDF.example, listcel[[1]])
  nprobes <- nrow(probes(pps))

  pc <- new("PPSet.container",
            pmProbes=matrix(NA, nprobes, 2, dimnames=list(NULL, rep(affyid,2))),
            mmProbes=matrix(NA, nprobes, 2)
           )

  pc[[1]] <- get.PPSet(affyid, CDF.example, listcel[[1]])
  pc[[2]] <- get.PPSet(affyid, CDF.example, listcel[[2]])

  express.summary.stat(pc, method="playerout", bg.correct="bg.correct.pmonly")

}
\keyword{}