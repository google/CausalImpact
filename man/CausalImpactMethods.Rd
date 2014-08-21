\name{CausalImpactMethods}
\alias{summary.CausalImpact}
\alias{print.CausalImpact}
\alias{plot.CausalImpact}
\title{Printing and plotting a \code{CausalImpact} object}
\description{
  Methods for printing and plotting the results of an
  analysis results object returned by \code{CausalImpact()}.
}
\usage{
  \method{summary}{CausalImpact}(object, ...)
  \method{print}{CausalImpact}(x, ...)
  \method{plot}{CausalImpact}(x, ...)
}
\arguments{
  \item{object}{A \code{CausalImpact} results object, as returned
  by \code{CausalImpact()}.}

  \item{x}{A \code{CausalImpact} results object, as returned
  by \code{CausalImpact()}.}

  \item{...}{Optional additional arguments. For \code{summary()}
  and \code{print()}, the first optional argument is \code{output},
  which can be \code{"summary"} (default) or \code{"report"}.
  Partial matches are allowed. Furthermore, \code{digits} can be
  used to customize the precision of the output, e.g.:
  \code{summary(..., "summary", digits = 3)}

  For \code{plot()}, the additional argument \code{metrics} can
  be used to specify which panels to include in the plot. The
  argument can be any combination of \code{"original"},
  \code{"pointwise"}, \code{"cumulative"}. Partial matches are
  allowed.}
}
\author{
  Kay H. Brodersen \email{kbrodersen@google.com}
}
\examples{
\dontrun{
impact <- CausalImpact(...)

# Print a summary table
print(impact)
summary(impact)

# Print a verbal analysis description
print(impact, "report")
summary(impact, "report")

# Create a plot
plot(impact)
plot(impact, "original")
plot(impact, "pointwise")
plot(impact, "cumulative")
plot(impact, c("original", "pointwise"))

# Customize a plot
impact.plot <- plot(impact)
impact.plot <- impact.plot + theme_bw(base_size = 20)
}
}
