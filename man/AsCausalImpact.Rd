\name{as.CausalImpact}
\alias{as.CausalImpact}
\title{Coercion to a \code{CausalImpact} object}
\description{
  Method for coercing objects to class \code{CausalImpact}.

  This function serves as a template to implement S3 methods for coercing other
  objects to \code{CausalImpact} objects in other packages. The function itself
  just dispatches a call to the appropriate S3 method.
}
\usage{
  as.CausalImpact(x, ...)
}
\arguments{
  \item{x}{Any \R object that should be coerced to a \code{CausalImpact}
    object.}
  \item{...}{Optional additional arguments (not currently used).}
}
\author{
  Kay H. Brodersen \email{kbrodersen@google.com}
}
