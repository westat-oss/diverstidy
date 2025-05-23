% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_linguistic_terms.R
\name{detect_linguistic_terms}
\alias{detect_linguistic_terms}
\title{Detect cultural/linguistic terms in unstructured text data}
\usage{
detect_linguistic_terms(data, id, input)
}
\arguments{
\item{data}{A data frame or data frame extension (e.g. a tibble).}

\item{id}{A numeric or character vector unique to each entry.}

\item{input}{Character vector of text data for linguistic terms to be detected.}
}
\description{
This function detects linguistic terms in unstructured text data. The input 
will be a character vector of text data such as a biomedical abstract, a Twitter bio, 
or a chapter from a novel. The output column will provide the number of linguistic terms
detected in the entry.
}
\examples{

library(tidyverse)
library(diverstidy)
data(pubmed_data)

detected_terms <- pubmed_data \%>\%
  detect_linguistic_terms(fk_pmid, abstract)
  
}
