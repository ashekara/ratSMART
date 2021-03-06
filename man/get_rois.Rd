% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rois.R
\name{get_rois}
\alias{get_rois}
\title{Get ROI data from a mapped wholebrain dataset.}
\usage{
get_rois(dataset, rois = c("ACB", "CEA"))
}
\arguments{
\item{dataset}{(required) Whole brain region mapped dataset generated from
the \code{\link[=forward_warp]{forward_warp()}} function}

\item{rois}{(optional, default = c("ACB", "CEA")) A character vector
containing the regions of interest.}
}
\value{
Returns a dataset of just the regions of interest from the whole
brain dataset.
}
\description{
Allows the user to enter a character vector of the regions of
interest (ROIs) and obtain a subset of the dataframe from the wholebrain
dataset of just the ROIs.
}
