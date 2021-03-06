% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/im_sort.R
\name{im_sort}
\alias{im_sort}
\title{A function to sort image paths for large imaging datasets}
\usage{
im_sort(setup, extension = "tif", separator = "_", position = NULL)
}
\arguments{
\item{setup}{(required) Argument used to access registration and segmentation
folders.}

\item{extension}{(optional, default = "tif") File extension of the imaging
data.}

\item{separator}{(optional, default = "_") Separator character to parse
filename.}

\item{position}{(optional) Position of z number in filename. If this argument
is called, the user input is skipped.}
}
\value{
\emph{setup} is returned with \emph{setup$image_paths} filled as a list of
sorted paths. The first element is a sorted character vector of
registration channel paths. The second element is a sorted character vector
of segmentation channel paths.
}
\description{
User friendly way to sort images from the registration channel.
Asks for user input to account for flexible naming conventions for images.
}
\details{
Compared to the \code{\link[=list.files]{list.files()}} function, this function will sort 100+
files in their appropriate order by z plane.
}
