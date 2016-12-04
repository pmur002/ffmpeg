\name{ffmpeg}
\alias{ffmpeg}
\title{
  Process multimedia.
}
\description{
  General-purpose front-end to the ffmpeg command.
}
\usage{
ffmpeg(inputs, outputs, filters = NULL, 
       overwrite = FALSE, wait = TRUE, echo = FALSE)
}
\arguments{
  \item{inputs}{
    An input object (or a list of input objects), as created by,
    e.g., \code{\link{fileInput}}.
  }
  \item{outputs}{
    An output object (or a list of output objects), as created by,
    e.g., \code{\link{fileOutput}}.
  }
  \item{filters}{
    Not currently supported.
  }
  \item{overwrite}{
    A logical indicating whether to ask before overwriting output files.
  }
  \item{wait}{
    A logical indicating whether R should block and wait for the
    video operation to complete.
  }
  \item{echo}{
    A logical indicating whether to echo the ffmpeg command.
  }
}
\details{
  Read one or more input files, apply zero or more filters, and write
  one or more output files.
}
\references{
  \url{https://ffmpeg.org/}
}
\author{
  Paul Murrell
}
\seealso{
  \link[=fileInput]{Inputs} and \link[=fileOutput]{Outputs}.
}
\examples{
\dontrun{
ffmpeg(screenInput(), outputs=fileOutput("test.webm"))
}
}
\keyword{ utilities }
