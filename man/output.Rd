\name{Outputs}
\alias{fileOutput}
\title{
  Describe an output
}
\description{
  Create an object that contains a description of an output for ffmpeg.
}
\usage{
fileOutput(filename, vcodec=NULL, acodec=NULL, scodec=NULL) 
}
\arguments{
  \item{filename}{
    The name of an output file.
  }
  \item{vcodec}{
    The name of a video codec, e.g., \code{"VP8"}.
  }
  \item{acodec}{
    The name of an audio codec, e.g., \code{"Vorbis"}.
  }
  \item{scodec}{
    The name of a sub-title codec (not currently supported).
  }
}
\value{
  An output object.
}
\seealso{
  \code{\link{ffmpeg}}
}
\references{
  \url{https://ffmpeg.org/}
}
\author{
  Paul Murrell
}
\keyword{ utilities }


