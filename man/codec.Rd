\name{Codecs}
\alias{copy}
\alias{VP8}
\alias{vp8}
\alias{libvpx}
\alias{vorbis}
\alias{Vorbis}
\alias{libvorbis}
\title{
  Describe a codec
}
\description{
  Create an object that contains a description of a codec for ffmpeg.
}
\usage{
copy()
VP8(bitrate = 1, quality = 10, quantizer = NULL, constant = FALSE)
vorbis()
}
\arguments{
  \item{bitrate}{
    A number of megabits per second.
  }
  \item{quality}{
    The constant quality setting (between 4 and 63) for variable bitrate
    mode.
  }
  \item{quantizer}{
    If not \code{NULL}, a vector of two values specifying minimum and
    maximum quantizer settings (between 0 and 63).
  }
  \item{constant}{
    A logical indicating whether to use a constant bitrate.  If this is
    \code{TRUE}, \code{quality} and \code{quantizer} are ignored.
  }
}
\value{
  A codec object.
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


