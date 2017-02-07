\name{Codecs}
\alias{copy}
\alias{VP8}
\alias{vp8}
\alias{libvpx}
\alias{VP9}
\alias{vp9}
\alias{libvpx_vp9}
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
VP8(bitrate = 1, quality = 10, quantizer = NULL, constant = FALSE,
    altref = 0)
VP9(bitrate = 1, quality = 10, quantizer = NULL, constant = FALSE,
    pass = NULL, altref = 1, laginframes = ifelse(altref, 0, 16),
    threads = NULL, speed = NULL, tilecolumns = NULL, frameparallel = 0)
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
  \item{pass}{
    If not \code{NULL}, either 1 or 2 to indicate which pass is being
    encoded (in two-pass encoding).
  }
  \item{altref}{
    Either 0 or 1, indicating whether to enable (1) or disable (0)
    the alternate reference frame.
  }
  \item{laginframes}{
    Number of frames to look ahead for alternate reference frame.
  }
  \item{threads}{
    Number of threads to use for encoding.
  }
  \item{speed}{
    Quality/speed ratio (higher numbers speed up encoding at the expense
    of quality).
  }
  \item{tilecolumns}{
    Number of tile columns.
  }
  \item{frameparallel}{
    Enable (1) or disable (0) frame parallel decodability features.
  }
}
\details{
  For \code{VP9}, setting both \code{bitrate} and \code{quality}
  produces \dQuote{constrained quality}.  To get \dQuote{constant quality},
  you must set \code{bitrate} to zero.
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


