\name{Inputs}
\alias{fileInput}
\alias{screenInput}
\alias{concatInput}
\title{
  Describe an input
}
\description{
  Create an object that contains a description of an input for ffmpeg.
}
\usage{
fileInput(filename, duration=NULL) 
screenInput(x=0, y=0, w=640, h=480, fps=25, display="0.0",
            duration=2)
concatInput(filenames) 
}
\arguments{
  \item{filename}{
    The name of an input file.  
  }
  \item{filenames}{
    The names of one or more input files.  
  }
  \item{duration}{
    The number of seconds to read from the input.
  }
  \item{x,y,w,h}{
    The region of the screen to capture.
  }
  \item{display}{
    The X11 display to capture.
  }
  \item{fps}{
    The frame rate for screen capture.
  }
}
\value{
  An input object.
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

