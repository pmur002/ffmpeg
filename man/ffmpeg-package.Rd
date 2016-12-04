\name{ffmpeg-package}
\alias{ffmpeg-package}
\docType{package}
\title{
\packageTitle{ffmpeg}
}
\description{
\packageDescription{ffmpeg}
}
\details{
  An R interface to the \code{ffmpeg} command (on Linux).
}
\author{
\packageAuthor{ffmpeg}

Maintainer: \packageMaintainer{ffmpeg}
}
\references{
  \url{https://ffmpeg.org/}
}
\keyword{ package }
\seealso{
  \code{\link{ffmpeg}} 
}
\examples{
\dontrun{
ffmpeg(screenInput(), outputs=fileOutput("test.webm"))
}
}
