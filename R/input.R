
## Functions that create inputs

## The basic idea/hope is to create a simple S3 object containing the
## relevant information, PLUS an as.character() method, so we can
## just paste() them to get a valid ffmpeg command

fileInput <- function(filename, duration=NULL) {
    x <- list(name=filename,
              duration=duration)
    class(x) <- c("FFmpeg_input_file", "FFmpeg_input")
    x
}

as.character.FFmpeg_input_file <- function(x, ...) {
    if (is.null(x$duration)) {
        sprintf("-i %s", x$name)
    } else {
        sprintf("-t %f -i %s", x$duration, x$name)
    }
}

screenInput <- function(x=0, y=0, w=640, h=480, fps=25,
                        display=Sys.getenv("DISPLAY"),
                        duration=2) {
    format <- switch(R.version$os,
                     "linux-gnu"="x11grab",
                     stop("Only Linux is currently supported"))
    x <- list(format=format,
              x=x,
              y=y,
              w=w,
              h=h,
              fps=fps,
              display=display,
              duration=duration)
    class(x) <- c("FFmpeg_input_screen", "FFmpeg_input")
    x
}

as.character.FFmpeg_input_screen <- function(x, ...) {
    sprintf("-f %s -video_size %dx%d -framerate %d -t %f -i %s+%d,%d ",
            x$format, x$w, x$h, x$fps, x$duration, x$display, x$x, x$y)
}

concatInput <- function(filenames) {
    x <- list(files=filenames)
    class(x) <- c("FFmpeg_input_concat", "FFmpeg_input")
    x
}

as.character.FFmpeg_input_concat <- function(x, ...) {
    tmpfile <- tempfile()
    writeLines(paste0("file '",
                      normalizePath(x$files), "'"), tmpfile)
    ## -safe 0 allows for absolute file paths
    sprintf("-f concat -safe 0 -i %s ", tmpfile)
}

