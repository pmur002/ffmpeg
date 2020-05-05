
## Functions that create inputs

## The basic idea/hope is to create a simple S3 object containing the
## relevant information, PLUS an as.character() method, so we can
## just paste() them to get a valid ffmpeg command

fileInput <- function(filename, duration=NULL, framerate=NULL) {
    x <- list(name=filename,
              duration=duration,
              framerate=framerate)
    class(x) <- c("FFmpeg_input_file", "FFmpeg_input")
    x
}

as.character.FFmpeg_input_file <- function(x, ...) {
    fmt <- ""
    args <- list()
    if (!is.null(x$duration)) {
        fmt <- paste0(fmt, "-t %f ")
        args <- c(args, list(x$duration))
    }
    if (!is.null(x$framerate)) {
        fmt <- paste0(fmt, "-r %f ")
        args <- c(args, list(x$framerate))
    }
    fmt <- paste0(fmt, "-i %s ")
    args <- c(args, list(x$name))
    do.call(sprintf, c(list(fmt), args))
}

screenInput <- function(x=0, y=0, w=640, h=480, fps=25,
                        format,
                        input,
                        duration=2) {
    if (missing(format)) {
        format <- switch(.Platform$OS.type,
                         "unix"="x11grab",
                         "windows"="gdigrab")
    }
    if (missing(input)) {
        input <- switch(.Platform$OS.type,
                         "unix"=sprintf("%s+%d,%d",
                                        Sys.getenv("DISPLAY"), x$x, x$y),
                         "windows"="desktop")
    }
    x <- list(format=format,
              x=x,
              y=y,
              w=w,
              h=h,
              fps=fps,
              input=input,
              duration=duration)
    class(x) <- c("FFmpeg_input_screen", "FFmpeg_input")
    x
}

as.character.FFmpeg_input_screen <- function(x, ...) {
    sprintf("-f %s -video_size %dx%d -framerate %d -t %f -i %s ",
            x$format, x$w, x$h, x$fps, x$duration, x$input)
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

