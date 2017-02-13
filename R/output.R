
fileOutput <- function(filename, vcodec=NULL, acodec=NULL, scodec=NULL,
                       framerate=NULL) {
    x <- list(filename=filename,
              vcodec=vcodec,
              acodec=acodec,
              scodec=scodec,
              framerate=framerate)
    class(x) <- c("FFmpeg_output_file", "FFmpeg_output")
    x
}

as.character.FFmpeg_output_file <- function(x, ...) {
    fmt <- ""
    args <- list()
    if (!is.null(x$vcodec)) {
        fmt <- paste0(fmt, "-c:v %s ")
        args <- c(args, list(as.character(x$vcodec)))
    }
    if (!is.null(x$acodec)) {
        fmt <- paste0(fmt, "-c:a %s ")
        args <- c(args, list(as.character(x$acodec)))
    }
    if (!is.null(x$scodec)) {
        fmt <- paste0(fmt, "-c:s %s ")
        args <- c(args, list(as.character(x$scodec)))
    }
    if (!is.null(x$framerate)) {
        fmt <- paste0(fmt, "-r %f ")
        args <- c(args, list(x$framerate))
    }
    fmt <- paste0(fmt, "%s ")
    args <- c(args, list(x$filename))
    do.call(sprintf, c(list(fmt), args))
}

