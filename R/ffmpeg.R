
## Lowest-level user interface

## One or more input files are read in, demuxed into streams
## (audio, video, sub-titles, and/or metadata),
## filtered, and written to one or more output files

## An input is typically just a file, but may also be the screen
## A filter can be simple (1 input and 1 output) or complex
## An output is also typically a file, but we need to specify
##   a container format, video/audio/sub-title codecs, and quality
##   information (e.g., frame rate, bit rate) [at least]
ffmpeg <- function(inputs, outputs, filters=NULL, 
                   overwrite=FALSE, wait=TRUE, echo=FALSE) {
    if (is.null(getOption("ffmpeg.path"))) {
        path <- Sys.which("ffmpeg")
        if (path == "") {
            stop("Unable to find ffmpeg (try setting 'ffmpeg.path' option)")
        } else {
            options("ffmpeg.path"=path)
        }
    }
    if (!is.null(filters)) {
        stop("Filters are currently unsupported")
    }
    ## Allow for single input
    if (inherits(inputs, "FFmpeg_input")) {
        inputs <- list(inputs)
    }
    ## Allow for single output
    if (inherits(outputs, "FFmpeg_output")) {
        outputs <- list(outputs)
    }
    options <- ""
    if (overwrite) {
        options <- paste0(options, "-y ")
    }
    cmd <- paste(getOption("ffmpeg.path"),
                 options,
                 do.call(paste, inputs),
                 ## Filters will go here (?)
                 do.call(paste, outputs))
    if (.Platform$OS.type == "windows") {
        shell(cmd, wait=wait)
    } else {
        system(cmd, wait=wait)
    }
    if (echo) {
        cat(cmd, "\n")
    }
}
