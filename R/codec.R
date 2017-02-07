
################################################################################
## Functions to generate codecs

copy <- function() {
    x <- list()
    class(x) <- c("FFmpeg_codec_copy", "FFmpeg_codec")
    x
}

as.character.FFmpeg_codec_copy <- function(x, ...) {
    fmt <- "copy "
    args <- list()
    do.call(sprintf, c(list(fmt), args))
}


################################################################################
## Video codecs

## bitrate is a number of megabits per second
## quality is from 4-63 (lower better)
## quantizer is NULL or two values from 0-63
## constant is boolean
VP8 <- function(bitrate=1, quality=10, quantizer=NULL, constant=FALSE,
                altref=0) {
    x <- list(bitrate=bitrate,
              quality=quality,
              quantizer=quantizer,
              constant=constant,
              altref=altref)
    class(x) <- c("FFmpeg_codec_VP8", "FFmpeg_codec")
    x
}

## Aliases for VP8
vp8 <- libvpx <- VP8

as.character.FFmpeg_codec_VP8 <- function(x, ...) {
    ## For now force disable of alternate reference frames
    ## (else get error when running PNGs into webm video)
    fmt <- "libvpx -b:v %dM -auto-alt-ref %d "
    args <- list(x$bitrate, x$altref)
    if (x$constant) {
        fmt <- paste0(fmt, "-minrate %dM -maxrate %dM ")
        args <- c(args, list(x$bitrate))
    } else {
        fmt <- paste0(fmt, "-crf %d ")
        args <- c(args, list(x$quality))
    }
    if (!is.null(x$quantizer)) {
        fmt <- paste0(fmt, "-qmin %d -qmax %d")
        args <- c(args, list(x$quantizer[1], x$quantizer[2]))
    }
    do.call(sprintf, c(list(fmt), args))
}

################################################################################
## Audio codecs

vorbis <- function() {
    x <- list()
    class(x) <- c("FFmpeg_codec_vorbis", "FFmpeg_codec")
    x
}

## Aliases for VP8
Vorbis <- libvorbis <- vorbis

as.character.FFmpeg_codec_vorbis <- function(x, ...) {
    fmt <- "libvorbis "
    args <- list()
    do.call(sprintf, c(list(fmt), args))
}

