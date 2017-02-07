
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

VP9 <- function(bitrate=1, quality=10, quantizer=NULL, constant=FALSE,
                pass=NULL, altref=1, laginframes=ifelse(altref, 0, 16),
                threads=NULL, speed=NULL, tilecolumns=NULL,
                frameparallel=0, an=FALSE) {
    x <- list(bitrate=bitrate,
              quality=quality,
              quantizer=quantizer,
              constant=constant,
              pass=pass,
              altref=altref,
              laginframes=laginframes,
              threads=threads,
              speed=speed,
              tilecolumns=tilecolumns,
              frameparallel=frameparallel,
              an=an)
    class(x) <- c("FFmpeg_codec_VP9", "FFmpeg_codec")
    x
}

## Aliases for VP8
vp9 <- libvpx_vp9 <- VP9

as.character.FFmpeg_codec_VP9 <- function(x, ...) {
    ## For now force disable of alternate reference frames
    ## (else get error when running PNGs into webm video)
    fmt <- "libvpx-vp9 -f webm -b:v %dM -auto-alt-ref %d "
    args <- list(x$bitrate, x$altref)
    if (x$constant) {
        fmt <- paste0(fmt, "-minrate %dM -maxrate %dM ")
        args <- c(args, list(x$bitrate))
    } else {
        fmt <- paste0(fmt, "-crf %d ")
        args <- c(args, list(x$quality))
    }
    if (!is.null(x$quantizer)) {
        fmt <- paste0(fmt, "-qmin %d -qmax %d ")
        args <- c(args, list(x$quantizer[1], x$quantizer[2]))
    }
    if (!is.null(x$pass)) {
        fmt <- paste0(fmt, "-pass %d ")
        args <- c(args, list(x$pass))
    }
    if (x$laginframes > 0) {
        fmt <- paste0(fmt, "-lag-in-fames %d ")
        args <- c(args, list(x$laginframes))
    }
    if (!is.null(x$threads)) {
        fmt <- paste0(fmt, "-threads %d ")
        args <- c(args, list(x$threads))
    }
    if (!is.null(x$speed)) {
        fmt <- paste0(fmt, "-speed %d ")
        args <- c(args, list(x$speed))
    }
    if (!is.null(x$tilecolumns)) {
        fmt <- paste0(fmt, "-tile-columns %d ")
        args <- c(args, list(x$tilecolumns))
    }
    if (x$frameparallel) {
        fmt <- paste0(fmt, "-frame-parallel %d ")
        args <- c(args, list(x$frameparallel))
    }
    if (x$an) {
        fmt <- paste0(fmt, "-an ")
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

