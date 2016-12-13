
## Map from codec names to codec specifications
mapCodec <- function(x) {
    if (is.null(x)) {
        x
    } else {
        switch(x,

               ## Audio codecs
               libvorbis=,
               Vorbis=,
               vorbis="libvorbis",
               
               ## Video codecs
               libvpx=,
               vp8=,
               VP8="libvpx",
               
               ## Some codecs translate to themselves, e.g., "copy"
               x)
    }
}
