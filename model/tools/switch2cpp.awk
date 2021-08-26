#!/usr/bin/env gawk
# Awk script to convert WW3 style switches in Fortran source files
# to C pre-processor style #define blocks.
# Chris Bunney, UK Met Office. Feb 2020

BEGIN {
    lastsw = ""
    swcnt = 0
    defcnt = 0
    swpre = "W3_"
} {
    if(match($0, /^!\/([A-Za-z0-9]+)([ !\/])(.*)$/, arr) || 
        match($0, /^!\/([A-Za-z0-9]+)$/, arr) ) {

        # Switch found at start if line
        if(lastsw != arr[1]) {
            # close any previous ifdef section:
            if(lastsw != "")
                printf("#endif\n")

            # open a new ifdef section with new switch
            printf("#ifdef %s%s\n", swpre, arr[1])
                defcnt += 1
        }

        # Print out rest of line after switch
        sub("/", "", arr[2]) # if switch ended with "/", then remove it
        printf("%s%s\n", arr[2], arr[3])

        # keep track of current ifdef section
        lastsw = arr[1]

        swcnt += 1
    } else {
        # no switch on this line, close any open ifdef sections
        if(lastsw != "")
            printf("#endif\n")

        # print unadulterated line
        print $0

        # clear last switch
        lastsw = ""
    }
} END {
    printf("Converted %d switches to %d #ifdef blocks\n", swcnt, defcnt) | "cat >&2"
}

