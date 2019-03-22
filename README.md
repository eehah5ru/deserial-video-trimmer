# random-video-trimmer

## requirements
- mediainfo
- ffmpeg

## building

build `stack build`

## trim-n-times

run `stack exec trim-n-times -- OPTIONS`

options:
```
rim-n-times

Usage: trim-n-times --in PATH --out PATH --black PATH [--black-prob INT]
                    [--min-len INT] [--max-len INT] [--min-black-len INT]
                    [--max-black-len INT] [--count INT]
  trim videos N times

Available options:
  --in PATH                input directory with videos
  --out PATH               output directory for trimmed videos
  --black PATH             path to black video
  --black-prob INT         probability of black video (0..100). 0 by
                           default (default: 0)
  --min-len INT            min length of trimmed cut in secs. default 2
                           secs (default: 2)
  --max-len INT            max length of trimmed cut in secs. default 4
                           secs (default: 4)
  --min-black-len INT      min length of black cut in secs. default 2
                           secs (default: 2)
  --max-black-len INT      max length of black cut in secs. default 4
                           secs (default: 4)
  --count INT              count of cuts. default 10 (default: 10)
  -h,--help                Show this help text
```


## trim-total-duration

TBA
