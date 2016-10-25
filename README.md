# UCL job searcher

University College London job site search engine. UCLs job site is a dire piece of crap that is very difficult to navigate (see code). This script will get a listing of open positions, pull down their contents and do a really basic keyword search on the actual job pages.

## Requirements

- sbcl (it may work on other lisps, haven't tried)
- systems
  - drakma
  - closure-html
  - cxml
  - css-selectors
  - cl-ppcre
  - split-sequence

## Install

`cd ~/lisp/` and `git clone ...`

Then start everyone favourite text editor (emacs) and load the system file before `ql:quickload :ucl-job-searcher`

## Disclaimer

I am in no way associated with UCL what so ever. This was just a curiosity for me to sharpen my lisp teeth on.


