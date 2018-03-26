;;; Create an image to use with SLY

(in-package "CL-USER")
(load-all-patches)

; Required for the `mrepl` in SLY: We need to have `flexi-streams` in the image and the easiest way is to use Quicklisp for
; it. Of course that means we need to first load in quicklisp itself, so the following three lines are essentially what
; `ql:add-to-init-file` creates.
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload 'flexi-streams)

; As specified at http://www.lispworks.com/documentation/lw71/LW/html/lw-95.htm#83244 
(save-image "~/lw-console"
	    :console t
	    :multiprocessing t
	    :environment nil)

;; Sample run:
;; --------------
;
;; > /Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin -build ~/Lisp/non-gui-image-maker.lisp
;;
;; ; Loading text file /Applications/LispWorks 7.1 (64-bit)/Library/lib/7-1-0-0/private-patches/load.lisp
;; ;  Loading fasl file /Applications/LispWorks 7.1 (64-bit)/Library/lib/7-1-0-0/private-patches/lisp-memory-copy-32-chunks.64xfasl
;; ;  Loading fasl file /Applications/LispWorks 7.1 (64-bit)/Library/lib/7-1-0-0/private-patches/replace-i-vectors.64xfasl
;; LispWorks(R): The Common Lisp Programming Environment
;; Copyright (C) 1987-2017 LispWorks Ltd.  All rights reserved.
;; Version 7.1.0
;; Saved by LispWorks as lispworks-7-1-0-amd64-darwin, at 16 Oct 2017 14:33
;; User agam on arrakis.lan
;; ; Loading text file /Users/agam/Lisp/non-gui-image-maker.lisp
;; ;  Loading text file /Applications/LispWorks 7.1 (64-bit)/Library/lib/7-1-0-0/private-patches/load.lisp
;; Build saving image: /Users/agam/lw-console
;; Build saved image: /Users/agam/lw-console
