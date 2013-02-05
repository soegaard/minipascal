#lang racket/base
 
(require (planet neil/charterm))
 
(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 10 5)
 (charterm-display "Hello, ")
 (charterm-bold)
 (charterm-display "you")
 (charterm-normal)
 (charterm-display ".")
 (charterm-cursor 1 1)
 (charterm-display "Press a key...")
 (let ((key (charterm-read-key)))
   (charterm-cursor 1 1)
   (charterm-clear-line)
   (printf "You pressed: ~S\r\n" key)))
