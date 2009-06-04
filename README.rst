:Date: 3 June 2009
:Author: Anand Patil
:Contact: anand.prabhakar.patil@gmail.com
:Copyright: Anand Patil, 2009.
:License: See LICENSE


Purpose
=======

Pleajure lets you write Clojure S-expressions using whitespace in place of parentheses. The resulting look-and-feel is a bit like Python.

It also provides a macro for infix arithmetic, called ``|``. The macro was posted to the Clojure mailing list by Jeff Bester.


Usage example 
=============
::
    
    (load-file "pleajure.clj")

Translate and display::

    (pleajure-print "test.plj")

Translate and load (like load-file)::
    
    (pleajure-load "test.plj")



Translation rule
================
 
There is only one translation rule now. Indented blocks are nested S-expressions. That means::

    a ......
        b ......
            (c ......)

translates to::

    (a ...... 
        (b ...... 
            (c ......)))
