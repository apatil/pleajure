Pleajure.clj
By Anand Patil
February 20, 2009


Purpose
=======

Pleajure lets you write Clojure S-expressions using whitespace in place of parentheses. The resulting look-and-feel is a bit like Python.



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
            c ......

translates to ::

    (a ...... (b ...... (c ......)))
