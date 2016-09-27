# jstools

This is not a reall public project but please feel happy to take whatever of the code you like. It's just a collection of
functions and classes, that I did find handy again and again. If anybody likes the functionality of any of these, let me 
know if they should be contributed to some bigger projects and taken out of my hacky little library.

The packages pretty much explain what areas the functionality belongs to. Here are the highlights (if any):

* XmlMatchers allows to match Xml in [ScalaTest](scalatest.org) and get some useful output
* CSVFile wraps Opencsv reader to be a little more functional
* files implements a simple recursive find function for files
* xml implements a transformer class that can be used to manipulate XML
* Table implements a basic table and allows to export it into HMTL and filter it
* CollectionTools just implement an implicit function for contains that checks if a tuple is contained
in a Map

As you can see it is a wild mix of stuff. 

_Note_: This library references other libraries and might therefore come with license strings attached. The author does not grant any use and refer to this library or any dependent library.
