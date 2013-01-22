lalily
======

lalily is a framework and a collection of utilities, to extend lilypond.
To use it, the "master" lalily.ly has to be included.

folders and files
-----------------

* lalily.ly - is the file to include the lalily framework
* lalily.ily - is included conditionally (once) by lalily.ly to run initialization once
* lalily.log - is created, if lalily.ly is compiled directly (not included). This MAY be done, to check, if alle extensions and templates compile.
* lalily/ - is the folder, which contains the actual framework
* lalily-extensions/ - is a folder, which MAY be created. All files in this folder of a name ending with ".ly" are included by lalily. If a file config.scm is found, it is loaded.
* in examples are a few examples using lalily
