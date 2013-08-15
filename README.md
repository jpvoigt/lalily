lalily
======

lalily is a framework and a collection of utilities, to extend lilypond.
To use it, include 'lalily.ly'.

folders and files
-----------------

* lalily.ly - is the file to include the lalily framework
* lalily.ily - is included conditionally (once) by lalily.ly to run initialization once
* lalily.log - is created, if lalily.ly is compiled directly (not included). 
  This MAY be done, to check, if all extensions and templates actually do compile.
* lalily/ - is the folder, which contains the framework
* lalily-extensions/ - is a folder, which MAY be created in this project folder and/or next to this project folder.
  All files in this folder with a name suffix ".ly" are included by lalily.
  If a file config.scm is found, it is loaded first.
* in examples are a few examples using lalily

the idea
========

templates
---------

When I started to use lilypond to engrave music, I did copy, paste and adapt several parts of my files to
create the score. My first approach to have templates was to include a template file, which makes use of
predefined variables. Since it was mostly all choral music, I define variables 'sopran', 'alt', 'tenor' and 'bass'
then wrote \\include "satb.ily":

     sopran = \relative c'' { c4 }
     alt = \relative c' { c4 }
     tenor = \relative c' { c4 }
     bass = \relative c { c4 }

     soprantext = \lyricmode { la }
     alttext = \lyricmode { le }
     tenortext = \lyricmode { li }
     basstext = \lyricmode { loo }

     \include "satb.ily"

and satb.ily:

    \new ChoirStaff <<
      \new Staff \new Voice = "sop" \sopran
      \new Lyrics \lyricsto "sop" \lyricmode { \soprantext }
      \new Staff \new Voice = "alt" \alt
      \new Lyrics \lyricsto "alt" \lyricmode { \alttext }
      \new Staff { \clef "G_8" \new Voice = "ten" \tenor }
      \new Lyrics \lyricsto "ten" \lyricmode { \tenortext }
      \new Staff { \clef "bass" \new Voice = "bas" \bass }
      \new Lyrics \lyricsto "bas" \lyricmode { \basstext }
    >>

This is quite handy in the first step, but what, if you have two soprano voices or only one mens voice? Do I really want to produce
an include-file for each and every case? To make the long story a little bit shorter, I started to use music-functions.
If you are familiar with lilypond, this won't surprise you. Now I can have one include file defining all functions I need
and just use (for example) \\satb to create the needed ChoirStaff.

While I was getting more familiar with scheme - don't be scared, scheme is not that difficult, you just might not be used to
read it! - I saw, that one can have arbitrary arguments to have one function for SATB, SSAATTBB and different counts of lyric lines.
But then I wanted to have two choirs. Wouldn't it be nice just to call \\satb twice?

lalily-templates
----------------

The result is, that I don't use lily-variables like sopran=... for the music, but have a tree-like structure, where I store my music.
This is accompanioned by two functions \\putMusic and \\getMusic. My template-music-functions now all have the same signature
and are stored with \\registerTemplate and called with \\callTemplate. Music and templates are stored and addressed with a path,
which is in fact a scheme-list. And there is a globally stored current path or music-folder, which these functions refer to.
So if I have a music-folder #'(music choral altatrinita) (in current lily-devel-version it's just music.choral.altatrinita -
I am working on an update) and make a call \\putMusic #'(sop melody) {...}, it will store the music in music.choral.altatrinita.sop.melody .
A template function is called with \\callTemplate template.path music.path options and the signature of template function is

    #(define-music-function (parser location piece options)(list? list?)
        ; ... return some music here

When the template function is called, the current music path and the current template path are set, and any calls to \\getMusic
or \\callTemplate fetch music and templates relative to the current path. So a satb template might look like this:

    \registerTemplate #'(lalily demo choral satb)
    #(define-music-function (parser location piece options)(list? list?)
       #{
         \new StaffGroup \with {
           % disable SpanBar like in ChoirStaff, but leave the possibility to display it with
           % \once \override StaffGroup.BarLine #'allow-span-bar = ##t
           \override BarLine #'allow-span-bar = ##f
         } <<
           % call template #'(choral staff) relative to this template path
           % with path #'(sop) relative to the current piece/path
           % tith options specifying clef, instrument name and short name
           \callTemplate #'(.. staff) #'(sop) #'((clef . "G")(instrname . "Soprano")(shortname . "S"))
           % do the same for alt, ten and bas
           \callTemplate #'(.. staff) #'(alt) #'((clef . "G")(instrname . "Alto")(shortname . "A"))
           \callTemplate #'(.. staff) #'(ten) #'((clef . "G_8")(instrname . "Tenoro")(shortname . "T"))
           \callTemplate #'(.. staff) #'(bas) #'((clef . "bass")(instrname . "Basso")(shortname . "B"))
         >>
       #})

We have a lalily.demo.choral.satb template which calls lalily.demo.choral.staff:

    \registerTemplate #'(lalily demo choral staff)
    #(define-music-function (parser location piece options)(list? list?)
      (let* ((instrname (ly:assoc-get 'instrname options "Voc?" #f))
             (shortname (ly:assoc-get 'shortname options #f #f))
             ; voicename is instrument name, if not specified
             (voicename (ly:assoc-get 'vocname options instrname #f))
             ; staffname is voicename, if not specified
             (staffname (ly:assoc-get 'staffname options voicename #f))
             (lyricname (ly:assoc-get 'lyricname options staffname #f))
             (clef (ly:assoc-get 'clef options "G" #f)))
       #{
          <<
             \new Staff = $staffname \with {
	        instrumentName = $instrname
                shortInstrumentName = $shortname
	     } \new Voice = $voicename { \clef $clef \getMusic #'(melody) }
             \new Lyrics = $lyricname \lyricsto $voicename \lyricmode { \getMusic #'(lyrics) }
          >>
       #}))

(You can find a running example (for lilypond 2.16) in the examples folder)

And when you have a choral piece under another path, you just change the current music-folder and call the template.

Templates can be bound with dedicated options to a specific path:

    \setDefaultTemplate #'(music choral altatrinita) #'(lalily demo choral satb) #'()

There are some helper functions to call the bound templates with the current path. First there is \\lalilyTest,
which only creates the score, if the output-name in the parser matches the filename in the current location.
So the score is only created, if this file is actually compiled and not just included. This is quite handy to test
a file but don't create output, if the music shall only be stored for use in another context.

Now one might have a big score containing an orchestra and a choir. The choir part is shown here and can be called from
another template, for example:

     \callTemplate #'(/ lalily demo choral satb) #'(choir a) #'()

(if a path starts with '/', it is read as an absolute path)

Internally the music is stored in a singleton tree-structure.

TODO

lalily-editions
---------------

Using the formerly described templates, one can typeset the same music with different templates for different purposes.
Every tweak one might apply, shall only show up with a dedicated output or edition. So there is an editionEngraver,
which is able to apply overrides, sets and breaks to the output. To make it happen, a context has to consist the editionEngraver

    \consists \editionEngraver <a path>

Then you can have an edition with

    \addEdition Partitur

And make changes with

    \editionMod Partitur 2 2/4 <a path> \once \override DynamicText #'extra-offset = #'(-1 . 1)

The dynamic text gets an extra-offset in measure 2 on the 3rd quarter. Addressing the right context
is a little bit clumsy, but if it is integrated into the templates, its more reasonable.
I would call this experimental, but I use it regularly for \\shape and \\break, to tweak the output
without polluting the music-source with tag-once-override and similar constructs.

TODO
