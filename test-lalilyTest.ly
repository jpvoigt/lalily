\version "2.24.0"
\include "lalily/bootstrap.ily"

% Setze eine einfache Test-Musik
\putMusic global { \time 4/4 s1*4 \bar "|." }
\putMusic sop { c'1 d' e' f' }
\putMusic alt { a1 b c' d' }
\putMusic ten { f1 g a b }
\putMusic bas { c1 d e f }

% Setze Template
\setTemplate lalily.vocal.satb

% lalilyTest sollte nur hier ausgeführt werden (Hauptdatei)
% nicht wenn diese Datei per \include eingebunden wird
\lalilyTest
