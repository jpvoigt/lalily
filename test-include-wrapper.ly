\version "2.24.0"
\include "lalily/bootstrap.ily"

% Diese Datei bindet test-lalilyTest.ly ein
% Der \lalilyTest Befehl dort sollte NICHT ausgeführt werden
\include "test-lalilyTest.ly"

% Hier ein anderer Test
\putMusic other { g'1 a' b' c'' }
