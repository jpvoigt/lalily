# Migration zu LilyPond 2.24 und Guile 3.0

Dieses Dokument beschreibt die durchgeführten Änderungen zur Migration des lalily-Projekts von LilyPond 2.19.x/Guile 1.8 zu LilyPond 2.24/Guile 3.0.

## Status: ✅ Migration durchgeführt

---

## 1. Guile-spezifische Änderungen

### 1.1 `assoc-set!` und `assoc-remove!`

**Ergebnis:** ✅ **Keine Änderung nötig** – `assoc-set!` und `assoc-remove!` sind in Guile 3.0 weiterhin verfügbar. Die ursprüngliche Annahme, dass sie entfernt wurden, war falsch.

### 1.2 `dotted-list?` Prädikat

**Ergebnis:** ✅ **Keine Änderung nötig** – `dotted-list?` ist über `(srfi srfi-1)` in Guile 3.0 weiterhin verfügbar.

---

## 2. LilyPond API-Änderungen

### 2.1 Parser/Location implizit in Music-Functions

**Problem:** In LilyPond 2.24 erhalten `define-music-function`, `define-scheme-function`, `define-void-function` etc. `parser` und `location` nicht mehr als explizite Argumente. Sie sind implizit als `(*parser*)` und `(*location*)` in den Funktionskörpern verfügbar.

**Durchgeführte Änderungen:**

| Datei | Änderung |
|-------|----------|
| [lalily/lalily/lascm-init.scm](lalily/lalily/lascm-init.scm) | `assocGet`, `assocSet`, `assocSetAll`: `(parser location ...)` → `(...)` |
| [lalily/lalily/markup.scm](lalily/lalily/markup.scm) | `registerMarkup`: `(parser location ...)` → `(...)` |
| [lalily/lalily/markup-init.scm](lalily/lalily/markup-init.scm) | `setStyle`: `(parser location ...)` → `(...)` |
| [lalily/lalily/lyrics-init.scm](lalily/lalily/lyrics-init.scm) | `define-lyric-markup`, `lyricSize`, `lyricStyle`, `lyricScale`, `addExtMup`, `addLEx`: `(parser location ...)` → `(...)` |
| [lalily/templates-tools.ly](lalily/templates-tools.ly) | Alle 8 `define-*-function`: `(parser location ...)` → `(...)` |
| [lalily-extensions/shapeTieCol.ly](lalily-extensions/shapeTieCol.ly) | `shapeTieColumn`: `(parser location all-offsets)` → `(all-offsets)` |

**Wichtig:** In `lyrics-init.scm` wurde auch die Aufrufkonvention repariert: Extrahierte Funktionen via `ly:music-function-extract` erhalten in 2.24 nur die deklarierten Argumente, nicht `parser`/`location`. D.h. `(lsf parser location lyrics)` → `(lsf lyrics)`.

### 2.2 `ly:parser-output-name` Signaturänderung

**Problem:** `ly:parser-output-name` nimmt in 2.24 kein Argument mehr (optionaler Parser entfällt).

**Durchgeführte Änderungen:**

| Datei | Funktion |
|-------|----------|
| [lalily/lalily/laly.scm](lalily/lalily/laly.scm) | `lalily-test-location?` |
| [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm) | `includeLocal`, `executeLocal` |
| [lalily/lalily/store-init.scm](lalily/lalily/store-init.scm) | `write-lalily-log-file` |
| [lalily/lalily/edition.scm](lalily/lalily/edition.scm) | `finalize`-Callback, `annoCollect` |

Alle Aufrufe: `(ly:parser-output-name parser)` → `(ly:parser-output-name)`

### 2.3 `ly:parser-define!` und `ly:parser-lookup`

**Ergebnis:** ✅ **Keine Änderung nötig** – Diese Funktionen existieren in LilyPond 2.24 weiterhin (ohne Parser-Argument). Die ursprüngliche Annahme, dass sie entfernt wurden, war falsch.

### 2.4 Bare `location`-Referenzen in Funktionskörpern

**Problem:** Da `parser` und `location` nicht mehr als Parameter übergeben werden, sind nackte `location`-Referenzen in Funktionskörpern ungebunden.

**Durchgeführte Änderungen:**

| Datei | Änderung |
|-------|----------|
| [lalily/lalily/laly.scm](lalily/lalily/laly.scm) | `includeFolder`: `location` → `(*location*)` |
| [lalily/lalily/laly.scm](lalily/lalily/laly.scm) | `lalily-markup`: `location` → `#f` (kein Location-Kontext verfügbar) |
| [lalily/lalily/persons-init.scm](lalily/lalily/persons-init.scm) | `getPersonName/Life`: `location` → `(*location*)` |
| [lalily/lalily/persons-init.scm](lalily/lalily/persons-init.scm) | `personName/Life` (Markup-Commands): `ly:input-warning location` → `ly:warning` |
| [lalily/lalily/persons-init.scm](lalily/lalily/persons-init.scm) | `set-person!` Lambda: `location` → `(*location*)` |
| [lalily/templates-tools.ly](lalily/templates-tools.ly) | 3× `location` → `(*location*)` |

---

## 3. Override-Syntax-Änderungen

**Problem:** Die alte Override-Syntax `\override Grob #'property = value` wurde in LilyPond 2.22+ entfernt. Neue Syntax: `\override Grob.property = value`.

**Durchgeführte Änderungen:**

| Datei | Änderung |
|-------|----------|
| [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm) | `mergeRestsOn/Off/mergeRests`: Override/Revert-Syntax aktualisiert |
| [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm) | `markFerm`, `markDaX`: Override-Syntax aktualisiert |
| [lalily/lalily/store-init.scm](lalily/lalily/store-init.scm) | `cueMusic`: `InstrumentSwitch #'direction` → `InstrumentSwitch.direction` etc. |
| [lalily/lalily/lyrics-init.scm](lalily/lalily/lyrics-init.scm) | `addExtMup/addLEx`: `LyricExtender #'stencil` → `LyricExtender.stencil` |

**Hinweis:** `\override #'(property . value)` in **Markup-Kontext** ist weiterhin korrekt und wurde NICHT geändert.

---

## 4. `ly:make-moment` Signaturänderung

**Problem:** 4-Argument-Aufrufe mit redundantem Grace-Anteil `0 1` können auf 2 Argumente reduziert werden.

**Durchgeführte Änderungen:**

| Datei | Änderung |
|-------|----------|
| [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm) | `midiTempo`: `(ly:make-moment num den 0 1)` → `(ly:make-moment num den)` |
| [lalily-extensions/extractMusic.ly](lalily-extensions/extractMusic.ly) | `(ly:make-moment 0/1 0/1)` → `(ly:make-moment 0)` |
| [lalily-extensions/edition-helper.ly](lalily-extensions/edition-helper.ly) | `(ly:make-moment 0 0)` → `(ly:make-moment 0)` |

---

## 5. Versions-Updates

**Durchgeführt:** Alle `.ly`-Dateien auf `\version "2.24.0"` aktualisiert.

Betroffen waren 31+ Dateien mit Versionen von 2.17.29 bis 2.20.0.

---

## 6. Bug-Fixes (während der Migration entdeckt)

| Datei | Bug | Fix |
|-------|-----|-----|
| [lalily/lalily/store.scm](lalily/lalily/store.scm) | `(> (length head))` – fehlender zweiter Vergleichswert | `(> (length head) 0)` |
| [lalily/bootstrap.ily](lalily/bootstrap.ily) | `la:parser-include-file` mit 3 Argumenten aufgerufen (Funktion akzeptiert nur 2) | Entfernung des überflüssigen `(*parser*)`-Arguments |
| [lalily/bootstrap.ily](lalily/bootstrap.ily) | `do-layout` mit `parser`-Argument aufgerufen | `parser`-Parameter entfernt |

---

## 7. Nicht geänderte Dateien und Begründung

- **edition.scm `oop->string`**: Generiert Display-Strings mit `#'property`-Syntax – nur für Logging, kein funktionaler Code
- **verlag.ly `jpv:published?`/`jpv:doTitle?`**: Plain-Scheme-Funktionen mit explizitem `(parser location)` – werden extern mit `(*parser*) (*location*)` aufgerufen, daher kompatibel
- **laly.scm `lalily-test-location?`**: Behält `(parser location)` Signatur bei, da externer API-Vertrag – Aufrufer übergeben `(*parser*) (*location*)`

---

## 8. Bekannte Risiken

1. **`(defined?)` in Modul-Kontext**: Funktionen wie `lalily-markup` verwenden `(defined? sym)` und `(primitive-eval sym)`. In Guile 3 operiert `defined?` im aktuellen Modul, nicht zwingend in guile-user. Könnte bei Markup-Registrierung zu `#f`-Ergebnissen führen.

2. **`ly:parser-define!`/`ly:parser-lookup` Modul-Scope**: Diese Funktionen arbeiten im lily-user-Modul. Wenn Code aus sub-Modulen (z.B. `(lalily edition)`) aufgerufen wird, könnten Scope-Differenzen auftreten.

3. **GOOPS-Kompatibilität**: Die Edition-Engraver-Klassen wurden nicht geändert. GOOPS ist in Guile 3.0 weitgehend kompatibel, aber Edge-Cases sind möglich.

---

## 9. Test-Empfehlungen

Nach der Migration sollten folgende Dateien getestet werden:
- `test-lalily-simple.ly` – Basis-Funktionalität
- `test-lalilyTest.ly` – `lalilyTest`-Funktion
- `examples/01_templates-satb.ly` – SATB-Templates
- `examples/03_editionEngraver.ly` – Edition-Engraver
- `examples/06_use-case-SATB.ly` – Realer Use-Case

---

**Erstellt:** 21. Januar 2026
**Migration durchgeführt:** Januar 2026
**Projekt:** lalily - LilyPond Extension
**Ziel:** Migration von LilyPond 2.19.x/Guile 1.8 zu LilyPond 2.24/Guile 3.0
