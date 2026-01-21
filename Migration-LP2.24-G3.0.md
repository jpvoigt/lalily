# Migration zu LilyPond 2.24 und Guile 3.0

Dieses Dokument beschreibt die notwendigen Änderungen zur Migration des lalily-Projekts von LilyPond 2.22/Guile 1.8 zu LilyPond 2.24/Guile 3.0.

## 1. Guile-spezifische Änderungen

### 1.1 `assoc-set!` und `assoc-remove!` (KRITISCH)

**Problem:** Diese destruktiven Assoziationslisten-Funktionen wurden in Guile 2.0+ entfernt.

**Betroffene Dateien:**
- [lalily/lalily/laly.scm](lalily/lalily/laly.scm#L28)
- [lalily/lalily/lascm.scm](lalily/lalily/lascm.scm#L64)
- [lalily/lalily/store.scm](lalily/lalily/store.scm#L353)
- [lalily/lalily/store-init.scm](lalily/lalily/store-init.scm#L176)
- [lalily/lalily/markup.scm](lalily/lalily/markup.scm#L30)
- [lalily/lalily/persons.scm](lalily/lalily/persons.scm#L51)
- [lalily/lalily/lascm-init.scm](lalily/lalily/lascm-init.scm#L29)

**Lösung:** Ersetzen durch nicht-destruktive Varianten:
```scheme
;; Alt (Guile 1.8):
(assoc-set! alist key value)
(assoc-remove! alist key)

;; Neu (Guile 3.0):
(assoc-set alist key value)
(assoc-remove alist key)
```

**Hinweis:** Da die neuen Funktionen nicht destruktiv sind, muss der Rückgabewert immer zugewiesen werden:
```scheme
;; Alt:
(assoc-set! alist key value)

;; Neu:
(set! alist (assoc-set alist key value))
```

### 1.2 `dotted-list?` Prädikat

**Problem:** In Guile 3.0 nicht mehr verfügbar.

**Betroffene Datei:**
- [lalily/lalily/lascm.scm](lalily/lalily/lascm.scm#L50)

**Lösung:** Eigene Implementierung hinzufügen (in lascm.scm):
```scheme
(define-public (dotted-list? lst)
  "Check if lst is a dotted list (improper list)"
  (and (pair? lst)
       (not (list? lst))))
```

## 2. LilyPond Parser-API Änderungen (KRITISCH)

### 2.1 Parser-Funktionen entfernt

**Problem:** `ly:parser-define!`, `ly:parser-lookup`, `ly:parser-clone` wurden in LilyPond 2.20+ entfernt.

**Betroffene Dateien:**
- [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm#L66) (`ly:parser-define!`, `ly:parser-lookup`)
- [lalily/lalily/laly.scm](lalily/lalily/laly.scm#L184) (`ly:parser-define!`, `ly:parser-lookup`)
- [lalily/lalily/store-init.scm](lalily/lalily/store-init.scm#L285)
- [lalily/lalily/edition-init.scm](lalily/lalily/edition-init.scm#L40)

**Betroffene Funktionen:**
- `parserDefine` in laly-init.scm
- `clralist`, `setalist`, `addalist`, `remalist` in laly.scm
- Verschiedene Template-Registrierungs-Funktionen

**Lösung:** Verwenden des neuen Parser-losen Ansatzes:

#### Option 1: Modul-basierte Speicherung
```scheme
;; Statt ly:parser-define!:
(define-public variable-name value)

;; Statt ly:parser-lookup:
;; Direkter Zugriff auf die definierte Variable
```

#### Option 2: Registry-basierte Speicherung (empfohlen für lalily)
```scheme
;; Statt:
(ly:parser-define! name val)

;; Verwenden:
(set-registry-val (list 'lalily 'parser 'defs name) val)

;; Statt:
(ly:parser-lookup name)

;; Verwenden:
(get-registry-val (list 'lalily 'parser 'defs name))
```

#### Option 3: Hash-Table basiert
```scheme
;; Globale Hash-Table für Parser-Definitionen
(define-public lalily:parser-defs (make-hash-table))

;; Statt ly:parser-define!:
(hash-set! lalily:parser-defs name val)

;; Statt ly:parser-lookup:
(hash-ref lalily:parser-defs name)
```

### 2.2 `ly:parser-output-name` entfernt

**Problem:** Diese Funktion ist nicht mehr verfügbar.

**Betroffene Dateien:**
- [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm#L133) (`includeLocal`, `executeLocal`)

**Lösung:** Alternative Ansätze:
- Verwenden von `(*location*)` für Dateinamen-Extraktion
- Explizite Parameter statt automatischer Erkennung

## 3. Module und Header Management

### 3.1 `make-module` und `module-define!`

**Problem:** Module-API hat sich geändert, Header werden nicht mehr als Module behandelt.

**Betroffene Datei:**
- [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm#L160) (`set-book-headers!`, `set-score-headers!`)

**Lösung:** Verwendung der neuen Header-API:
```scheme
;; Für Book-Header:
(define-public (set-book-headers! book header)
  (let ((bookhead (ly:book-header book)))
    (if (not bookhead)
        (set! bookhead '()))
    (if (not (list? header))
        (set! header (assoc-get 'header (get-music-folder-options) '())))
    (for-each (lambda (p)
                (if (pair? p)
                    (set! bookhead (assoc-set bookhead (car p) (cdr p)))))
              header)
    (ly:book-set-header! book bookhead)))

;; Für Score-Header analog
```

## 4. LilyPond Versionsnummern

**Problem:** Alle `.ly` Dateien verwenden veraltete Version 2.19.x

**Betroffene Dateien:**
- [lalily.ly](lalily.ly#L18)
- Alle Beispieldateien in `examples/`:
  - [01_templates-satb.ly](examples/01_templates-satb.ly#L18)
  - [02_templates-satb.ly](examples/02_templates-satb.ly#L18)
  - [03_editionEngraver.ly](examples/03_editionEngraver.ly#L18)
  - [04_template-options.ly](examples/04_template-options.ly#L18)
  - [05_annotations.ly](examples/05_annotations.ly#L18)
  - [06_use-case-SATB.ly](examples/06_use-case-SATB.ly#L18)
  - [07_use-case-include-music.ly](examples/07_use-case-include-music.ly#L18)
  - [Dowland-ComeAgain.ly](examples/Dowland-ComeAgain.ly#L1)
  - [Dowland-UnquietThoughts.ly](examples/Dowland-UnquietThoughts.ly#L1)
  - [JSB-Psalm117.ly](examples/JSB-Psalm117.ly#L1)
- Template-Dateien in `lalily/`:
  - [templates-base.ly](lalily/templates-base.ly#L18)
  - [templates-piano.ly](lalily/templates-piano.ly#L1)
  - [paper.lalily-default.ly](lalily/paper.lalily-default.ly#L18)
- Extension-Dateien in `lalily/extensions/`

**Lösung:** Version aktualisieren:
```lilypond
\version "2.24.0"
```

**Automatisierung möglich:**
```bash
find . -name "*.ly" -exec sed -i 's/\\version "2\.19\.[0-9]*"/\\version "2.24.0"/g' {} \;
find . -name "*.ly" -exec sed -i 's/\\version "2\.20\.[0-9]*"/\\version "2.24.0"/g' {} \;
```

## 5. `ly:make-moment` API-Änderung

**Problem:** Die Signatur von `ly:make-moment` hat sich möglicherweise geändert.

**Betroffene Dateien:**
- [lalily/lalily/laly-init.scm](lalily/lalily/laly-init.scm#L197)
- [lalily/lalily/store.scm](lalily/lalily/store.scm#L135)
- [lalily/lalily/edition.scm](lalily/lalily/edition.scm#L629)

**Alte Syntax (2.22):**
```scheme
(ly:make-moment numerator denominator grace-num grace-den)
```

**Neue Syntax (2.24):**
```scheme
;; Normale Momente (ohne Grace):
(ly:make-moment numerator denominator)

;; Mit Grace-Komponenten:
(ly:make-moment numerator denominator grace-num grace-den)
```

**Aktion:** Alle Verwendungen überprüfen und anpassen, wo nötig. Insbesondere bei 4-Parameter-Aufrufen mit `0 1` für Grace-Komponenten können diese weggelassen werden.

## 6. String-Encoding Änderungen

**Problem:** Guile 3.0 verwendet UTF-8 standardmäßig, während Guile 1.8 Latin-1 verwendete.

**Mögliche Auswirkungen:**
- Dateien mit Nicht-ASCII-Zeichen (Umlaute, Sonderzeichen)
- String-Längenberechnungen
- Byte-Position vs. Character-Position

**Lösung:** 
- Sicherstellen, dass alle Quelldateien als UTF-8 kodiert sind
- String-Operationen überprüfen, die Byte-Positionen verwenden

**Überprüfung:**
```bash
file -I *.scm
```

**Konvertierung falls nötig:**
```bash
iconv -f ISO-8859-1 -t UTF-8 file.scm > file_utf8.scm
```

## 7. Zusätzliche potenzielle Probleme

### 7.1 GOOPS (Guile Object System)

**Betroffene Datei:**
- [lalily/lalily/edition.scm](lalily/lalily/edition.scm#L22) (verwendet GOOPS intensiv)

**Änderungen in Guile 3.0:**
- GOOPS-API ist größtenteils kompatibel
- Einige deprecated Features wurden entfernt
- Performance-Verbesserungen

**Aktion:** 
- Code-Review der GOOPS-Verwendung
- Insbesondere `define-class`, `define-method`, Accessor/Setter überprüfen
- Testen aller Klassen und Methoden

### 7.2 Regex-Modul

**Verwendung:**
- [lalily/lalily/laly.scm](lalily/lalily/laly.scm#L20) (`ice-9 regex`)
- [lalily/lalily/lascm.scm](lalily/lalily/lascm.scm#L20)

**Hinweis:** 
- `(ice-9 regex)` ist weiterhin verfügbar
- API sollte kompatibel sein
- Edge cases und Unicode-Handling überprüfen

### 7.3 Deprecated Features

**Zu überprüfende Funktionen:**
- `call-with-input-string`, `call-with-output-string` (sollten OK sein)
- `with-input-from-file`, `with-output-to-file` (sollten OK sein)
- Alle `(@@ (lily) ...)` Zugriffe auf interne Funktionen

## 8. Empfohlene Migrations-Reihenfolge

### Phase 1 - Kritische Änderungen (2-3 Stunden)
1. **Alle `assoc-set!` → `assoc-set` ersetzen**
   - Suchen: `assoc-set!`
   - Ersetzen: `(set! <var> (assoc-set <var> ...))` statt `(assoc-set! <var> ...)`
   
2. **Alle `assoc-remove!` → `assoc-remove` ersetzen**
   - Analog zu assoc-set!

3. **`dotted-list?` Hilfsfunktion implementieren**
   - In lascm.scm vor der ersten Verwendung hinzufügen

### Phase 2 - Parser-API Refactoring (8-12 Stunden)
1. **Registry-basierte Speicherung implementieren**
   - Wrapper-Funktionen für Parser-Zugriffe erstellen
   - Schrittweise Migration

2. **Betroffene Funktionen anpassen:**
   - `parserDefine` in laly-init.scm
   - `clralist`, `setalist`, `addalist`, `remalist` in laly.scm
   - Template-Registrierung in store-init.scm
   - Edition-Funktionen in edition-init.scm

3. **`ly:parser-output-name` Ersatz**
   - Alternative Implementierung für `includeLocal` und `executeLocal`

### Phase 3 - Version Updates (1 Stunde)
1. **Alle `\version` Statements aktualisieren**
   - Automatisiert mit sed/awk
   - Manuelle Überprüfung

2. **`ly:make-moment` Aufrufe überprüfen**
   - 4-Parameter-Aufrufe mit `0 1` auf 2-Parameter reduzieren

### Phase 4 - Header Management (2 Stunden)
1. **`set-book-headers!` anpassen**
2. **`set-score-headers!` anpassen**
3. **Alle Header-Verwendungen testen**

### Phase 5 - Testing (4-8 Stunden)
1. **Unit-Tests (falls vorhanden)**
2. **Beispieldateien durchlaufen:**
   - examples/01_templates-satb.ly
   - examples/06_use-case-SATB.ly
   - examples/03_editionEngraver.ly
3. **Edge cases testen**
4. **Performance-Checks**

## 9. Testempfehlungen

### Minimale Tests
Diese Beispieldateien sollten nach der Migration funktionieren:
- [examples/01_templates-satb.ly](examples/01_templates-satb.ly) - Basis-Templates
- [examples/06_use-case-SATB.ly](examples/06_use-case-SATB.ly) - Realer Use-Case
- [examples/03_editionEngraver.ly](examples/03_editionEngraver.ly) - Edition-Engraver

### Umfassende Tests
- Alle Beispieldateien kompilieren
- MIDI-Output überprüfen
- PDF-Output visuell vergleichen
- Performance-Messungen (Kompilierungszeit)

### Regressionstests
1. **Vor Migration:** Alle Beispiele kompilieren und Output speichern
2. **Nach Migration:** Neu kompilieren und Output vergleichen
3. **Diff-Tool:** `diff -r old_output/ new_output/`

## 10. Geschätzter Aufwand

| Phase | Beschreibung | Zeit |
|-------|--------------|------|
| Phase 1 | Kritische Änderungen (assoc-set!, dotted-list?) | 2-3 h |
| Phase 2 | Parser-API Refactoring | 8-12 h |
| Phase 3 | Version Updates, ly:make-moment | 1-2 h |
| Phase 4 | Header Management | 2-3 h |
| Phase 5 | Testing und Debugging | 4-8 h |
| **Gesamt** | | **17-28 h** |

## 11. Backup und Rollback-Strategie

### Vor Beginn der Migration:
```bash
# Git Branch erstellen
git checkout -b migration-lilypond-2.24-guile-3.0

# Oder komplette Backup-Kopie
cp -r lalily lalily-backup-2022-$(date +%Y%m%d)
```

### Schrittweises Vorgehen:
- Jede Phase als separater Commit
- Aussagekräftige Commit-Messages
- Regelmäßiges Testen

### Bei Problemen:
```bash
# Zurück zum letzten funktionierenden Stand
git reset --hard HEAD~1

# Oder zurück zum Ausgangspunkt
git checkout main
```

## 12. Nützliche Ressourcen

- [LilyPond 2.24 Änderungslog](http://lilypond.org/doc/v2.24/Documentation/changes/)
- [Guile 3.0 Migration Guide](https://www.gnu.org/software/guile/manual/html_node/Guile-3-Migration-Guide.html)
- [LilyPond Parser API Changes](http://lilypond.org/doc/v2.20/Documentation/contributor/scheme-compatibility)

## 13. Bekannte Breaking Changes Zusammenfassung

### Guile 1.8 → 3.0
- ❌ `assoc-set!`, `assoc-remove!` entfernt → `assoc-set`, `assoc-remove`
- ❌ `dotted-list?` entfernt → eigene Implementierung
- ⚠️ String-Encoding: Latin-1 → UTF-8
- ⚠️ Module-System: Änderungen in der Syntax
- ⚠️ Performance: Generell schneller, aber anders optimiert

### LilyPond 2.22 → 2.24
- ❌ `ly:parser-define!` entfernt → Modul/Registry-basiert
- ❌ `ly:parser-lookup` entfernt → Modul/Registry-basiert
- ❌ `ly:parser-clone` entfernt
- ❌ `ly:parser-output-name` entfernt
- ⚠️ `ly:make-moment` Signatur überprüfen
- ⚠️ Header-Management geändert
- ⚠️ Context-Properties erweitert

---

**Erstellt:** 21. Januar 2026  
**Projekt:** lalily - LilyPond Extension  
**Ziel:** Migration von LilyPond 2.22/Guile 1.8 zu LilyPond 2.24/Guile 3.0
