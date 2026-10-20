       01 Konto.
           05 Kontonummer PIC 9(10) VALUE 1.
           05 Kontoinhaber PIC X(30) VALUE "Mara Beispiel".
           05 Kontostand PIC S9(9)V99 COMP-3 VALUE 1250.00.
           05 Kontostatus PIC X VALUE "A".
              88 Aktiv VALUE "A".
              88 Inaktiv VALUE "I".
              88 Gesperrt VALUE "G".
