
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMOBANK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 Konto.
           05 Kontonummer PIC 9(10) VALUE ZEROS.
           05 Kontoinhaber PIC X(30) VALUE SPACES.
           05 Kontostand PIC S9(9)V99 COMP-3 VALUE 1250.00.
           05 Kontostatus PIC X(1) VALUE "G".
              88 Aktiv VALUE "A".
              88 Inaktiv VALUE "I".
              88 Gesperrt VALUE "G".

       01 Fehlercode PIC 9 VALUE ZERO.
          88 Kein-Fehler                 VALUE 0.
          88 Keine-Zahl                  VALUE 1.
          88 Betrag-Nicht-Positiv        VALUE 2.
          88 Deckung-Fehlt               VALUE 3.
          88 Zu-viele-Nachkommastellen   VALUE 4.
          88 Zahlenueberschreitung       VALUE 5.
          88 Konto-Nicht-Aktiv             VALUE 6.

       01 Einzahlung PIC S9(9)V99 COMP-3 VALUE ZERO.
       01 Auszahlung PIC S9(9)V99 COMP-3 VALUE ZERO.

       01 Eingabe PIC X(30) VALUE SPACES.
       01 Eingabe-Laenge PIC 9(2) VALUE ZERO.
       01 Position-Index PIC 9(2) VALUE ZERO.
       01 Dezimal-Position PIC 9(2) VALUE ZERO.
       01 Dezimal-Anzahl PIC 9(2) VALUE ZERO.
       01 Nachkommastellen PIC 9(2) VALUE ZERO.

       01 Format-Status PIC X(1) VALUE "G".
          88 Format-Gueltig   VALUE "G".
          88 Format-Ungueltig VALUE "U".

       78 MAX-BETRAG VALUE 999999999.99.

       PROCEDURE DIVISION.
           PERFORM EINZAHLUNG-BUCHEN
           PERFORM AUSZAHLUNG-BUCHEN
           STOP RUN.

       EINZAHLUNG-BUCHEN.
           DISPLAY "Einzahlungsbetrag eingeben:"
           ACCEPT Eingabe

           PERFORM BETRAG-PRUEFEN

           IF Kein-Fehler
               MOVE FUNCTION NUMVAL(Eingabe)
                   TO Einzahlung

               ADD Einzahlung TO Kontostand
                   ON SIZE ERROR
                       SET Zahlenueberschreitung TO TRUE
               END-ADD
           END-IF

           IF Kein-Fehler
               DISPLAY "Neuer Kontostand: " Kontostand
           ELSE
               PERFORM FEHLER-BEHANDELN
           END-IF.

       AUSZAHLUNG-BUCHEN.
           DISPLAY "Auszahlungsbetrag eingeben:"
           ACCEPT Eingabe

           PERFORM BETRAG-PRUEFEN
           IF Kein-Fehler AND NOT Aktiv
               SET Konto-Nicht-Aktiv TO TRUE
           END-IF

           IF Kein-Fehler
               MOVE FUNCTION NUMVAL(Eingabe)
                   TO Auszahlung

               IF Kontostand >= Auszahlung
                   SUBTRACT Auszahlung FROM Kontostand
                       ON SIZE ERROR
                           SET Zahlenueberschreitung TO TRUE
                   END-SUBTRACT
               ELSE
                   SET Deckung-Fehlt TO TRUE
               END-IF
           END-IF

           IF Kein-Fehler
               DISPLAY "Neuer Kontostand: " Kontostand
           ELSE
               PERFORM FEHLER-BEHANDELN
           END-IF.

       BETRAG-PRUEFEN.
           SET Kein-Fehler TO TRUE
           SET Format-Gueltig TO TRUE

           MOVE ZERO TO Dezimal-Position
                        Dezimal-Anzahl
                        Nachkommastellen
                        Eingabe-Laenge

           MOVE FUNCTION TRIM(Eingabe) TO Eingabe

           COMPUTE Eingabe-Laenge =
               FUNCTION LENGTH(FUNCTION TRIM(Eingabe))

           PERFORM VARYING Position-Index FROM 1 BY 1
               UNTIL Position-Index > Eingabe-Laenge

               EVALUATE TRUE
                   WHEN Eingabe(Position-Index:1) = "."
                       ADD 1 TO Dezimal-Anzahl
                       MOVE Position-Index
                           TO Dezimal-Position

                   WHEN Eingabe(Position-Index:1) IS NUMERIC
                       CONTINUE

                   WHEN OTHER
                       SET Format-Ungueltig TO TRUE
               END-EVALUATE
           END-PERFORM

           IF Dezimal-Position > ZERO
               COMPUTE Nachkommastellen =
                   Eingabe-Laenge - Dezimal-Position
           END-IF

           EVALUATE TRUE
               WHEN Eingabe-Laenge = ZERO
                   SET Keine-Zahl TO TRUE

               WHEN Format-Ungueltig
                   SET Keine-Zahl TO TRUE

               WHEN Dezimal-Anzahl > 1
                   SET Keine-Zahl TO TRUE

               WHEN Dezimal-Position = 1
                   SET Keine-Zahl TO TRUE

               WHEN Dezimal-Position = Eingabe-Laenge
                   SET Keine-Zahl TO TRUE

               WHEN Nachkommastellen > 2
                   SET Zu-viele-Nachkommastellen TO TRUE

               WHEN FUNCTION TEST-NUMVAL(Eingabe) NOT = 0
                   SET Keine-Zahl TO TRUE

               WHEN FUNCTION NUMVAL(Eingabe) <= 0
                   SET Betrag-Nicht-Positiv TO TRUE

               WHEN FUNCTION NUMVAL(Eingabe) > MAX-BETRAG
                   SET Zahlenueberschreitung TO TRUE
           END-EVALUATE.

       FEHLER-BEHANDELN.
           EVALUATE TRUE
               WHEN Keine-Zahl
                   DISPLAY "Fehler: Ungueltige Zahl."

               WHEN Betrag-Nicht-Positiv
                   DISPLAY "Fehler: Betrag muss positiv sein."

               WHEN Deckung-Fehlt
                   DISPLAY "Fehler: Kontodeckung reicht nicht."

               WHEN Zu-viele-Nachkommastellen
                   DISPLAY "Fehler: Maximal 2 Nachkommastellen."

               WHEN Zahlenueberschreitung
                   DISPLAY "Fehler: Zahlenueberschreitung."
                   DISPLAY "Betrag oder Kontostand zu gross."
              WHEN Konto-Nicht-Aktiv
                   EVALUATE TRUE
                       WHEN Inaktiv
                           DISPLAY "Fehler: Konto ist inaktiv."
                       WHEN Gesperrt
                           DISPLAY "Fehler: Konto ist gesperrt."
                       WHEN OTHER
                           DISPLAY "Fehler: Ungueltiger Kontostatus."
                   END-EVALUATE
               WHEN OTHER
                   DISPLAY "Fehler: Unbekannter Status."
           END-EVALUATE.
                          