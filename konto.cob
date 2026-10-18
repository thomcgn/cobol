       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DEMOBANK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Konto.
           05 Kontonummer PIC 9(10) VALUE ZEROS.
           05 Kontoinhaber PIC X(30) VALUE SPACES.
           05 Kontostand PIC S9(9)V99 COMP-3 VALUE 1250.00.
           05 Kontostatus PIC X(1) VALUE "A".
       
       
       01 Fehlercode PIC 9 VALUE 0.
          88 Kein-Fehler          VALUE 0.
          88 Keine-Zahl           VALUE 1.
          88 Betrag-Nicht-Positiv VALUE 2.
          88 Deckung-Fehlt        VALUE 3.

       01 Einzahlung PIC S9(9)V99 COMP-3 VALUE 250.00.
       01 Auszahlung PIC S9(9)V99 COMP-3 VALUE 1800.00.
       01 Eingabe PIC X(30) VALUE SPACES.
       01 Eingabe-Laenge PIC 9(2) VALUE ZEROS.

       PROCEDURE DIVISION.
           PERFORM EINZAHLUNG-BUCHEN.
           PERFORM AUSZAHLUNG-BUCHEN.
           STOP RUN.
       
       
       
       EINZAHLUNG-BUCHEN.
           SET Kein-Fehler TO TRUE

           DISPLAY "Einzahlungsbetrag eingeben:"
           ACCEPT Eingabe

           COMPUTE Eingabe-Laenge =
               FUNCTION LENGTH(FUNCTION TRIM(Eingabe))

           DISPLAY "Eingabelaenge: " Eingabe-Laenge

           IF FUNCTION TEST-NUMVAL(Eingabe) NOT = 0
               SET Keine-Zahl TO TRUE
           ELSE
               IF FUNCTION NUMVAL(Eingabe) > 0
                   MOVE FUNCTION NUMVAL(Eingabe)
                       TO Einzahlung
                   ADD Einzahlung TO Kontostand
               ELSE
                   SET Betrag-Nicht-Positiv TO TRUE
               END-IF
           END-IF

           IF Kein-Fehler
               DISPLAY "Neuer Kontostand: "
                   Kontostand
           ELSE
               PERFORM FEHLER-BEHANDELN
           END-IF.


       AUSZAHLUNG-BUCHEN.
           SET Kein-Fehler TO TRUE

           DISPLAY "Bitte geben Sie den Auszahlungsbetrag ein: ".
           ACCEPT Eingabe.
           
              IF FUNCTION TEST-NUMVAL(Eingabe) NOT = 0 THEN
                 SET Keine-Zahl TO TRUE
              ELSE
                 IF FUNCTION NUMVAL(Eingabe) > 0 THEN
                     MOVE FUNCTION NUMVAL(Eingabe) TO Auszahlung
                     IF Kontostand >= Auszahlung THEN
                          SUBTRACT Auszahlung FROM Kontostand
                     ELSE
                          SET Deckung-Fehlt TO TRUE
                     END-IF
                 ELSE
                     SET Betrag-Nicht-Positiv TO TRUE
                 END-IF
              END-IF
             
              IF Kein-Fehler THEN
                   DISPLAY "Neuer Kontostand: " Kontostand
              ELSE
                   PERFORM FEHLER-BEHANDELN
              END-IF.

       FEHLER-BEHANDELN.
           EVALUATE TRUE
               WHEN Keine-Zahl
                   DISPLAY "Fehler: Keine gültige Zahl."
               WHEN Betrag-Nicht-Positiv
                   DISPLAY "Fehler: Der Betrag muss positiv sein."
               WHEN Deckung-Fehlt
                   DISPLAY "Fehler: Unzureichende Kontodeckung."
                WHEN OTHER
                   DISPLAY "Unbekannter Fehler."
           END-EVALUATE.