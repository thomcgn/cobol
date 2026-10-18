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
          88 Kein-Fehler                 VALUE 0.
          88 Keine-Zahl                  VALUE 1.
          88 Betrag-Nicht-Positiv        VALUE 2.
          88 Deckung-Fehlt               VALUE 3.
          88 Zu-viele-Nachkommastellen   VALUE 4.
          88 Zahlenüberschreitung        VALUE 5.

       01 Einzahlung PIC S9(9)V99 COMP-3 VALUE 250.00.
       01 Auszahlung PIC S9(9)V99 COMP-3 VALUE 1800.00.
       01 Eingabe PIC X(30)              VALUE SPACES.
       01 Eingabe-Laenge PIC 9(2)        VALUE ZEROS.
       01 Position-Index PIC 9(2)        VALUE ZEROS.
       01 Dezimal-Position PIC 9(2)      VALUE ZEROS.
       01 Nachkommastellen PIC 9(2)      VALUE ZEROS.
       78 MAX-BETRAG                     VALUE 999999999.99.

       PROCEDURE DIVISION.
           PERFORM EINZAHLUNG-BUCHEN.
           PERFORM AUSZAHLUNG-BUCHEN.
           STOP RUN.
       
       
       
       EINZAHLUNG-BUCHEN.
          
           DISPLAY "Einzahlungsbetrag eingeben:"
           ACCEPT Eingabe
           PERFORM BETRAG-PRUEFEN

           IF Kein-Fehler
               MOVE FUNCTION NUMVAL(Eingabe) TO Einzahlung
               ADD Einzahlung TO Kontostand
               ON SIZE ERROR
                   SET Zahlenüberschreitung TO TRUE 
               END-ADD    
           END-IF

           IF Kein-Fehler
               DISPLAY "Neuer Kontostand: "
                   Kontostand
           ELSE
               PERFORM FEHLER-BEHANDELN
           END-IF.


       AUSZAHLUNG-BUCHEN.
           
           DISPLAY "Bitte geben Sie den Auszahlungsbetrag ein: ".
           ACCEPT Eingabe.
           PERFORM BETRAG-PRUEFEN.
           
            IF Kein-Fehler THEN
              MOVE FUNCTION NUMVAL(Eingabe) TO Auszahlung
              IF Kontostand >= Auszahlung THEN
                  SUBTRACT Auszahlung FROM Kontostand
                  ON SIZE ERROR
                      SET Zahlenüberschreitung TO TRUE
                  END-SUBTRACT
              ELSE
                  SET Deckung-Fehlt TO TRUE
              END-IF
              END-IF
             
              IF Kein-Fehler THEN
                   DISPLAY "Neuer Kontostand: " Kontostand
              ELSE
                   PERFORM FEHLER-BEHANDELN
              END-IF.

       BETRAG-PRUEFEN.
           SET Kein-Fehler TO TRUE
           MOVE ZERO TO Dezimal-Position
                        Nachkommastellen

           MOVE FUNCTION TRIM(Eingabe) TO Eingabe

           COMPUTE Eingabe-Laenge =
               FUNCTION LENGTH(FUNCTION TRIM(Eingabe))

           PERFORM VARYING Position-Index FROM 1 BY 1
               UNTIL Position-Index > Eingabe-Laenge
               IF Eingabe(Position-Index:1) = "."
                   MOVE Position-Index TO Dezimal-Position
               END-IF
           END-PERFORM

           IF Dezimal-Position > 0
               COMPUTE Nachkommastellen =
                   Eingabe-Laenge - Dezimal-Position
           END-IF

           EVALUATE TRUE
               WHEN FUNCTION TEST-NUMVAL(Eingabe) NOT = 0
                   SET Keine-Zahl TO TRUE
               WHEN Nachkommastellen > 2
                   SET Zu-viele-Nachkommastellen TO TRUE
               WHEN FUNCTION NUMVAL(Eingabe) <= 0
                   SET Betrag-Nicht-Positiv TO TRUE
               WHEN FUNCTION NUMVAL(Eingabe) > MAX-BETRAG
                   SET Zahlenüberschreitung TO TRUE
           END-EVALUATE.

       FEHLER-BEHANDELN.
           EVALUATE TRUE
               WHEN Keine-Zahl
                   DISPLAY "Fehler: Keine gültige Zahl."
               WHEN Betrag-Nicht-Positiv
                   DISPLAY "Fehler: Der Betrag muss positiv sein."
               WHEN Deckung-Fehlt
                   DISPLAY "Fehler: Unzureichende Kontodeckung."
                WHEN Zu-viele-Nachkommastellen
                   DISPLAY "Fehler: Maximal 2 Nachkommastellen erlaubt."
                WHEN Zahlenüberschreitung
                   DISPLAY  "Fehler: Zahlenüberschreitung."
                            "Der Betrag ist zu groß."
                WHEN OTHER
                   DISPLAY "Unbekannter Fehler."
           END-EVALUATE.
