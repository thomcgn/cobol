       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DEMOBANK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Konto.
           05 Kontonummer PIC 9(10) VALUE ZEROS.
           05 Kontoinhaber PIC X(30) VALUE SPACES.
           05 Kontostand PIC S9(9)V99 COMP-3 VALUE 1250.00.
           05 Kontostatus PIC X(1) VALUE "A".
       01 Einzahlung PIC S9(9)V99 COMP-3 VALUE 250.00.
       01 Auszahlung PIC S9(9)V99 COMP-3 VALUE 1800.00.
       01 Eingabe PIC X(30) VALUE SPACES.
       01 Eingabe-Laenge PIC 9(2) VALUE ZEROS.
       PROCEDURE DIVISION.
           PERFORM EINZAHLUNG-BUCHEN.
           PERFORM AUSZAHLUNG-BUCHEN.
           STOP RUN.
       
       
       EINZAHLUNG-BUCHEN.
           DISPLAY "Bitte geben Sie den Einzahlungsbetrag ein: ".
           ACCEPT Eingabe.
           COMPUTE Eingabe-Laenge = FUNCTION LENGTH(
           FUNCTION TRIM(Eingabe)).
           DISPLAY "Eingabelänge: " Eingabe-Laenge

           IF FUNCTION TEST-NUMVAL(Eingabe) = 0 THEN
               IF FUNCTION NUMVAL(Eingabe) >0 THEN
                    MOVE FUNCTION NUMVAL(Eingabe) TO Einzahlung
                    ADD Einzahlung TO Kontostand
                    DISPLAY "Neuer Kontostand nach Einzahlung: "
                    Kontostand
               ELSE
                   DISPLAY "Fehler: Ungültiger Betrag."
               END-IF
           ELSE
               DISPLAY "Fehler: Ungültiger Betrag."
           END-IF.

       AUSZAHLUNG-BUCHEN.
           DISPLAY "Bitte geben Sie den Auszahlungsbetrag ein: ".
           ACCEPT Eingabe.
           IF FUNCTION TEST-NUMVAL(Eingabe) = 0 THEN
               IF FUNCTION NUMVAL(Eingabe) >0 THEN
               IF FUNCTION NUMVAL(Eingabe) > Kontostand THEN
                   DISPLAY "Fehler: Kontodeckung nicht ausreichend."
               ELSE
                   MOVE FUNCTION NUMVAL(Eingabe) TO Auszahlung
                   SUBTRACT Auszahlung FROM Kontostand
                   DISPLAY "Neuer Kontostand: " Kontostand
               END-IF
              ELSE
                     DISPLAY "Fehler: Ungültiger Betrag."
                END-IF
            ELSE
               DISPLAY "Fehler: Ungültiger Betrag."
            END-IF.

         