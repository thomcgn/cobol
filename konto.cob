       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMOBANK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Historien-Datei
               ASSIGN TO "/workspace/cobol/buchungen.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS Historien-Dateistatus.
       DATA DIVISION.
       FILE SECTION.

       FD Historien-Datei.
       01 Historien-Zeile.
           05 Export-Nummer PIC 9(3).
           05 Export-Trenner-1 PIC X.
           05 Export-Art PIC X.
           05 Export-Trenner-2 PIC X.
           05 Export-Betrag PIC 9(9).99.
           05 Export-Trenner-3 PIC X.
           05 Export-Saldo PIC +9(9).99.
       WORKING-STORAGE SECTION.

       COPY "ACCOUNT.cpy".
       COPY "BOOKING.cpy".
       01 Historien-Dateistatus PIC XX VALUE SPACES.

       01  Pruefsaldo PIC S9(9)V99 COMP-3 VALUE ZERO.
       01  Neuer-Pruefsaldo PIC S9(9)V99 COMP-3 VALUE ZERO.
       01  Import-Betrag PIC 9(9)V99 COMP-3 VALUE ZERO.
       01  Import-Saldo PIC S9(9)V99 COMP-3 VALUE ZERO.
       01  Import-Status PIC 9 VALUE ZERO.
              88 Import-Gueltig VALUE 0.
              88 Import-Ungueltig VALUE 1.
       01 Erwartete-Buchungsnummer PIC 9(3) COMP VALUE 1.

       01  Lese-Ende PIC 9 VALUE ZERO.
              88 Dateiende VALUE 1.
       01  Export-Gelesen PIC 9 VALUE ZERO.

       01  Fehlercode PIC 9 VALUE ZERO.
           88 Kein-Fehler                 VALUE 0.
           88 Keine-Zahl                  VALUE 1.
           88 Betrag-Nicht-Positiv        VALUE 2.
           88 Deckung-Fehlt               VALUE 3.
           88 Zu-viele-Nachkommastellen   VALUE 4.
           88 Zahlenueberschreitung       VALUE 5.
           88 Konto-Nicht-Aktiv           VALUE 6.
           88 Historie-Voll               VALUE 7.

       01 Einzahlung PIC S9(9)V99 COMP-3 VALUE ZERO.
       01 Auszahlung PIC S9(9)V99 COMP-3 VALUE ZERO.
       01 Historien-Index PIC 9(3) COMP VALUE ZERO.

       01 Eingabe PIC X(30) VALUE SPACES.
       01 Eingabe-Laenge PIC 9(2) VALUE ZERO.
       01 Position-Index PIC 9(2) VALUE ZERO.
       01 Dezimal-Position PIC 9(2) VALUE ZERO.
       01 Dezimal-Anzahl PIC 9(2) VALUE ZERO.
       01 Nachkommastellen PIC 9(2) VALUE ZERO.

       01 Menue-Auswahl PIC X(30) VALUE SPACES.


       01 Format-Status PIC X(1) VALUE "G".
          88 Format-Gueltig   VALUE "G".
          88 Format-Ungueltig VALUE "U".

       78 MAX-BETRAG VALUE 999999999.99.
       78 STARTSALDO VALUE 1250.00.

       PROCEDURE DIVISION.
           
           PERFORM KONTO-ANZEIGEN

           PERFORM UNTIL Menue-Auswahl = "3"
              DISPLAY "1. Einzahlung buchen"
              DISPLAY "2. Auszahlung buchen"
              DISPLAY "3. Beenden"
              DISPLAY "4. Historie anzeigen"
              DISPLAY "5. Letzten Export anzeigen"
              DISPLAY "Bitte Option waehlen (1 - 5):"
              ACCEPT Menue-Auswahl

              EVALUATE Menue-Auswahl
                 WHEN "1"
                    PERFORM EINZAHLUNG-BUCHEN
                 WHEN "2"
                    PERFORM AUSZAHLUNG-BUCHEN
                 WHEN "3"
                    DISPLAY "Programm wird beendet."
                 WHEN "4"
                    PERFORM HISTORIE-ANZEIGEN
                 WHEN "5"
                    PERFORM EXPORT-ANZEIGEN
                 WHEN OTHER
                    DISPLAY "Ungueltige Option."
                    DISPLAY "Bitte waehlen Sie 1 bis 5."
              END-EVALUATE

           END-PERFORM
           DISPLAY "Historieneintraege: " Buchungsanzahl
           DISPLAY "Endsaldo: " Kontostand
           PERFORM HISTORIE-ANZEIGEN
           PERFORM HISTORIE-EXPORTIEREN
           STOP RUN.

       KONTO-ANZEIGEN.
           DISPLAY "Kontonummer: " Kontonummer
           DISPLAY "Kontoinhaber: " Kontoinhaber
           DISPLAY "Kontostand: " Kontostand

           EVALUATE TRUE
               WHEN Aktiv
                   DISPLAY "Kontostatus: Aktiv"
               WHEN Inaktiv
                   DISPLAY "Kontostatus: Inaktiv"
               WHEN Gesperrt
                   DISPLAY "Kontostatus: Gesperrt"
               WHEN OTHER
                   DISPLAY "Kontostatus: Ungueltig"
           END-EVALUATE.

       EINZAHLUNG-BUCHEN.
           SET Kein-Fehler TO TRUE

           IF NOT (Aktiv OR Gesperrt)
               SET Konto-Nicht-Aktiv TO TRUE
               PERFORM FEHLER-BEHANDELN
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Einzahlungsbetrag eingeben:"
           ACCEPT Eingabe

           PERFORM BETRAG-PRUEFEN
           IF Kein-Fehler
               IF Buchungsanzahl >= 100
                   SET Historie-Voll TO TRUE
               END-IF
           END-IF

           IF Kein-Fehler
               MOVE FUNCTION NUMVAL(Eingabe)
                   TO Einzahlung

               ADD Einzahlung TO Kontostand
                   ON SIZE ERROR
                       SET Zahlenueberschreitung TO TRUE
               END-ADD
               IF Kein-Fehler
                   ADD 1 TO Buchungsanzahl
                   MOVE Buchungsanzahl
                       TO Buchungsnummer(Buchungsanzahl)
                   MOVE "E"
                       TO Buchungsart(Buchungsanzahl)
                   MOVE Einzahlung
                       TO Buchungsbetrag(Buchungsanzahl)
                   MOVE Kontostand
                       TO Saldo-Danach(Buchungsanzahl)
               END-IF

           END-IF

           IF Kein-Fehler
               DISPLAY "Neuer Kontostand: " Kontostand
           ELSE
               PERFORM FEHLER-BEHANDELN
           END-IF.

       AUSZAHLUNG-BUCHEN.
           SET Kein-Fehler TO TRUE

           IF NOT Aktiv
               SET Konto-Nicht-Aktiv TO TRUE
               PERFORM FEHLER-BEHANDELN
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Auszahlungsbetrag eingeben:"
           ACCEPT Eingabe

           PERFORM BETRAG-PRUEFEN

           IF Kein-Fehler AND Buchungsanzahl >= 100
               SET Historie-Voll TO TRUE
           END-IF

           IF Kein-Fehler
               MOVE FUNCTION NUMVAL(Eingabe)
                   TO Auszahlung

               IF Kontostand >= Auszahlung
                   SUBTRACT Auszahlung FROM Kontostand
                       ON SIZE ERROR
                           SET Zahlenueberschreitung TO TRUE
                   END-SUBTRACT
                   IF Kein-Fehler
                       ADD 1 TO Buchungsanzahl
                       MOVE Buchungsanzahl
                           TO Buchungsnummer(Buchungsanzahl)
                       MOVE "A"
                           TO Buchungsart(Buchungsanzahl)
                       MOVE Auszahlung
                           TO Buchungsbetrag(Buchungsanzahl)
                       MOVE Kontostand
                           TO Saldo-Danach(Buchungsanzahl)
                   END-IF
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
               WHEN Historie-Voll
                   DISPLAY "Fehler: Buchungshistorie ist voll."
               WHEN OTHER
                   DISPLAY "Fehler: Unbekannter Status."
           END-EVALUATE.

       HISTORIE-ANZEIGEN.
           IF  Buchungsanzahl = ZERO
               DISPLAY "Noch keine Buchungen vorhanden."
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING Historien-Index FROM 1 BY 1
               UNTIL Historien-Index > Buchungsanzahl

               DISPLAY "Buchung: "
                   Buchungsnummer(Historien-Index)
               DISPLAY "Art: "
                   Buchungsart(Historien-Index)
               DISPLAY "Betrag: "
                   Buchungsbetrag(Historien-Index)
               DISPLAY "Saldo danach: "
                   Saldo-Danach(Historien-Index)
           END-PERFORM.

       HISTORIE-EXPORTIEREN.
      *> Ein leerer Durchlauf soll den letzten Export nicht loeschen.
           IF Buchungsanzahl = ZERO
               DISPLAY "Keine Buchungen: Export bleibt unveraendert."
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT Historien-Datei

           IF Historien-Dateistatus NOT = "00"
               DISPLAY "Fehler beim Oeffnen: "
                   Historien-Dateistatus
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING Historien-Index FROM 1 BY 1
               UNTIL Historien-Index > Buchungsanzahl

               INITIALIZE Historien-Zeile

               MOVE ";" TO Export-Trenner-1
                           Export-Trenner-2
                           Export-Trenner-3

               MOVE Buchungsnummer(Historien-Index)
                   TO Export-Nummer
               MOVE Buchungsart(Historien-Index)
                   TO Export-Art
               MOVE Buchungsbetrag(Historien-Index)
                   TO Export-Betrag
               MOVE Saldo-Danach(Historien-Index)
                   TO Export-Saldo

               WRITE Historien-Zeile

               IF Historien-Dateistatus NOT = "00"
                   DISPLAY "Fehler beim Schreiben: "
                       Historien-Dateistatus
                   PERFORM HISTORIEN-DATEI-SCHLIESSEN
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM

           CLOSE Historien-Datei

           IF Historien-Dateistatus = "00"
               DISPLAY "Historie exportiert."
           ELSE
               DISPLAY "Fehler beim Schliessen: "
                   Historien-Dateistatus
           END-IF.

       EXPORT-ANZEIGEN.
      *> Nur Dateiansicht: kein Import in Konto oder Buchungstabelle.
           MOVE 1 TO Erwartete-Buchungsnummer
           MOVE STARTSALDO TO Pruefsaldo
           MOVE ZERO TO Lese-Ende Export-Gelesen
           OPEN INPUT Historien-Datei

           IF Historien-Dateistatus = "35"
               DISPLAY "Noch keine Exportdatei vorhanden."
               EXIT PARAGRAPH
           END-IF

           IF Historien-Dateistatus NOT = "00"
               DISPLAY "Fehler beim Oeffnen des Exports: "
                   Historien-Dateistatus
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Letzter Export (keine aktuellen Sitzungsdaten):"
           PERFORM UNTIL Dateiende
               READ Historien-Datei
               EVALUATE Historien-Dateistatus
                   WHEN "00"
                       MOVE 1 TO Export-Gelesen
                       DISPLAY Historien-Zeile
                       PERFORM IMPORT-WERTE-LESEN
                       IF Import-Ungueltig
                           DISPLAY "Dateipruefung abgebrochen."
                           PERFORM HISTORIEN-DATEI-SCHLIESSEN
                           EXIT PARAGRAPH
                       END-IF
                   WHEN "10"
                       SET Dateiende TO TRUE
                   WHEN OTHER
                       DISPLAY "Fehler beim Lesen: "
                           Historien-Dateistatus
                       PERFORM HISTORIEN-DATEI-SCHLIESSEN
                       EXIT PARAGRAPH
               END-EVALUATE
           END-PERFORM

           IF Export-Gelesen = ZERO
               DISPLAY "Die Exportdatei ist leer."
           END-IF
           PERFORM HISTORIEN-DATEI-SCHLIESSEN.

       HISTORIEN-DATEI-SCHLIESSEN.
           CLOSE Historien-Datei
           IF Historien-Dateistatus NOT = "00"
               DISPLAY "Fehler beim Schliessen: "
                   Historien-Dateistatus
           END-IF.
       
       IMPORT-RECHNUNG-TESTEN.
           MOVE Kontostand TO Pruefsaldo
           MOVE 100 TO Import-Betrag

           ADD Import-Betrag TO Pruefsaldo
               GIVING Neuer-Pruefsaldo
           END-ADD

           MOVE 1340 TO Import-Saldo

           IF Neuer-Pruefsaldo = Import-Saldo
               DISPLAY "Saldo stimmt mit der Dateizeile ueberein."
               MOVE Neuer-Pruefsaldo TO Pruefsaldo
           ELSE
               DISPLAY "Fehler: Dateisaldo passt nicht zur Buchung."
           END-IF
           DISPLAY "Saldo nach Vergleich: " Pruefsaldo
           DISPLAY "Berechneter Saldo: " Neuer-Pruefsaldo
           DISPLAY "Echter Kontostand: " Kontostand.

       IMPORT-WERTE-LESEN.
           SET Import-Gueltig TO TRUE
           
           IF Export-Nummer IS NOT NUMERIC
               DISPLAY "Fehler: Ungueltige Buchungsnummer."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Export-Nummer NOT = Erwartete-Buchungsnummer
               DISPLAY "Fehler: Buchungsreihenfolge stimmt nicht."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Export-Trenner-1 NOT = ";"
               OR Export-Trenner-2 NOT = ";"
               OR Export-Trenner-3 NOT = ";"
               DISPLAY "Fehler: Ungueltige Trennzeichen."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Export-Art NOT = "E" AND Export-Art NOT = "A"
               DISPLAY "Fehler: Unbekannte Buchungsart."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Export-Betrag(1:9) IS NOT NUMERIC
               OR Export-Betrag(10:1) NOT = "."
               OR Export-Betrag(11:2) IS NOT NUMERIC
               DISPLAY "Fehler: Ungueltiges Betragsformat."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Export-Saldo(1:1) NOT = "+"
               AND Export-Saldo(1:1) NOT = "-"
               DISPLAY "Fehler: Ungueltiges Saldovorzeichen."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Export-Saldo(2:9) IS NOT NUMERIC
               OR Export-Saldo(11:1) NOT = "."
               OR Export-Saldo(12:2) IS NOT NUMERIC
               DISPLAY "Fehler: Ungueltiges Saldoformat."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF
           
           IF FUNCTION TEST-NUMVAL(Export-Betrag) NOT = ZERO
               SET Import-Ungueltig TO TRUE
           END-IF

           IF FUNCTION TEST-NUMVAL(Export-Saldo) NOT = ZERO
               SET Import-Ungueltig TO TRUE
           END-IF

           IF Import-Ungueltig
               DISPLAY "Fehler: Ungueltige Zahl in Exportzeile."
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(Export-Betrag)
               TO Import-Betrag
           MOVE FUNCTION NUMVAL(Export-Saldo)
               TO Import-Saldo

           IF Import-Betrag <= ZERO
               DISPLAY "Fehler: Importbetrag muss positiv sein."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF Import-Saldo < ZERO
               DISPLAY "Fehler: Negativer Saldo nicht erlaubt."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

                      EVALUATE Export-Art
               WHEN "E"
                   COMPUTE Neuer-Pruefsaldo =
                       Pruefsaldo + Import-Betrag
                       ON SIZE ERROR
                           SET Import-Ungueltig TO TRUE
                   END-COMPUTE
               WHEN "A"
                   SUBTRACT Import-Betrag FROM Pruefsaldo
                       GIVING Neuer-Pruefsaldo
                       ON SIZE ERROR
                           SET Import-Ungueltig TO TRUE
                   END-SUBTRACT
           END-EVALUATE

           IF Import-Ungueltig
               DISPLAY "Fehler: Ueberlauf bei Saldenpruefung."
               EXIT PARAGRAPH
           END-IF

           IF Neuer-Pruefsaldo NOT = Import-Saldo
               DISPLAY "Fehler: Saldo passt nicht zur Buchung."
               SET Import-Ungueltig TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE Neuer-Pruefsaldo TO Pruefsaldo
           DISPLAY "Saldenpruefung erfolgreich."

           DISPLAY "Gelesener Betrag: " Import-Betrag
           DISPLAY "Gelesener Saldo: " Import-Saldo
           
           ADD 1 TO Erwartete-Buchungsnummer.

           