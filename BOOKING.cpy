       01 Buchungsanzahl PIC 9(3) COMP VALUE ZERO.
       01 Buchungshistorie.
           05 Buchung OCCURS 100 TIMES.
               10 Buchungsnummer PIC 9(3).
               10 Buchungsart PIC X.
               10 Buchungsbetrag PIC 9(9)V99 COMP-3.
               10 Saldo-Danach PIC S9(9)V99 COMP-3.
