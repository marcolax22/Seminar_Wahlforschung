# Vertiefungsseminar Wahlforschung bei Prof. Shikano

Hier wird der Code für die Seminararbeit: Entwicklung der Parteiidentifizierung bei Erstwählern im deutschen Mehrparteiensystem, zur Verfügung gestellt.

**Der Ordner hat folgende Struktur:**

1. Dem R Projekt: Seminar_wahlforschung, dass die R Codes in einem Projekt Ordner strukturiert. In diesem R Projekt werden die Daten in einem Unterordner mit dem Namen "data" abgespeichert, um die folgenden R Codes korrekt zu laden. Zudem werden die R Codes `install_packages.R`, `datamanagement_code.R`, `datamanagement_RDD.R`und `dataanalysis_code.R` im R Projekt Seminar_Wahlforschung gespeichert.
2. Der R Code: `install_packages.R`, sorgt dafür, dass alle genutzten R packages vom Anwender korrekt geladen werden.
3. Der R Code: `datamanagement_code.R`, strukturiert die Daten der GLES-Panel (ZA5770) und stellt die Datenstruktur für das Regression Discontinuity Design bereit.
4. Der R Code: `datamanagement_RDD.R`, hier werden die Daten für die Analyse bereitgestellt und die Datensätze für die verschiedenen RDD´s gecoded.
5. Der R Code: `dataanalysis_code.R`, führt das RDD durch, und liefert die Ergebnisse für das RDD.


