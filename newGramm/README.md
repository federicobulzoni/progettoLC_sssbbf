# Compilatore (front-end) per il linguaggio Scala40
In questo file viene fornita una breve descrizione dei principali file che compongono il compilatore, una panoramica sull'output generato e le istuzioni per eseguire dei test. 
Per una descrizione più approfondita sul progetto si rimanda alla relazione e ai commenti nel codice.

## Guida ai file
I file principali che compongono il compilatore sono:
* ```AbsGramm.hs```: dichiarazione dei ```data``` e dei relativi costruttori. Utilizzato durante la costruzione dell'albero di sintassi astratta. Generato inizialmente da *BNFC* ma pesantemente modificato manualmente.
* ```Lexer.hs```: esecuzione dei fare di lexing. Generato da *BNFC*.
* ```Parser.hs```: esecuzione della fase di parsing. Generato da *BNFC* e modificato in alcune sue parti.
* ```Printer.hs```: stampa delle componenti dopo la fase di parsing. Il file è stato generato da *BNFC*, ma è stato pesantemente modificato per usufruire delle modifiche fatte in ```AbsGramm``` e per migliorare la stampa dell'albero tipato e linearizzato.
* **```StaticAnalysis.hs```**: file che implementa la fase di analisi statica e la conseguente generazione dell'albero tipato.
* **```Environment.hs```**: insieme di funzioni per la creazione e gestione degli scope e dello stak di environment.
* **```ThreeAddressCode.hs```**: file per la generazione del codice intermedio.
* **```PrintTAC.hs```**: file per la gestione della stampa del TAC. Presa la lista di istruzioni TAC, si occupa di stamparle, migliorandone la lettura.
* **```AbsTAC.hs```**: definizione delle istruzioni TAC, dei tipi indirizzo ed etichetta utili durante la generazione di codice intermedio.
* ```Scala40Compiler.hs```: file principale per le chiamate dei file sopracitati. In ordine esegue: lexer, parser, analisi statica e generazione del codice intermedio. Prima del codice intermedio si occupa di stampare gli errori.
* **```Errors.hs```**: definizione di tutti i possibili log (Error o Warning) e delle funzioni utili a gestirli.
* **```Typed.hs```**: definizione delle proprietà dei tipi dichiarati in ```AbsGramm.hs```.
* **```Color.hs```**: funzioni per la gestione dei colori nell'output. Utile sia durante la stampa linearizzata dell'albero tipato, sia nella stampa del TAC.
## Compilazione
I file vengono già forniti compilati e pronti per l'esecuzione. Se fosse necessario dover ricompilare è sufficiente utilizzare il comando: ```make```.

## Output del compilatore
Il compilatore prende in input un file di test e dopo aver eseguito la generazione del codice intermedio stampa:
* Il programma in input linearizzato
* Il programma in input tipato e linearizzato
* La lista degli eventuali Warning ed errori
* Il Three Address Code

## Esecuzione di un test
Sono stati proposti alcuni esempi per far comprendere il funzionamento e la correttezza del compilatore **Scala40**.
Per eseguire dei test semplici utili a vedere il risultato finale restituito dal compilatore, è sufficiente eseguire il comando ```make test```.
I file di test risiedono nella castella *test* e sono suddivisi nelle cartelle *corretti* (contenente file di esempio senza errori) e *sbagliati* (contenente file di esempio con errori vari).
Per eseguire test su file più specifici è sufficiente utilizzare il comando: 
<center>

```./Scala40Compiler test/<tipo_esempio>/<nome_file>```
</center>

dove ```<tipo_esempio>``` corrisponde a *corretti* o *sbagliati*, mentre ```<nome_file>``` è il nome del file che si vuole compilare.

### Autori
* Federico Bulzoni (142242)
* Sara Biavaschi (130512)
* Simone Scaboro (143191)
