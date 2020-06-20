// Test puntatori - sbadato.

var x : Array[Array[Int](4)] (3);
var y : *Array[*Int](3);
var u : ***Int;

var a1 : Int = 5;
// a1 non è un puntatore.
var a2 : *Int = *a1;
// I tipi non combaciano.
var a3 : **Int = *a2;
// I tipi non combaciano.
var a4 : ***Int = &a2;
var a5 : ****Int = &a4;

var b : *Int = Null;

def main () = {
	// Null assegnato ad una L-espressione che non ha tipo puntatore.
	****a5 = Null;
}