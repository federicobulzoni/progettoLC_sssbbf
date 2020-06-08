// Test puntatori - corretto.

var x : Array[Array[Int](4)] (3);
var y : *Array[*Int](3);
var u : ***Int;

var a1 : Int = 4;
var a2 : *Int = &a1;
var a3 : **Int = &a2;
var a4 : ***Int = &a3;
var a5 : ****Int = &a4;

var b : *Int = Null;


def main () = {
	****a5 = 2;
}