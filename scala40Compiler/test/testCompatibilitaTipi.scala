// Test compatibilità tipi.

def main () = {
	var a : String = 3;
	var b : Float = 3.5 + (False * 4 + 4+5);
	var ll : Int = 3;
	var d : Bool;
	var c : Array [*Float] (3) = Array(&d, &b, &d);
	var n : Array [*Int] (1) = Array(&ll);
	var f : Float = *n[1];
}