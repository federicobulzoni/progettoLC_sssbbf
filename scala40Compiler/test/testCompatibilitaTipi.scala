// Test compatibilità tipi.

def main () = {
	var a : String = 3;
	var b : Float = 3.5 + (False * 4 + 4+5);

	var d : Bool;
	var c : Array [*Float] (3) = Array(&b, &b, &d);
}