def main () = {
	def proc (ref x : Int)( ref y : Float, ref z : *Char) = {
		y = x;
	}
	var a : Char = True;
	proc (3)(3, &a);

	var arr : Array [Float] (3) = Array('c', True, 5); // Qua gli elementi andrebbero tutti castati a Float!

	// Non viene usato l'assegnamento from array
	var x : Float;
	x = arr[0];

	// Grave errore viene persa la lexp.
	var y : Float = arr[0];



	// TEST PUNTATORI: PERFETTO.
	var point : * * Char;
	var c1 : Char = 'a';
	break;
	**point = c1;
	*point = &c1;
	return;

	var e : Int = if (3==3) True else 'c';
	var s : String = 'c';

}