def main () = {
	def proc (x : Int)(y : Float, z : *Char) = {
		y = x;
	}
	var a : Char = True;
	proc (3)(3, &a);
}