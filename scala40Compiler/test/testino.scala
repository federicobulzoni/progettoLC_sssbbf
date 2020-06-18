def prova(a : Int)(b : Float, c : *Char) : Float = a + b;
def prova2(a : Int)(b : Float, c : *Char) = writeInt(3);

def main () = {
	var c : Int = 3;
	var b : Array[*Int](3);
	var a : Array[Array[*Int](3)](2);

	*b[1] = 3;
	a[1] = b;
	var t : Int;
	var g : Char;
	var k : Float = prova(c)(t, &g);
	prova2(c)(t, &g);
}