def prova(ref a : Int)(ref b : Float, ref c : *Char) : Float = a + b;
def prova2(ref a : Int)(ref b : Float, ref c : *Char) = writeInt(3);
//def arduo () : Float = if (4 == 3) True else if (3 > 1) False else 3;
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


	var ciao : Array[Float](3) = Array('c',1,True);
	var oo : Float = if (4 == 3) 3.5 + True else 4;
	//var pro : String = 'c';
	while (9 < 3)
		break;
	while (9 < 2){
		def fun (ref a : Int) = {
			while(3 < 4) break;
		}
		continue;
	}	
}