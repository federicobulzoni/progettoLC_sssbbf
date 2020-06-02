/*
def fun (a:Int, b:Float) : Int = {
	var a : Int = a + b;
	var c : Int = c + d;
}
def proc () = {
	var x : Int = c+b;
}
*/
def clauses (a:Int)(b:Float)(c:Array[Int](3)) : Int = {
	return a;
}

def inLineFun (a:Int) : Int = 2*a

var b : Int = inLineFun (3);