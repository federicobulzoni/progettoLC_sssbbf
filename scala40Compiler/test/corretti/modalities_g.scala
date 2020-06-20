// Meaningless example using all four parameter modes.
def main() = {
	def foo(ref a : Int)(val b : Int)(res c : Int)(valres d : Int) : Int = {
		a = a + 1;
		d = a;
		c = 2;
		if (c > a) return c;
		var r : Int = a;
		var p : Int = a + b;
		return p;
	}
	var x : Int = 1;
	var y : Int = 2;
	var w : Int = 5;
	var l : Float = foo(x)(1)(y)(w);

	def bar(ref a : Array[Int](2)) = {
		return;
		a[1] = 4;
	}

}
