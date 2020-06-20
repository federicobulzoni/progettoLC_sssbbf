
def main() = {
	def foo(ref a : Int)(val b : Int)(res c : Int)(valres d : Int) : Int = {
		a = a + 1;
		d = a;
		c = 2;
		var r : Int = a;
		var p : Int = a + b;
		return 2;
	}
	var x : Int = 1;
	var y : Int = 2;
	var w : Int = 5;
	var l : Float = foo(1)(1)(&x)(y);
	var z : **Int;
	**z = 4;

	def bar(ref a : Array[Int](2)) = {
		a[1] = 4;
	}

}
