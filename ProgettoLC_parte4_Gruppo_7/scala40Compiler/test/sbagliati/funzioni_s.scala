def foo(ref a : *Int)(val b: *Int): Int = *a + *b;

var K : Int;
var a : Int = 5; var b : Int = 2;

def setK(val k : Int) = { K = k; } 

def main() = {
	setK(50);
	def bar(res x : Array[*Int](2)) : Int = {
		var t : Int = foo(x[1])(x[2]);
		if (t > K) return t;
		else {
			K -= !5; 
            bar(Array(&t,x[2]));
		}
	}
	bar(Array(&a,&b));
}

