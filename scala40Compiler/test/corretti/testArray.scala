// Test Array

var a : Array[Int](3);

def main() = {
	a = Array(1,2,3);
	var b : *Array[Int](3);
	b = &a;
}
