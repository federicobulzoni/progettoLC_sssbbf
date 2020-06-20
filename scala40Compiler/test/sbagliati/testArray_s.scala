// Test Array

var a : Array[Float](3);

def main() = {
    // il tipo stringa non è compatibile con float
	a = Array(2.4,'c',"3");
	var b : *Array[Float](3);
	b = &a;
}
