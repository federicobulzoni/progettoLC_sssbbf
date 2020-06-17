def main () = {
	var c : Int = 3;
	var b : Array[*Int](3);
	var a : Array[Array[*Int](3)](2);

	*b[1] = 3;
	a[1] = b;
}