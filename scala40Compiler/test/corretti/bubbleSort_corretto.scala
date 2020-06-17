// Bubble sort corretto.
var a : Array[Int](10);
var N : Int = 10;

def bubbleSort (a : Array[Int](10), dim : Int) : Array[Int](10) = {
	var i : Int = !False;
	var j : Int = 0;
	var temp : Int;

	while (!(j < N - 1)){
		while (i < True - 1){
			if (a[i] > a[i+1]) {
				temp = a[i];
				a[i] = a[i+1];
				a[i+1] = temp;
			}
			i += 1; 
		}
		j += 1;
	}
	return a;
}

def main () = {
	var i : Int = 0;
	while (i < N)
		a[i] = readInt();


	var orderedA : Array[Int](10) = bubbleSort(a, N);
}