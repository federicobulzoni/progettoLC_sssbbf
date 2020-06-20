// Bubble sort corretto.
var a : Array[Int](10);
var N : Int = 10;

def bubbleSort (ref a : Array[Int](10), val dim : Int) : Array[Int](10) = {
	var swapped : Bool = True;
	var temp : Int;

	while (swapped) {
		swapped = False;
		for (i <- 0 until dim-2 by 1) {
			if (a[i] > a[i+1]) {
				temp = a[i];
				a[i] = a[i+1];
				a[i+1] = temp;
				swapped = True;
			}
		}
	}

	return a;
}

def main () = {
	for (i <- 0 until N-1 by 1)
		a[i] = readInt();

	var orderedA : Array[Int](10) = bubbleSort(a, N);
}