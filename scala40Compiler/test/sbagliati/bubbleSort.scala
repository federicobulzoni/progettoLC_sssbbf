// Bubble sort sbadato.
var a : Array[Int](10);
var N : Int = 10;

					
def bubbleSort (val a : Array[Int](10), ref dim : Int) = {
	// Assegnamento errato.
	var i : Int = "ciao";
	var j : Int = 0;

	while (j < N - 1){
		while (i < N - 1){
			if (a[i] > a[i+1]) {
				// temp non dichiarato.
				temp = a[i];
				a[i] = a[i+1];
				// temp non dichiarato.
				a[i+1] = temp;
			}
			i += 1; 
		}
		j += 1;
	}
	// non viene ritornato un array come da firma della funzione.
}

def main () = {
	// i non dichiarato.
	while (i < N)
		a[i] = readInt();
	
	// Troppi argomenti.
	var orderedA : Array[Int](10) = bubbleSort(a, N, "ciao");
	return 0;
}