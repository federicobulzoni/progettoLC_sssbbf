// insertion sort
var N : Int = 10;

def insertion_sort(ref A : Array[Int](10)) = {
    var j : Int = 1;
    var key : Int;
    var i : Int;
    while(j < N) {
        var key : Int = A[j];
        i = j - 1;
        while(i >= 0 && A[i] > key){
            A[i + 1] = A[i];
            i -= 1;
        }
        A[i + 1] = key;
        j += 1;
    }
}

def main() = {
    var A : Array[Int](10);
    insertion_sort(A);
}