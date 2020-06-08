// insertion sort
var N : Int = 100;

def insertion_sort(A : Array[Int](10)) : Array[Int](10) = {
    var j : Int = 2;
    var key : Int;
    var i : Int;
    while(j < N) {
        var key : Int = A[j];
        i = j - 1;
        while(i > 0 && A[i] > key){
            A[i + 1] = A[i];
            i -= 1;
        }
        A[i + 1] = key;
    }
    return A;
}

def main() = {
    var A : Array[Int](10);
    A = insertion_sort(A);
}