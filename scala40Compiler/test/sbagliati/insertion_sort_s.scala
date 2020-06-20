// insertion sort
var N : Int = 100.1;

def insertion_sort(ref A : Array[Int](10)) = {
    var j : Int = 2;
    var key : Int;
    var i : Int;
    while(j < N) {
        var key : Int = A[j];
        i = j - 1;
        while(i > 0 && A[i] > key){
            A[i + 1] = A[i*1.0];
            i -= 1;
            
        }
        A[i + 1] = !key;
    }
    break;
}

def main() = {
    var A : Array[Int](10);
    insertion_sort(A);
}