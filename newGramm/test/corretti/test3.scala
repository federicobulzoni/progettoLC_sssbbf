var a : Array[*Int](3);
var b : Array[Int](3);

def _foo() = {
    b[1] ^= 2;
}
def main() = {
    var c : Int = b[1];
    *a[1] = 1;
    c = *a[1];
}