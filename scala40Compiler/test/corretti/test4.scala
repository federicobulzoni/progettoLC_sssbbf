var a : Array[Array[*Int](3)](2);
var b : Array[*Int](3);

def mul (val k : *Int) : Int = *k* *k; 
def main() = {
    var o : Int = 2;
    //var o : Float;
    var c : *Int = &o;
    b = Array(c,c,c);
    a = Array(b,b);
    a[1.1] = c;
}