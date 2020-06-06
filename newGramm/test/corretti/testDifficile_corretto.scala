// Test difficile - corretto.
var aInt3 : Array[Int](3) = Array(11,12,13);
var i : Int = 4;

def fun1 () : *Array[Int](3) = {
    var a2_aInt3 : Array[Array[Int](3)](2) = Array(aInt3,Array(aInt3[0], aInt3[0], aInt3[1]));
    var a12 : Int = a2_aInt3[1][2];

    return & a2_aInt3[a12];
}

def proc1 () = { 
    i = 4;
    {
        i *= i;
        i /= i;
        def proc2 (i : Int) = proc1 ();
    }
}

var h : Float;
var aPointInt4 : Array[*Int](4);

 
def main() = {
    def main1 (a : Int) : Int = {
        return a;
    }
    proc1 ();
}

def bar() : Bool = {
    var x : Int = 3 + 4 * i ^ 2;
   	if (x < i / i * 12)
   		return True;
   	else
   		return False;
}

def mm () : Int = {
	var j : Int = readInt();
    do {
    	i += 1;
        writeInt(i);
    } while(j < i);
    return j;
}
