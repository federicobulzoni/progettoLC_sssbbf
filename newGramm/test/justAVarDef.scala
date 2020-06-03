var x : Array[Int](3) = Array(11,12,13);
var y : Int = 4;

def proc () = {
    var a : Array[Array[Int](3)](2) = Array(x,x);
    var b : Int = a[1][2];

    var c : *Array[Int](3) = &a[1];
}


def foo () = {
    y = 4;
    
    def foo2() = {
        var t : Int;
    }
    
}

var h : Float;
var k : Array[*Int](4);

def main() : Int = 3;
 
def main12() = {
    var x : Int;
    def main1 (a : Int) : Int = {
        return 1;
    }
    var y : Int;
    foo ();
}

def bar() : Bool = {
    var x : Int = 3 + 4 * 3;
    def bar2(k : Int) : Float = {
        if( 3 < 4+4)
            return 3.4;
        return 3.3;
    }
    var b : Float = bar2(3+3);
    return True;
}

def mm () : Int = {
    do {
        writeInt(3);
    } while(3 < 5);
    return 3;
}

var z : Array[Int](3) = Array(11,12,13);