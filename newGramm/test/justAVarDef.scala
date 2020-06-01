var x : Array[Int](3) = Array(11,12,13);
var y : Int = x[1];

def foo () = {
    y= 4;
    def foo2() = {
        var t : Int;
    }
}

var h : Float;
var k : Array[*Int](4);

def bar() : Int = {
    var x : Int = 3 + 4 * 3;
    def bar2(k : Int) : Float = {
        if( 3 < 4+4)
            return 3.4;
        return 3.3;
    }
    var b : Float = bar2(3+3);
    return 1;
}
var z : Array[Int](3) = Array(11,12,13);