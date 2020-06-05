// assegnamenti

var n : Int = 3;
var i : *Int = &n;
var j : **Int = &i;
var j1 : **Int;
var l : Array[Int](2);

def main () = {
    var k : Int = 3;
    var s : Int = **j;
    j = j1;
    l[1] = k;
    l[1] = l[2];
    k = l[1];
    if(l[1] < 3){
        writeInt(3);
    }
    *j = *j;
}