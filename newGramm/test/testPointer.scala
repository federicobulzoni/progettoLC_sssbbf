/*
For instance [3][4]int is an array of 3 arrays of 4 integers 
&[3]&int is a pointer to an array of 3 pointers to integers.

*/

var x : Array[Array[Int](4)] (3);
var y : *Array[*Int](3);
//var z : = Array (& x, * y, Array (x, y));
var u : ***Int;

var a1 : Int = 4;
var a2 : *Int = &a1;
var a3 : **Int = &a2;
var a4 : ***Int = &a3;
var a5 : ****Int = &a4;

var b : *Int = Null;


def proc () = {
	****a5 = 2;
}




