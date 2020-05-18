/*
For instance [3][4]int is an array of 3 arrays of 4 integers 
&[3]&int is a pointer to an array of 3 pointers to integers.

*/
var x : Array[3](Array[4](int))
var y : &Array[3](&int)

var z = Array (& x, * y, Array (x, y))

var u : & & &int