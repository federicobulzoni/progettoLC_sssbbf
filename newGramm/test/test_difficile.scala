var z : *Int;
var y : *Int;
var w : *Int;
var z1 : *Int;
var y1 : *Int;
var w1 : *Int;
var b : Array[**Int](3) = Array(&y, &z, &w);
var c : Array[**Int](3) = Array(&y1, &z1, &w1);
var a : Array[*Array[**Int](3)](5) = Array(&b, &c, &b, &b, &b);

/*
var a[3] = Array(1,2)
a[0] = 1
a[1] = 2
*/