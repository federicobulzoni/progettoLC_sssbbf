var arr : Array[Array[*Int](3)](2);

def foo(val arr1 : Array[*Int](3)) : Float = {
    arr[0] = arr1;
    var b : Int = !(True);
    var c : Float = - (*arr1[1]);

    def foo2(ref p1 : Int) (res p2 : Int) : Float = p1 * p2 * 3.3;

    var d1 : Int = -b;
    var d : Float = foo2(b)(d1); 
    //var d : Float = foo2(b, -b);

    for (iter <- 0 until *arr1[1] by 'c'){
        while(iter < iter^2){
            if(iter < 10)
                break;
            else
            {
                iter+=1;
                continue;
            }
                
        }
    }
    do{
        writeInt(3);
        break;
    }while(c < 10);

    return !True;
}

def main() = {
    var a : Int = 3;
    var b : Int = 2;
    var c : Int = a;
    foo(Array(&a, &b, &c));
}