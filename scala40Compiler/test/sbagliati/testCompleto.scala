
var arr : Array[Array[*Int](3)](3);

def foo(val arr1 : Array[*Int](3)) : Float = {
    arr[0] = arr1;
    var b : Int = !(True);
    var c : Float = - (*arr1[1]);

    def foo2(ref p1 : Int) (res p2 : Int) : Float = p1 * p2 * 3.3;

    // assegnamento sbagliato
    var d1 : Int = Null;
    var d : Float = foo2(b)(d1); 
    // doppia dichiarazione e -b è un'espressione e non una lexp
    var d : Float = foo2(b, -b);
    // break fuori dal ciclo
    break;
    for (iter <- 0 until *arr1[1] by 'c'){
        while(iter < iter^2){
            // null sbagliato
            if(iter < 10 && Null)
                break;
            else
            {
                iter+=1;
                continue;
            }
                
        }
    }
    do{
        // writeInt è una procedura
        var e: Int = writeInt(3);
        break;
    }while(c < 10);
    // manca il return
    //return !True;
}

def main() = {
    var a : Int = 3;
    var b : Int = 2;
    var c : Int = a;
    foo(Array(&a, &b, &c));
    return True;
}