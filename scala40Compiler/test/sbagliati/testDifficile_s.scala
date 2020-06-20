// Test difficile - sbadato.
var aInt3 : Array[Int](3) = Array(11,12,13);
var i : Int = 4;

def fun1 () : *Array[Int](3) = {
                                                                // accesso sbagliato.
    var a2_aInt3 : Array[Array[Int](3)](2) = Array(aInt3,Array(aInt3[0][0], aInt3[0], aInt3[1]));
    // accesso sbagliato.
    var a12 : Int = aInt3[1][2];

    if (i > 4) return & a2_aInt3[a12];
    // Warning: non è detto che si raggiunga un return.
}

def proc1 () = { 
    {
        var j : Int = 12^4;
        j *= i;
        j /= i;
        def proc2 (ref i : Int) = proc1 ();
    }
    // Entrambi gli identificatori qua non sono visibili.
    proc2 (j);
}

def main1() = {
    def main (val a : Int) : Int = {
        // Nessun valore di ritorno.
        return;
    }

    proc1 ();
}

def bar() : Bool = {
    // Incompatibilità tra tipi.
    var x : Int = 3 + 4 * "ciao" ^ 2;
    // Confronto tra tipi diversi.
    if (x < i / i * 12 == i)
        // String come ritorno invece che Bool.
        return "True";
    else
        return False;
}

def mm () : Int = {
    {
        // writeInt() non ritorna un intero.
        var j : Int = writeInt(i);
    }
    do {
        // j non è visibile qua.
        j += 1; // j = j + 1
        writeInt(i);
    } while(i < i);
    return i;
}

// Warning: manca un main.