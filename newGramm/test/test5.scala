def f0 () : Int = 3+3;

def f1 () = { 
    var o : Int = 3; 
}

// def f2() = f1();
def f2 () = f0();

def f3 () : Int = f1();

def f4 () = 3+3;