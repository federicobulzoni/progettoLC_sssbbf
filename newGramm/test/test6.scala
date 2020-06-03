var a : Int = 3;

def foo () : Int = 9;

def maino() = {
    a *= 3*foo();
    a += 3*foo();
    a -= 3*foo();
    a %= 3*foo();
}