def main() = {
    var i : Int;
    var j : Int;
    var k : Int = i + j;
    def foo() = {
    while (i < j){
        k = 0;
        
            if (k < 10){
                k = 1;
                continue;
            } else {
                k = 2;
                break;
            }
        }
        k = 3;
    
    k = 4;
    }
}