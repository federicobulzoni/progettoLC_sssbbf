def easy ( ) : Int = {
    writeInt(3);
    if( 3 < 4){ 
        // solo con questo da errore
        return 3;
    }
    {
        // con questo non da errore
        return 2;
    }
}
var z : Int = 1;