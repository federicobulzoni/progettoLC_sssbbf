def fattoriale(num: Int) : Int = {
    def fat (num: Int): Int = { 
        if (num==1) return 1;
        else return num * fat(num - 1) ;
    }
    return fat(num);
}


