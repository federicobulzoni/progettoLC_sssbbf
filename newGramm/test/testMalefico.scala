// // Il nostro primo programma in Scala.

// def myProgram (): * Array [Array [**Int](2)](3) = {
//     //var a : *Int = &5;
//     var a1 : Int = 5;
//     var b1 : *Int = &a1;

//     var a2 : Int = 2;
//     var b2 : *Int = &a2;
//     var array1 : Array [**Int] (2) = Array (b1, &b2);

//     var array2 : Array [Array [**Int] (2) ] (3) = Array (array1, array1, array1);

//     return (&array2);
// } 


def muahah (a : Int)(b : Float, c : Char) = {
    // Redefinizione di a
    var a : Int;
    // Return che non deve esserci.
    return a;

    // Questo è corretto
    var d : Int = a * 2 ^ 3;
    // b è un float e non si può fare
    var e : Float = d * b;
    // corretto
    var f : Int = d % 3;
    {
        // corretto
        var d : Char = c;
        // Redefinizione di d.
        var d : Int = 4;
    }

    muahah (d, e)(f);
}
["identificatore a usato in precedenza per una variabile in posizione(18,13).\n",
"Valore di ritorno inaspettato alla posizione(22, 12).",
"L'operatore * in posizione (27, 21) non puo' essere applicato ad un'espressione di tipo Int e un'espressione di tipo Float."
,"identificatoredusato in precedenza per una variabile in posizione(32,13).\n","
Errore in posizione (37, 5) nella chiamata alla funzione \"muahah\" la firma della funzione e': \"IF\" \"Il\" \"Io\" \"Ia\" \"It\" \"I \" \"IC\" \"Ih\" \"Ia\" \"Ir\" \"nF\" \"nl\" \"no\" \"na\" \"nt\" \"n \" \"nC\" \"nh\" \"na\" \"nr\" \"tF\" \"tl\" \"to\" \"ta\" \"tt\" \"t \" \"tC\" \"th\" \"ta\" \"tr\":Void mentre i parametri passati sono di tipo: \"II\" \"In\" \"It\" \"nI\" \"nn\" \"nt\" \"tI\" \"tn\" \"tt\" \" I\" \" n\" \" t\" \"FI\" \"Fn\" \"Ft\" \"lI\" \"ln\" \"lt\" \"oI\" \"on\" \"ot\" \"aI\" \"an\" \"at\" \"tI\" \"tn\" \"tt\"."])