def fun()() = {
	var variable : Char;
	while (variable == 'c'){

		variable = 'a';
	}
}
/*
Non va bene, il type system dà questo errore:
TestGramm.exe: AbsGramm.hs:174:5-39: Non-exhaustive patterns in function getType
*/