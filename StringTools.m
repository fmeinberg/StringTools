BeginPackage["StringTools`"]

StringMapAt::usage="";
StringSplitAt::usage="";
CamelCase::usage="";
CharacterName::usage="";
StringTogether::usage="";
ToASCII::usage="";

Begin["Private`"]

StringMapAt[foo_, string_, pos_] :=
 StringJoin@MapAt[foo, Characters[string], pos]

StringSplitAt[string_,pos_] :=
    {StringTake[string,{1,pos-1}],StringTake[string,{pos+1,-1}]};

CamelCase[s_String] :=
    StringReplace[StringReplace[s,Whitespace->" "]," "~~l_:>ToUpperCase@l];

CharacterName[c_String] :=
    StringReplace[ToString[c, InputForm, CharacterEncoding -> "ASCII"],
    	"\"\\[" ~~ a__ ~~ "]\"" :> a]

StringTogether[l_List] :=
    StringJoin[StringJoin/@Transpose[{l,Table[" ",{Length[l]}]}]];

ToASCII[string_] :=
    StringReplace[string, StringTools`asciifiRules]

StringTools`asciifiRules =
	Join[{"\:1ed3"-> "o","\:0105" -> "a", "\:0110" -> "Dh", "\:0111" -> "dh", "\:0117" -> "e", "\:0119" -> "e", "\:011b" -> "e", "\:011f" -> "g", "\:0120" -> "G", "\:0121" -> "g", "\:012a" -> "I", "\:012b" -> "i", "\:0130" -> "I", "\:0136" -> "K", "\:0137" -> "k", "\:013c" -> "l", "\:0144" -> "n", "\:0146" -> "n", "\:0148" -> "n", "\:014c" -> "O", "\:014d" -> "o", "\:014f" -> "o", "\:0157" -> "r", "\:0159" -> "r", "\:015a" -> "S", "\:015b" -> "s", "\:015e" -> "S", "\:015f" -> "s", "\:0162" -> "T", "\:0163" -> "T", "\:016b" -> "u", "\:0173" -> "u", "\:017d" -> "Z", "\:017e" -> "z", "\:018f" -> "a", "\:0259" -> "a", "\:02bf" -> "", "\:0328" -> "", "\:0331" -> "", "\[AE]" -> "ae", "\[AAcute]" -> "a", "\[ABar]" -> "a", "\[AGrave]" -> "a", "\[ACup]" -> "a", "\[AHat]" -> "a", "\[ARing]" -> "a", "\[ADoubleDot]" -> "a", "\[ATilde]" -> "a", "\[CapitalAAcute]" -> "A", "\[CapitalABar]" -> "A", "\[CapitalARing]" -> "A", "\[CHacek]" -> "c", "\[CCedilla]" -> "c", "\[CapitalCHacek]" -> "C", "\[CapitalCCedilla]" -> "C", "\[Eth]" -> "dh", "\[EAcute]" -> "e", "\[EBar]" -> "e", "\[EGrave]" -> "e", "\[EDoubleDot]" -> "e", "\[CapitalEAcute]" -> "E", "\[DotlessI]" -> "i", "\[IAcute]" -> "i", "\[ICup]" -> "i", "\[IHat]" -> "i", "\[IDoubleDot]" -> "i", "\[CapitalIHat]" -> "I", "\[LSlash]" -> "l", "\[CapitalLSlash]" -> "L", "\[NTilde]" -> "n", "\[CapitalNTilde]" -> "N", "\[OAcute]" -> "o", "\[OGrave]" -> "o", "\[OHat]" -> "o", "\[ODoubleDot]" -> "o", "\[ODoubleAcute]" -> "o", "\[OSlash]" -> "o", "\[CapitalOAcute]" -> "O", "\[CapitalODoubleDot]" -> "O", "\[CapitalOSlash]" -> "O", "\[SHacek]" -> "s", "\[CapitalSHacek]" -> "S", "\[UAcute]" -> "u", "\[UDoubleDot]" -> "u", "\[CapitalUAcute]" -> "U", "\[YAcute]" -> "y"}, {"\:0126" -> "H", "\:0127" -> "h", "\:014e" -> "O", "\:0158" -> "R", "\:016a" -> "U", "\:016c" -> "V", "\:016d" -> "v", "\:016f" -> "u", "\:017a" -> "z", "\:017b" -> "Z", "\:017c" -> "z", "\[CapitalAGrave]" -> "A", "\[CapitalAHat]" -> "A", "\[CapitalADoubleDot]" -> "A", "\[CAcute]" -> "c", "\[CapitalCAcute]" -> "C", "\[EHat]" -> "e", "\[CapitalEBar]" -> "E", "\[CapitalEGrave]" -> "E", "\[IGrave]" -> "i", "\[CapitalIAcute]" -> "I", "\[OTilde]" -> "o", "\[CapitalOGrave]" -> "O", "\[CapitalODoubleAcute]" -> "O", "\[SZ]" -> "ss", "\[CapitalThorn]" -> "Th", "\[UGrave]" -> "u", "\[UHat]" -> "u", "\[UDoubleAcute]" -> "u", "\[CapitalUDoubleDot]" -> "U", "\[YDoubleDot]" -> "y"}, {"." -> "", "'" -> "", "/" -> "", "\[AAcute]" -> "a", "\[EAcute]" -> "e", "\[IAcute]" -> "i", "\[NTilde]" -> "n", "\[OAcute]" -> "o", "\[UAcute]" -> "u", "\[UDoubleDot]" -> "u"}];

(* String Generators *)

TimeStampString[]:=
	DateString[{"Year","Month","Day","Hour","Minute"}]


End[];
EndPackage[];


