use "vals.sml";
use "APCO.sml";
use "SPS.sml";



val showMapTestCases = mapTestCases(S,testCases);
val testSequences = insertETS(STS,SS,ETS);
val lenTestSequences = List.length(testSequences);
