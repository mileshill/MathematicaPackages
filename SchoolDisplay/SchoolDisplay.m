(* ::Package:: *)

(* Output for easy column display *)
SetOptions[DisplayColumn,{Alignment->Left,Dividers->All,Spacings->1.5,Background->White}];
DisplayColumn[title_,{headers__},{sols__}]/;(Length[{headers}]==Length[{sols}]):=Block[{Title},
(* customization for row headers *)
Title[size_:16]:=Style[#,{FontFamily->"Calibri",FontSize->size,Background->Lighter[Gray, 0.5]}]&;

Column[
Riffle[Title[]/@{headers},{sols}],Options[DisplayColumn]]//Labeled[#,Title[20][title],Top]&//Panel
];
