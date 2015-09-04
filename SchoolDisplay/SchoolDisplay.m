(* ::Package:: *)

(* :Title: SchoolDisplay *)

(* :Author:  Miles Hill *)

(* :Summary:
This package provides simple formatting for homework solutions.
*)

(* :Context:  SchoolDisplay` *)

(* :Source: *)

(* :History:
	Version 1.0 by Miles Hill, 2015
*)

(* :Package Version: 1.0 *)


BeginPackage["SchoolDisplay`"];

(* USAGE STATEMENTS *)
If[Not@ValueQ[SchoolDispaly::usage],SchoolDisplay::usage="SchoolDisplay` contains functions for project formatting"];

If[Not@ValueQ[DisplayColumn::usage],DisplayColumn::usage="DisplayColumn formats output into a more aesthetic format."];

(* OPTIONS *)
Options[DisplayColumn]= Options[Column];




Begin["`Private`"]


(* DISPLAYCOLUMN USAGE *)
DisplayColumn::badarg="DisplayColumn takes 3 args. {Panel Title, { Solution Headers },{ Solutions }}. You entered `1`";
DisplayColumn::badlen="Headers and solutions must have equal length.";

DisplayColumn[title_String,{headers__},{sols__}, opts___Rule]/;(Length[{headers}]==Length[{sols}]):=Block[
{Title},
(* customization for row headers *)
Title[size_:16]:=Style[#,{FontFamily->"Calibri",FontSize->size,Background->Lighter[Gray, 0.5]}]&;

Column[
Riffle[Title[]/@{headers},{sols}], opts, Alignment->Left,Spacings->1.5, Background->White,Dividers->All ]//Labeled[#,Title[20][title],Top]&//Panel
];

DisplayColumn[title_,{headers__},{sols__},___]:=Message[DisplayColumn::badlen];
DisplayColumn[args___/;Length[{args}]<3]:=Message[DisplayColumn::badarg,Length[{args}]];





End[];
EndPackage[];
