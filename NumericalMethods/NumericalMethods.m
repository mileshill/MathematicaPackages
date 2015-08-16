(* ::Package:: *)

(* :Title: NumericalMethods *)

(* :Author:  Miles Hill *)

(* :Summary:
This packages provides the basic functions introduced to undergraduates in a Numerical Analysis course. 
Roots: Bisection, FalsePosition
*)

(* :Context:  NumericalMethods` *)

(* :Source: *)

(* :History:
	Version 1.0 by Miles Hill, 2015
*)

(* :Package Version: 1.0 *)


BeginPackage["NumericalMethods`"];


(*USAGE STATEMENTS*)
If[Not@ValueQ[NumericalMethods::usage],NumericalMethods::usage="NumericalMethods` provides functions for various root-finding techniques and iterative solutions."];

(* Bisection *)
If[Not@ValueQ[Bisection::usage],Bisection::usage="Bisection[ func, {lower, upper} ] approximates the root of < func > in the interval x \[Element] [lower,upper]."];

(* FalsePosition *)
If[Not@ValueQ[FalsePosition::usage],FalsePosition::usage="FalsePosition[ func, {lower,upper} ] approximates the roof of < func > in the interval x\[Element] [lower,upper]."];


(* OPTIONS *)
Options[Bisection]={MaxSteps->10};
Options[FalsePosition]={MaxSteps->10};

Begin["`Private`"];


NumericalMethods::badarg="You called `1` with `2` argument(s).  It must have the  `3` argument(s).";


(* BISECTION USAGE *)
Bisection::badarg="The first argument must be a function. The second an ordered list of bounds. Your input was `1`";

(* BISECTION UPVALUES *)
NumericQ[Bisection]^= True;

(* BISECTION DOWNVALUES *)
(* Primary *)
Bisection[f:(_Function|_Symbol),{lower_,upper_},opts___Rule]/; OrderedQ[{lower,upper}]:=Block[
(*initialize local vars*)
{a=lower,b=upper,i,FA,p,FP},
(*Code body*)
i=1;
FA=f[a];
While[i<MaxSteps/.{opts}/.Options[Bisection] ,
p=a+(b-a)/2;
FP=f[p];
If[FP==0,Return[p];Break[]];

If[FA*FP>0.0,a=p;FA=FP,b=p];
i++;
](*end While*);
Return[p]];

(* Error Messages *)
Bisection[f_,{lower_,upper_},opts___Rule]:=Message[Bisection::badarg,{f,{lower,upper}}];
Bisection[args___/;Length[{args}]<2]:=Message[NumericalMethods::badarg,Bisection,Length[{args}],2];


(* FALSEPOSITION USAGE *)
FalsePosition::badarg="The first argument must be a function. The second an ordered list of bounds. You input was `1`.";

(* FALSEPOSITION UPVALUES *)
NumericQ[FalsePosition]^=True;

(* FALSEPOSITION DOWNVALUES *)
(* Primary *)
FalsePosition[f:(_Function|_Symbol),{lower_, upper_},opts___Rule]/;OrderedQ[{lower,upper}]:=
Block[{a=lower,b=upper,i,FA,p,FP},
i=1;
FA=f[a];
While[i< MaxSteps/.{opts}/.Options[FalsePosition],
p=b-(f[b](a-b))/(f[a]-f[b]);
FP=f[p];

If[FP==0,
Return[p];Break[]];
i++;

If[FA*FP > 0.,
a=p;FA=FP,
b=p]](* end While *);
Return[p]];

(* Error Messages *)
FalsePosition[f_,{lower_,upper_},opts___Rule]:=Message[FalsePosition::badarg,{f,{lower,upper}}];
FalsePosition[args___/;Length[{args}]<2]:=Message[NumericalMethods::badarg,FalsePosition,Length[{args}],2];



End[](*End Private`*);
EndPackage[];