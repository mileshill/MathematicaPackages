(* ::Package:: *)

BeginPackage["NumericalMethods`"];


If[!ValueQ[NumericalMethods::usage],NumericalMethods::usage="NumericalMethods` provides functions for various root-finding techniques and iterative solutions."];
If[!ValueQ[Bisection::usage],Bisection::usage="Bisection[ func, {lower, upper} ] approximates the root of < func > in the interval x \[Element] [lower,upper]."];
If[!ValueQ[FalsePosition::usage],FalsePosition::usage="FalsePosition[ func, {lower,upper} ] approximates the roof of < func > in the interval x\[Element] [lower,upper]."];


Options[Bisection]={MaxSteps->10};
Options[FalsePosition]={MaxSteps->10};
Begin["`Private`"];


NumericalMethods::badarg="You called `1` with `2` argument(s).  It must have the  `3` argument(s).";


Bisection::badarg="The first argument must be a function. The second an ordered list of bounds. Your input was `1`";
NumericQ[Bisection]^=True;
Bisection[f:_Function|_Symbol,{lower_,upper_},opts___Rule]/;OrderedQ[{lower,upper}]:=Block[{a=lower,b=upper,i,FA,p,FP},i=1;FA=f[a];While[i<MaxSteps/. {opts}/. Options[Bisection],p=a+(b-a)/2;FP=f[p];If[FP==0,Return[p];Break[]];If[FA FP>0.,a=p;FA=FP,b=p];i++;];Return[p]];
Bisection[f_,{lower_,upper_},opts___Rule]:=Message[Bisection::badarg,{f,{lower,upper}}];
Bisection[args___/;Length[{args}]<2]:=Message[NumericalMethods::badarg,Bisection,Length[{args}],2];


FalsePosition::badarg="The first argument must be a function. The second an ordered list of bounds. You input was `1`.";
NumericQ[FalsePosition]^=True;
FalsePosition[f:_Function|_Symbol,{lower_,upper_},opts___Rule]/;OrderedQ[{lower,upper}]:=Block[{a=lower,b=upper,i,FA,p,FP},i=1;FA=f[a];While[i<MaxSteps/. {opts}/. Options[FalsePosition],p=b-(f[b] (a-b))/(f[a]-f[b]);FP=f[p];If[FP==0,Return[p];Break[]];i++;If[FA FP>0.,a=p;FA=FP,b=p]];Return[p]];
FalsePosition[f_,{lower_,upper_},opts___Rule]:=Message[FalsePosition::badarg,{f,{lower,upper}}];
FalsePosition[args___/;Length[{args}]<2]:=Message[NumericalMethods::badarg,FalsePosition,Length[{args}],2];


NewtonRaphson::badarg="The first argument must be a function. The second is value has the form: x_Real. Your input was '1'.";
NumericQ[NewtonRaphson]^=True;
NewtonRaphson[f:_Function|_Symbol,x_,opts___Rule]:=Block[{steps,tol,g},tol=Tolerance/. {opts}/. Tolerance->1;steps=MaxSteps/. {opts}/. MaxSteps->1;g=#1-f[#1]/Derivative[1][f][#1]&;(Which[tol!=1,FixedPointList[#1,N[#2],SameTest->(Abs[#1-#2]<tol&)],steps!=1,FixedPointList[#1,N[#2],steps],True,FixedPointList[#1,N[#2],10]]&)[g,x]];
NewtonRaphson[f_,x_,opts___Rule]:=Message[NewtonRaphson::badarg,{f,x}];
NewtonRaphson[args___/;Length[{args}]<2]:=Message[NumericalMethods::badarg,NewtonRaphson,Length[{args}],2];


End[];
EndPackage[];
