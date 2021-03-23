(* ::Package:: *)

BeginPackage["Tools`"]
TurningPointForm::usage = "For the expression: a \!\(\*SuperscriptBox[\(x\), \(2\)]\)+b x+c, CompleteTheSquare[x,{a,b,c}] will return the equation in turning point form"

SetAttributes[CompleteTheSquare, HoldAll]
TurningPointForm[var_, expression_] :=
    Module[{out},
        x2co = Coefficient[expression, var, 2];
        xco = Coefficient[expression, var, 1];
        cons = Coefficient[expression, var, 0];
        out = x2co (var+xco/(2 x2co))^2+cons -(xco)^2/(4 x2co)
    ]



SetAttributes[RestrictedFunction, HoldAll]
RestrictedFunction::usage = "f := ConditionalFunction[x,exp,condition] is equivelant to f(x)=exp, condition"
RestrictedFunction[variable_, expression_, condition_] :=
    Function[variable, ConditionalExpression[expression, condition]]


SetAttributes[RestrictedInverse,HoldAll]
RestrictedInverse::usage = "For those too lazy to put Restricted function in inverse"
RestrictedInverse[variable_,expression_,condition_]:=
	InverseFunction[RestrictedFunction[variable,expression,condition]]


Begin["`Private`"]


End[]
EndPackage[]
