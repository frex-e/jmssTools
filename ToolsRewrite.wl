(* ::Package:: *)
Unprotect @@ Names["Tools`*"];
ClearAll @@ Names["Tools`*"];

BeginPackage["Tools`"]
isLoaded = True;

DetailedPlot::usage = "Works like standard Plot, but includes details"


Begin["`Private`"]


(* ::Subsection:: *)
(*DetailedPlot*)

SetAttributes[DetailedPlot, HoldAll]
Options[DetailedPlot] = Flatten[Append[Options[Plot], {"Labels" -> True, "DecimalPlaces" -> False, "LabelOffset" -> {-1.2, 0.5}}]];

DetailedPlot[exp_,args__,opts:OptionsPattern[]] :=
    Module[{graph,currentExpression,exps,whichLabels,currentSolutions,points,
    Intersections,Stationary,Intercepts,var,startPoint,endPoint},

    var = args[[1]];
    startPoint = args[[2]];
    endPoint = args[[3]];


    (*Draws the first graph*)
    graph = Plot[exps, args, Evaluate[FilterRules[{opts}, Options[Plot]]]];

    (*Redefines exp as a list*)
    exps = If[Head@exp =!= List, {exp},exp];

    (* Defines the points association*)
    points = <|Intersections -> {}, Stationary -> {}, Intercepts -> {}|>;

    (*Loops through each graph*)
    Table[
        (* Sets the current expression *)
        currentExpression = exps[[i]];
        
        (* Y Intercepts*)
        Off[Power::infy];
        If[Element[currentExpression /. var -> 0,Reals],
            points[Intercepts] = Append[points[Intercepts],{0,currentExpression /. var -> 0}];
        ];
        On[Power::infy];

        (*X Intercepts*)
        currentSolutions = Solve[currentExpression == 0 && startPoint<= var <= endPoint,var,Reals];
        Table[
            points[Intercepts] = Append[points[Intercepts],{var /. currentSolutions[[n]],0}];
        ,{n,Length[currentSolutions]}];

        (*Stationary Points*)
        currentSolutions = Solve[(D[currentExpression,var]// Evaluate) == 0 && startPoint <= var <= endPoint,var, Reals];
        Table[
            points[Stationary] = Append[points[Stationary],{var /. currentSolutions[[n]],Simplify[currentExpression /. currentSolutions[[n]]]}];
        ,{n,1,Length[currentSolutions]}];


    ,{i,Length[exps]}];
    points
    ]
End[]

Protect @@ Names["Tools`*"];

EndPackage[]
