(* ::Package:: *)

ClearAll["Tools`*"]
BeginPackage["Tools`"]
TurningPointForm::usage = "For the expression: a \!\(\*SuperscriptBox[\(x\), \(2\)]\)+b x+c, CompleteTheSquare[x,{a,b,c}] will return the equation in turning point form";
RestrictedFunction::usage = "f := ConditionalFunction[x,exp,condition] is equivelant to f(x)=exp, condition";
RestrictedInverse::usage = "For those too lazy to put Restricted function in inverse";
DetailedPlot::usage = "The same as Plot but with intercepts and intersections";
FindVariation::usage = "Basically a shortcut of FindFit[] for direct and inverse variation";
SolveTriangle::usage = "Finds the missing angles and side lengths of a triangle";
FTest::usage = "Finds whether equations using a given function are true"
isLoaded//ClearAll
isLoaded = True;
Begin["`Private`"]



(* ::Subsection:: *)
(*FTest*)


(* ::Text:: *)
(*I still have no idea how this works*)


SetAttributes[FTest,HoldAll]
FTest[exp_,equations_,var_,func_]:=
	Module[{solutions,f,eq},
	ClearAll[func];
	eq = If[Head@equations =!= List,{equations},equations];
	
	solutions = Table[func:=Function[var,exp];If[FullSimplify[eq[[i]]]===True,ClearAll[func];StringForm["`` is True",eq[[i]]],ClearAll[func];StringForm["`` is False",eq[[i]]]],{i,1,Length[eq]}];
	ClearAll[func];
	TableForm[solutions]
	]


(* ::Subsection::Closed:: *)
(*SolveTriangle*)


Options[SolveTriangle]={"Degrees"-> True,"Area"-> False};
SetAttributes[SolveTriangle,HoldAll]
SolveTriangle[sides_,angles_,OptionsPattern[]]:=
	Module[{a,b,c,A,B,CC,solutions,vars},
	vars = {};
	a = sides[[1]];
	b = sides[[2]];
	c = sides[[3]];
	A = angles[[1]];
	B = angles[[2]];
	CC = angles[[3]];
	Table[
		If[Head@sides[[i]]===Symbol,vars = Append[vars,sides[[i]]]];
		If[Head@angles[[i]]===Symbol,vars = Append[vars,angles[[i]]]]
	,{i,1,3}];
	If[OptionValue["Degrees"],
	solutions = Solve[a/Sin[A \[Degree]]==b/Sin[B \[Degree]]&&b/Sin[B \[Degree]]==c/Sin[CC \[Degree]]&&0< A <180&&0 < B < 180&& 0 < CC < 180&&A + B + CC ==180  ,vars,Reals],
	solutions = Solve[a/Sin[A]==b/Sin[B ]&&b/Sin[B ]==c/Sin[CC]&&0< A <Pi&&0 < B < Pi&& 0 < CC < Pi&&A + B + CC ==Pi  ,vars,Reals]
	];
	If[OptionValue["Area"],(a/2*b*Sin[CC*If[OptionValue["Degrees"],Degree,1]])/.FullSimplify[solutions],solutions = FullSimplify[solutions]]
	

	]
	


SetAttributes[TurningPointForm, HoldAll]
TurningPointForm[var_, expression_] :=
    Module[{out,exp},
        exp = Expand[expression];
        x2co = Coefficient[expression, var, 2];
        xco = Coefficient[expression, var, 1];
        cons = Coefficient[expression, var, 0];
        out = x2co (var+xco/(2 x2co))^2+cons -(xco)^2/(4 x2co);
        out
    ]



(* ::Subsection:: *)
(*RestrictedFunction*)


SetAttributes[RestrictedFunction, HoldAll]

RestrictedFunction[variable_, expression_, condition_] :=
    Function[variable, ConditionalExpression[expression, condition]]


(* ::Subsection::Closed:: *)
(*RestrictedInverse*)


SetAttributes[RestrictedInverse,HoldAll]

RestrictedInverse[variable_,expression_,condition_]:=
	InverseFunction[RestrictedFunction[variable,expression,condition]]


(* ::Subsection::Closed:: *)
(*DetailedPlot*)


(* ::Text:: *)
(*To anyone who may look at this in the future, my sincere apologies.*)


SetAttributes[DetailedPlot, HoldAll]
Options[DetailedPlot] = Flatten[Append[Options[Plot], {"Labels" -> True, "DecimalPlaces" -> False, "LabelOffset" -> {-1.2, 0.5}}]];
DetailedPlot[exp_, args__, opts:OptionsPattern[]] :=
    Module[{graph, points, exps, current, solutions, POINTSS, labels, n, i, p, y, whichlabels, labeledpoints, intercepts, intercections, stationary},
        points = <||>;
        labeledpoints = {};
        solutions = {};
        graph = Plot[exp, args, Evaluate[FilterRules[{opts}, Options[Plot]]]];
        exps = If[Head @ exp =!= List,
            {exp}
            ,
            exp
        ];
        whichlabels = If[Head @ OptionValue["Labels"] === String,
            {OptionValue["Labels"]}
            ,
            OptionValue["Labels"]
        ];
        AssociateTo[points, "Intercections" -> {}];
        AssociateTo[points, "Intercepts" -> {}];
        AssociateTo[points, "Stationary" -> {}];
        For [n = 1, n <= Length[exps], n++,
        current = exps[[n]];
        (*Y-int*)
        points[["Intercepts"]] = Append[points[["Intercepts"]], {0, current /. args[[1]] -> 0}];
        (*X-int*)
        solutions = Solve[current == 0 && args[[2]] <= args[[1]] <= args[[3]], args[[1]], Reals];
        Table[
            points[["Intercepts"]] = Append[points[["Intercepts"]], {args[[1]] /. solutions[[i]], 0}]
            , {i, 1, Length[solutions]}
        ];
        solutions = {};
        (*Turning and stationary points*)
        solutions = Solve[D[current, args[[1]]] == 0 && args[[2]] <= args[[1]] <= args[[3]], args[[1]], Reals];
        Table[
            points[["Stationary"]] = Append[points[["Stationary"]], {args[[1]] /. solutions[[i]], Simplify[current /. solutions[[i]]]}]
            , {i, 1, Length[solutions]}
        ];
        (*Intersections*)
        Table[
            If[i =!= n,
                solutions = Solve[yy == current && yy == exps[[i]] && args[[2]] <= args[[1]] <= args[[3]], {args[[1]], yy}, Reals];
                Table[
                    If[!MemberQ[points, {args[[1]], yy} /. solutions[[p]]],
                        points[["Intercections"]] = Append[points[["Intercections"]], {args[[1]], yy} /. solutions[[p]]]
                    ]
                    , {p, 1, Length[solutions]}
                ]
            ]
            , {i, 1, Length[exps]}
        ]
        ];
        ClearAll[bbb];
        points = DeleteCases[points, bbb_ /; Head @ N[bbb[[1]]] =!= Real || Head @ N[bbb[[2]]] =!= Real, {2}];
        ClearAll[bbb];
        
        (*Simplifies points*)
        points[["Intercections"]] = FullSimplify[points[["Intercections"]]];
        points[["Stationary"]] = FullSimplify[points[["Stationary"]]];
        points[["Intercepts"]] = FullSimplify[points[["Intercepts"]]];
        (*Removes duplicates*)
        points[["Intercections"]] = DeleteDuplicates[points[["Intercections"]]];
        points[["Stationary"]] = DeleteDuplicates[points[["Stationary"]]];
        points[["Intercepts"]] = DeleteDuplicates[points[["Intercepts"]]];
        (*Draws intercepts*)
        current = points[["Intercepts"]];
        intercepts = Graphics[{PointSize[Medium], Blue, Table[Tooltip[Point[current[[i]]], If[OptionValue["DecimalPlaces"] === False,
            current[[i]]
            ,
            If[OptionValue["DecimalPlaces"] === True,
                N[current[[i]]]
                ,
                Round[N[current[[i]]], N[1 / 10^OptionValue["DecimalPlaces"]]]
            ]
        ]], {i, 1, Length[current]}]}];
        If[MemberQ[whichlabels, "Intercepts"] || whichlabels,
            Table[labeledpoints = Append[labeledpoints, current[[i]]], {i, 1, Length[current]}]
        ];
        (*Draws intercections*)
        current = points[["Intercections"]];
        intercections = Graphics[{PointSize[Medium], Red, Table[Tooltip[Point[current[[i]]], If[OptionValue["DecimalPlaces"] === False,
            current[[i]]
            ,
            If[OptionValue["DecimalPlaces"] === True,
                N[current[[i]]]
                ,
                Round[N[current[[i]]], N[1 / 10^OptionValue["DecimalPlaces"]]]
            ]
        ]], {i, 1, Length[current]}]}];
        If[MemberQ[whichlabels, "Intersections"] || whichlabels,
            Table[labeledpoints = Append[labeledpoints, current[[i]]], {i, 1, Length[current]}]
        ];
        (*Draws stationary*)
        current = points[["Stationary"]];
        stationary = Graphics[{PointSize[Medium], Green, Table[Tooltip[Point[current[[i]]], If[OptionValue["DecimalPlaces"] === False,
            current[[i]]
            ,
            If[OptionValue["DecimalPlaces"] === True,
                N[current[[i]]]
                ,
                Round[N[current[[i]]], N[1 / 10^OptionValue["DecimalPlaces"]]]
            ]
        ]], {i, 1, Length[current]}]}];
        If[MemberQ[whichlabels, "Stationary"] || whichlabels,
            Table[labeledpoints = Append[labeledpoints, current[[i]]], {i, 1, Length[current]}]
        ];
        (*Converts to decimals if DecimalPlaces\[NotEqual]False*)
        If[OptionValue["DecimalPlaces"] =!= False,
            If[OptionValue["DecimalPlaces"] === True,
                labeledpoints = N[labeledpoints]
                ,
                labeledpoints = Round[N[labeledpoints], N[1 / 10^OptionValue["DecimalPlaces"]]]
            ]
        ];
        (*Displayes the graphics*)
        (*POINTSS = Graphics[{PointSize[Medium],Table[Tooltip[Point[points[[i]]],points[[i]]], {i, 1, Length[points]}]}];*)
        labels = Graphics[Table[
            Text[labeledpoints[[i]](*StringForm["(``,``)", labeledpoints[[i, 1]], labeledpoints[[i, 2]]]*), labeledpoints[[i]], OptionValue["LabelOffset"]],
            {i, 1, Length[labeledpoints]}
        ]];
        If[whichlabels =!= False,
            Show[graph, intercepts, intercections, stationary, labels]
            ,
            Show[graph, intercepts, intercections, stationary]
        ]
    ]


(* ::Subsection::Closed:: *)
(*FindVariation*)


SetAttributes[FindVariation, HoldAll]
FindVariation[dataa_, varr_, tolerance_:0.01] :=
    If[Length[dataa[[1]]] === 2,
        Module[{solution, xx, k, b},
            solution = FindFit[dataa, k * xx^b, {k, b}, xx];
            If [tolerance =!= 0, solution = Rationalize[solution, tolerance]];
            Simplify[k varr^b /. solution]
        ]
        ,
        If[Length[dataa[[1]]] === 3,
            Module[{solution, xx, k, b1, b2, zz},
                solution = FindFit[dataa, k * xx^b1 * zz^b2, {k, b1, b2}, {xx, zz}];
                If[tolerance =!= 0,
                    solution = Rationalize[solution, tolerance]
                ];
                Simplify[k varr[[1]]^b1 varr[[2]]^b2 /. solution]
            ]
            ,
            Message[FindVariation::badargs];
            $Failed
        ]
    ]


(* ::Subsection:: *)
(*End Statements*)


End[]
EndPackage[]
