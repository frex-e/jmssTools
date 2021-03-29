(* ::Package:: *)

BeginPackage["Tools`"]
TurningPointForm::usage = "For the expression: a \!\(\*SuperscriptBox[\(x\), \(2\)]\)+b x+c, CompleteTheSquare[x,{a,b,c}] will return the equation in turning point form"
isLoaded//ClearAll
isLoaded = True

TurningPointForm //ClearAll
SetAttributes[TurningPointForm, HoldAll]
TurningPointForm[var_, expression_] :=
    Module[{out},
        x2co = Coefficient[expression, var, 2];
        xco = Coefficient[expression, var, 1];
        cons = Coefficient[expression, var, 0];
        out = x2co (var+xco/(2 x2co))^2+cons -(xco)^2/(4 x2co)
    ]



RestrictedFunction //ClearAll
SetAttributes[RestrictedFunction, HoldAll]
RestrictedFunction::usage = "f := ConditionalFunction[x,exp,condition] is equivelant to f(x)=exp, condition"
RestrictedFunction[variable_, expression_, condition_] :=
    Function[variable, ConditionalExpression[expression, condition]]


RestrictedInverse //ClearAll
SetAttributes[RestrictedInverse,HoldAll]
RestrictedInverse::usage = "For those too lazy to put Restricted function in inverse"
RestrictedInverse[variable_,expression_,condition_]:=
	InverseFunction[RestrictedFunction[variable,expression,condition]]


DetailedPlot //ClearAll
SetAttributes[DetailedPlot, HoldAll]
Options[DetailedPlot]= Flatten[Append[Options[Plot],{"Labels"-> True,"DecimalPlaces"->False,"LabelOffset"-> {-1.2,0.5}}]];
DetailedPlot::usage = "The same as Plot but with intercepts and intersections"
DetailedPlot[exp_, args__, opts:OptionsPattern[]] :=
    Module[{graph, points, exps, current, solutions, POINTSS, labels, n, i, p, y,whichlabels,labeledpoints,intercepts,intercections,stationary},
        points = <||>;
        labeledpoints={};
        solutions = {};
        graph = Plot[exp, args, Evaluate[FilterRules[{opts},Options[Plot]]]];
        exps = If[Head @ exp =!= List,
            {exp}
            ,
            exp
        ];
        whichlabels = If[Head@OptionValue["Labels"] === String,{OptionValue["Labels"]},OptionValue["Labels"]];
        AssociateTo[points,"Intercepts"-> {}];
        AssociateTo[points,"Stationary"-> {}];
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
        solutions = Solve[D[current,args[[1]]]==0&&args[[2]] <= args[[1]] <= args[[3]],args[[1]],Reals];
        Table[
            points[["Stationary"]] = Append[points[["Stationary"]], {args[[1]] /. solutions[[i]], Simplify[current/.solutions[[i]]]}]
            , {i, 1, Length[solutions]}
        ];
        
        (*Intersections*)
        AssociateTo[points,"Intercections"-> {}];
        Table[
            If[i =!= n,
                solutions = Solve[y == current && y == exps[[i]] && args[[2]] <= args[[1]] <= args[[3]], {args[[1]], y}, Reals];
                Table[
                    If[!MemberQ[points, {args[[1]], y} /. solutions[[p]]],
                        points[["Intercections"]] = Append[points[["Intercections"]], {args[[1]], y} /. solutions[[p]]]
                    ]
                    , {p, 1, Length[solutions]}
                ]
            ]
            , {i, 1, Length[exps]}
        ]
        ];
        ClearAll[bbb];
        points = DeleteCases[points,bbb_/;Head@N[bbb[[1]]]=!=Real||Head@N[bbb[[2]]]=!=Real,{2}];
        ClearAll[bbb];
        points[["Intercections"]] = FullSimplify[points[["Intercections"]]];
        points[["Stationary"]] = FullSimplify[points[["Stationary"]]];
        points[["Intercepts"]] = FullSimplify[points[["Intercepts"]]];
        
        (*Converts to decimals if DecimalPlaces\[NotEqual]False*)
        If[OptionValue["DecimalPlaces"]=!=False,
            If[OptionValue["DecimalPlaces"]===True,points=N[points],points = Round[N[points],N[1/10^OptionValue["DecimalPlaces"]]]]
        ];
        (*Draws intercepts*)
        current=points[["Intercepts"]];
        intercepts = Graphics[{PointSize[Medium],Blue,Table[Tooltip[Point[current[[i]]],current[[i]]], {i, 1, Length[current]}]}];
        If[MemberQ[whichlabels,"Intercepts"]||whichlabels,Table[labeledpoints=Append[labeledpoints,current[[i]]],{i,1,Length[current]}]];
        
        (*Draws intercections*)
        current=points[["Intercections"]];
        intercections = Graphics[{PointSize[Medium],Red,Table[Tooltip[Point[current[[i]]],current[[i]]], {i, 1, Length[current]}]}];
        If[MemberQ[whichlabels,"Intersections"]||whichlabels,Table[labeledpoints=Append[labeledpoints,current[[i]]],{i,1,Length[current]}]];
        
        (*Draws stationary*)
        current=points[["Stationary"]];
        stationary = Graphics[{PointSize[Medium],Green,Table[Tooltip[Point[current[[i]]],current[[i]]], {i, 1, Length[current]}]}];
        If[MemberQ[whichlabels,"Stationary"]||whichlabels,Table[labeledpoints=Append[labeledpoints,current[[i]]],{i,1,Length[current]}]];
        
        points = DeleteDuplicates[points];
        (*Displayes the graphics*)
        (*POINTSS = Graphics[{PointSize[Medium],Table[Tooltip[Point[points[[i]]],points[[i]]], {i, 1, Length[points]}]}];*)
        labels = Graphics[Table[
            Text[labeledpoints[[i]](*StringForm["(``,``)", labeledpoints[[i, 1]], labeledpoints[[i, 2]]]*), labeledpoints[[i]], OptionValue["LabelOffset"]],
            {i, 1, Length[labeledpoints]}
        ]];
        If[whichlabels=!=False,Show[graph, intercepts,intercections,stationary, labels],Show[graph,intercepts,intercections,stationary]]
        
    ]


SetAttributes[FindVariation,HoldAll]
FindVariation::usage="Basically a shortcut of FindFit[] for direct and inverse variation"
FindVariation[dataa_,varr_,tolerance_:0.01]:=
	If[Length[dataa[[1]]]===2,
		Module[{solution,xx,k,b},
			solution=FindFit[dataa,k*xx^b,{k,b},xx];
			If [tolerance=!=0,solution=Rationalize[solution,tolerance]];
			
			Simplify[k varr^b/.solution]
		]
	,If[Length[dataa[[1]]]===3,
		Module[{solution,xx,k,b1,b2,zz},
			solution = FindFit[dataa,k*xx^b1*zz^b2,{k,b1,b2},{xx,zz}];
			If[tolerance=!=0,solution=Rationalize[solution,tolerance]];
			Simplify[k varr[[1]]^b1 varr[[2]]^b2/.solution]
		]
	,Message[FindVariation::badargs];
	$Failed]
	]



Begin["`Private`"]


End[]
EndPackage[]
