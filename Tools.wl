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


SetAttributes[DetailedPlot,HoldAll]
DetailedPlot::usage = "The same as Plot but with intercepts"
DetailedPlot[exp_,args__, opts : OptionsPattern[Plot]]:=
	Module[{graph,points,exps,current,solutions,POINTSS,labels},
		points = {};
		solutions = {};
		graph = Plot[exp,args,opts];
		exps = If[Head@exp =!= List,
			{exp},exp];
		For [n = 1,n<= Length[exps],n++,
			current = exps[[n]];
			(*Y-int*)
			points = Append[points,{0,current/.args[[1]]->0}];
			
			(*X-int*)
			solutions = Solve[current==0&&args[[2]]<= args[[1]]<= args[[3]],args[[1]],Reals];
			Table[
				points = Append[points,{args[[1]]/.solutions[[i]],0}]
			,{i,1,Length[solutions]}];
			
			solutions = {};
			(*Intersections*)
			Table[
			If[i=!=n,
				solutions = Solve[y==current&&y==exps[[i]]&&args[[2]]<= args[[1]]<= args[[3]],{args[[1]],y},Reals];
				Table[
					points = Append[points,{args[[1]],y}/.solutions[[p]]]
				,{p,1,Length[solutions]}]
			]
			,{i,1,Length[exps]}]
		];
		POINTSS = Graphics[Table[Point[points[[i]]],{i,1,Length[points]}]];
		
		labels = Graphics[Table[
				Text[points[[i]],points[[i]],{-1.2,0}],
				{i,1,Length[points]}]];
		Show[graph,POINTSS,labels]
]


Begin["`Private`"]


End[]
EndPackage[]
