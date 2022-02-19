# jmssTools

### __A Collection of Mathematica Functions - Useful for VCE Mathematical Methods__

## Usage:
1. Clone this repo or click [here](https://github.com/frex-e/jmssTools/archive/master.zip) for a zip folder
2. Extract the zip file.
3. Read the documentation.nb file. (Please do read it. I spent "a lot" of time on it)
4. Open / Create a new Mathematica booklet in the same directory
5. Run in the booklet:
 ```If[! ValueQ[Tools`isLoaded], SetDirectory[NotebookDirectory[]]; << Tools.wl; "Tools.ws loaded", "Tools.ws already loaded"]```
6. All commands should work in your notebook until mathematica is quit
7. To makes sure that the package has loaded correctly, run a command like the one below:
 ```?TurningPointForm```

**NOTE:** *Also, if the software malfunctions, resulting in you losing a mark, I'm honestly really sorry, but I'm not a professional programmer, I'm not even that good at it, I'm just doing this for fun in my free time, so use this at your own risk.*

## Commands:

### FindVariation:
Gives you the variation for a given set of data. Note: you need generally need at least 3 data points  
In= ```FindVariation[{{1, 5}, {8, 2.5}, {64, 1.25}}, x]```  
Out= ```5/x^(1/3)```

Optional argument for the tolerances of rationalizing, to turn rationalizing off, put 0 there instead.  
In= ```FindVariation[{{1, 5}, {8, 2.5}, {64, 1.25}}, x, 0]```  
Out= ```5./x^0.333333```

You can also do joint variation. (Make sure you put data, and variables in the same order)  
I.e `FindVariation[{x1,y1,result1},...},{x,y}]`  
In= ```FindVariation[{{2, 10, 15/2}, {3, 4, 4/3}, {5, 50, 6}}, {x, z}]```  
Out= ```(3 z)/x^2```

An example of what happens when you mess with tolerances:  
In= ```FindVariation[{{2, 10, 15/2}, {3, 4, 4/3}, {5, 50, 6}}, {x, z}, 1]```  
Out= ```(2 z)/x```  

### DetailedPlot
Works the same as Plot, but adds intercepts, intersections, and stationary points.  
In= ```DetailedPlot[x + 1, {x, -5, 5}]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot8.png)

Types of points are colour coded:
- Intersections: Red
- Stationary Points: Green
- Axes Intercepts: Blue

In the case where 1 point may be 2 types, priority is Stationary Points > Intersections > Axes Intercepts

In= ```DetailedPlot[{x^2 + 2, 2 x + 5}, {x, -5, 5}]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot7.png)

If you you wish to see labels in decimal form, use DecimalPlaces -> True, or DecimalPlaces -> n, for n number of decimal places
In= ```DetailedPlot[Sin[x] + 1/2, {x, -\[Pi], \[Pi]}, DecimalPlaces -> True]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot6.png)

In= ```DetailedPlot[Sin[x] + 1/2, {x, -\[Pi], \[Pi]}, DecimalPlaces -> 2]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot5.png)

As you can see, it can get a little cluttered:  
In= ```DetailedPlot[{Sin[x], Cos[x]}, {x, 0, 10}]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot4.png)

If labels are appearing off the graph, you can double click the graph, and drag them back on.

If it gets too cluttered, you can filter which types of labels are shown with the Labels argument:  
In= ```DetailedPlot[{Sin[x], Cos[x]}, {x, 0, 10}, Labels -> "Intersections"]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot3.png)

Valid arguments are "Intercepts", "Intersections", or "Stationary" (For stationary points)  
If you wish to show 2 types of labels, simply use a list.  
In= ```DetailedPlot[{Sin[x], Cos[x]}, {x, 0, 10}, Labels -> {"Intersections", "Stationary"}]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot2.png)

You can also change the offset of the labels, with the argument LabelOffset -> {x,y}  
(It might not move the direction you expect. Play around with it a bit first.)  
In= ```DetailedPlot[{Sin[x], Cos[x]}, {x, 0, 10}, Labels -> {"Intersections", "Stationary"}, LabelOffset -> {1, 1}]```
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/DetailedPlot1.png)

### TurningPointForm
Converts a quadratic into turning point form.  
In= ```TurningPointForm[x, x^2 + 16 x + 9]```  
Out= ```-55 + (8 + x)^2```

Don't put something that isn't a quadratic, it will make me very sad :(  
In= ```TurningPointForm[x, 2]```
```
During evaluation of In[25]:= Power::infy: Infinite expression 1/0 encountered.

During evaluation of In[25]:= Infinity::indet: Indeterminate expression 0 ComplexInfinity encountered.

During evaluation of In[25]:= Power::infy: Infinite expression 1/0 encountered.

During evaluation of In[25]:= Infinity::indet: Indeterminate expression 0 ComplexInfinity encountered.
```
Out= ```Indeterminate```

### RestrictedFunction
Creates a function with a restricted domain/condition.  
In= `RestrictedFunction[var, expresion, condition]`  
In= `RestrictedFunction[x, x + 1, x > 1]`  
Out= `Function[x, [1 + x if x > 1]]`  

In= `f := RestrictedFunction[x, x + 1, x > 1]`  
In= `f[t]`  
Out= `1 + t if t > 1`

In= `f[0]`  
Out= `Undefined`

For the purposes of an inverse function:  
In= `h1 := RestrictedFunction[x, (x - 2)^2 + 2, x >= 2]`  
In= `Plot[{h1[x], InverseFunction[h1][x]}, {x, 0, 4}, AspectRatio -> Automatic, PlotLegends -> "Expressions", PlotRange -> {0, 4}, AxesOrigin -> {0, 0}]`

![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/RestrictedFunction2.png?raw=true)

In= `h2 := RestrictedFunction[x, (x - 2)^2 + 2, x <= 2]`  
In= `Plot[{h2[x], InverseFunction[h2][x]}, {x, 0, 4}, AspectRatio -> Full, PlotLegends -> "Expressions"]`
![](https://raw.githubusercontent.com/frex-e/jmssTools/master/images/RestrictedFunction1.png?raw=true)

### SolveTriangle
This will solve for missing side lengths and angles of a triangles. At least 3 values and 1 side length is necessary to solve.  
**NOTE:** *Function is set to use degrees by default; Mathematica defaults to radians usually*

In= `SolveTriangle[{a, b, c}, {A, B, CC}]`

a,b and c are side lengths.  
A,B, and CC are angles.  
(CC is used because C is a built in variable)

Sine Rule Question  
In= `SolveTriangle[{12, b, c}, {A, 59, 73}] // N`  
Out= `{{A -> 48., b -> 13.8412, c -> 15.442}}`

Cosine Rule Question  
In= `SolveTriangle[{a, 16, 30}, {60, B, CC}] // N`  
Out= `{{a -> 26., B -> 32.2042, CC -> 87.7958}}`

You can also use radians with the settings Degrees -> False  
In= `SolveTriangle[{a, 16, 30}, {Pi /3, B, CC}, Degrees -> False] // N`  
Out= `{{a -> 26., B -> 0.56207, CC -> 1.53233}}`
