(* ::Package:: *)

(* ::Code::Initialization::Plain:: *)
(* PACKAGE.M
 * Progetto d'esame di Matematica Computazionale + Calcolo Numerico e Software Didattico
 * Corsi di laurea magistrale in Informatica e Matematica
 * Anno accademico 2017/2018
 * 
 * Autori:
 *   Simone Bertolini, Alessio Francesconi, Matteo Grillini
 *
 * Mathematica Version:
 * 	 11.1
 *)
 
BeginPackage[ "MClib`"];

	(* Package Beginning*)

	Unprotect["MClib`*"] (* Remove functions redefinition protection *)
	ClearAll["MClib`*"];
	
	(* Usage Definitions *)
		getP1::usage="[G1, q=0] Variable that indicates that user has already chosen something"
		getP2::usage="[G1, m!=0] Variable that indicates that user has already chosen something"
		getP3::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		getP4::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		getP5::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		getP6::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		getP7::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		getP8::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		getP9::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		getTeacherEQ::usage="Exercises solutions list"
		getUserAnswer::usage="List of all the user answer"
		getExercises::usage="List containing all exercises for each grade and kind (solution + errors)"
		getEnableAnswer::usage="List of all boolean variable that disable exercises after having done them"
		getCorrectCounter::usage="Number of correct answer"
		getClicked::usage="click first verify"
		getClicked2::usage="click first verify"
		getClicked2B::usage="click first verify"
		getClicked3B::usage="click first verify"
		getClicked3::usage="click first verify"
		getClicked1b::usage="click second verify"
		
		
		
		
		setP1::usage="[G1, q=0] Variable that indicates that user has already chosen something"
		setP2::usage="[G1, m!=0] Variable that indicates that user has already chosen something"
		setP3::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		setP4::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		setP5::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		setP6::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		setP7::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		setP8::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		setP9::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		setClicked::usage="click first verify"
		setClicked2::usage="click first verify"
		setClicked2B::usage="click first verify"
		setClicked3B::usage="click first verify"
		setClicked3::usage="click first verify"
		setClicked1b::usage="click second verify"
		
		load::usage = "Load initial variables randomly"
		
		loadFiles::usage = "Load initial variables from files and randomly"
	
		checkAnswer::usage = "Check the answer for 1 exercise Kind
			\n@row_@ exercises row index"
	
		exKindAPrinter::usage = "Generate form Kind A for students
			\n@row_@ exercises row index"
			
		exKindBPrinter::usage = "Generate form Kind B for students
			\n@row_@ exercises row index"
	
		generateExercisesG1::usage="Generate a list of all exercises GRADE 1 (solutions + errors)
			\n@l_@ List of solutions"
			
		generateExercisesG2::usage="Generate a list of all exercises GRADE 2 (solutions + errors)
			\n@l_@ List of solutions"
			
		generateExercisesG3::usage="Generate a list of all exercises GRADE 3 (solutions + errors)
			\n@l_@ List of solutions"
	
	
		solutionReadAndRandomFill::usage = "This function load exercises from a file and completes the excercises list adding Random ones till minimum number is reached
			\n@file_@ File to load
			\n@grade_@ Grade o the function {1..N}
			\n@numMin_@ Minimum number of excercises {1..N}"
			
			
		randomFill::usage = "This function completes the excercises list adding Random ones till minimum number is reached
			\n@grade_@ Grade o the function {1..N}
			\n@numMin_@ Minimum number of excercises {1..N}"
	 
		plotWithZoomButtons::usage = "Draw a Graph with zoom buttons inside
			\n@n_@ Function to plot
			\n@x_@ Symbol that has to be used in order to resolve the interal equations"
		
		plotWithZoomButtonsA::usage = "Draw a Graph with zoom buttons inside
			\n@n_@ Function to plot
			\n@x_@ Symbol that has to be used in order to resolve the interal equations"	
		
		plotWithZoomButtonsB::usage = "Draw a Graph with zoom buttons inside
			\n@n_@ Function to plot
			\n@x_@ Symbol that has to be used in order to resolve the interal equations"	
			
		plotWithZoomButtonsC::usage = "Draw a Graph with zoom buttons inside
			\n@n_@ Function to plot
			\n@x_@ Symbol that has to be used in order to resolve the interal equations"		
			
		plotWithZoomButtonsT::usage = "Draw a teacherEQ Graph with zoom buttons inside
			\n@row_@ row index
			\n@col@ col index
			\n@x_@ Symbol that has to be used in order to resolve the interal equations"
			
		plotWithZoomButtonsE::usage = "Draw a exercises Graph with zoom buttons inside
			\n@row_@ row index
			\n@col@ col index
			\n@pos@ pos index
			\n@x_@ Symbol that has to be used in order to resolve the interal equations"
	
		hyperText::usage = "Creating a cliccable text. The click trig a MessageDialog
			\n@txt@ Text to show
			\n@msg@ Message in the MessageDialog
			\n@curs@ Cursor type on mouse over"

		d::usage = "Print a form with all the functions that the teacher is inserting in the list
			\n@l_@ List to analize"
	 
		resetGrade::usage = "Reset variables for the given grade
			\n@g_@ List to analize"	
	 
	 
	(* End of Usage Definitions *)
	
	Begin["`Private`"]; 
	
		(* Beginning of Private functions definition space *)
		
			(* Variables *)
			
				p1 = 0 (* Variable that indicates that user has already chosen something [G1, q=0] *)
				p2 = 0 (* Variable that indicates that user has already chosen something [G1, m!=0] *)
				p3 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
				p4 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
				p5 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
				p6 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
				p7 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
				p8 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
				p9 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
				clicked = 0
				clicked2 = 0
				clicked1b = 0
				clicked2B = 0
				clicked3 = 0;
				clicked3B = 0;
				testo = "";
				MINNUMBEROFEXERCISES = 3 (* CONSTANT- Minimum number of exercises per module *)		
				
				teacherEQ = { } (* Exercises solutions list *)
				
				exercises = { } (* List containing all exercises for each grade and kind (solution + errors) *)
								
				userAnswer = { } (* List of all user answer to the questions *)
				
				enableAnswer = { } (* Forms enabler *)
				
				correctCounter = 0 (* Number of correct answer *)
			
			(* /Variables *)
		

			(* Functions *)	
				
				(* Getter Private Variables*)
				getP1[] := Module[{},p1]
				getP2[] := Module[{},p2]
				getP3[] := Module[{},p3]
				getP4[] := Module[{},p4]
				getP5[] := Module[{},p5]
				getP6[] := Module[{},p6]
				getP7[] := Module[{},p7]
				getP8[] := Module[{},p8]
				getP9[] := Module[{},p9]
				getTeacherEQ[] := Module[{},teacherEQ]
				getUserAnswer[] := Module[{},userAnswer]
				getExercises[] := Module[{},exercises]
				getEnableAnswer[] := Module[{},enableAnswer]
				getCorrectCounter[] := Module[{},correctCounter]			
				getClicked[] := Module[{},clicked]
				getClicked2[] := Module[{},clicked2]
				getClicked2B[] := Module[{},clicked2B]
				getClicked3B[] := Module[{},clicked3B]
				getClicked3[] := Module[{},clicked3]
				getClicked1b[] := Module[{},clicked1b]
				(* End of Getter Private Variables*)
				
				(* Setter Private Variables*)
				setP1[v_] := Module[{},p1=v]
				setP2[v_] := Module[{},p2=v]
				setP3[v_] := Module[{},p3=v]
				setP4[v_] := Module[{},p4=v]
				setP5[v_] := Module[{},p5=v]
				setP6[v_] := Module[{},p6=v]
				setP7[v_] := Module[{},p7=v]
				setP8[v_] := Module[{},p8=v]
				setP9[v_] := Module[{},p9=v]
				setClicked[v_] := Module[{},clicked=v]
				setClicked2[v_] := Module[{},clicked2=v]
				setClicked2B[v_] := Module[{},clicked2B=v]
				setClicked3B[v_] := Module[{},clicked3B=v]
				setClicked3[v_] := Module[{},clicked3=v]
				setClicked1b[v_] := Module[{},clicked1b=v]
				(* End of Setter Private Variables*)

				
				(* Load initial variables from files and it fill them randomly *)
				loadFiles[] := Module[{},
					teacherEQ = { };				    
					exercises = { };				    
					userAnswer = { };				    
					enableAnswer = { };
					correctCounter = 0;
					
					AppendTo[teacherEQ,solutionReadAndRandomFill["g1A.txt",1,MINNUMBEROFEXERCISES]]; (* Exercises Grade 1 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g1B.txt",1,MINNUMBEROFEXERCISES]]; (* Exercises Grade 1 solutions list of Exercise Kind B *)				
					AppendTo[teacherEQ,solutionReadAndRandomFill["g2A.txt",2,MINNUMBEROFEXERCISES]]; (* Exercises Grade 2 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g2B.txt",2,MINNUMBEROFEXERCISES]]; (* Exercises Grade 2 solutions list of Exercise Kind B *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g3A.txt",3,MINNUMBEROFEXERCISES]]; (* Exercises Grade 3 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g3B.txt",3,MINNUMBEROFEXERCISES]]; (* Exercises Grade 3 solutions list of Exercise Kind B *)
					
					AppendTo[exercises,generateExercisesG1[teacherEQ[[1]]]]; (* Grade 1 Kind A *)
					AppendTo[exercises,generateExercisesG1[teacherEQ[[2]]]]; (* Grade 1 Kind B *)
					AppendTo[exercises,generateExercisesG2[teacherEQ[[3]]]]; (* Grade 2 Kind A *)
					AppendTo[exercises,generateExercisesG2[teacherEQ[[4]]]]; (* Grade 2 Kind B *)	
					AppendTo[exercises,generateExercisesG3[teacherEQ[[5]]]]; (* Grade 3 Kind A *)
					AppendTo[exercises,generateExercisesG3[teacherEQ[[6]]]]; (* Grade 3 Kind B *)	
					
					userAnswer = Table[0, 6, MINNUMBEROFEXERCISES];
					enableAnswer = Table[True, 6, MINNUMBEROFEXERCISES];
				]
				
				
				(* Load initial variables randomly *)
				load[] := Module[{}, 
					teacherEQ = { };				    
				    exercises = { };				    
				    userAnswer = { };				    
				    enableAnswer = { };		
					correctCounter = 0;				
				
					AppendTo[teacherEQ,randomFill[1,MINNUMBEROFEXERCISES]]; (* Exercises Grade 1 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,randomFill[1,MINNUMBEROFEXERCISES]]; (* Exercises Grade 1 solutions list of Exercise Kind B *)				
					AppendTo[teacherEQ,randomFill[2,MINNUMBEROFEXERCISES]]; (* Exercises Grade 2 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,randomFill[2,MINNUMBEROFEXERCISES]]; (* Exercises Grade 2 solutions list of Exercise Kind B *)
					AppendTo[teacherEQ,randomFill[3,MINNUMBEROFEXERCISES]]; (* Exercises Grade 3 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,randomFill[3,MINNUMBEROFEXERCISES]]; (* Exercises Grade 3 solutions list of Exercise Kind B *)
					
					AppendTo[exercises,generateExercisesG1[teacherEQ[[1]]]]; (* Grade 1 Kind A *)
					AppendTo[exercises,generateExercisesG1[teacherEQ[[2]]]]; (* Grade 1 Kind B *)
					AppendTo[exercises,generateExercisesG2[teacherEQ[[3]]]]; (* Grade 2 Kind A *)
					AppendTo[exercises,generateExercisesG2[teacherEQ[[4]]]]; (* Grade 2 Kind B *)	
					AppendTo[exercises,generateExercisesG3[teacherEQ[[5]]]]; (* Grade 3 Kind A *)
					AppendTo[exercises,generateExercisesG3[teacherEQ[[6]]]]; (* Grade 3 Kind B *)	
					
					userAnswer = Table[0, 6, MINNUMBEROFEXERCISES];
					enableAnswer = Table[True, 6, MINNUMBEROFEXERCISES];
				]
				
				
				(* reset variables for the given grade *)
				resetGrade[grade_, count_] := Module[
									{},
									teacherEQ[[grade*2-1]] = randomFill[grade,MINNUMBEROFEXERCISES];
									teacherEQ[[grade*2]] = randomFill[grade ,MINNUMBEROFEXERCISES];
									If[grade === 1,
										exercises[[grade*2-1]] = generateExercisesG1[teacherEQ[[1]]];
										exercises[[grade*2]] = generateExercisesG1[teacherEQ[[2]]]]
										
										If[grade === 2,
											exercises[[grade*2-1]] = generateExercisesG2[teacherEQ[[3]]];
											exercises[[grade*2]] = generateExercisesG2[teacherEQ[[4]]],
										] 
										If[grade === 3,
											exercises[[grade*2-1]] = generateExercisesG3[teacherEQ[[5]]];
											exercises[[grade*2]] = generateExercisesG3[teacherEQ[[6]]]
										] 
									;
									enableAnswer[[grade*2-1]] =	Table[True, MINNUMBEROFEXERCISES];	
									enableAnswer[[grade*2]] = Table[True, MINNUMBEROFEXERCISES];	
									userAnswer[[grade*2-1]] = Table[0, MINNUMBEROFEXERCISES];	
									userAnswer[[grade*2]] =	Table[0, MINNUMBEROFEXERCISES];									
									correctCounter = correctCounter - count;
								]
			
			
				(* Check answer of section row *)
				checkAnswer[row_] := Module[
										{StringAnswer = ""},										
										For[i = 1, i <= Length[teacherEQ[[row]]], i++,
											If[userAnswer[[row,i]] === teacherEQ[[row,i]],
												StringAnswer = StringJoin[StringAnswer, StringJoin[StringJoin["\nEs #", ToString[i]], ": Risposta corretta"]];(* 1 is correct *)
												enableAnswer[[row,i]] = False; correctCounter++;(* Disable retring *),
												If[userAnswer[[row,i]] === 0,
													StringAnswer = StringJoin[StringJoin["Domanda ", ToString[i]], " non completata"];(* 1 answer has not be given *)
													enableAnswer[[row,i]] = True; (* Enable retring *) 
													Break[], 
													StringAnswer = StringJoin[StringAnswer, "\nEs #", ToString[i], ": Risposta errata. Quella corretta era     ", ToString[teacherEQ[[row,i]],InputForm]]; (* 1 error*) 
													enableAnswer[[row,i]] = False(* Disable retring *)
												]
											]
										];
										MessageDialog[StringAnswer](* Output the solution *)						 								
									]
			
				
				(* Draw Exercises KIND A of a grade *)
				exKindAPrinter[row_] := Module[
											{},										
											Column[
												Table[
													With[{i = i}, 
														Row[{
															Panel[
																Column[{
																If[row === 1, testo ="A quale funzione corrisponde la seguente retta?";] ;
																If[row === 3, testo= "A quale funzione corrisponde la seguente parabola?";]; 
																If[row === 5, testo= "A quale funzione corrisponde la seguente cubica?";];
																	Style[testo, FontWeight -> Bold], 
																	plotWithZoomButtonsT[row,i, Symbol["x"]],
																	Dynamic[
																		RadioButtonBar[
																			Dynamic[userAnswer[[row,i]]], 
																			exercises[[row,i]], 
																			Enabled -> Dynamic[enableAnswer[[row,i]]]
																		]
																	]
																}]
															]
														}]
													],
													{i, Length[exercises[[row]]]}
												]
											]
										];
			
			
			
				(* Draw Exercises KIND B of a grade *)
				exKindBPrinter[row_] := Module[
											{},
											Column[
												Table[
													With[
														{i = i},
														Row[{
															Panel[
																Column[{
																	Dynamic[
																	
																			Style[
																			Text["Seleziona tramite checkbox il grafico della funzione "]
																			Text[teacherEQ[[row,i]]],
																			FontWeight->Bold
																			]
																		
																	],
																	Row[
																		Table[
																			With[
																				{j = j},
																				Row[{														 
																					Dynamic[
																						RadioButton[
																							Dynamic[userAnswer[[row,i]]], 
																							exercises[[row,i,j]], 
																							Enabled -> Dynamic[enableAnswer[[row,i]]]
																						]
																					],
																					plotWithZoomButtonsE[row,i,j, Symbol["x"]]
																				}]
																			],
																			{j, Length[exercises[[row,i]]]}
																		]
																	]
																}]
															]
														}]
													],
													{i, Length[exercises[[row]]]}
												]
											]											
										];
			
			
				(* This function load exercises from a file and complete the exercises list adding Random ones till minimum number is reached *)
				solutionReadAndRandomFill[file_,grade_,numMin_] := Module[
																		{return = {},try,len = 0},	
																		return =  ReadList[file]; (* Reading the file *)																		
																		If[Length[return === 0], (* Checking if ReadFile has been accomplished*)
																			return = {}
																		];
																		len = Length[return];
																		If[Length[return] < numMin, (* Checking number of equations read *)
																			For[i = 0, i < (numMin - len), i++, (* Starting Autofilling *)																			
																				try = 0;																				
																				For[j = 0, j <= grade, j++, (* Iterating for the grade *)	(* Generating new random expression *)																					
																					try = try + RandomInteger[{-10, +10}]*Symbol["x"]^j
																				]
																				While[ContainsAll[return, {try}] || Length[CoefficientList[try, Symbol["x"]]]<grade+1, (* Checking that the expression is not already in the solutions list and grade+1 coefficent of try is != 0 *)
																					try = 0;
																					For[j = 0, j <= grade, j++, (* Iterating for the grade *)	(* If it is, generate a new one *)
																						try = try + RandomInteger[{-10, +10}]*Symbol["x"]^j
																					] 
																				];	
																				AppendTo[return, try]
																			]
																		];
																		return (* Return the completed list *)
																	];
				
				(* Complete the exercises list adding Random ones till minimum number is reached *)				
				randomFill[grade_,numMin_]:= Module[
												{return = {},try,len = 0},
												len = 0;
												If[Length[return] < numMin, (* Checking number of equations read *)
													For[i = 0, i < (numMin - len), i++, (* Starting Autofilling *)																			
														try = 0;																				
														For[j = 0, j <= grade, j++, (* Iterating for the grade *)	(* Generating new random expression *)																					
															try = try + RandomInteger[{-10, +10}]*Symbol["x"]^j
														]
														While[ContainsAll[return, {try}] || Length[CoefficientList[try, Symbol["x"]]]<grade+1, (* Checking that the expression is not already in the solutions list and grade+1 coefficent of try is != 0 *)
															try = 0;
															For[j = 0, j <= grade, j++, (* Iterating for the grade *)	(* If it is, generate a new one *)
																try = try + RandomInteger[{-10, +10}]*Symbol["x"]^j
															] 
														];	
														AppendTo[return, try]
													]
												];
												return (* Return the completed list *)
											];
				
	
				(* Generate Exercise for grade 1 *)
				generateExercisesG1[l_] := Module[
											{return = {}, sol={}, coeff},
											For[i = 1, i <= Length[l], i++,
												sol = {};
												AppendTo[sol, l[[i]]];(* Add correct answer *)
												coeff = CoefficientList[l[[i]], Symbol["x"]];(* Get coefficients from expression i *)
												AppendTo[sol, coeff[[1]] - Symbol["x"]*coeff[[2]]];(* Add y=-mx+q *)
												try = RandomInteger[{-10, 10}] + Symbol["x"]*RandomInteger[{-10, 10}];(* Adding y=RND x + RND *)
												While[ContainsAll[sol, {try}], 
													try = RandomInteger[{-10, 10}] + Symbol["x"]*RandomInteger[{-10, 10}]
												];(* Check that the expression in not already in the solutions list *)
												AppendTo[sol, try];
												try = RandomInteger[{-10, 10}] + Symbol["x"]*coeff[[2]];(* Adding y=mx + RND *)
												While[ContainsAll[sol, {try}], 
													try = RandomInteger[{-10, 10}] + Symbol["x"]*coeff[[2]]
												];(* Check that the expression in not already in the solutions list *)
												AppendTo[sol, try];
												sol = RandomSample[sol]; (* Randomizing the order in the list *)
												AppendTo[return, sol](* Add exercise to exercises list *)
											];
											return
										];
										
				(* Generate Exercise for grade 2 *)					
				generateExercisesG2[l_] := Module[
											{return = {}, sol={}, coeff},
											For[i = 1, i <= Length[l], i++,
												sol = {};
												AppendTo[sol, l[[i]]];(* Add correct answer *)
												coeff = CoefficientList[l[[i]], Symbol["x"]];(* Get coefficients from expression i *)
												AppendTo[sol, RandomInteger[{-10, 10}] - Symbol["x"]*coeff[[2]] + Symbol["x"]^2*coeff[[3]]];(* Add y=ax^2 - bx + RND *)
												AppendTo[sol, Symbol["x"]*coeff[[2]] + Symbol["x"]^2*coeff[[3]]]; (* Adding y=ax^2 + bx *)
												AppendTo[sol, coeff[[1]] + Symbol["x"]*coeff[[2]] - Symbol["x"]^2*coeff[[3]]];(* Adding y=-ax^2 + bx + c *)
												sol = RandomSample[sol]; (* Randomizing the order in the list *)
												AppendTo[return, sol](* Add exercise to exercises list *)
											];
											return
										];
										
				(* Generate Exercise for grade 3 *)						
				generateExercisesG3[l_] := Module[
											{return = {}, sol={}, coeff},
											For[i = 1, i <= Length[l], i++,
												sol = {};
												AppendTo[sol, l[[i]]];(* Add correct answer *)
												coeff = CoefficientList[l[[i]], Symbol["x"]];(* Get coefficients from expression i *)
												AppendTo[sol, coeff[[1]] + Symbol["x"]*coeff[[2]] + Symbol["x"]^2*coeff[[3]] - Symbol["x"]^3*coeff[[4]]];(* Adding y=-ax^3 + bx^2 + cx + d *)
												try = coeff[[1]] - Symbol["x"]*RandomInteger[{0, 10}] - Symbol["x"]^2*RandomInteger[{0, 10}] + Symbol["x"]^3*coeff[[4]];(* Adding y=ax^3 - RNDx^2 - RNDx + d *)
												While[ContainsAll[sol, {try}], 
													try = coeff[[1]] - Symbol["x"]*RandomInteger[{0, 10}] - Symbol["x"]^2*RandomInteger[{0, 10}] + Symbol["x"]^3*coeff[[4]]
												];(* Check that the expression in not already in the solutions list *)
												AppendTo[sol, try];
												try = RandomInteger[{-10, 10}] + Symbol["x"]*coeff[[2]] + Symbol["x"]^2*coeff[[3]] + Symbol["x"]^3*coeff[[4]];(* Adding y=ax^3 + bx^2 + cx + RND *)
												While[ContainsAll[sol, {try}], 
													try = RandomInteger[{-10, 10}] + Symbol["x"]*coeff[[2]] + Symbol["x"]^2*coeff[[3]] + Symbol["x"]^3*coeff[[4]];
												];(* Check that the expression in not already in the solutions list *)
												AppendTo[sol, try];												
												sol = RandomSample[sol]; (* Randomizing the order in the list *)
												AppendTo[return, sol](* Add exercise to exercises list *)
											];
											return
										];
				
				
				plotWithZoomButtons[n_,x_] := DynamicModule[
										{j = 5, y = 0,l},
										Column[{
											Row[{
												"Zoom sugli zeri:",
												Row[
													Table[
														With[
															{i = i},
															Button[
																i,
																l = Solve[Reduce[n == 0 && -11 < x < 11, x], x][[i]]; (* On button click set zoom center Y and surround area J *)
																j = 1; 
																y = x /. l[[1]];
															]
														],
														{i, Length[Solve[Reduce[n == 0 && -11 < x < 11, x], x]]}
													]
												]
											}],
											Row[{Dynamic[Plot[n, {x, y - j, y + j}, ImageSize -> Medium, PlotLegends->Placed[n, Above]]]}], 
											Button["Reset Zoom", j = 5; y = 0]
											}]
										];
										
										
				(*Draw Graph of function and if zoom stamp text case 3 of zero function 2 grade*)								
				plotWithZoomButtonsA[n_,x_] := DynamicModule[
										{j = 5, y = 0,l},
										Column[{
											Row[{
												"Zoom sugli zeri:",
												Row[
													Table[
														With[
															{i = i},
															Button[
																i,
																l = Solve[Reduce[n == 0 && -11 < x < 11, x], x][[i]]; (* On button click set zoom center Y and surround area J *)
																j = 1; 
																y = x /. l[[1]];
															]
														],
														{i, Length[Solve[Reduce[n == 0 && -11 < x < 11, x], x]]}
													]
												]
											}],
											DynamicModule[{},Dynamic[If[ j === 5 && y === 0, Text[""], Text["zero di molteplicit\[AGrave] 1"]]]]
											Row[{Dynamic[Plot[n, {x, y - j, y + j}, ImageSize -> Medium,  PlotLegends->Placed[n, Above]]]}], 
											Button["Reset Zoom", j = 5; y = 0]
											}]
										];	
										
					(*Draw Graph of function and if zoom stamp text case 3 of zero function 2 grade*)						
					plotWithZoomButtonsB[n_,x_] := DynamicModule[
									{j = 5, y = 0,l,ind = 0},
									Column[{
										Row[{
											"Zoom sugli zeri:",
											Row[
												Table[
													With[
														{i = i},
														Button[
															i,
															l = Solve[Reduce[n == 0 && -11 < x < 11, x], x][[i]]; (* On button click set zoom center Y and surround area J *)
															j = 1; 
															y = x /. l[[1]];
															ind = i
														]
													],
													{i, Length[Solve[Reduce[n == 0 && -11 < x < 11, x], x]]}
												]
											]
										}],
											DynamicModule[{},Dynamic[If[ ind === 0, Text[""],If[ind===1,Text["zero di molteplicit\[AGrave] 1"], Text["zero di molteplicit\[AGrave] 2"]]]]]
										Row[{Dynamic[Plot[n, {x, y - j, y + j}, ImageSize -> Medium,  PlotLegends->Placed[n, Above]]]}], 
										Button["Reset Zoom", j = 5; y = 0; ind = 0]
										}]
									];
									
					(*Draw Graph of function and if zoom stamp text case 3 of zero function 3 grade*)	
					plotWithZoomButtonsC[n_,x_] := DynamicModule[
									{j = 5, y = 0,l},
									Column[{
										Row[{
											"Zoom sugli zeri:",
											Row[
												Table[
													With[
														{i = i},
														Button[
															i,
															l = Solve[Reduce[n == 0 && -11 < x < 11, x], x][[i]]; (* On button click set zoom center Y and surround area J *)
															j = 1; 
															y = x /. l[[1]];
														]
													],
													{i, Length[Solve[Reduce[n == 0 && -11 < x < 11, x], x]]}
												]
											]
										}],
										DynamicModule[{},Dynamic[If[ j === 5 && y === 0, Text[""], Text["zero di molteplicit\[AGrave] 3"]]]],
										Row[{Dynamic[Plot[n, {x, y - j, y + j}, ImageSize -> Medium,  PlotLegends->Placed[n, Above]]]}], 
										Button["Reset Zoom", j = 5; y = 0]
										}]
									];											
				
				
					(* Draw a Graph with zoom buttons inside teacherEQ*)
					plotWithZoomButtonsT[row_,col_,x_] := DynamicModule[
											{j = 5, y = 0,l},
											Column[{
												Row[{
													"Zoom sugli zeri:",
													Row[
														Table[
															With[
																{i = i},
																Button[
																	i,
																	l = Solve[Reduce[teacherEQ[[row,col]] == 0 && -11 < x < 11, x], x][[i]]; (* On button click set zoom center Y and surround area J *)
																	j = 1; 
																	y = x /. l[[1]]
																]
															],
															{i, Length[Solve[Reduce[teacherEQ[[row,col]] == 0 && -11 < x < 11, x], x]]}
														]
													]
												}],
												Row[{Dynamic[Plot[teacherEQ[[row,col]], {x, y - j, y + j}, ImageSize -> Medium]]}], 
												Button["Reset Zoom", j = 5; y = 0]
												}]
											];
				
				
				
					(* Draw a Graph with zoom buttons inside teacherEQ*)
					plotWithZoomButtonsE[row_,col_,ind_,x_] := DynamicModule[
											{j = 5, y = 0,l},
											Column[{
												Row[{
													"Zoom sugli zeri:",
													Row[
														Table[
															With[
																{i = i},
																Button[
																	i,
																	l = Solve[Reduce[exercises[[row,col,ind]] == 0 && -11 < x < 11, x], x][[i]]; (* On button click set zoom center Y and surround area J *)
																	j = 1; 
																	y = x /. l[[1]]
																]
															],
															{i, Length[Solve[Reduce[exercises[[row,col,ind]] == 0 && -11 < x < 11, x], x]]}
														]
													]
												}],
												Row[{Dynamic[Plot[exercises[[row,col,ind]], {x, y - j, y + j}, ImageSize -> Medium]]}], 
												Button["Reset Zoom", j = 5; y = 0]
												}]
											];
				
					(* Creating a cliccable text *)
					hyperText[txt_,msg_, curs_] := EventHandler[
						MouseAppearance[
							Style[txt, FontColor -> Blue], 
							curs
						], 
						{"MouseClicked" :>  MessageDialog[msg]}
					]; 
				
					(* Print a form with all the functions that the teacher is inserting in the list l_ *)
					d[l_] := Panel[
								Grid[
									Table[
										With[
											{i = i, j = j}, 
											Row[
												{l[[i]], 
												"\t", 
												Button["X", l = Delete[l, Position[l, l[[i]]]]]}
											]
										], 
										{i, Length[l]},
										{j, 1}
									], 
									Alignment -> Right
								]
							];	
							
							
							
					(* Allow to function d to ovverride l value with the value of the given variable *)
-					SetAttributes[d, HoldAll];
			
			(* Functions *)
		(* End of Private functions definition space *)
		
	End[];
	
	Protect["MClib`*"] (* Protect package's names *)
	
	(* End of Package *)
	
EndPackage[];



