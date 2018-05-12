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
		p1::usage="[G1, q=0] Variable that indicates that user has already chosen something"
		p2::usage="[G1, m!=0] Variable that indicates that user has already chosen something"
		p3::usage="[G1, m=0] Variable that indicates that user has already chosen something"
		teacherEQ::usage="Exercises solutions list"
	
		loadFiles::usage = "Load initial Files"
	
		solutionReadAndRandomFill::usage = "This function load exercises from a file and complete the excercises list adding Random ones till minimum number is reached
	 \n@file_@ File to load
	 \n@grade_@ Grade o the function {1..N}
	 \n@numMin_@ Minimum number of excercises {1..N}"
	 
		plotWithZoomButtons::usage = "Draw a Graph with zoom buttons inside
	 \n@n_@ Function to plot
	 \n@x_@ Symbol that has to be used in order to resolve the interal equations"
	
		hyperText::usage = "Creating a cliccable text. The click trig a MessageDialog
	 \n@txt@ Text to show
	 \n@msg@ Message in the MessageDialog
	 \n@curs@ Cursor type on mouse over"

		d::usage = "Print a form with all the functions that the teacher is inserting in the list
	 \n@l_@ List to analize"
	 
		
	 
	 
	(* End of Usage Definitions *)
	
	Begin["`Private`"]; 
	
		(* Beginning of Private functions definition space *)
		
			(* Variables *)
			
				p1 = 0 (* Variable that indicates that user has already chosen something [G1, q=0] *)
				p2 = 0 (* Variable that indicates that user has already chosen something [G1, m!=0] *)
				p3 = 0 (* Variable that indicates that user has already chosen something [G1, m=0] *)
			
				
				MINNUMBEROFEXERCISES = 3
				teacherEQ = { }
				
				
			
			(* /Variables *)
		

			(* Functions *)	

				loadFiles[] := Module[{}, 
					AppendTo[teacherEQ,solutionReadAndRandomFill["g1A.txt",1,MINNUMBEROFEXERCISES]]; (* Exercises Grade 1 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g1B.txt",1,MINNUMBEROFEXERCISES]]; (* Exercises Grade 1 solutions list of Exercise Kind B *)				
					AppendTo[teacherEQ,solutionReadAndRandomFill["g2A.txt",2,MINNUMBEROFEXERCISES]]; (* Exercises Grade 2 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g2B.txt",2,MINNUMBEROFEXERCISES]]; (* Exercises Grade 2 solutions list of Exercise Kind B *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g3A.txt",3,MINNUMBEROFEXERCISES]]; (* Exercises Grade 3 solutions list of Exercise Kind A *)
					AppendTo[teacherEQ,solutionReadAndRandomFill["g3B.txt",3,MINNUMBEROFEXERCISES]] (* Exercises Grade 3 solutions list of Exercise Kind B *)
				]
			
			
				(* This function load exercises from a file and complete the excercises list adding Random ones till minimum number is reached *)
				solutionReadAndRandomFill[file_,grade_,numMin_] := Module[
																		{return = {},try,app},	
																		return =  ReadList[file]; (* Reading the file *)
																		If[Length[return === 0], (* Checking if ReadFile has been accomplished*)
																			return = {}
																		];
																		If[Length[return] < numMin, (* Checking number of equations read *)
																			For[i = 0, i < numMin - Length[return], i++, (* Starting Autofilling *)
																				try = 0;
																				For[j = 0, j <= grade, j++, (* Iterating for the grade *)	(* Generating new random expression *)																					
																					try = try + RandomInteger[{-10, +10}]*Symbol["x"]^j
																				]
																				While[ContainsAll[sol, {try}] || Length[CoefficientList[try, Symbol["x"]]]<grade+1, (* Checking that the expression is not already in the solutions list and grade+1 coefficent of try is != 0 *)
																					try = 0;
																					For[j = 0, j <= grade, j++, (* Iterating for the grade *)	(* If it is, generate a new one *)
																						try = try + RandomInteger[{-10, +10}]*Symbol["x"]^j
																					] 
																				];
																				AppendTo[return, try];
																			]
																		];
																		return (* Return the completed list *)
																	];
				
				
				
				(* Draw a Graph with zoom buttons inside *)
				plotWithZoomButtons[n_,x_] := Module[
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
																y = x /. l[[1]]
															]
														],
														{i, Length[Solve[Reduce[n == 0 && -11 < x < 11, x], x]]}
													]
												]
											}],
											Row[{Dynamic[Plot[n, {x, y - j, y + j}, ImageSize -> Medium]]}], 
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
				SetAttributes[d, HoldAll];	
			
			(* Functions *)
		(* End of Private functions definition space *)
		
	End[];
	
	(*Protect["MClib`*"]*) (* Protect package's names *)
	
	(* End of Package *)
	
EndPackage[];






