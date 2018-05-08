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
		(* /Variables *)
		p1 = 0(* Variable that indicates that user has already chosen something [G1, q=0] *)
		p2 = 0(* Variable that indicates that user has already chosen something [G1, m!=0] *)
		p3 = 0(* Variable that indicates that user has already chosen something [G1, m=0] *)

		(* Functions *)
		
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






