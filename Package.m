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
	
	
	
	(* End of Usage Definitions *)
	
	Begin["`Private`"]; 
	
		(* Beginning of Private functions definition space *)
		
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
		
		(* End of Private functions definition space *)
		
	End[];
	
	Protect["MClib`*"] (* Protect package's names *)
	
	(* End of Package *)
	
EndPackage[];
