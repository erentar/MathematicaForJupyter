(************************************************
				CompletionUtilities.wl
*************************************************
Description:
	Utilities for the aiding in the
		(auto-)completion of Wolfram Language
		code
Symbols defined:
	rewriteNamedCharacters,
	getCursorCompletion
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotCompletionUtilities],
	
	WolframLanguageForJupyter`Private`$GotCompletionUtilities = True;

(************************************
	load required
		WolframLanguageForJupyter
		files
*************************************)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "Initialization.wl"}]]; (* unicodeNamedCharactersReplacements,
																					verticalEllipsis *)

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	utilities for rewriting
		Wolfram Language code
*************************************)

	(* rewrite names (in a code string) into named characters *)
	rewriteNamedCharacters[codeToAnalyze_?StringQ] :=
		Module[
			{codeUsingFullReplacements},
			codeUsingFullReplacements =
				StringReplace[
					codeToAnalyze,
					Normal @ unicodeNamedCharactersReplacements
				];
			If[
				StringCount[
					codeUsingFullReplacements,
					verticalEllipsis | "\\["
				] != 1,
				Return[{codeUsingFullReplacements}];
			];
			Return[
				Flatten[
					StringCases[
						codeUsingFullReplacements,
						before___ ~~ name : ((verticalEllipsis | "\\[") ~~ rest__ ~~ EndOfString) :>
							(
								(StringJoin[before, #1] &) /@
									Values[
										KeySelect[
											unicodeNamedCharactersReplacements,
											StringMatchQ[#1, name ~~ ___] &
										]
									]
							)
					]
				]
			];
		];

	(* returns {matches, cursor_start, cursor_end} *)
	getCursorCompletion[code_String, cursorPos_Integer] :=
		Module[
			{codeStr, namedCharMatch, tokenMatch, token, tokenWLStart, matches},
			(*	have to decapitate the rest of the string because rewriteNamedCharacters
				cannot handle it *)
			codeStr = StringTake[code, {1, cursorPos}];
			

			(* find \[Alpha] symbols *)
			namedCharMatch = StringCases[
				codeStr,
				"\\" ~~ "[" ~~ LetterCharacter...
			];
			If[Length[namedCharMatch] > 0,
				Return[{
					Prepend[
						Select[
							rewriteNamedCharacters[namedCharMatch],
							(!containsPUAQ[#])&
						],
						codeStr
					],
					0,
					StringLength[codeStr]
				}]
			];

			(* find WL identifiers *)
			tokenMatch = StringCases[
				codeStr,
				(LetterCharacter | "$") ~~ (LetterCharacter | DigitCharacter | "$" | "`")... ~~ EndOfString
			];
			If[Length[tokenMatch] == 0,
				Return[{
					{},
					StringLength[codeStr],
					StringLength[codeStr]
				}]
			];

			token = First[tokenMatch];
			matches = Names[token <> "*", IgnoreCase->True];
			If[!StringContainsQ[token, "`"], (* if the input token does not have a context qualifier *)
				matches = DeleteDuplicates[
					StringReplace[
						matches,
						StartOfString ~~ ___ ~~ "`" ~~ rest__ :> rest (* truncate context qualifier*)
					]
				]
			];
			Return[{
				matches,
				StringLength[codeStr] - StringLength[token],
				StringLength[codeStr]
			}]
		];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotCompletionUtilities *)
