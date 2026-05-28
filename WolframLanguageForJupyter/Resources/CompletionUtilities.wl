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

	usageStr[name_String] := With[{
			usage1 = Quiet[
				ToString[
					ToExpression[name<>"::usage"],
					OutputForm
				]
			]
		},
      	If[StringQ[usage1], 
			usage1,
			""
		]
    ];


	(* returns matches, cursorStart, cursorEnd, docstrings *)
	getCursorCompletion[code_String, cursorPos_Integer] :=
		Module[
			{codeStr, namedCharMatch, tokenMatch, token, tokenWLStart, matches, docstrings},
			(*	have to decapitate the rest of the string because rewriteNamedCharacters
				cannot handle it *)
			codeStr = StringTake[code, {1, Min[cursorPos, StringLength[code]]}];

			(* find \[Alpha] symbols *)
			namedCharMatch = StringCases[
				codeStr,
				"\\" ~~ "[" ~~ LetterCharacter...
			];
			If[Length[namedCharMatch] > 0,
				Return[Association[
					"matches" -> 
						Prepend[
							Select[
								rewriteNamedCharacters[namedCharMatch],
								(!containsPUAQ[#])&],
						codeStr
					],
					"cursorStart" -> 0,
					"cursorEnd" -> StringLength[codeStr],
					"docstrings" -> {}
				]]
			];

			(* find WL identifiers *)
			tokenMatch = StringCases[
				codeStr,
				(LetterCharacter | "$") ~~ (LetterCharacter | DigitCharacter | "$" | "`")... ~~ EndOfString
			];
			If[Length[tokenMatch] == 0,
				Return[Association[
					"matches" -> {},
					"cursorStart" -> StringLength[codeStr],
					"cursorEnd" -> StringLength[codeStr],
					"docstrings" -> {}
				]]
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

			docstrings = Map[
      			usageStr,
      			matches
      		];


			Return[Association[
				"matches" -> matches,
				"cursorStart" -> StringLength[codeStr] - StringLength[token],
				"cursorEnd" -> StringLength[codeStr],
				"docstrings" -> docstrings
			]];
		];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotCompletionUtilities *)
