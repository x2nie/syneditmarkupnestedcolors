unit SynHighlighterBracket;
(*
  This is an example how to implement my own highlighter.

  This example extends the Simple and Context HL:
  - The token '{' and '}' (must be surrounded by space or line-begin/end to be
    a token of their own) will add foldable sections

    Multply { and } can be nested.

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter,
  SynEditHighlighterFoldBase, SynColorFoldHighlighter
  ;

{$define colorfold}
type

  { TSynHighlighterBracket }

  //TSynHighlighterBracket = class(TSynCustomFoldHighlighter)
  TSynHighlighterBracket = class({$ifdef colorfold}TSynColorFoldHighlighter{$else}TSynCustomFoldHighlighter{$endif})
  private
    FFoldAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    //fSpaceAttri: TSynHighlighterAttributes;
    procedure SetFoldAttri(AValue: TSynHighlighterAttributes);
    procedure SetIdentifierAttri(AValue: TSynHighlighterAttributes);
    //procedure SetSpaceAttri(AValue: TSynHighlighterAttributes);
  protected
    // accesible for the other examples
    FTokenPos, FTokenEnd: Integer;
    FLineText: String;
  public
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
  public
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    constructor Create(AOwner: TComponent); override;
  published
    (* Define 4 Attributes, for the different highlights. *)
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write SetIdentifierAttri;
    //property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      //write SetSpaceAttri;
    property FoldAttri: TSynHighlighterAttributes read FFoldAttri
      write SetFoldAttri;
  end;

implementation


constructor TSynHighlighterBracket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  (* Create and initialize the attributes *)
  FFoldAttri := TSynHighlighterAttributes.Create('fold', 'fold');
  AddAttribute(FFoldAttri);
  FFoldAttri.Style := [fsBold];

  fIdentifierAttri := TSynHighlighterAttributes.Create('ident', 'ident');
  AddAttribute(fIdentifierAttri);

  {fSpaceAttri := TSynHighlighterAttributes.Create('space', 'space');
  AddAttribute(fSpaceAttri);
  fSpaceAttri.FrameColor := clSilver;
  fSpaceAttri.FrameEdges := sfeBottom;//sfeLeft;//sfeAround;}
end;

procedure TSynHighlighterBracket.Next;
var
  l: Integer;
begin
  // FTokenEnd should be at the start of the next Token (which is the Token we want)
  FTokenPos := FTokenEnd;
  // assume empty, will only happen for EOL
  FTokenEnd := FTokenPos;

  // Scan forward
  // FTokenEnd will be set 1 after the last char. That is:
  // - The first char of the next token
  // - or past the end of line (which allows GetEOL to work)

  l := length(FLineText);
  If FTokenPos > l then
    // At line end
    exit
  else

  if FLineText[FTokenEnd] = '{' then
  begin
    {$ifdef colorfold}
    StartCodeFoldBlock(FTokenPos-1, FTokenEnd);
    {$else}
    StartCodeFoldBlock(nil);
    //CodeFoldRange.FoldStart := Point (FTokenPos, LineIndex );
    CodeFoldRange.FoldSign[True] := FoldSign(FTokenPos, FTokenEnd, LineIndex);
    {$endif}
    inc (FTokenEnd);
  end
  else
  if FLineText[FTokenEnd] = '}' then
  begin
    {$ifdef colorfold}
    EndCodeFoldBlock(FTokenPos-1, FTokenEnd);
    {$else}
    //CodeFoldRange.FoldFinish := Point (FTokenPos, LineIndex );
    CodeFoldRange.FoldSign[False] := FoldSign(FTokenPos, FTokenEnd, LineIndex);
    EndCodeFoldBlock;
    {$endif}
    inc (FTokenEnd);
  end
  else
    if FLineText[FTokenEnd] in [#9, ' '] then
    // At Space? Find end of spaces
    while (FTokenEnd <= l) and (FLineText[FTokenEnd] in [#0..#32]) do inc (FTokenEnd)
  else
    // At None-Space? Find end of None-spaces
    while (FTokenEnd <= l) and not(FLineText[FTokenEnd] in [#9, ' ', '{', '}']) do inc (FTokenEnd);

end;

(* Setters for attributes / This allows using in Object inspector*)
procedure TSynHighlighterBracket.SetIdentifierAttri(AValue: TSynHighlighterAttributes);
begin
  fIdentifierAttri.Assign(AValue);
end;

procedure TSynHighlighterBracket.SetFoldAttri(AValue: TSynHighlighterAttributes);
begin
  FFoldAttri.Assign(AValue);
end;


{procedure TSynHighlighterBracket.SetSpaceAttri(AValue: TSynHighlighterAttributes);
begin
  fSpaceAttri.Assign(AValue);
end;}


procedure TSynHighlighterBracket.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  Next;
end;



function TSynHighlighterBracket.GetEol: Boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

procedure TSynHighlighterBracket.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TSynHighlighterBracket.GetTokenAttribute: TSynHighlighterAttributes;
var s : string;
begin
  // Match the text, specified by FTokenPos and FTokenEnd

  if FLineText[FTokenPos] in [#9, ' '] then
    Result := WhitespaceAttribute
  else
    if FLineText[FTokenPos] in ['{', '}'] then
      Result := FoldAttri
    else
      Result := IdentifierAttri;
end;

function TSynHighlighterBracket.GetToken: String;
begin
  Result := copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TSynHighlighterBracket.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TSynHighlighterBracket.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  // Some default attributes
  case Index of
    //SYN_ATTR_COMMENT: Result := fSpecialAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    //SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynHighlighterBracket.GetTokenKind: integer;
var
  a: TSynHighlighterAttributes;
begin
  // Map Attribute into a unique number
  a := GetTokenAttribute;
  Result := 0;
  //if a = fSpaceAttri then Result := 1;
  if a = fIdentifierAttri then Result := 3;
  if a = FFoldAttri then Result := 4;
end;

end.

