{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynColorFoldHighlighter.pas, released 2015-12-08.
The Original Code is based on the SynHighlighterPas.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is x2nie.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a minimum requirement of nested-color-fold highlighter for SynEdit)
@author(Fathony <x2nie AT yahoo DOT com>)
@created(8 Dec 2015)
@lastmod()
The SynColorFoldHighlighter unit can be used as a base for another highlighter
to use together with TSynEditMarkupFoldColors.
}
unit SynColorFoldHighlighter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FileUtil, LazUTF8Classes, Graphics,
  SynEditTypes, SynEditHighlighter, SynEditHighlighterFoldBase;

type

  { TSynColorFoldHighlighterRange }

  TSynColorFoldHighlighterRange = class(TSynCustomHighlighterRange)
  private
    FPasFoldEndLevel: Smallint;
    FPasFoldFixLevel: Smallint;
    FPasFoldMinLevel: Smallint;
  public
    function MaxFoldLevel: Integer; override;
    function Add(ABlockType: Pointer = nil; IncreaseLevel: Boolean = True):
        TSynCustomCodeFoldBlock; override;
    procedure Pop(DecreaseLevel: Boolean = True); override;
    procedure Clear; override;
    function Compare(Range: TSynCustomHighlighterRange): integer; override;
    procedure Assign(Src: TSynCustomHighlighterRange); override;


    property PasFoldEndLevel: Smallint read FPasFoldEndLevel write FPasFoldEndLevel;
    property PasFoldFixLevel: Smallint read FPasFoldFixLevel write FPasFoldFixLevel;
    property PasFoldMinLevel: Smallint read FPasFoldMinLevel write FPasFoldMinLevel;

  end;

  { TSynColorFoldHighlighter }

  TSynColorFoldHighlighter = class(TSynCustomFoldHighlighter)
  private
    FCatchNodeInfo: Boolean;
    FCatchNodeInfoList: TLazSynFoldNodeInfoList;
    function GetPasCodeFoldRange: TSynColorFoldHighlighterRange;
    procedure InitNode(out Node: TSynFoldNodeInfo; SignX, SignX2: Integer;
                       EndOffs: Integer;
                       ABlockType: Integer; aActions: TSynFoldActions;
                       AIsFold: Boolean);

  protected
    function GetRangeClass: TSynCustomHighlighterRangeClass; override;
    property PasCodeFoldRange: TSynColorFoldHighlighterRange read GetPasCodeFoldRange;

    procedure InitFoldNodeInfo(AList: TLazSynFoldNodeInfoList; Line: TLineIdx); override;
    // Open/Close Folds
    function StartCodeFoldBlock(SignX,SignX2 : Integer; ABlockType: Pointer=nil;
              IncreaseLevel: Boolean = true): TSynCustomCodeFoldBlock; virtual; reintroduce;
    procedure EndCodeFoldBlock(SignX,SignX2 : Integer;
              DecreaseLevel: Boolean = True); virtual; reintroduce;

  public
  published
  end;

implementation

{ TSynColorFoldHighlighterRange }

function TSynColorFoldHighlighterRange.MaxFoldLevel: Integer;
begin
  // Protect from overly mem consumption, by too many nested folds
  Result := -1;// 100;
end;

function TSynColorFoldHighlighterRange.Add(ABlockType: Pointer;
  IncreaseLevel: Boolean): TSynCustomCodeFoldBlock;
begin
  //Result:=inherited Add(ABlockType, IncreaseLevel);
  Result := inherited Add(ABlockType, True);
  if IncreaseLevel and assigned(result) then
    inc(FPasFoldEndLevel);
end;

procedure TSynColorFoldHighlighterRange.Pop(DecreaseLevel: Boolean);
begin
  //inherited Pop(DecreaseLevel);
  if assigned(Top.Parent) then begin
    if DecreaseLevel then
      dec(FPasFoldEndLevel);
    if FPasFoldMinLevel > FPasFoldEndLevel then
      FPasFoldMinLevel := FPasFoldEndLevel;
  end;
  inherited Pop(True);

end;

procedure TSynColorFoldHighlighterRange.Clear;
begin
  inherited Clear;
  FPasFoldEndLevel := 0;
  FPasFoldFixLevel := 0;
  FPasFoldMinLevel := 0;

end;

function TSynColorFoldHighlighterRange.Compare(Range: TSynCustomHighlighterRange
  ): integer;
begin
  Result:=inherited Compare(Range);
  if Result<>0 then exit;

  //Result:=ord(FMode)-ord(TSynPasSynRange(Range).FMode);
  //if Result<>0 then exit;
  //Result := FBracketNestLevel - TSynColorFoldHighlighterRange(Range).FBracketNestLevel;
  //if Result<>0 then exit;
  //Result := FLastLineCodeFoldLevelFix - TSynColorFoldHighlighterRange(Range).FLastLineCodeFoldLevelFix;
  //if Result<>0 then exit;
  Result := FPasFoldEndLevel - TSynColorFoldHighlighterRange(Range).FPasFoldEndLevel;
  if Result<>0 then exit;
  Result := FPasFoldMinLevel - TSynColorFoldHighlighterRange(Range).FPasFoldMinLevel;
  if Result<>0 then exit;
  //Result := FPasFoldFixLevel - TSynColorFoldHighlighterRange(Range).FPasFoldFixLevel;
end;

procedure TSynColorFoldHighlighterRange.Assign(Src: TSynCustomHighlighterRange);
begin
  if (Src<>nil) and (Src<>TSynCustomHighlighterRange(NullRange)) then begin
    inherited Assign(Src);
    //FMode:=TSynColorFoldHighlighterRange(Src).FMode;
    //FBracketNestLevel:=TSynColorFoldHighlighterRange(Src).FBracketNestLevel;
    //FLastLineCodeFoldLevelFix := TSynColorFoldHighlighterRange(Src).FLastLineCodeFoldLevelFix;
    FPasFoldEndLevel := TSynColorFoldHighlighterRange(Src).FPasFoldEndLevel;
    FPasFoldMinLevel := TSynColorFoldHighlighterRange(Src).FPasFoldMinLevel;
    //FPasFoldFixLevel := TSynColorFoldHighlighterRange(Src).FPasFoldFixLevel;
  end;
end;

{ TSynColorFoldHighlighter }

procedure TSynColorFoldHighlighter.InitNode(out Node: TSynFoldNodeInfo;
  SignX, SignX2: Integer;
  EndOffs: Integer; ABlockType: Integer; aActions: TSynFoldActions;
  AIsFold: Boolean);
var
  OneLine: Boolean;
  i: Integer;
  nd: PSynFoldNodeInfo;
begin
  aActions := aActions + [sfaMultiLine];
  Node.LineIndex := LineIndex;
  Node.LogXStart := SignX;
  Node.LogXEnd := SignX2;
  Node.FoldType := Pointer(PtrInt(ABlockType));
  Node.FoldTypeCompatible := Pointer(PtrInt(ABlockType));//Pointer(PtrInt(PascalFoldTypeCompatibility[ABlockType]));
  Node.FoldAction := aActions;
  node.FoldGroup := 0;//FOLDGROUP_PASCAL;
  if AIsFold then begin
    Node.FoldLvlStart := PasCodeFoldRange.PasFoldEndLevel;
    Node.NestLvlStart := PasCodeFoldRange.CodeFoldStackSize;
    OneLine := (EndOffs < 0) and (Node.FoldLvlStart > PasCodeFoldRange.PasFoldMinLevel); // MinimumCodeFoldBlockLevel);
  end else begin
    Node.FoldLvlStart := PasCodeFoldRange.CodeFoldStackSize; // Todo: zero?
    Node.NestLvlStart := PasCodeFoldRange.CodeFoldStackSize;
    OneLine := (EndOffs < 0) and (Node.FoldLvlStart > PasCodeFoldRange.MinimumCodeFoldBlockLevel);
  end;
  Node.NestLvlEnd := Node.NestLvlStart + EndOffs;
  if not (sfaFold in aActions) then
    EndOffs := 0;
  Node.FoldLvlEnd := Node.FoldLvlStart + EndOffs;
  if OneLine then begin // find opening node
    i := FCatchNodeInfoList.CountAll - 1;
    nd := FCatchNodeInfoList.ItemPointer[i];
    while (i >= 0) and
          ( (nd^.FoldType <> node.FoldType) or
            (nd^.FoldGroup <> node.FoldGroup) or
            (not (sfaOpenFold in nd^.FoldAction)) or
            (nd^.FoldLvlEnd <> Node.FoldLvlStart)
          )
    do begin
      dec(i);
      nd := FCatchNodeInfoList.ItemPointer[i];
    end;
    if i >= 0 then begin
      nd^.FoldAction  := nd^.FoldAction + [sfaOneLineOpen, sfaSingleLine] - [sfaMultiLine];
      Node.FoldAction := Node.FoldAction + [sfaOneLineClose, sfaSingleLine] - [sfaMultiLine];
      if (sfaFoldHide in nd^.FoldAction) then begin
        assert(sfaFold in nd^.FoldAction, 'sfaFoldHide without sfaFold');
        // one liner: hide-able / not fold-able
        nd^.FoldAction  := nd^.FoldAction - [sfaFoldFold];
        Node.FoldAction := Node.FoldAction - [sfaFoldFold];
      end else begin
        // one liner: nether hide-able nore fold-able
        nd^.FoldAction  := nd^.FoldAction - [sfaOpenFold, sfaFold, sfaFoldFold];
        Node.FoldAction := Node.FoldAction - [sfaCloseFold, sfaFold, sfaFoldFold];
      end;
    end;
  end;
  //
end;

function TSynColorFoldHighlighter.GetPasCodeFoldRange: TSynColorFoldHighlighterRange;
begin
  Result := TSynColorFoldHighlighterRange(CodeFoldRange);
end;

function TSynColorFoldHighlighter.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
  Result := TSynColorFoldHighlighterRange;
end;

procedure TSynColorFoldHighlighter.InitFoldNodeInfo(
  AList: TLazSynFoldNodeInfoList; Line: TLineIdx);
var
  nd: PSynFoldNodeInfo;
  i: Integer;
begin
  FCatchNodeInfo := True;
  FCatchNodeInfoList := TLazSynFoldNodeInfoList(AList);

  StartAtLineIndex(Line);
  //fStringLen := 0;
  NextToEol;

  {fStringLen := 0;
  i := LastLinePasFoldLevelFix(Line+1, FOLDGROUP_PASCAL, True);  // all pascal nodes (incl. not folded)
  while i < 0 do begin
    EndPascalCodeFoldBlock;
    FCatchNodeInfoList.LastItemPointer^.FoldAction :=
      FCatchNodeInfoList.LastItemPointer^.FoldAction + [sfaCloseForNextLine];
    inc(i);
  end;
  if Line = CurrentLines.Count - 1 then begin
    // last line, close all folds
    // Run (for LogXStart) is at line-end
    i := FCatchNodeInfoList.CountAll;
    while TopPascalCodeFoldBlockType <> cfbtNone do
      EndPascalCodeFoldBlock(True);
    while FSynPasRangeInfo.EndLevelIfDef > 0 do
      EndCustomCodeFoldBlock(cfbtIfDef);
    while FSynPasRangeInfo.EndLevelRegion > 0 do
      EndCustomCodeFoldBlock(cfbtRegion);
    while i < FCatchNodeInfoList.CountAll do begin
      nd := FCatchNodeInfoList.ItemPointer[i];
      nd^.FoldAction := nd^.FoldAction + [sfaLastLineClose];
      inc(i);
    end;
  end;}
  FCatchNodeInfo := False;
end;

function TSynColorFoldHighlighter.StartCodeFoldBlock(SignX, SignX2: Integer;
  ABlockType: Pointer; IncreaseLevel: Boolean
  ): TSynCustomCodeFoldBlock;
var
  p: PtrInt;
  FoldBlock, BlockEnabled: Boolean;
  act: TSynFoldActions;
  nd: TSynFoldNodeInfo;
begin
  if FCatchNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    BlockEnabled := False;//FFoldConfig[PtrInt(ABlockType)].Enabled;
    FoldBlock := True;
    act := [sfaOpen, sfaOpenFold]; //TODO: sfaOpenFold not for cfbtIfThen
    act := act + [sfaFold, sfaFoldFold, sfaMarkup];//x2nie
    if BlockEnabled then
      act := act + FFoldConfig[longint(ABlockType)].FoldActions;
    //if not FAtLineStart then
      //act := act - [sfaFoldHide];
    InitNode(nd, SignX,SignX2, +1, PtrInt(ABlockType), act, FoldBlock);
    FCatchNodeInfoList.Add(nd);
  end;
  result := inherited StartCodeFoldBlock(ABlockType, IncreaseLevel);
end;

procedure TSynColorFoldHighlighter.EndCodeFoldBlock(SignX, SignX2: Integer;
  DecreaseLevel: Boolean);
var
  //DecreaseLevel,
  BlockEnabled: Boolean;
  act: TSynFoldActions;
  BlockType: Integer;
  nd: TSynFoldNodeInfo;
begin
  if FCatchNodeInfo then begin // exclude subblocks, because they do not increase the foldlevel yet
    BlockEnabled := False;// FFoldConfig[PtrInt(BlockType)].Enabled;
    act := [sfaClose, sfaCloseFold];
    act := act + [sfaFold, sfaFoldFold, sfaMarkup];//x2nie
    if BlockEnabled then
      act := act + FFoldConfig[PtrInt(BlockType)].FoldActions - [sfaFoldFold, sfaFoldHide]; // TODO: Why filter?
    if not DecreaseLevel then
      act := act - [sfaFold, sfaFoldFold, sfaFoldHide];
    //if NoMarkup then       exclude(act, sfaMarkup);
    InitNode(nd, SignX,SignX2, -1, BlockType, act, DecreaseLevel);
    FCatchNodeInfoList.Add(nd);
  end;
  inherited EndCodeFoldBlock(DecreaseLevel);
end;

end.

