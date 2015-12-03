unit SynEditMarkupFoldColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, SynEditMarkup, SynEditMiscClasses, Controls,
  LCLProc, SynEditHighlighter, SynEditHighlighterFoldBase,
  SynEditMarkupWordGroup;

type

  { TSynEditMarkupFoldColors }

  TSynEditMarkupFoldColors = class(TSynEditMarkup)
  private
     // Physical Position
    FHighlightPos1: TWordPoint;
    Found : Boolean;
    Colors : array of TColor;
    ColorIndex : integer;
  protected

  public
    constructor Create(ASynEdit : TSynEditBase);
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;
    procedure PrepareMarkupForRow(aRow : Integer); override;

  end;

implementation
uses
  SynEdit;

{ TSynEditMarkupFoldColors }

constructor TSynEditMarkupFoldColors.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  MarkupInfo.Foreground := clGreen;
  //MarkupInfo.FrameColor := clAqua;
  MarkupInfo.Background := clNone; //clFuchsia;
  //MarkupInfo.BackPriority := 1000;
  //MarkupInfo.BackAlpha := 255;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];

  SetLength(Colors, 6);
  Colors[0] := clRed;
  Colors[1] := $000098F7; //orange
  Colors[2] := $0022CC40; //green
  Colors[3] := $0098CC42; //teal
  Colors[4] := $00FF682A; //blue
  Colors[5] := $00CF00C4; //purple
end;

function TSynEditMarkupFoldColors.GetMarkupAttributeAtRowCol(
  const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  Result := nil;
  if (FHighlightPos1.y = aRow) and  //Found and
   (aStartCol.Logical >= FHighlightPos1.x) and (aStartCol.Logical < FHighlightPos1.X2) then
  begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(FHighlightPos1.x, FHighlightPos1.x2);
    MarkupInfo.Foreground := Colors[ColorIndex];
  end
end;

procedure TSynEditMarkupFoldColors.GetNextMarkupColAfterRowCol(
  const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer);
  Procedure CheckCol(Column: Integer; var Result: Integer);
  begin
    if (Column <= aStartCol.Logical) or ((Result >= 0) and (Result < Column)) then exit;
    Result := Column;
  end;
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (FHighlightPos1.y = aRow) then begin
    CheckCol(FHighlightPos1.X, ANextLog);
    CheckCol(FHighlightPos1.X2, ANextLog);
  end;
end;

procedure TSynEditMarkupFoldColors.PrepareMarkupForRow(aRow: Integer);
var
  i,y,lvl: Integer;
  NodeList: TLazSynFoldNodeInfoList;
  HL: TSynCustomFoldHighlighter;
  StartNode, CloseNode, Node3, TmpNode: TSynFoldNodeInfo;
begin
  //inherited PrepareMarkupForRow(aRow);
  Found := False;
  if not (TCustomSynEdit(self.SynEdit).Highlighter is TSynCustomFoldHighlighter) then
    exit;

  y := aRow -1;

  HL := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
  HL.CurrentLines := Lines;
  HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used

  i := 0;
  NodeList := HL.FoldNodeInfo[y];
  NodeList.AddReference;
  try
    NodeList.ActionFilter := [sfaMarkup];
    TmpNode := NodeList[i];
    while (sfaInvalid in TmpNode.FoldAction) and (i < NodeList.Count) {(TmpNode.LogXEnd < LogCaret.X-1)} do
    begin
      inc(i);
      TmpNode := NodeList[i];
    end;
    if (sfaInvalid in TmpNode.FoldAction) then
        exit;

    Found := True;
    FHighlightPos1.Y  := TmpNode.LineIndex + 1;
    FHighlightPos1.X  := TmpNode.LogXStart + 1;
    FHighlightPos1.X2 := TmpNode.LogXEnd + 1;
    if sfaOpen in TmpNode.FoldAction then
      lvl := TmpNode.FoldLvlStart
    else
      lvl := TmpNode.FoldLvlEnd;
    ColorIndex := lvl mod (length(Colors));
  finally
    NodeList.ReleaseReference;
  end;
end;

end.

