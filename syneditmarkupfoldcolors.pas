unit SynEditMarkupFoldColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, SynEditMarkup, SynEditMiscClasses, Controls,
  LCLProc, SynEditHighlighter, SynEditHighlighterFoldBase,
  SynEditMarkupWordGroup;

type

  TMarkupFoldColorInfo = record
    Y, X, X2: Integer;
    ColorIdx: Integer;
  end;


  { TSynEditMarkupFoldColors }

  TSynEditMarkupFoldColors = class(TSynEditMarkup)
  private
     // Physical Position
    FHighlights : array of TMarkupFoldColorInfo;
    Colors : array of TColor;
    CurrentY : integer;
  protected
    // Notifications about Changes to the text
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override; // 1 based
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
var
  i : integer;
begin
  Result := nil;
  if (CurrentY = aRow) then
    for i := 0 to length(FHighlights)-1 do
      with FHighlights[i] do
        if (aStartCol.Logical >= x) and (aStartCol.Logical < X2) then
        begin
          Result := MarkupInfo;
          MarkupInfo.SetFrameBoundsLog(x, x2);
          if ColorIdx >= 0 then
            MarkupInfo.Foreground := Colors[ColorIdx]
          else
            MarkupInfo.Foreground := clFuchsia;
          break;
        end
end;

procedure TSynEditMarkupFoldColors.GetNextMarkupColAfterRowCol(
  const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer);
  Procedure CheckCol(HlposX: Integer; var Result: Integer);
  begin
    if (HlposX <= aStartCol.Logical) or ((Result >= 0) and (Result < HlposX)) then exit;
    Result := HlposX;
  end;
var i : integer;
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (CurrentY = aRow) then
  for i := 0 to length(FHighlights)-1 do begin
    if FHighlights[i].X  <= aStartCol.Logical then
      continue;
    if FHighlights[i].X2  < aStartCol.Logical then
      continue;
    ANextLog := FHighlights[i].X;
    break;
    //CheckCol(FHighlights[i].X, ANextLog);
    //CheckCol(FHighlights[i].X2, ANextLog);
  end;
end;

procedure TSynEditMarkupFoldColors.PrepareMarkupForRow(aRow: Integer);
  procedure AddHighlight( ANode: TSynFoldNodeInfo );
  var x,lvl : integer;
  begin
    x := Length(FHighlights);
    SetLength(FHighlights, x+1);
    with FHighlights[x] do begin
      Y  := ANode.LineIndex + 1;
      X  := ANode.LogXStart + 1;
      X2 := ANode.LogXEnd + 1;
      if sfaOpen in ANode.FoldAction then begin
        lvl := ANode.FoldLvlStart;
        ColorIdx := lvl mod (length(Colors));
      end
      else
        if sfaClose in ANode.FoldAction then begin
          lvl := ANode.FoldLvlEnd;
          ColorIdx := lvl mod (length(Colors));
        end
      else
        ColorIdx := -1;
      {if sfaOpen in ANode.FoldAction then
        lvl := ANode.NestLvlStart
      else
        lvl := ANode.NestLvlEnd;
      ColorIdx := ANode.NodeIndex mod (length(Colors));
      }

    end;
  end;
var
  i,y: Integer;
  NodeList: TLazSynFoldNodeInfoList;
  HL: TSynCustomFoldHighlighter;
  TmpNode: TSynFoldNodeInfo;
begin
  CurrentY := aRow;
  SetLength(FHighlights,0); //reset needed to prevent using of invalid area

  if not (TCustomSynEdit(self.SynEdit).Highlighter is TSynCustomFoldHighlighter) then
    exit;

  y := aRow -1;

  HL := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
  HL.CurrentLines := Lines;
  HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used


  NodeList := HL.FoldNodeInfo[y];
  NodeList.AddReference;
  try
    NodeList.ActionFilter := [
        {sfaMarkup,}
        {sfaFold,}
        //sfaFoldFold
        //sfaFoldHide
        //sfaSingleLine
        //sfaMultiLine
        //sfaOpen
        ];
    //NodeList.FoldFlags:= [sfbIncludeDisabled];
    i := 0;
    repeat
      TmpNode := NodeList[i];

      //find till valid
      {
      while (sfaInvalid in TmpNode.FoldAction) and (i < NodeList.Count) do
      begin
        inc(i);
        TmpNode := NodeList[i];
      end;
      if not (sfaInvalid in TmpNode.FoldAction) then
      }
          AddHighlight(TmpNode);

      inc(i);
    until i >= NodeList.Count;

  finally
    NodeList.ReleaseReference;
  end;
end;

procedure TSynEditMarkupFoldColors.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  inherited DoTextChanged(StartLine, EndLine, ACountDiff);
end;

end.

