unit SynEditMarkupFoldColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, SynEditMarkup, SynEditMiscClasses, Controls,
  LCLProc, SynEditHighlighter, SynEditHighlighterFoldBase,
  SynEditMarkupWordGroup;

type

  PMarkupFoldColorInfo = ^TMarkupFoldColorInfo;
  TMarkupFoldColorInfo = record
    Y, X, X2: Integer;
    ColorIdx: Integer;
    Border  : Boolean;
  end;

  TMarkupFoldColorInfos = array of TMarkupFoldColorInfo;


  { TSynEditMarkupFoldColors }

  TSynEditMarkupFoldColors = class(TSynEditMarkup)
  private
    FDefaultGroup: integer;
     // Physical Position
    FHighlights : TMarkupFoldColorInfos; //array of TMarkupFoldColorInfo;
    Colors : array of TColor;
    CurrentY : integer;
    function GetFoldHighLighter: TSynCustomFoldHighlighter;
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
    property DefaultGroup : integer read FDefaultGroup write FDefaultGroup;
  end;

implementation
uses
  SynEdit,SynEditTypes, SynEditFoldedView;

  function CompareFI(Item1, Item2: Pointer): Integer;
  begin
    result := PMarkupFoldColorInfo(Item1)^.X - PMarkupFoldColorInfo(Item2)^.X;
  end;

  function SortLeftMostFI(a: TMarkupFoldColorInfos): TMarkupFoldColorInfos;
  var
    l : TFpList;
    i : integer;
  begin
    l := TFpList.Create;
    for i := 0 to Pred(Length(a)) do
      l.Add( PMarkupFoldColorInfo(@a[i]) );
    l.Sort(@CompareFI);

    SetLength(result, Length(a));
    for i := 0 to Pred(l.Count) do
      result[i] := PMarkupFoldColorInfo(l[i])^;
     l.Free;
  end;

{ TSynEditMarkupFoldColors }

constructor TSynEditMarkupFoldColors.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  MarkupInfo.Foreground := clGreen;
  //MarkupInfo.FrameColor := clOlive;
  MarkupInfo.Background := clNone; //clFuchsia;
  //MarkupInfo.BackPriority := 1000;
  //MarkupInfo.BackAlpha := 255;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.FrameEdges:= sfeLeft;//sfeBottom;//

  SetLength(Colors, 6);
  Colors[0] := clRed;
  Colors[1] := $000098F7; //orange
  Colors[2] := $0022CC40; //green
  Colors[3] := $00D1D54A; //$0098CC42; //teal
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
          if ColorIdx >= 0 then
          begin
            MarkupInfo.FrameColor:= clNone;
            MarkupInfo.Foreground:= clNone;

            Result := MarkupInfo;
            MarkupInfo.SetFrameBoundsLog(x, x2);
            if Border then
              MarkupInfo.FrameColor:= Colors[ColorIdx]
            else
              MarkupInfo.Foreground := Colors[ColorIdx]
          end;

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
var
  i,y,iy: Integer;

  procedure AddVerticalLine( ANode: TSynFoldNodeInfo );
  var x,i,lvl : integer;
  begin
    //don't replace; don't add when already found
    x  := ANode.LogXStart + 1;
    for i := 0 to Pred(length(FHighlights)) do
      if FHighlights[i].X = x then
        ;//exit;



    x := Length(FHighlights);
    SetLength(FHighlights, x+1);
    with FHighlights[x] do begin
      Border := ANode.LineIndex + 1 <> aRow;
      Y  := aRow;//ANode.LineIndex + 1;
      X  := ANode.LogXStart + 1;
      X2 := X+2; //ANode.LogXEnd + 1;
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
      begin
        ColorIdx := -1;
        lvl := ANode.NestLvlStart;
        ColorIdx := lvl mod (length(Colors));
      end;


      if sfaOpen in ANode.FoldAction then
        lvl := ANode.NestLvlStart
      else
        lvl := ANode.NestLvlEnd;

      //ColorIdx := ANode.NodeIndex mod (length(Colors));

      lvl := ANode.NestLvlStart;
      //ColorIdx := lvl mod (length(Colors));



    end;
  end;

  procedure AddHighlight( ANode: TSynFoldNodeInfo );
  var x,lvl : integer;
  begin
    //exit; //debug
    x := Length(FHighlights);
    SetLength(FHighlights, x+1);
    with FHighlights[x] do begin
      Border := False;
      Y  := ANode.LineIndex + 1;
      X  := ANode.LogXStart + 1;
      X2 := ANode.LogXEnd + 1;
      if sfaOpen in ANode.FoldAction then begin
        lvl := ANode.FoldLvlStart;
        //lvl := ANode.NestLvlStart; //http://forum.lazarus.freepascal.org/index.php/topic,30122.msg194841.html#msg194841
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
      ColorIdx := lvl mod (length(Colors));
      }

    end;
  end;

var
  NodeList: TLazSynFoldNodeInfoList;
  HL: TSynCustomFoldHighlighter;
  TmpNode: TSynFoldNodeInfo;
  Nest : TLazSynEditNestedFoldsList;
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
        sfaFold
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
      while (sfaInvalid in TmpNode.FoldAction) and (i < NodeList.Count) do
      begin
        inc(i);
        TmpNode := NodeList[i];
      end;
      if not (sfaInvalid in TmpNode.FoldAction) then
          AddHighlight(TmpNode);

      inc(i);
    until i >= NodeList.Count;

  finally
    NodeList.ReleaseReference;
  end;

  //EXPERIMENTAL
  (* *)
  iy := aRow-1;
  Nest := TLazSynEditNestedFoldsList.Create(@GetFoldHighLighter);
  Nest.ResetFilter;
  Nest.Clear;
  Nest.Line := iy;
  Nest.FoldGroup := FDefaultGroup;//1;//FOLDGROUP_PASCAL;
  Nest.FoldFlags :=  [];//[sfbIncludeDisabled]; //
  Nest.IncludeOpeningOnLine := True; //False; //

  //i := 0; while i <  Nest.Count do
  i := Nest.Count -1;  while i >= 0 do
  begin
      TmpNode := Nest.HLNode[i];

      //find till valid

      while (sfaInvalid in TmpNode.FoldAction) and (i < NodeList.Count) do
      begin
        inc(i);
        TmpNode := NodeList[i];
      end;
      if not (sfaInvalid in TmpNode.FoldAction) then

          AddVerticalLine(TmpNode);

      //inc(i);
      dec(i);
      //break;//debug
  end;
  (*
  EXIT;
  *)

  FHighlights := SortLeftMostFI(FHighlights);
end;

function TSynEditMarkupFoldColors.GetFoldHighLighter: TSynCustomFoldHighlighter;
begin
  result := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
end;

procedure TSynEditMarkupFoldColors.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  inherited DoTextChanged(StartLine, EndLine, ACountDiff);
end;

end.

