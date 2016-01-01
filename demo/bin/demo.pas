unit SynEditMarkupFoldColoring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, SynEditMarkup, SynEditMiscClasses, Controls,
  LCLProc, SynEditHighlighter, SynEditHighlighterFoldBase;

type

  PMarkupFoldColorInfo = ^TMarkupFoldColorInfo;
  TMarkupFoldColorInfo = record
    Y, X, X2: Integer;
    ColorIdx: Integer;
    Border  : Boolean;
  end;

  TMarkupFoldColorInfos = array of TMarkupFoldColorInfo;
  TSynFoldNodeInfos     = array of TSynFoldNodeInfo; //for quick compare detection
  
  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;



  { TSynEditMarkupFoldColors }

  TSynEditMarkupFoldColors = class(TSynEditMarkup)
  private
    FDefaultGroup: integer;
     // Physical Position
    FHighlights : TMarkupFoldColorInfos; //array of TMarkupFoldColorInfo;
    Colors : array of TColor;
    CurrentY : integer;  //??
    FCaretY : integer;    // flag identify for refresh begin______
    FPrevCaretText : string;  // flag identify for refresh begin______

    procedure DoMarkupFoldAtRow(aRow: Integer);
    procedure DoMarkupParentFoldAtRow(aRow: Integer);
    procedure DoMarkupRangeFoldAtRow(aRow: Integer);
    function GetFoldHighLighter: TSynCustomFoldHighlighter;
  protected
    // Notifications about Changes to the text
    procedure DoTextChanged({%H-}StartLine, EndLine, {%H-}ACountDiff: Integer); override; // 1 based
    procedure DoCaretChanged(Sender: TObject); override;
  public
    constructor Create(ASynEdit : TSynEditBase);
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const {%H-}AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const {%H-}AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    procedure PrepareMarkupForRow(aRow : Integer); override;
    property DefaultGroup : integer read FDefaultGroup write FDefaultGroup;
  end;
  
var
  GlobalFoo : TFoo = nil;

// sample of routine declaration inside Inteface
function StrToCompilerMode(const aName: string): TCompilerMode;   
function StrToCompilerMode2(const aName: string): TCompilerMode;     

implementation
uses
  Forms {debug},
  SynEdit,SynEditTypes, SynEditFoldedView, SynEditMiscProcs;

// sample of routine declaration inside Implementation
Procedure First (n : longint); forward;

Procedure Second;
begin
  WriteLn ('In second. Calling first...');
  First (1);
end;

Function strlen (P : PChar) : Longint; cdecl; external;
begin
  WriteLn ('Length of (',p,') : ',strlen(p))
end;

Procedure First (n : longint);
begin
  WriteLn ('First received : ',n);
end;

{%region 'Trial IFDEF vs nested procedures' -fold}
  
// Not a bug anymore:
{$IFDEF BCB}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ELSE}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
{$ENDIF}
begin
  (FBackend as IDeviceContextSupport).Draw(DstRect, SrcRect, hSrc);
end;

(* wouldn't be relsolved, because HL not able to know which one is correct:

{$IFDEF foo}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ENDIF}
{$IFDEF bar}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ENDIF}
{$IFDEF abc}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ENDIF}
begin
end;

but it can be resolved like this:  *)

{$IFDEF foo}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ELSE}
  {$IFDEF bar}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
  {$ELSE}
    {$IFDEF abc}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
begin
end; 

// it is no longer a bug:
{$IFDEF bar}
      {$IFDEF abc}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
      {$ELSE}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
      {$ENDIF}
{$ELSE}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ENDIF}
begin
end;   

{%endregion}

procedure TBitmap32.DrawTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; DstX, DstY: Integer);
begin
  if Empty then Exit;
  (FBackend as IDeviceContextSupport).DrawTo(hDst, DstX, DstY);
end;

procedure TBitmap32.DrawTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; const DstRect, SrcRect: TRect);
begin
  if Empty then Exit;
  (FBackend as IDeviceContextSupport).DrawTo(hDst, DstRect, SrcRect);
end;

procedure TBitmap32.TileTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; const DstRect, SrcRect: TRect);
const
  MaxTileSize = 1024;
var
  DstW, DstH: Integer;
  TilesX, TilesY: Integer;
  Buffer: TCustomBitmap32;
  I, J: Integer;
  ClipRect, R: TRect;
  X, Y: Integer;
begin
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  TilesX := (DstW + MaxTileSize - 1) div MaxTileSize;
  TilesY := (DstH + MaxTileSize - 1) div MaxTileSize;
  Buffer := TBitmap32.Create;
  try
    for J := 0 to TilesY - 1 do
    begin
      for I := 0 to TilesX - 1 do
      begin
        ClipRect.Left := I * MaxTileSize;
        ClipRect.Top := J * MaxTileSize;
        ClipRect.Right := (I + 1) * MaxTileSize;
        ClipRect.Bottom := (J + 1) * MaxTileSize;
        if ClipRect.Right > DstW then ClipRect.Right := DstW;
        if ClipRect.Bottom > DstH then ClipRect.Bottom := DstH;
        X := ClipRect.Left;
        Y := ClipRect.Top;
        OffsetRect(ClipRect, -X, -Y);
        R := DstRect;
        OffsetRect(R, -X - DstRect.Left, -Y - DstRect.Top);
        Buffer.SetSize(ClipRect.Right, ClipRect.Bottom);
        StretchTransfer(Buffer, R, ClipRect, Self, SrcRect, Resampler, DrawMode, FOnPixelCombine);

        (Buffer.Backend as IDeviceContextSupport).DrawTo(hDst,
          MakeRect(X + DstRect.Left, Y + DstRect.Top, X + ClipRect.Right,
          Y + ClipRect.Bottom), MakeRect(0, 0, Buffer.Width, Buffer.Height)
        );
      end;
    end;
  finally
    Buffer.Free;
  end;
end;    

{.$define debug_FC_line_changed}
procedure TSynEditMarkupFoldColors.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
{$ifdef debug_FC_line_changed}
var F : TCustomForm;
begin
  F := GetParentForm(self.SynEdit);
  if F <> nil then
    //F.Caption := Format('Start:%d Endline:%d  Diff:%d',[StartLine, EndLIne, ACountDiff]);
  F.Caption := F.Caption +  Caret.LineText
{$else}

  procedure Blah;
  var a:b;
  begin foo(); 
  end;

  function GetPairCloseFold(aRow, X : integer  ): Integer;
  var
    y,i,LCnt : integer;
    HL: TSynCustomFoldHighlighter;
    NodeList: TLazSynFoldNodeInfoList;
    TmpNode, CloseNode: TSynFoldNodeInfo;

    function FindEndNode(StartNode: TSynFoldNodeInfo;
                       {var} YIndex, NIndex: Integer): TSynFoldNodeInfo;
      function SearchLine(ALineIdx: Integer; var ANodeIdx: Integer): TSynFoldNodeInfo;
      begin
        NodeList.Line := ALineIdx;
        repeat
          inc(ANodeIdx);
          Result := NodeList[ANodeIdx];
        until (sfaInvalid in Result.FoldAction)
           or (Result.NestLvlEnd <= StartNode.NestLvlStart);
      end;

    begin
      Result := SearchLine(YIndex, NIndex);
      if not (sfaInvalid in Result.FoldAction) then
        exit;

      inc(YIndex);
      while (YIndex < LCnt) and
            (HL.FoldBlockMinLevel(YIndex, StartNode.FoldGroup, [sfbIncludeDisabled])
             > StartNode.NestLvlStart)
      do
        inc(YIndex);
      if YIndex = LCnt then
        exit;

      NIndex := -1;
      Result := SearchLine(YIndex, NIndex);

      if (Result.LogXEnd = 0) or (sfaLastLineClose in Result.FoldAction) then
        Result.FoldAction := [sfaInvalid]; // LastLine closed Node(maybe force-closed?)
    end;

  begin
    Result := -1;
    y := aRow -1;

    HL := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
    HL.CurrentLines := Lines;
    LCnt := Lines.Count;
    HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used

    NodeList := HL.FoldNodeInfo[y];
    NodeList.AddReference;
    try
      NodeList.ActionFilter := [sfaOpen];
      i := 0;
      repeat
        TmpNode := NodeList[i];

        if TmpNode.LogXStart < X-1 then
        begin
          inc(i);
          continue;
        end;

        //find till valid
        while (sfaInvalid in TmpNode.FoldAction) and (i < NodeList.Count) do
        begin
          inc(i);
          TmpNode := NodeList[i];
        end;
        if not (sfaInvalid in TmpNode.FoldAction) then
        begin
          CloseNode := FindEndNode(TmpNode, y, i);
          //AddHighlight(TmpNode);
          Result := CloseNode.LineIndex;
          exit;
        end;

        inc(i);
      until i >= NodeList.Count;

    finally
      NodeList.ReleaseReference;
    end;
  end;


  function IsFoldMoved( aRow: Integer ): integer;
  var S : string;
    i,n : integer;
  begin
    Result := -1;
    n := -1;

    S := Caret.LineText;
    for i := 1 to Min(Length(S), Length(FPrevCaretText)) do
    begin
      if S[i] <> FPrevCaretText[i] then
      begin
        n := i;
        break;
      end;
    end;

    if n < 0 then exit;

    Result := GetPairCloseFold(aRow, n);
    //limit to screen bottom
    if Result > 0 then
    begin
      inc(Result);//because sometime 'end' has trailing vertical line
      with TCustomSynEdit(SynEdit) do
        Result := min(Result, TopLine +LinesInWindow);// . .RowToScreenRow(i);
    end;

  end;
var
  EndFoldLine,y : integer;
begin
  if EndLine < 0 then exit; //already refreshed by syn

  y := Caret.LineBytePos.y;
  EndFoldLine := IsFoldMoved(y);
  if EndFoldLine > 0 then
  begin
    InvalidateSynLines(y+1, EndFoldLine);
  end;

  FPrevCaretText := Caret.LineText;
  // I found that almost anything has been repaint by the SynEdit,
  // except the trailing space editing: we should repaint them here.
{$endif}
end;
  
procedure TForm1.FormCreate(Sender: TObject);
var r  : char;
  c : byte;
begin
  c := 9;
  case c of
    1: r := #32;
    2: if c = Ord(r) then
          if Ord(r) > 9 then
            r := 'A'
          else // NOT highlighted as case-label
            r:=#2
        else  // NOT highlighted as case-label
          r:=#3
    else  // highlighted as case-label
      r:=#32;
  end

end;

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
  MarkupInfo.Background := clNone; //clFuchsia;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.FrameEdges:= sfeLeft;//sfeAround;//sfeBottom;//

  SetLength(Colors, 6);
  Colors[0] := clRed;
  Colors[1] := $000098F7; //orange
  Colors[2] := $0022CC40; //green
  Colors[3] := $00D1D54A; // $0098CC42; //teal
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
            MarkupInfo.Background:= clNone;
            MarkupInfo.FrameEdges:= sfeNone;
            //MarkupInfo.FrameColor:= clGreen; //debug

            Result := MarkupInfo;
            MarkupInfo.SetFrameBoundsLog(x, x2);
            if Border then
            begin
              MarkupInfo.FrameColor:= Colors[ColorIdx];
              MarkupInfo.FrameEdges:= sfeLeft;//sfeAround;//
            end
            else
              MarkupInfo.Foreground := Colors[ColorIdx];

            //MarkupInfo.FrameEdges:= sfeAround; //debug

            //2nd debug
            if x > x2 then
            begin
              MarkupInfo.Background:= clYellow;
              MarkupInfo.SetFrameBoundsLog(x-1, x2+20);
              MarkupInfo.FrameColor:= clBlue; //debug
            end;

          end;

          break;
        end
end;

procedure TSynEditMarkupFoldColors.GetNextMarkupColAfterRowCol(
  const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer);
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
  end;
end;

procedure TSynEditMarkupFoldColors.DoMarkupFoldAtRow(aRow: Integer);

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
  y,i : integer;
  HL: TSynCustomFoldHighlighter;
  NodeList: TLazSynFoldNodeInfoList;
  TmpNode: TSynFoldNodeInfo;

begin
  y := aRow -1;

  HL := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
  HL.CurrentLines := Lines;
  HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used

  NodeList := HL.FoldNodeInfo[y];
  NodeList.AddReference;
  try
    NodeList.ActionFilter := [
        {sfaMarkup,}
//        sfaFold
      sfaOutline
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
      {while (sfaInvalid in TmpNode.FoldAction) and (i < NodeList.Count) do
      begin
        inc(i);
        TmpNode := NodeList[i];
      end;}
      if not (sfaInvalid in TmpNode.FoldAction) then
          AddHighlight(TmpNode);

      inc(i);
    until i >= NodeList.Count;

  finally
    NodeList.ReleaseReference;
  end;
end;

procedure TSynEditMarkupFoldColors.DoMarkupParentFoldAtRow(aRow: Integer);

  procedure AddVerticalLine( ANode: TSynFoldNodeInfo );
  var x,i,lvl : integer;
  begin
    //don't replace; don't add when already found
    x  := ANode.LogXStart + 1;
    for i := 0 to Pred(length(FHighlights)) do
      if FHighlights[i].X = x then
        exit;

    x := Length(FHighlights);
    SetLength(FHighlights, x+1);
    with FHighlights[x] do begin
      Border := ANode.LineIndex + 1 <> aRow;
      X  := ANode.LogXStart + 1;
      Y  := aRow;//ANode.LineIndex + 1;
      {if ANode.LineIndex + 1 = aRow then //mean on open/close tag
      begin
        //X  := ANode.LogXStart + 1
        ColorIdx := -1;
        exit;
      end
      else
        X  := ANode.LogVertGuideX + 1;}

      X2 := X+1; //ANode.LogXEnd + 1;
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

      {
      if sfaOpen in ANode.FoldAction then
        lvl := ANode.NestLvlStart
      else
        lvl := ANode.NestLvlEnd;

      //ColorIdx := ANode.NodeIndex mod (length(Colors));

      lvl := ANode.NestLvlEnd;
      //lvl := Longint(ANode.FoldTypeCompatible);
      ColorIdx := lvl mod (length(Colors));
      }


    end;
  end;
var
  i,y: Integer;
  Nest : TLazSynEditNestedFoldsList;
  TmpNode: TSynFoldNodeInfo;

begin
  y := aRow-1;
  Nest := TLazSynEditNestedFoldsList.Create(@GetFoldHighLighter);
  Nest.ResetFilter;
  Nest.Clear;
  Nest.Line := y;
  Nest.FoldGroup := FDefaultGroup;//1;//FOLDGROUP_PASCAL;
  Nest.FoldFlags :=  [];//[sfbIncludeDisabled]; //
  Nest.IncludeOpeningOnLine := False; //True; //

  i := 0; while i <  Nest.Count do
  //i := Nest.Count -1;  while i >= 0 do  //from right to left
  begin
      TmpNode := Nest.HLNode[i];

      //find till valid
      while (sfaInvalid in TmpNode.FoldAction ) and (i < Nest.Count) do
      begin
        inc(i);
        TmpNode := Nest.HLNode[i];
      end;
      //if not (sfaInvalid in TmpNode.FoldAction) then}
      if (sfaOutline in TmpNode.FoldAction ) then
          AddVerticalLine(TmpNode);

      inc(i);
      //dec(i);
  end;
end;

procedure TSynEditMarkupFoldColors.PrepareMarkupForRow(aRow: Integer);
begin
  CurrentY := aRow;
  SetLength(FHighlights,0); //reset needed to prevent using of invalid area

  if not (TCustomSynEdit(self.SynEdit).Highlighter is TSynCustomFoldHighlighter) then
    exit;

  DoMarkupFoldAtRow(aRow);
  DoMarkupParentFoldAtRow(aRow);
  //DoMarkupRangeFoldAtRow(aRow);

  FHighlights := SortLeftMostFI(FHighlights);
end;


procedure TSynEditMarkupFoldColors.DoMarkupRangeFoldAtRow(aRow: Integer);
var
  HL: TSynCustomFoldHighlighter;
  p1,p2 : TPoint;
  //Fg : TSynFoldSign;
  C : integer;

  procedure AddHighlight(ABorder : Boolean; AX,AX2, Lev : integer );
  var x,lvl : integer;

  begin
    //exit; //debug
    //lvl := HL.CurrentCodeFoldBlockLevel;
    //lvl := HL.FoldSignLevel(aRow-1);
    //if lvl <= 0 then exit;

    //Fg := HL.CurrentFoldSigns[True];
    //Fg := HL.FoldSignAtLine(aRow-1, True);
    //if Fg.Y < 0 then exit;

    x := Length(FHighlights);
    SetLength(FHighlights, x+1);
    with FHighlights[x] do begin
      Border := ABorder;
      Y  := aRow;//P1.y + 1;
      X  := AX;// Fg.X1st ;
      X2 := AX2+1;//Fg.X2 +3 ;
      ColorIdx := (Lev-1) mod (length(Colors));
      //lvl := ANode.NestLvlEnd;
      //
      //ColorIdx := (lvl-1) mod (length(Colors));

      {if sfaOpen in ANode.FoldAction then begin
        lvl := ANode.FoldLvlStart;
        //lvl := ANode.NestLvlStart; //http://forum.lazarus.freepascal.org/index.php/topic,30122.msg194841.html#msg194841
        ColorIdx := lvl mod (length(Colors));}


    end;
  end;

var
  y,i,lvl,lvl2 : integer;
  NodeList: TLazSynFoldNodeInfoList;
  TmpNode: TSynFoldNodeInfo;
  p : pointer;
  HC : TSynCustomHighlighterRange;
  R : TRect;
  //Prev,Fg : TSynFoldSign;

begin
(*  y := aRow -1;

  HL := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
  HL.CurrentLines := Lines;
  //R := HL.CurrentFoldSigns[Y];

  lvl := HL.FoldSignLevel(y);
  if lvl <= 0 then exit;

  //1. check if it 'begin' or 'end',  elsewise draw nested
  Fg := HL.FoldSignAtLine(y, True);
  if Fg.Y < 0 then exit;
  if Fg.Y = y then //open tag
    AddHighlight(False, Fg.X, Fg.X2, lvl)
  else
  begin
    if y > 0 then
    begin
      Prev := HL.FoldSignAtLine(y-1, False); //previous range
      if Prev.Y = y then // current line is the closing tag
      begin
        lvl2 := HL.FoldSignLevel(y-1);
        AddHighlight(False, Prev.X, Prev.X2, lvl2);
      end
      else //not closing nor opening. draw parent
      begin
        AddHighlight(True, Fg.X1st, Fg.X2, lvl)
      end;
    end;
  end;



  begin
    //HC := TSynCustomHighlighterRange(p);
    P1 := R.TopLeft;
    P2 := R.BottomRight;
    C := 0;
     //HC.FoldRoot;
     //AddHighlight();

  end;
*)
end;


function TSynEditMarkupFoldColors.GetFoldHighLighter: TSynCustomFoldHighlighter;
begin
  result := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
end;


procedure TSynEditMarkupFoldColors.DoCaretChanged(Sender: TObject);
var Y : integer;
begin
  Y := Caret.LineBytePos.y;
  if Y = FCaretY then exit;

  FCaretY := Y;
  FPrevCaretText := Caret.LineText;
  {$ifdef debug_FC_line_changed}
  with GetParentForm(self.SynEdit) do
    Caption:= Caret.LineText;
  {$endif}
end;



end.
