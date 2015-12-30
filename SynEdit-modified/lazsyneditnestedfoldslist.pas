unit LazSynEditNestedFoldsList;

{$mode objfpc}{$H+}
{$coperators on}
{$IFDEF CPUPOWERPC} {$INLINE OFF} {$ENDIF} (* Workaround for bug 12576 (fpc) see bugs.freepascal.org/view.php?id=12576 *)

{$IFOPT C+}
  {$DEFINE SynAssertFold}
{$ENDIF}
{$IFDEF SynAssert}
  {$DEFINE SynAssertFold}
{$ENDIF}

{$IFDEF SynFoldDebug}
  {$DEFINE SynDebug}
  {$DEFINE SynFoldSaveDebug}
{$ENDIF}
{$IFDEF SynFoldSaveDebug}
  {$DEFINE SynDebug}
{$ENDIF}

interface

uses
  {LCLProc, LazLoggerBase, LazClasses,
  Classes, SysUtils, SynEditHighlighterFoldBase, SynEditFoldedView, SynEditTypes;}
  LCLProc, LazLoggerBase, LazClasses, Graphics,
  Classes, SysUtils, LazSynEditText, SynEditTypes, SynEditMiscClasses,
  SynEditMiscProcs, SynEditPointClasses,
  SynEditHighlighter, SynEditHighlighterFoldBase, SynEditFoldedView;

type
    TLazSynEditNestedFoldsListEntry = record
    FFLags: set of (nfeHasHNode, nfeMaxPrevReached);
    FGroupEndLevels: Array of Integer;
    //OpenCount: Integer;
    LineIdx: TLineIdx;
    HNode: TSynFoldNodeInfo;    // Highlighter Node
    //FNode: TSynTextFoldAVLNode; // AvlFoldNode
    PrevNodeAtSameLevel: array of TLazSynEditNestedFoldsListEntry; // Only for same NodeGroup
  end;

  TSynGetHighLighter = function(): TSynCustomFoldHighlighter of object;

  TLazSynEditNestedFoldsList = class
  // TODO: in all methods: get "FoldNodeInfo" from FoldProvider, instead of Highlighter
  private
    FHighLighterGetter: TSynGetHighLighter;
    FFoldGroup: Integer;
    FLine: TLineIdx;
    procedure SetFoldGroup(AValue: Integer);
    procedure SetLine(AValue: TLineIdx);
  private
    FFoldFlags: TSynFoldBlockFilterFlags;
    FGroupCount: Integer;
    FGroupEndLevelsAtEval: Array of integer;
    FCount, FOpeningOnLineCount: Integer;
    FOpeningLineEndIndex: Integer;
    FIncludeOpeningOnLine: Boolean;
    FNestInfo, FOnLineNestInfo: Array of TLazSynEditNestedFoldsListEntry;
    FEvaluationIndex: Integer;
    FFoldNodeInfoList: TLazSynFoldNodeInfoList;
    FFoldNodeInfoListHoldCnt: integer;

    function GetHLNode(Index: Integer): TSynFoldNodeInfo;
    function GetNodeFoldGroup(Index: Integer): Integer;
    function GetNodeLine(Index: Integer): Integer;
    function GetNodeFoldType(Index: Integer): Pointer;
    function GetNodeLineEx(Index, PrevCount: Integer): Integer;
    procedure InitSubGroupEndLevels(const AHighlighter: TSynCustomFoldHighlighter);
    procedure InitNestInfoForIndex(AnIndex: Integer);
    procedure InitLineInfoForIndex(AnIndex: Integer);
    procedure InitCount(const AHighlighter: TSynCustomFoldHighlighter);
    procedure InitOpeningOnLine(const AHighlighter: TSynCustomFoldHighlighter);
    procedure SetFoldFlags(AValue: TSynFoldBlockFilterFlags);
    procedure SetIncludeOpeningOnLine(AValue: Boolean);
    procedure AquireFoldNodeInfoList(const AHighlighter: TSynCustomFoldHighlighter; const ALine: Integer = -1);
    procedure ReleaseFoldNodeInfoList;
    procedure SetOpeningLineEndIndex(AValue: Integer);
    function HasCount: Boolean;
  public
    constructor Create(AHighLighterGetter: TSynGetHighLighter);
    procedure Clear;
    procedure ResetFilter;
    function Count: Integer;
    function OpeningOnLineCount: Integer;  // ignores FFoldFlags
    procedure Debug;
    property Line: TLineIdx read FLine write SetLine;
    property FoldGroup: Integer read FFoldGroup write SetFoldGroup;
    property FoldFlags: TSynFoldBlockFilterFlags read FFoldFlags write SetFoldFlags;
    property IncludeOpeningOnLine: Boolean read FIncludeOpeningOnLine write SetIncludeOpeningOnLine;
    // OpeningLineEnd... can only be used with sfbIncludeDisabled
    // Highest included index (unfiltered index)
    property OpeningLineEndIndex: Integer read FOpeningLineEndIndex write SetOpeningLineEndIndex;
    //property OpeningLineEndLogicalPos: Integer read FOpeningLineEndLogicalPos write SetOpeningLineEndLogicalPos;
  public
    property HLNode[Index: Integer]: TSynFoldNodeInfo read GetHLNode;
    property NodeFoldType[Index: Integer]: Pointer read GetNodeFoldType;        // e.g.cfbtBeginEnd, cfbtcfbtProcedure ...
    property NodeFoldGroup[Index: Integer]: Integer read GetNodeFoldGroup;      // independend/overlapping folds, e.g begin/end; ifdef, region
    property NodeLine[Index: Integer]: Integer read GetNodeLine;                // Index
    property NodeLineEx[Index, PrevCount: Integer]: Integer read GetNodeLineEx; // Index
  end;

implementation

uses
  Math;

{ TLazSynEditNestedFoldsList }

constructor TLazSynEditNestedFoldsList.Create(AHighLighterGetter: TSynGetHighLighter);
begin
  FHighLighterGetter := AHighLighterGetter;
  FIncludeOpeningOnLine := True;
  FFoldFlags := [];
  FFoldGroup := 0;
  FFoldNodeInfoListHoldCnt := 0;
end;

procedure TLazSynEditNestedFoldsList.SetLine(AValue: TLineIdx);
begin
  if FLine = AValue then Exit;
  FLine := AValue;
  // Todo: might be able to re-use old data
  FCount := -1;                          // will trigger InitCount
  //FEvaluationIndex := -1;
  FOpeningOnLineCount := -1;
  SetLength(FGroupEndLevelsAtEval, 0);   // will trigger InitSubGroupEndLevels
  FGroupCount := -1;
end;

procedure TLazSynEditNestedFoldsList.Clear;
begin
  FGroupCount := -1;
  SetLength(FGroupEndLevelsAtEval, 0);
  FCount := -1;
  FOpeningOnLineCount := -1;
  FEvaluationIndex := -1;
  SetLength(FNestInfo, 0);
  SetLength(FOnLineNestInfo, 0);
end;

procedure TLazSynEditNestedFoldsList.ResetFilter;
begin
  if FIncludeOpeningOnLine and (FFoldFlags = []) and (FFoldGroup = 0) and
     (FOpeningLineEndIndex = -1)
  then
    exit;
  FIncludeOpeningOnLine := True;
  FFoldFlags := [];
  FFoldGroup := 0;
  FOpeningLineEndIndex := -1;
  Clear;
end;

procedure TLazSynEditNestedFoldsList.InitSubGroupEndLevels(const AHighlighter: TSynCustomFoldHighlighter);
var
  i: integer;
begin
  if Length(FGroupEndLevelsAtEval) > 0 then
    exit;

  if FFoldGroup = 0 then begin
    // special, join other groups
    FGroupCount := AHighlighter.FoldTypeCount;
    // start at 1, so FoldGroup can be used as index
    SetLength(FGroupEndLevelsAtEval, FGroupCount + 1);
    for i := 1 to FGroupCount do
      FGroupEndLevelsAtEval[i] := AHighlighter.FoldBlockEndLevel(FLine - 1, i, FFoldFlags);
  end
  else begin
    FGroupCount := 1;
    SetLength(FGroupEndLevelsAtEval, 1);
    FGroupEndLevelsAtEval[0] := Count - OpeningOnLineCount;
  end;
end;

function TLazSynEditNestedFoldsList.GetHLNode(Index: Integer): TSynFoldNodeInfo;
begin
  if (Index < 0) or (Index >= Count) then begin
    Result.FoldAction := [sfaInvalid];
    exit;
  end;
  if Index >= FCount then
    Result := FOnLineNestInfo[Index - FCount].HNode
  else begin
    InitNestInfoForIndex(Index);
    Result := FNestInfo[Index].HNode;
  end;
end;

function TLazSynEditNestedFoldsList.GetNodeFoldGroup(Index: Integer): Integer;
begin
  if FoldGroup <> 0 then
    Result := FoldGroup
  else
    Result := HLNode[Index].FoldGroup;
end;

function TLazSynEditNestedFoldsList.GetNodeLine(Index: Integer): Integer;
begin
  InitLineInfoForIndex(Index);
  Result := FNestInfo[Index].LineIdx
end;

function TLazSynEditNestedFoldsList.GetNodeFoldType(Index: Integer): Pointer;
var
  hl: TSynCustomFoldHighlighter;
begin
  Result := nil;

  if HasCount and
     ( (Index >= Count - OpeningOnLineCount) or // OpeningOnLine
       ( (Index >= FEvaluationIndex) and (nfeHasHNode in FNestInfo[Index].FFLags) )
     )
  then begin
    Result := HLNode[Index].FoldType;
    exit;
  end;

  hl := FHighLighterGetter();
  if hl = nil then exit;

  // TODO: Cache
  if (FFoldGroup > 0) and (hl.FoldBlockNestedTypes(Line - 1, Index, Result, FFoldGroup, FFoldFlags)) then
    exit;

  Result := HLNode[Index].FoldType;
end;

function TLazSynEditNestedFoldsList.GetNodeLineEx(Index, PrevCount: Integer): Integer;
var
  Node: TLazSynEditNestedFoldsListEntry;
  MinLvl, SearchLvl, Grp, PCnt, PLineIdx: Integer;
  hl: TSynCustomFoldHighlighter;
begin
  InitLineInfoForIndex(Index);
  Result := -1;

  Node := FNestInfo[Index];
  PCnt := length(Node.PrevNodeAtSameLevel);

  if PrevCount > PCnt then begin
    if (nfeMaxPrevReached in Node.FFLags) then
      exit;
    hl := FHighLighterGetter();
    if hl = nil then exit;

    if FoldGroup = 0 then begin
      InitNestInfoForIndex(Index);
      Grp    := Node.HNode.FoldGroup;
      if sfbIncludeDisabled in FFoldFlags then
        SearchLvl := Node.HNode.NestLvlStart
      else
        SearchLvl := Node.HNode.FoldLvlStart;
    end else begin
      Grp    := FoldGroup;
      SearchLvl := Index;
    end;
    if PCnt = 0 then
      PLineIdx := Node.LineIdx - 1
    else
      PLineIdx := Node.PrevNodeAtSameLevel[PCnt-1].LineIdx - 1;

    while true do begin

      MinLvl := hl.FoldBlockMinLevel(PLineIdx, Grp, FFoldFlags);
      while (PLineIdx >= 0) and (SearchLvl < MinLvl) do begin
        dec(PLineIdx);
        MinLvl := hl.FoldBlockMinLevel(PLineIdx, Grp, FFoldFlags);
      end;

      if PLineIdx >= 0 then begin
        if length(Node.PrevNodeAtSameLevel) = PCnt then
          SetLength(Node.PrevNodeAtSameLevel, Max(PrevCount, PCnt+1));
        Node.PrevNodeAtSameLevel[PCnt].LineIdx := PLineIdx;
        Node.PrevNodeAtSameLevel[PCnt].FFLags  := [];
        inc(PCnt);
        if PCnt = PrevCount then begin
          if length(Node.PrevNodeAtSameLevel) > PCnt then
            SetLength(Node.PrevNodeAtSameLevel, PCnt);
          Result := PLineIdx;
          exit;
        end;
      end;

      If (PLineIdx < 0) or (MinLvl < SearchLvl) then begin
        Include(Node.FFLags, nfeMaxPrevReached);
        if length(Node.PrevNodeAtSameLevel) > PCnt then
          SetLength(Node.PrevNodeAtSameLevel, PCnt);
        exit;
      end;

    end;
  end;

  Result := Node.PrevNodeAtSameLevel[PrevCount-1].LineIdx;
end;

procedure TLazSynEditNestedFoldsList.InitNestInfoForIndex(AnIndex: Integer);
var
  CurLine: TLineIdx;
  hl: TSynCustomFoldHighlighter;
  i, EvalIdx, c, t, l: Integer;
  NFilter: TSynFoldActions;
  nd: TSynFoldNodeInfo;
  GrpCnt: Array of integer;
begin
  if HasCount and
     ( (AnIndex >= Count - OpeningOnLineCount) or
       ( (AnIndex >= FEvaluationIndex) and (nfeHasHNode in FNestInfo[AnIndex].FFLags) )
     )
  then exit;

  hl := FHighLighterGetter();
  if hl = nil then exit;

  AquireFoldNodeInfoList(hl);
  try
    InitLineInfoForIndex(AnIndex);
    if (AnIndex >= Count - OpeningOnLineCount) or
       ( (AnIndex >= FEvaluationIndex) and (nfeHasHNode in FNestInfo[AnIndex].FFLags) )
    then exit;

    EvalIdx := AnIndex;
    CurLine := FNestInfo[EvalIdx].LineIdx;
    while (EvalIdx < FCount-1) and (FNestInfo[EvalIdx+1].LineIdx = CurLine) do inc(EvalIdx);
    assert(Length(FNestInfo[EvalIdx].FGroupEndLevels) > 0, 'Length(FNestInfo[EvalIdx].FGroupEndLevels)');

    GrpCnt := FNestInfo[EvalIdx].FGroupEndLevels;

    NFilter := [sfaOpenFold];
    if not(sfbIncludeDisabled in FFoldFlags) then Include(NFilter, sfaFold);
    FFoldNodeInfoList.Line := CurLine;
    FFoldNodeInfoList.ActionFilter := NFilter;
    FFoldNodeInfoList.GroupFilter := FFoldGroup;
    c := FFoldNodeInfoList.Count - 1;
    //debugln(['TLazSynEditNestedFoldsList.InitNestInfoForIndex CurLine=',CurLine, '  c=',c, '  EvalIdx=',EvalIdx]);
    assert(c >= 0, 'InitNestInfoForIndex: FFoldNodeInfoList.Count');

    for i := c downto 0 do begin
      nd := FFoldNodeInfoList[i];

      if FFoldGroup = 0
      then t := nd.FoldGroup
      else t := 0;

      if (sfbIncludeDisabled in FFoldFlags)
      then l := nd.NestLvlStart
      else l := nd.FoldLvlStart;
      if l >= GrpCnt[t] then continue;

      dec(GrpCnt[t]);

      assert(GrpCnt[t] >= 0, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex GroupEndLevel < 0');
      assert(EvalIdx >= 0, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex FEvaluationIndex < 0');
      assert(FNestInfo[EvalIdx].LineIdx = CurLine, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex FNestInfo[EvalIdx].LineIdx = CurLine');

      //FNestInfo[EvalIdx].LineIdx := CurLine;
      include(FNestInfo[EvalIdx].FFLags, nfeHasHNode);
      FNestInfo[EvalIdx].HNode := nd;

      dec(EvalIdx);
    end;

  finally
    ReleaseFoldNodeInfoList;
  end;
  //for i := FCount-1 downto 0 do  DbgOut([', ',dbgs(nfeHasHNode in FNestInfo[i].FFLags)]); DebugLn();
  assert(nfeHasHNode in FNestInfo[AnIndex].FFLags, 'nfeHasHNode in FNestInfo[AnIndex].FFLags');
  assert(AnIndex >= FEvaluationIndex, 'TLazSynEditNestedFoldsList.InitNestInfoForIndex Index not found');
end;

procedure TLazSynEditNestedFoldsList.InitLineInfoForIndex(AnIndex: Integer);
var
  CurLine: TLineIdx;
  hl: TSynCustomFoldHighlighter;
  i, c, c1, l: Integer;
begin
  if HasCount and ((AnIndex >= Count - OpeningOnLineCount) or (AnIndex >= FEvaluationIndex)) then exit;
  assert(FEvaluationIndex > 0, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex already finilhed');

  hl := FHighLighterGetter();
  if hl = nil then exit;

  AquireFoldNodeInfoList(hl);
  try
    if (AnIndex >= Count - OpeningOnLineCount) or (AnIndex >= FEvaluationIndex) then exit;

    InitSubGroupEndLevels(hl);

    FNestInfo[FEvaluationIndex-1].FGroupEndLevels := copy(FGroupEndLevelsAtEval,0, length(FGroupEndLevelsAtEval));

    if (FEvaluationIndex = FCount) then
      CurLine := Line - 1
    else
      CurLine := FNestInfo[FEvaluationIndex].LineIdx - 1;

    inc(CurLine);
    while CurLine > 0 do begin
      dec(CurLine);

      c := 0;
      if FFoldGroup = 0 then begin
        i := FGroupCount;
        while (i > 0) do begin
          l := hl.FoldBlockMinLevel(CurLine, i, FFoldFlags);
          if (l < FGroupEndLevelsAtEval[i]) then begin
            c1 := FGroupEndLevelsAtEval[i] - l;
            FGroupEndLevelsAtEval[i] := FGroupEndLevelsAtEval[i] - c1;
            c := c + c1;
          end;
          dec(i);
        end;
      end
      else begin
        l := hl.FoldBlockMinLevel(CurLine, FFoldGroup, FFoldFlags);
        if l < FGroupEndLevelsAtEval[0] then begin
          c := FGroupEndLevelsAtEval[0] - l;
          FGroupEndLevelsAtEval[0] := FGroupEndLevelsAtEval[0] - c;
        end;
      end;
      if c = 0 then continue;

      while c > 0 do begin
        dec(FEvaluationIndex);
        FNestInfo[FEvaluationIndex].LineIdx := CurLine;
        FNestInfo[FEvaluationIndex].FFLags:= [];
        dec(c);
      end;

      if (AnIndex >= FEvaluationIndex) then Break;

      FNestInfo[FEvaluationIndex-1].FGroupEndLevels := copy(FGroupEndLevelsAtEval,0, length(FGroupEndLevelsAtEval));
    end;

  finally
    ReleaseFoldNodeInfoList;
  end;
  //debugln(['TLazSynEditNestedFoldsList.InitLineInfoForIndex FEvaluationIndex=', FEvaluationIndex, '  AnIndex=',AnIndex]);
  //for i := FCount-1 downto 0 do begin DbgOut([', ',FNestInfo[i].LineIdx]); if length(FNestInfo[i].FGroupEndLevels) > 0 then begin DbgOut(' ('); for c := 0 to length(FNestInfo[i].FGroupEndLevels)-1 do DbgOut([',',FNestInfo[i].FGroupEndLevels[c]]);  DbgOut(') '); end; end; DebugLn();
  assert(CurLine >= 0, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex Curline < 0');
  assert(AnIndex >= FEvaluationIndex, 'TLazSynEditNestedFoldsList.InitLineInfoForIndex Index not found');
end;

procedure TLazSynEditNestedFoldsList.InitCount(const AHighlighter: TSynCustomFoldHighlighter);
begin
  FCount := AHighlighter.FoldBlockEndLevel(FLine - 1, FFoldGroup, FFoldFlags);
  FEvaluationIndex := FCount;
  SetLength(FNestInfo, FCount);
end;

procedure TLazSynEditNestedFoldsList.InitOpeningOnLine(const AHighlighter: TSynCustomFoldHighlighter);
var
  nd: TSynFoldNodeInfo;
  OpenIdx: Array of Array of Integer; // List of open-node-index, for each FoldCroup
  OpenCnt: Array of Integer; // List of open-node-index, for each FoldCroup
  Grp, c, i, j, GrpLow, GrpHigh, ListCnt: Integer;
  oc: LongInt;
begin
  Assert((FOpeningLineEndIndex < 0) or (sfbIncludeDisabled in FoldFlags), 'OpeningLineEndIndex only implemented for sfbIncludeDisabled');

  FOpeningOnLineCount := 0;
  if FCount < 0 then
    InitCount(AHighlighter);

  if not FIncludeOpeningOnLine then
    exit;
  // FOnLineNestInfo

  AquireFoldNodeInfoList(AHighlighter, FLine);
  try
    if (sfbIncludeDisabled in FFoldFlags) then
      FFoldNodeInfoList.ActionFilter := []
    else
      FFoldNodeInfoList.ActionFilter := [sfaFold];
    FFoldNodeInfoList.GroupFilter := 0;

    if FFoldGroup = 0 then begin
      FGroupCount := AHighlighter.FoldTypeCount;
      GrpLow := 1;
      GrpHigh := FGroupCount;
    end
    else begin
      FGroupCount := 1;
      GrpLow := FFoldGroup;
      GrpHigh := FFoldGroup;
    end;
    SetLength(OpenCnt, FGroupCount);
    for Grp := 0 to FGroupCount - 1 do
      OpenCnt[Grp] := 0;
    ListCnt := FFoldNodeInfoList.Count;
    if ListCnt < 0 then
      exit;
    SetLength(OpenIdx, FGroupCount, ListCnt);

    for Grp := GrpLow to GrpHigh do begin
      (* Filtering group in the loop instead of the list only works, if 0 is the only special group
         See use of NodeIndex below, if changing this *)
      //FFoldNodeInfoList.GroupFilter := Grp;
      for i := 0 to ListCnt - 1 do begin
        nd := FFoldNodeInfoList[i];
        if (sfaInvalid in nd.FoldAction) or (nd.FoldGroup <> Grp) then
          Continue;
        if (FOpeningLineEndIndex >= 0) and (nd.AllNodeIndex > FOpeningLineEndIndex) then
          break;

        if sfaOpen in nd.FoldAction then begin
          inc(FOpeningOnLineCount);
          OpenIdx[Grp - GrpLow, OpenCnt[Grp - GrpLow]] := nd.NodeIndex; // Using NodeIndex only works, because we do NOT change the filter
          inc(OpenCnt[Grp - GrpLow]);
        end
        else
        if (nd.FoldAction * [sfaClose, sfaFold, sfaSingleLine] = [sfaClose, sfaSingleLine]) then begin
          dec(FOpeningOnLineCount);
          dec(OpenCnt[Grp - GrpLow]);
        end;
      end;
    end;

    SetLength(FOnLineNestInfo, FOpeningOnLineCount);

    //FFoldNodeInfoList.ActionFilter := [];
    //FFoldNodeInfoList.GroupFilter := 0;
    c := ListCnt - 1;
    if (FOpeningLineEndIndex >= 0) and (c > FOpeningLineEndIndex) then
      c := FOpeningLineEndIndex;
    j := FOpeningOnLineCount;

    For i := c downto 0 do begin
      if j = 0 then break;
      nd := FFoldNodeInfoList[i];
      Grp := nd.FoldGroup;
      if (Grp < GrpLow) or (Grp > GrpHigh) then Continue;
      oc := OpenCnt[Grp - GrpLow];
      Assert(oc >= 0, 'TLazSynEditNestedFoldsList.InitOpeningOnLine bad count for '+IntToStr(Grp));
      Assert((oc=0) or (OpenIdx[Grp - GrpLow, oc-1] <= i), 'TLazSynEditNestedFoldsList.InitOpeningOnLine bad index for '+IntToStr(i)+' G='+IntToStr(Grp));
      if (oc > 0) and (OpenIdx[Grp - GrpLow, oc-1] = i) then begin
        dec(OpenCnt[Grp - GrpLow]);
        dec(j);
        FOnLineNestInfo[j].LineIdx := FLine;
        FOnLineNestInfo[j].HNode := nd;
        FOnLineNestInfo[j].HNode.NodeIndex := j;
      end;
    end;

    Assert(j=0, 'TLazSynEditNestedFoldsList.InitOpeningOnLine did not fill all nodes '+IntToStr(j));
  finally
    ReleaseFoldNodeInfoList;
  end;
end;

procedure TLazSynEditNestedFoldsList.SetFoldFlags(AValue: TSynFoldBlockFilterFlags);
begin
  if FFoldFlags = AValue then Exit;
  FFoldFlags := AValue;
  Clear;
end;

procedure TLazSynEditNestedFoldsList.SetIncludeOpeningOnLine(AValue: Boolean);
begin
  if FIncludeOpeningOnLine = AValue then Exit;
  FIncludeOpeningOnLine := AValue;
  //Clear; // Do not Clear, keep the data, can be re-enabled
end;

procedure TLazSynEditNestedFoldsList.AquireFoldNodeInfoList(const AHighlighter: TSynCustomFoldHighlighter;
  const ALine: Integer);
begin
  if FFoldNodeInfoListHoldCnt = 0 then begin
    FFoldNodeInfoList := AHighlighter.FoldNodeInfo[ALine];
    FFoldNodeInfoList.AddReference;
  end else
    FFoldNodeInfoList.Line := ALine;
  inc(FFoldNodeInfoListHoldCnt);
end;

procedure TLazSynEditNestedFoldsList.ReleaseFoldNodeInfoList;
begin
  dec(FFoldNodeInfoListHoldCnt);
  if FFoldNodeInfoListHoldCnt = 0 then
    ReleaseRefAndNil(FFoldNodeInfoList);
end;

procedure TLazSynEditNestedFoldsList.SetOpeningLineEndIndex(AValue: Integer);
begin
  if FOpeningLineEndIndex = AValue then Exit;
  FOpeningLineEndIndex := AValue;
  Clear; // TODO only clear current line, the rest will still be valid
end;

function TLazSynEditNestedFoldsList.HasCount: Boolean;
begin
  Result := (FCount >= 0) and ( (not FIncludeOpeningOnLine) or (FOpeningOnLineCount >= 0) );
end;

procedure TLazSynEditNestedFoldsList.SetFoldGroup(AValue: Integer);
begin
  if FFoldGroup = AValue then Exit;
  FFoldGroup := AValue;
  Clear;
end;


function TLazSynEditNestedFoldsList.Count: Integer;
var
  hl: TSynCustomFoldHighlighter;
begin
  if (FCount < 0) then begin
    hl := FHighLighterGetter();
    if hl = nil then exit(0);
    InitCount(hl);
  end;
  if FIncludeOpeningOnLine and (FOpeningOnLineCount < 0) then begin
    hl := FHighLighterGetter();
    if hl = nil then exit(0);
    InitOpeningOnLine(hl);
  end;

  Result := FCount + OpeningOnLineCount;
end;

function TLazSynEditNestedFoldsList.OpeningOnLineCount: Integer;
var
  hl: TSynCustomFoldHighlighter;
begin
  if (not FIncludeOpeningOnLine) or (FLine < 0) then
    exit(0);

  if (FOpeningOnLineCount < 0) then begin
    hl := FHighLighterGetter();
    if hl = nil then exit(0);
    InitOpeningOnLine(hl);
  end;

  Result := FOpeningOnLineCount;
end;

procedure TLazSynEditNestedFoldsList.Debug;
var
  i: Integer;
begin
{  Debugln(['TLazSynEditNestedFoldsList for FFoldGroup=', FFoldGroup, ' FLine=', FLine,
           ' FFoldFlags=', dbgs(FFoldFlags), ' FGroupCount=', FGroupCount,
           ' FIncludeOpeningOnLine=', dbgs(FIncludeOpeningOnLine), ' FEvaluationIndex=', FEvaluationIndex,
           ' FCount=', FCount, ' FOpeningOnLineCount=', FOpeningOnLineCount]);
  Debugln(['FGroupEndLevelsAtEval=', length(FGroupEndLevelsAtEval), ': ']); for i := 0 to length(FGroupEndLevelsAtEval)-1 do DbgOut([FGroupEndLevelsAtEval[i]]); Debugln;
  for i := 0 to length(FNestInfo)-1 do
    Debugln(['N-Info ', i,': ',dbgs(FNestInfo[i])]);
}
end;

end.

