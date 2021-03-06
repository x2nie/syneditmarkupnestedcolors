unit SynGutterFoldDebug;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Graphics, Menus, LCLIntf, SynGutterBase, SynEditMiscProcs,
  SynEditFoldedView, SynEditMouseCmds, SynEditHighlighterFoldBase, LCLProc, LCLType, ImgList;

type

  { TSynGutterFoldDebug }

  TSynGutterFoldDebug = class(TSynGutterPartBase)
  private
    procedure PaintFoldLvl(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    procedure PaintPasFoldLvl(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    //procedure PaintColorFoldLvl(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    function GetFoldHighLighter: TSynCustomFoldHighlighter;
  public
    constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
  end;

implementation
uses
  SynEdit,SynEditHighlighter,SynTextDrawer,
  typinfo,
  SynHighlighterJScript,
  //SynHighlighterMiniPas2,
  SynHighlighterPas;
  //SynColorFoldHighlighter;

type
  //TSynColorFoldHighlighterAccess = class(TSynColorFoldHighlighter);
  TSynCustomFoldHighlighterAccess = class(TSynCustomFoldHighlighter);
  TSynPasSynAccess = class(TSynPasSyn);
  //TSynMiniPasSynAccess = class(SynHighlighterMiniPas2.TSynPasSyn);


{ TSynGutterFoldDebug }

procedure TSynGutterFoldDebug.PaintFoldLvl(Canvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  TextDrawer: TheTextDrawer;
  c, i,j, iLine, LineHeight: Integer;
  rcLine: TRect;
  dc: HDC;
  s: String;
  RngLst: TSynHighlighterRangeList;
  r:  TSynCustomHighlighterRange;//TSynPasSynRange;
  HL : TSynCustomFoldHighlighterAccess;
  Y,NestCount,KeyWords : integer;
  Nest : TLazSynEditNestedFoldsList;
  TmpNode: TSynFoldNodeInfo;
  NodeList: TLazSynFoldNodeInfoList;
  x1st,x1,x2,ty,oc : string;
  InProc:string;
  p : Pointer;
 begin
   //y := aRow-1;
   Nest := TLazSynEditNestedFoldsList.Create(@GetFoldHighLighter);
   Nest.ResetFilter;
   Nest.Clear;
   Nest.FoldGroup := 0;//FDefaultGroup;//1;//FOLDGROUP_PASCAL;
   Nest.FoldFlags :=  [];//[sfbIncludeDisabled]; //
   Nest.IncludeOpeningOnLine := False; //True; //

  if TCustomSynEdit(SynEdit).Highlighter = nil then exit;
  if not(TCustomSynEdit(SynEdit).Highlighter is TSynCustomFoldHighlighter)  then exit;
  //TCustomSynEdit(SynEdit).Highlighter.CurrentLines := TheLinesView;
  TextDrawer := Gutter.TextDrawer;
  dc := Canvas.Handle;
  //TSynHighlighterPasRangeList
  //mojo
  //woles
  //getuk
  //RngLst := TSynHighlighterRangeList(TheLinesView.Ranges[TCustomSynEdit(SynEdit).Highlighter]);

  HL := TSynCustomFoldHighlighterAccess( TCustomSynEdit(self.SynEdit).Highlighter );
  RngLst := HL.CurrentRanges;

  // Clear all
  TextDrawer.BeginDrawing(dc);
  try
    TextDrawer.SetBackColor(Gutter.Color);
    TextDrawer.SetForeColor(TCustomSynEdit(SynEdit).Font.Color);
    TextDrawer.SetStyle(TCustomSynEdit(SynEdit).Font.Style);
    TextDrawer.SetFrameColor(clNone);
     with AClip do
       TextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);

    rcLine := AClip;
    rcLine.Top := 0;//x2nie
    rcLine.Bottom := AClip.Top;
    LineHeight := TCustomSynEdit(SynEdit).LineHeight;
    c := TCustomSynEdit(SynEdit).Lines.Count;
    //FirstLine := TCustomSynEdit(SynEdit).TopLine;
    LastLine := TCustomSynEdit(SynEdit).TopLine+ TCustomSynEdit(SynEdit).LinesInWindow;
    for i := FirstLine to LastLine do
    begin
      iLine := FoldView.DisplayNumber[i];
      if (iLine < 0) or (iLine > c) then break;
      // next line rect
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := rcLine.Bottom + LineHeight;

      if iLine = TCustomSynEdit(SynEdit).CaretY then
        TextDrawer.SetBackColor(TCustomSynEdit(SynEdit).LineHighlightColor.Background )
      else
        TextDrawer.SetBackColor(Gutter.Color);


      if iLine = TCustomSynEdit(SynEdit).TopLine then
        s := 'Kwd Nst   Min End  NMi Nst '
      else
      if iLine > 0 then begin
        s := '';
        y := iLine -1;
        Keywords := HL.FoldNodeInfo[y].Count;
        NodeList := HL.FoldNodeInfo[y];
        x1st := '';
        x1   := '';
        x2   := '';
        ty   := '';
        InProc := '- - ';
        if TSynCustomFoldHighlighter(HL) is {SynHighlighterMiniPas2.}TSynPasSyn then
        begin
          //j:=SynHighlighterMiniPas2.TSynHighlighterPasRangeList(TSynMiniPasSynAccess(HL).CurrentRanges).PasRangeInfo[y].EndLevelIfDef;
          //InProc := inttostr(j)+' ';
          InProc := {InProc +} inttostr(TSynPasSynAccess(HL).InProcLevel) + ' ';

          if TSynPasSynAccess(HL).InProcNeck then
            InProc := InProc + 'T '
          else
            InProc := InProc + '. ';

          {if TSynMiniPasSynAccess(HL).IsProcedureNeckInRange then
            InProc := InProc + 'R '
          else
            InProc := InProc + '- ';}

          //p := p - PtrUInt(CountPascalCodeFoldBlockOffset);
          //p := TSynMiniPasSynAccess(HL).TopCodeFoldBlockType();
          //InProc := InProc + copy( GetEnumName(TypeInfo(TPascalCodeFoldBlockType), PtrUint(p) ), 5,100) ;
        end;

            NodeList.AddReference;
            if NodeList.Count > 0 then
            try
              NodeList.ActionFilter := [
                  {sfaMarkup,}
            //      sfaFold
                  //sfaFoldFold
                  //sfaFoldHide
                  //sfaSingleLine
                  //sfaMultiLine
                  //sfaOpen
                  ];
              for j := 0 to min(2, NodeList.Count-1) do
              begin
              //NodeList.FoldFlags:= [sfbIncludeDisabled];
                TmpNode := NodeList[j];
                 //x1st  := IntToStr( TmpNode.LogVertGuideX );
                  x1   := IntToStr( TmpNode.LogXStart );
                  x2   := IntToStr( TmpNode.LogXEnd );
                  y    := TmpNode.LineIndex;
                  if sfaOpen in TmpNode.FoldAction then
                    oc := '<-'
                  else if sfaClose in TmpNode.FoldAction then
                    oc := '->'
                  else oc := '?~';
                  if TSynCustomFoldHighlighter(HL) is TSynPasSyn
                  //or TSynCustomFoldHighlighter(HL) is SynHighlighterMiniPas2.TSynPasSyn
                  then
                  begin
                    p := TmpNode.FoldType;
                    if p >= CountPascalCodeFoldBlockOffset then
                    p := p - PtrUInt(CountPascalCodeFoldBlockOffset);
                    ty   := copy( GetEnumName(TypeInfo(TPascalCodeFoldBlockType), PtrUint(p) ), 5,6) ;
                  end
                  else
                  if TSynCustomFoldHighlighter(HL) is TSynJScriptSyn then
                  begin
                    p := TmpNode.FoldType;
                    ty   := copy( GetEnumName(TypeInfo(TJScriptFoldBlockType), PtrUint(p) ), 4,100) ;
                  end;
                s := s + Format('%10s %s..%s,%s', [oc+ ty, x1,x2, IntToStr(y)])
              end;

            finally
              NodeList.ReleaseReference;
            end;
        //TmpNode := NodeList[i];
        Nest.Line := iLine -1;

        NestCount:= Nest.Count;
        r := TSynCustomHighlighterRange(RngLst.Range[iLine-1]);
        s:= format(' %2d  %2d  %3d %3d  %3d %3d  @%s  %s',
                   [//iLine, //r.PasFoldEndLevel, r.PasFoldMinLevel, r.PasFoldFixLevel,
                    KeyWords, NestCount,
                    r.MinimumCodeFoldBlockLevel, r.CodeFoldStackSize, //, r.LastLineCodeFoldLevelFix
                    r.MinimumNestFoldBlockLevel, r.NestFoldStackSize,
                    InProc,
                    s
                   ]
                  );
      end
      else
        s:= '-';

      TextDrawer.ExtTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED, rcLine,
        PChar(Pointer(S)),Length(S));
    end;

  finally
    TextDrawer.EndDrawing;
    Nest.Free;
  end;

end;

procedure TSynGutterFoldDebug.PaintPasFoldLvl(Canvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  TextDrawer: TheTextDrawer;
  c, i, iLine, LineHeight: Integer;
  rcLine: TRect;
  dc: HDC;
  s: String;
  RngLst: TSynHighlighterRangeList;
  r: TSynPasSynRange;//TSynPasSynRange;
  HL : TSynPasSynAccess;
begin
  if TCustomSynEdit(SynEdit).Highlighter = nil then exit;
  if not(TCustomSynEdit(SynEdit).Highlighter is TSynPasSyn)  then exit;
  //TCustomSynEdit(SynEdit).Highlighter.CurrentLines := TheLinesView;
  TextDrawer := Gutter.TextDrawer;
  dc := Canvas.Handle;
  //TSynHighlighterPasRangeList
  //mojo
  //woles
  //getuk
  //RngLst := TSynHighlighterRangeList(TheLinesView.Ranges[TCustomSynEdit(SynEdit).Highlighter]);

  HL := TSynPasSynAccess( TCustomSynEdit(self.SynEdit).Highlighter );
  RngLst := HL.CurrentRanges;
  // Clear all
  TextDrawer.BeginDrawing(dc);
  try
    TextDrawer.SetBackColor(Gutter.Color);
    TextDrawer.SetForeColor(TCustomSynEdit(SynEdit).Font.Color);
    TextDrawer.SetFrameColor(clNone);
     with AClip do
       TextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);

    rcLine := AClip;
    rcLine.Bottom := AClip.Top;
    LineHeight := TCustomSynEdit(SynEdit).LineHeight;
    c := TCustomSynEdit(SynEdit).Lines.Count;
    for i := FirstLine to LastLine do
    begin
      iLine := FoldView.DisplayNumber[i];
      if (iLine < 0) or (iLine > c) then break;
      // next line rect
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := rcLine.Bottom + LineHeight;

      if iLine = TCustomSynEdit(SynEdit).CaretY then
        TextDrawer.SetBackColor(TCustomSynEdit(SynEdit).LineHighlightColor.Background )
      else
        TextDrawer.SetBackColor(Gutter.Color);


      if iLine > 0 then begin
        r := TSynPasSynRange(RngLst.Range[iLine-1]);
        s:= format('%2d %2d %2d  %2d %2d  ',
                   [r.PasFoldEndLevel, r.PasFoldMinLevel, r.PasFoldFixLevel,
                    r.CodeFoldStackSize, r.MinimumCodeFoldBlockLevel //, r.LastLineCodeFoldLevelFix
                   ]
                  );
      end
      else
        s:= '-';

      TextDrawer.ExtTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED, rcLine,
        PChar(Pointer(S)),Length(S));
    end;

  finally
    TextDrawer.EndDrawing;
  end;

end;

function TSynGutterFoldDebug.GetFoldHighLighter: TSynCustomFoldHighlighter;
begin
  result := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
end;

{procedure TSynGutterFoldDebug.PaintColorFoldLvl(Canvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  TextDrawer: TheTextDrawer;
  c, i, iLine, LineHeight: Integer;
  rcLine: TRect;
  dc: HDC;
  s: String;
  RngLst: TSynHighlighterRangeList;
  r: TSynCustomHighlighterRange;//TSynPasSynRange;
  HL : TSynColorFoldHighlighterAccess;
begin
  if TCustomSynEdit(SynEdit).Highlighter = nil then exit;
  if not(TCustomSynEdit(SynEdit).Highlighter is TSynColorFoldHighlighter)  then exit;
  //TCustomSynEdit(SynEdit).Highlighter.CurrentLines := TheLinesView;
  TextDrawer := Gutter.TextDrawer;
  dc := Canvas.Handle;
  //TSynHighlighterPasRangeList
  //mojo
  //woles
  //getuk
  //RngLst := TSynHighlighterRangeList(TheLinesView.Ranges[TCustomSynEdit(SynEdit).Highlighter]);

  HL := TSynColorFoldHighlighterAccess( TCustomSynEdit(self.SynEdit).Highlighter );
  RngLst := HL.CurrentRanges;
  // Clear all
  TextDrawer.BeginDrawing(dc);
  try
    TextDrawer.SetBackColor(Gutter.Color);
    TextDrawer.SetForeColor(TCustomSynEdit(SynEdit).Font.Color);
    TextDrawer.SetFrameColor(clNone);
     with AClip do
       TextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);

    rcLine := AClip;
    rcLine.Bottom := AClip.Top;
    LineHeight := TCustomSynEdit(SynEdit).LineHeight;
    c := TCustomSynEdit(SynEdit).Lines.Count;
    for i := FirstLine to LastLine do
    begin
      iLine := FoldView.DisplayNumber[i];
      if (iLine < 0) or (iLine > c) then break;
      // next line rect
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := rcLine.Bottom + LineHeight;

      if iLine > 0 then begin
        r := TSynCustomHighlighterRange(RngLst.Range[iLine-1]);
        s:= format('%2d %2d %2d  %2d %2d  ',
                   [//r.PasFoldEndLevel, r.PasFoldMinLevel, r.PasFoldFixLevel,
                    r.CodeFoldStackSize, r.MinimumCodeFoldBlockLevel //, r.LastLineCodeFoldLevelFix
                   ]
                  );
      end
      else
        s:= '-';

      TextDrawer.ExtTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED, rcLine,
        PChar(Pointer(S)),Length(S));
    end;

  finally
    TextDrawer.EndDrawing;
  end;

end; }

constructor TSynGutterFoldDebug.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := False;
  Width := 600;//PreferedWidth;
end;

procedure TSynGutterFoldDebug.Paint(Canvas: TCanvas; AClip: TRect; FirstLine,
  LastLine: integer);
begin
  Canvas.Pen.Color := clAqua;
  with AClip do
  canvas.Line(left,top,right,bottom);
  {if TCustomSynEdit(self.SynEdit).Highlighter is TSynColorFoldHighlighter then //higher checked first
    PaintColorFoldLvl(Canvas, AClip, FirstLine, LastLine)
  else}
  //if TCustomSynEdit(self.SynEdit).Highlighter is TSynPasSyn then //lower
    //PaintPasFoldLvl(Canvas, AClip, FirstLine, LastLine)  else
    PaintFoldLvl(Canvas, AClip, FirstLine, LastLine);
end;

end.

