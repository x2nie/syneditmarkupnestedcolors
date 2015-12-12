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
  SynHighlighterPas;
  //SynColorFoldHighlighter;

type
  //TSynColorFoldHighlighterAccess = class(TSynColorFoldHighlighter);
  TSynCustomFoldHighlighterAccess = class(TSynCustomFoldHighlighter);
  TSynPasSynAccess = class(TSynPasSyn);

{ TSynGutterFoldDebug }

procedure TSynGutterFoldDebug.PaintFoldLvl(Canvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  TextDrawer: TheTextDrawer;
  c, i, iLine, LineHeight: Integer;
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
  x1st,x1,x2 : string;
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

      if iLine = TCustomSynEdit(SynEdit).TopLine then
        s := 'Kwd Nst   End Min'
      else
      if iLine > 0 then begin
        y := iLine -1;
        Keywords := HL.FoldNodeInfo[y].Count;
        NodeList := HL.FoldNodeInfo[y];
        x1st := '';
        x1   := '';
        x2   := '';
            NodeList.AddReference;
            if NodeList.Count > 0 then
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
                TmpNode := NodeList[0];
                 //x1st  := IntToStr( TmpNode.LogVertGuideX );
                  x1   := IntToStr( TmpNode.LogXStart );
                  x2   := IntToStr( TmpNode.LogXEnd );
                  y    := TmpNode.LineIndex;
            finally
              NodeList.ReleaseReference;
            end;
        //TmpNode := NodeList[i];
        Nest.Line := iLine -1;

        NestCount:= Nest.Count;
        r := TSynCustomHighlighterRange(RngLst.Range[iLine-1]);
        s:= format(' %2d  %2d     %2d %2d   %2s..%2s ,%2s',
                   [//iLine, //r.PasFoldEndLevel, r.PasFoldMinLevel, r.PasFoldFixLevel,
                    KeyWords, NestCount,
                    r.CodeFoldStackSize, r.MinimumCodeFoldBlockLevel //, r.LastLineCodeFoldLevelFix
                    ,x1,x2, IntToStr(y)
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
  Width := 500;//PreferedWidth;
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

