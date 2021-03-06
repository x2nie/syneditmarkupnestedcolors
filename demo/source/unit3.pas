unit unit3;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, Grids,
  SynEditHighlighterFoldBase,
  SynEditMarkupWordGroup,
  SynEditHighlighter,
  //SynHighlighterMiniPas2,//must before original
  //OriSynHighlighterPas, //must before PAS
  SynHighlighterPas,
  SynHighlighterLFM,
  SynHighlighterPython,
  SynHighlighterHTML,
  SynHighlighterXML,
  SynHighlighterJScript,
  SynEditTypes,
  TypInfo;

const
  MaxArray = 7;
type

  TMarkupWordGroupAccess = class(TSynEditMarkupWordGroup)
  end;

  { TForm3 }


  TForm3 = class(TForm)
    btnConfig: TButton;
    btnOpen: TButton;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    StatusBar2: TStatusBar;
    StringGrid1: TStringGrid;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynXMLSyn1: TSynXMLSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    procedure btnConfigClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure PageControl1Change(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1Selection(Sender: TObject; aCol, aRow: Integer);

  private
    { private declarations }
    FCell : TPoint;
    FSyns : array[0..MaxArray] of TSynCustomHighlighter;
    FSynEdits : array[0..MaxArray] of TSynEdit;
    FRecords : array[0..MaxArray] of PTypeInfo;
    procedure CreateSynEdits;
    procedure LeaveOnly(ASynEdit:TSynEdit);
    procedure CaretChanged(LogCaret: TPoint);
  protected
    FHL : TSynCustomFoldHighlighter;
    function Current : integer;
  public
    { public declarations }
  end;

var
  Form3: TForm3;

implementation

{$define gutterFoldDebug}
{$define coloring}

uses
  SynGutterFoldDebug,
  SynEditMarkupFoldColoring,
  SynEditMarkupIfDef,
  foldhl, SynHighlighterBracket

  ;

{$R *.lfm}

{ TForm3 }

procedure TForm3.FormCreate(Sender: TObject);
begin
  if self <> Form3 then
    exit; // avoid infinite loop

  with  SynFreePascalSyn1 do begin
    //FoldConfig[ord(cfbtIfThen)].Modes:=[fmMarkup, fmOutline]; //.Enabled := True;
    FoldConfig[ord(cfbtIfThen)].Enabled := True;
   // FoldConfig[ord(cfbtIfThen)].SupportedModes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
  //  FoldConfig[ord(cfbtIfThen)].Modes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
    FoldConfig[ord(cfbtIfThen)].Modes:= FoldConfig[ord(cfbtIfThen)].Modes + [fmMarkup]  - [fmFold];//;//
{
    FoldConfig[ord(cfbtForDo)].Enabled := True;
    FoldConfig[ord(cfbtForDo)].Modes:= FoldConfig[ord(cfbtForDo)].Modes + [fmMarkup]  - [fmFold];//;//


    FoldConfig[ord(cfbtWithDo)].Enabled := True;
    FoldConfig[ord(cfbtWhileDo)].Enabled := True;
 }

  end;

  CreateSynEdits;
  PageControl1Change(self); //show config


  //SynEditMiniPas.Lines.Assign( SynEditPas.Lines);
{  SynEditMiniPas.Highlighter := SynHighlighterMiniPas2.TSynPasSyn.Create(self);
  with SynHighlighterMiniPas2.TSynPasSyn(SynEditMiniPas.Highlighter) do begin
    FoldConfig[ord(cfbtIfThen)].SupportedModes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
    FoldConfig[ord(cfbtIfThen)].Modes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
 //   FoldConfig[ord(cfbtCaseElse)].Enabled := True;
//    FoldConfig[ord(cfbtCaseElse)].Enabled := True;
    CaseLabelAttri.Background:= clYellow;
    //FoldConfig[ord(cfbtIfThen)].Enabled := True;
    CommentAttri.Foreground:=clTeal;
    DirectiveAttri.Foreground := clRed;

  end;
 }
  //================= INDIVIDUAL CHECK, so debug can be focused ================
  //LeaveOnly(SynEditDemoFold);
  //LeaveOnly(SynEditLFM);
  //LeaveOnly(SynEditMiniPas);

end;

procedure TForm3.btnConfigClick(Sender: TObject);
begin

end;

procedure TForm3.btnOpenClick(Sender: TObject);
begin
  OpenDialog1.Filter:= FSyns[self.Current].DefaultFilter;
  if OpenDialog1.Execute then
     FSynEdits[Current].Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm3.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin

  if [scCaretX, scCaretY] * Changes <> [] then
    CaretChanged(FSynEdits[Current].LogicalCaretXY );
end;

procedure TForm3.PageControl1Change(Sender: TObject);
   function Str(b:boolean): string; overload;
   begin
     if b then
       result := '[x]'
     else
       result := '';
   end;
   function Str(b,b2:boolean): string; overload;
   begin
     if b
     then
       result := 'v'
     else
       result := ' ';
     if b2 then
       result := result + '  | [x]'
     else
       result := result + '  |';
   end;
var i,j :integer;
begin
  if FSyns[ PageControl1.ActivePageIndex] is TSynCustomFoldHighlighter then
     FHL := TSynCustomFoldHighlighter(FSyns[ PageControl1.ActivePageIndex])
  else
      FHL := nil;

  if (FHL <> nil) and (FRecords[ PageControl1.ActivePageIndex ] <> nil) then
  begin
    StringGrid1.RowCount:= FHL.FoldConfigCount + StringGrid1.FixedRows;
    for i := 0 to Pred(FHL.FoldConfigCount) do begin
      StringGrid1.Cells[0,i + StringGrid1.FixedRows] := GetEnumName(FRecords[ PageControl1.ActivePageIndex ] , i  );
      StringGrid1.Cells[1,i + StringGrid1.FixedRows] := Str(FHL.FoldConfig[i].Enabled);

      StringGrid1.Cells[2,i + StringGrid1.FixedRows] := Str(fmFold in FHL.FoldConfig[i].Modes, fmFold in FHL.FoldConfig[i].SupportedModes  );
      StringGrid1.Cells[3,i + StringGrid1.FixedRows] := Str(fmHide in FHL.FoldConfig[i].Modes, fmHide in FHL.FoldConfig[i].SupportedModes );
      StringGrid1.Cells[4,i + StringGrid1.FixedRows] := Str(fmMarkup in FHL.FoldConfig[i].Modes, fmMarkup in FHL.FoldConfig[i].SupportedModes );
      StringGrid1.Cells[5,i + StringGrid1.FixedRows] := Str(fmOutline in FHL.FoldConfig[i].Modes, fmOutline in FHL.FoldConfig[i].SupportedModes );
    end;

  end
  else
      StringGrid1.RowCount:=1;
end;

procedure TForm3.StringGrid1DblClick(Sender: TObject);
begin
  if (FCell.x > StringGrid1.FixedCols) and (FCell.y > StringGrid1.FixedRows) then begin

  end;
  //StringGrid1.cur;
end;

procedure TForm3.StringGrid1Selection(Sender: TObject; aCol, aRow: Integer);
begin
  FCell  := Point(aCol, ARow);
  //caption := format('%d, %d', [aCol, aRow]);
end;



procedure TForm3.CreateSynEdits;
var i : integer;
    M : TSynEditMarkupFoldColors;
    S : TSynEdit;
    Mi : TSynEditMarkupIfDef;
  F : array[0..7] of string = ('pas', 'pas', 'lfm', 'fold', 'bracket', 'py', 'xml', 'js');
  Pan : TPanel;
begin
  FSyns[0] := SynFreePascalSyn1;
  FSyns[1] := nil;//OriSynHighlighterPas.TSynFreePascalSyn.Create(self);
  FSyns[2] := SynLFMSyn1;
  FSyns[3] := TSynDemoHlFold.Create(self);
  FSyns[4] := TSynHighlighterBracket.Create(self);
  FSyns[5] := SynPythonSyn1;
  FSyns[6] := SynXMLSyn1;
  FSyns[7] := SynJScriptSyn1;

  for i := 0 to 7 do
      self.FRecords[i] := nil;
  FRecords[0] := TypeInfo(SynHighlighterPas.TPascalCodeFoldBlockType);
  FRecords[1] := nil;//TypeInfo(OriSynHighlighterPas.TPascalCodeFoldBlockType);
  FRecords[2] := TypeInfo(TLfmCodeFoldBlockType);

  FRecords[6] := TypeInfo(TXmlCodeFoldBlockType);
  FRecords[7] := TypeInfo(TJScriptFoldBlockType);


  for i := 0 to Pred(self.PageControl1.PageCount) do
  begin
    if FSyns[i] = nil then continue;
    FSynEdits[i] := TSynEdit.Create(self);
    s := FSynEdits[i];
    s.Parent := PageControl1.Page[i];
    s.Align:= alClient;
    s.Highlighter := FSyns[i];
    S.LineHighlightColor.Background:=panel1.Color;

    S.Lines.LoadFromFile('demo.'+ F[i]);
    S.AfterLoadFromFile();

    S.OnStatusChange:= @SynEditStatusChange;

    {$ifdef coloring}
    M := TSynEditMarkupFoldColors.Create(S);
    M.DefaultGroup := 0;
    S.MarkupManager.AddMarkUp(M);
    {$endif}


    if S.Highlighter is TSynCustomFoldHighlighter then
    begin
      {$ifdef gutterFoldDebug}
      TSynGutterFoldDebug.Create(S.RightGutter.Parts);
      {$endif}

      {if S.Highlighter is SynHighlighterPas.TSynPasSyn then
      begin
        Mi := TSynEditMarkupIfDef.Create(S);
        //Mi.FoldView := S.FoldedTextBuffer);
        S.MarkupManager.AddMarkUp(Mi);
        Mi.Highlighter := TSynPasSyn(S.Highlighter);
      end;}

    end;
    //
    Pan := TPanel.Create(self);
    Pan.Parent := PageControl1.Page[i];
    Pan.Height := 32;
    Pan.Align:=alTop;
    Pan.Alignment:= taLeftJustify;
    Pan.BevelOuter:= bvNone;
    Pan.BevelInner:= bvLowered;
    Pan.BorderWidth:=1;
  end;

end;



procedure TForm3.LeaveOnly(ASynEdit: TSynEdit);
var
  i : integer;
  S : TSynEdit;
begin
  for i := Pred(ComponentCount) downto 0 do
  begin
    if Components[i] is TSynEdit then
    begin
      S := TSynEdit(Components[i]);
      if S <> ASynEdit then
      FreeAndNil(s);
    end;
  end;
  PageControl1.ActivePage := TTabSheet(ASynEdit.Parent);
end;

procedure TForm3.CaretChanged(LogCaret: TPoint);
  procedure sfa();
  var
    hl : TSynCustomFoldHighlighter;
    TmpNode: TSynFoldNodeInfo;
    NodeList: TLazSynFoldNodeInfoList;
    y,i,c,j : integer;
    p : pointer;
  begin
    if not (FSyns[Current] is TSynCustomFoldHighlighter) then
       exit;
    hl := TSynCustomFoldHighlighter(FSyns[Current]);
    y := LogCaret.Y - 1;
    //LCnt := Lines.Count;
    //HL.CurrentLines := Lines;
    HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used

    (* Find the node under caret.
       - for "end" (Procedure and begin) this is the inner (end of begin) node
       - For "{$Else}" this is the closing node
    *)
    i := 0;
    NodeList := HL.FoldNodeInfo[y];
    NodeList.AddReference;
    StatusBar2.Panels[1].Text := '-';
    StatusBar2.Panels[2].Text := '-';
    c := NodeList.Count;
    try
      NodeList.ActionFilter := [];// [sfaMarkup];
      TmpNode := NodeList[i];
      while (i < c) and not(sfaInvalid in TmpNode.FoldAction) and (TmpNode.LogXEnd < LogCaret.X-1) do
      begin
        inc(i);
        TmpNode := NodeList[i];
      end;
      if (TmpNode.LogXStart > LogCaret.X - 1) {or (sfaInvalid in TmpNode.FoldAction)} then begin
        StatusBar1.Panels[1].Text := '-';
        StatusBar1.Panels[2].Text := '-';
        exit;
      end;

      j := PtrUint(TmpNode.FoldType);
      if j <= ord(high(TPascalCodeFoldBlockType)) then
         StatusBar1.Panels[1].Text :=  GetEnumName(TypeInfo(SynHighlighterPas.TPascalCodeFoldBlockType), j );
          //} inttostr(TmpNode.FoldGroup);
      StatusBar1.Panels[2].Text := SetToString(PTypeInfo(TypeInfo(TSynFoldActions)), integer( TmpNode.FoldAction), true);

      if (i+1 < c) then
      begin
        inc(i);
        TmpNode := NodeList[i];
        if not (sfaInvalid in TmpNode.FoldAction) then begin
          StatusBar2.Panels[1].Text := GetEnumName(TypeInfo(SynHighlighterPas.TPascalCodeFoldBlockType), Ptrint(TmpNode.FoldType) );
          StatusBar2.Panels[2].Text := SetToString(PTypeInfo(TypeInfo(TSynFoldActions)), integer( TmpNode.FoldAction), true);
        end;
      end;
    finally
      NodeList.ReleaseReference;
    end;
  end;
begin
  StatusBar1.Panels[0].Text := format(' %d, %d',[LogCaret.x, LogCaret.y]);
  sfa();
end;

function TForm3.Current: integer;
begin
  result :=  PageControl1.ActivePageIndex;
end;


end.

