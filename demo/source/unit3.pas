unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  TypInfo,
  SynEditMarkupWordGroup,
  SynEditHighlighter,
  //SynHighlighterMiniPas2,//must before original
  OriSynHighlighterPas, //must before PAS
  SynHighlighterPas,
  SynHighlighterLFM,
  SynHighlighterPython,
  SynHighlighterHTML,
  SynHighlighterXML,
  SynHighlighterJScript, SynEditTypes;

const
  MaxArray = 7;
type

  TMarkupWordGroupAccess = class(TSynEditMarkupWordGroup)
  end;

  { TForm3 }


  TForm3 = class(TForm)
    btnConfig: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
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
    procedure FormCreate(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    { private declarations }
    FSyns : array[0..MaxArray] of TSynCustomHighlighter;
    FSynEdits : array[0..MaxArray] of TSynEdit;
    procedure CreateSynEdits;
    procedure LeaveOnly(ASynEdit:TSynEdit);
    procedure CaretChanged(LogCaret: TPoint);
  protected
    function Current : integer;
  public
    { public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  SynGutterFoldDebug,
  SynEditHighlighterFoldBase,
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
    //FoldConfig[ord(cfbtIfThen)].Enabled := True;
   // FoldConfig[ord(cfbtIfThen)].SupportedModes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
  //  FoldConfig[ord(cfbtIfThen)].Modes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
    FoldConfig[ord(cfbtIfThen)].Modes:= FoldConfig[ord(cfbtIfThen)].Modes + [fmMarkup] - [fmFold];

  end;

  CreateSynEdits;

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

procedure TForm3.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin

  if [scCaretX, scCaretY] * Changes <> [] then
    CaretChanged(FSynEdits[Current].LogicalCaretXY );
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
  FSyns[1] := OriSynHighlighterPas.TSynFreePascalSyn.Create(self);
  FSyns[2] := SynLFMSyn1;
  FSyns[3] := TSynDemoHlFold.Create(self);
  FSyns[4] := TSynHighlighterBracket.Create(self);
  FSyns[5] := SynPythonSyn1;
  FSyns[6] := SynXMLSyn1;
  FSyns[7] := SynJScriptSyn1;

  for i := 0 to Pred(self.PageControl1.PageCount) do
  begin
    FSynEdits[i] := TSynEdit.Create(self);
    s := FSynEdits[i];
    s.Parent := PageControl1.Page[i];
    s.Align:= alClient;
    S.Lines.LoadFromFile('demo.'+ F[i]);
    s.Highlighter := FSyns[i];
    S.LineHighlightColor.Background:=panel1.Color;

    S.OnStatusChange:= @SynEditStatusChange;


    M := TSynEditMarkupFoldColors.Create(S);
    M.DefaultGroup := 0;
    S.MarkupManager.AddMarkUp(M);

    if S.Highlighter is TSynCustomFoldHighlighter then
    begin
      TSynGutterFoldDebug.Create(S.RightGutter.Parts);

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
    y,i,c : integer;
  begin
    if not (FSyns[Current] is TSynCustomFoldHighlighter) then exit;
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

      StatusBar1.Panels[1].Text := inttostr(TmpNode.FoldGroup);
      StatusBar1.Panels[2].Text := SetToString(PTypeInfo(TypeInfo(TSynFoldActions)), integer( TmpNode.FoldAction), true);

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

