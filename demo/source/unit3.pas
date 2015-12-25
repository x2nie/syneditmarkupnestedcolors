unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  SynEditMarkupWordGroup,
  SynEditHighlighter,
  //SynHighlighterMiniPas2,//must before original
  SynHighlighterPas,
  SynHighlighterLFM,
  SynHighlighterPython,
  SynHighlighterHTML,
  SynHighlighterXML,
  SynHighlighterJScript;

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
  private
    { private declarations }
    FSyns : array[0..7] of TSynCustomHighlighter;
    procedure CreateSynEdits;
    procedure LeaveOnly(ASynEdit:TSynEdit);
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
    FoldConfig[ord(cfbtIfThen)].SupportedModes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
    FoldConfig[ord(cfbtIfThen)].Modes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;

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




procedure TForm3.CreateSynEdits;
var i : integer;
    M : TSynEditMarkupFoldColors;
    S : TSynEdit;
    Mi : TSynEditMarkupIfDef;
  F : array[0..7] of string = ('pas', 'pas', 'lfm', 'fold', 'bracket', 'py', 'xml', 'js');
begin
  FSyns[0] := SynFreePascalSyn1;
  FSyns[1] := SynFreePascalSyn1;
  FSyns[2] := SynLFMSyn1;
  FSyns[3] := TSynDemoHlFold.Create(self);
  FSyns[4] := TSynHighlighterBracket.Create(self);
  FSyns[5] := SynPythonSyn1;
  FSyns[6] := SynXMLSyn1;
  FSyns[7] := SynJScriptSyn1;

  for i := 0 to Pred(self.PageControl1.PageCount) do
  begin
    S := TSynEdit.Create(self);
    s.Parent := PageControl1.Page[i];
    s.Align:= alClient;
    S.Lines.LoadFromFile('demo.'+ F[i]);
    s.Highlighter := FSyns[i];
    S.LineHighlightColor.Background:=panel1.Color;


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


end.

