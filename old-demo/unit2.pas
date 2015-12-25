unit unit2;

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

  { TForm2 }


  TForm2 = class(TForm)
    btnConfig: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SynEditMiniPas: TSynEdit;
    SynEditPas: TSynEdit;
    SynEditLFM: TSynEdit;
    SynEditDemoFold: TSynEdit;
    SynEditPython: TSynEdit;
    SynEditXML: TSynEdit;
    SynEditJS: TSynEdit;
    SynEditColorFold: TSynEdit;
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
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure AddMarkupFoldColors;
    procedure FillLfmToSynEdit2;
    procedure LeaveOnly(ASynEdit:TSynEdit);
    procedure SetHighlighter(ASynEdit:TSynEdit; HLClass: TSynCustomHighlighterClass);
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  SynGutterFoldDebug,
  SynEditHighlighterFoldBase,
  SynEditMarkupFoldColoring,
  SynEditMarkupIfDef,
  foldhl, SynHighlighterBracket,
  uConfig
  ;

{$R *.lfm}

function ComponentToStringProc(Component: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  //s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create('');
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  if self <> Form2 then
    exit; // avoid infinite loop

  with  SynFreePascalSyn1 do begin
    //FoldConfig[ord(cfbtIfThen)].Modes:=[fmMarkup, fmOutline]; //.Enabled := True;
    FoldConfig[ord(cfbtIfThen)].SupportedModes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;
    FoldConfig[ord(cfbtIfThen)].Modes:=[fmFold, fmMarkup, fmOutline]; //.Enabled := True;

  end;


  SynEditMiniPas.Lines.Assign( SynEditPas.Lines);
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

  FillLfmToSynEdit2();

  SetHighlighter(SynEditColorFold, TSynHighlighterBracket);

  SetHighlighter(SynEditDemoFold, TSynDemoHlFold);

  AddMarkupFoldColors();
end;


procedure TForm2.btnConfigClick(Sender: TObject);
begin
  frmConfig.ShowModal;
end;

procedure TForm2.AddMarkupFoldColors;
var
  M : TSynEditMarkupFoldColors;
  i : integer;
  S : TSynEdit;
  Mi : TSynEditMarkupIfDef;
begin
  for i := 0 to Pred(ComponentCount) do
  begin
    if Components[i] is TSynEdit then
    begin
      S := TSynEdit(Components[i]);
      if not (S.Highlighter is TSynCustomFoldHighlighter) then
        continue;

      S.LineHighlightColor.Background:=panel1.Color;
      TSynGutterFoldDebug.Create(S.RightGutter.Parts);

      //continue; //debug
      M := TSynEditMarkupFoldColors.Create(S);
      M.DefaultGroup := 0;
      S.MarkupManager.AddMarkUp(M);

      //if S.Highlighter.ClassNameIs('TSynPasSyn') then
      if S.Highlighter is SynHighlighterPas.TSynPasSyn then
      begin
        Mi := TSynEditMarkupIfDef.Create(S);
        //Mi.FoldView := S.FoldedTextBuffer);
        S.MarkupManager.AddMarkUp(Mi);
        Mi.Highlighter := TSynPasSyn(S.Highlighter);
      end;

    end;
  end;
end;

procedure TForm2.FillLfmToSynEdit2;
var F : TForm2;
var
  i : integer;
  S : TSynEdit;
begin
  if self <> Form2 then
    exit; // avoid infinite loop
  if SynEditLFM = nil then
    exit;
  F := TForm2.Create(nil);
  for i := 0 to Pred(F.ComponentCount) do
  begin
    if F.Components[i] is TSynEdit then
    begin
      S := TSynEdit(F.Components[i]);
      S.Lines.Text := S.Name;
    end;
  end;
  SynEditLFM.Lines.Text := ComponentToStringProc(F);
  F.Free;
  //SynEditLFM.Highlighter := SynHighlighterLFM2.TSynLFMSyn.Create(self);
end;

procedure TForm2.LeaveOnly(ASynEdit: TSynEdit);
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

procedure TForm2.SetHighlighter(ASynEdit: TSynEdit;
  HLClass: TSynCustomHighlighterClass);
begin
  if Assigned(ASynEdit) then
    ASynEdit.Highlighter := HLClass.Create(self);
end;

end.

