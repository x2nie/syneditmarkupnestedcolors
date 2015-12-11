unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, SynEditMarkupWordGroup,
  SynHighlighterLFM,
  SynHighlighterPython,
  SynHighlighterHTML;

type

  TMarkupWordGroupAccess = class(TSynEditMarkupWordGroup)
  end;

  { TForm1 }


  TForm1 = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    SynEdit3: TSynEdit;
    SynEdit4: TSynEdit;
    SynEdit5: TSynEdit;
    SynEditColorFold: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPythonSyn1: TSynPythonSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure AddMarkupFoldColors;
    procedure FillLfmToSynEdit2;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SynGutterFoldDebug,
  SynEditHighlighterFoldBase,
  SynEditMarkupFoldColors, foldhl, SynHighlighterBracket,
  SynHighlighterLFM2;

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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  if self <> Form1 then
    exit; // avoid infinite loop

  FillLfmToSynEdit2();
  SynEditColorFold.Highlighter := TSynHighlighterBracket.Create(self);

  SynEdit3.Highlighter := TSynDemoHlFold.Create(self);

  AddMarkupFoldColors();
end;

procedure TForm1.AddMarkupFoldColors;
var
  M : TSynEditMarkupFoldColors;
  i : integer;
  S : TSynEdit;
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
    end;
  end;
end;

procedure TForm1.FillLfmToSynEdit2;
var F : TForm1;
var
  i : integer;
  S : TSynEdit;
begin
  if self <> Form1 then
    exit; // avoid infinite loop
  F := TForm1.Create(nil);
  for i := 0 to Pred(F.ComponentCount) do
  begin
    if F.Components[i] is TSynEdit then
    begin
      S := TSynEdit(F.Components[i]);
      S.Lines.Text := S.Name;
    end;
  end;
  SynEdit2.Lines.Text := ComponentToStringProc(F);
  F.Free;
  SynEdit2.Highlighter := SynHighlighterLFM2.TSynLFMSyn.Create(self);
end;

end.

