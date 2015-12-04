unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, SynEditMarkupWordGroup,
  SynHighlighterLFM, SynHighlighterPython, SynHighlighterHTML;

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
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPythonSyn1: TSynPythonSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure AddMarkupFoldColors;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SynEditMarkupFoldColors, foldhl;

{$R *.lfm}

function ComponentToStringProc(Component: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
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
var
  F : TSynDemoHlFold;
begin
  AddMarkupFoldColors();

  F := TSynDemoHlFold.Create(self);
  SynEdit3.Highlighter := F;
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
      M := TSynEditMarkupFoldColors.Create(S);
      M.DefaultGroup := 0;
      S.MarkupManager.AddMarkUp(M);
    end;
  end;
end;

end.

