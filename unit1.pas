unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, SynEditMarkupWordGroup,
  SynHighlighterLFM;

type

  TMarkupWordGroupAccess = class(TSynEditMarkupWordGroup)
  end;

  { TForm1 }


  TForm1 = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPasSyn1: TSynPasSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    colors : array of TColor;
    Markup : TMarkupWordGroupAccess;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SynEditMarkupFoldColors;

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
var M : TSynEditMarkupFoldColors;
begin
  //Markup := TMarkupWordGroupAccess.Create(SynEdit1);
  //Markup.Lines := SynEdit1.TextBuffer;
  //Markup.Highlighter := SynEdit1.Highlighter;
  M := TSynEditMarkupFoldColors.Create(SynEdit1);
  M.DefaultGroup := 1;
  SynEdit1.MarkupManager.AddMarkUp(M);

  SynEdit2.Lines.Text:= ComponentToStringProc(self);
  M := TSynEditMarkupFoldColors.Create(SynEdit2);
  M.DefaultGroup := 1;
  SynEdit2.MarkupManager.AddMarkUp(M);
end;

end.

