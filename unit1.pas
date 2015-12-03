unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, SynEditMarkupWordGroup;

type

  TMarkupWordGroupAccess = class(TSynEditMarkupWordGroup)
  end;

  { TForm1 }


  TForm1 = class(TForm)
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynPasSyn1: TSynPasSyn;
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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var M : TSynEditMarkupFoldColors;
begin
  //Markup := TMarkupWordGroupAccess.Create(SynEdit1);
  //Markup.Lines := SynEdit1.TextBuffer;
  //Markup.Highlighter := SynEdit1.Highlighter;
  M := TSynEditMarkupFoldColors.Create(SynEdit1);
  SynEdit1.MarkupManager.AddMarkUp(M);

end;

end.

