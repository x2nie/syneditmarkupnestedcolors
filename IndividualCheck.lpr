program IndividualCheck;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit2, SynEditMarkupFoldColoring, //SynColorFoldHighlighter,
SynHighlighterBracket, SynGutterFoldDebug, uConfig
  { you can add units after this };

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TfrmConfig, frmConfig);
  Application.Run;
end.

