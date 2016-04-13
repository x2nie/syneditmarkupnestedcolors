program demo1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit3, SynEditMarkupFoldColoring, //SynColorFoldHighlighter,
SynHighlighterBracket, SynGutterFoldDebug, SynEditMarkupNestLine;

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

