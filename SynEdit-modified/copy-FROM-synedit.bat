del *.pas
del *.pp
del *.lpk

rem SET LAZ=C:\lazarus_trunk
SET LAZ=E:\v\Githubs\LiteZarus

SET SYNEDIT=%LAZ%\components\synedit
copy %SYNEDIT%\SynEdit.LPK
copy %SYNEDIT%\SynEditHighlighterFoldBase.pas
copy %SYNEDIT%\SynEditFoldedView.pp
copy %SYNEDIT%\SynHighlighterXML.pas
copy %SYNEDIT%\SynHighlighterLFM.pas
copy %SYNEDIT%\SynEditMarkupFoldColoring.pas

SET IDE=%LAZ%\idE
copy %ide%\SourceSynEditor.pas


pause