del *.pas
del *.pp
del *.lpk

SET LAZ=C:\lazarus_trunk
rem SET LAZ=E:\v\Githubs\LiteZarus

SET SYNEDIT=%LAZ%\components\synedit
copy %SYNEDIT%\SynEdit.LPK
copy %SYNEDIT%\SynEditHighlighterFoldBase.pas
copy %SYNEDIT%\SynEditFoldedView.pp
copy %SYNEDIT%\SynHighlighterXML.pas
copy %SYNEDIT%\SynHighlighterLFM.pas
copy %SYNEDIT%\SynHighlighterPAS.pp
copy %SYNEDIT%\SynEditMarkupFoldColoring.pas
copy %SYNEDIT%\SynEditMarkupWordGroup.pp

SET IDE=%LAZ%\idE
copy %ide%\SourceSynEditor.pas


pause