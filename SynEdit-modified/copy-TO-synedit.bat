
REM SET LAZ=c:\lazarus_1_6_x
SET LAZ=C:\lazarus_trunk
rem SET LAZ=E:\v\Githubs\LiteZarus

SET SYNEDIT=%LAZ%\components\synedit
copy *.pas %SYNEDIT%\
copy *.pp %SYNEDIT%\
copy *.lpk %SYNEDIT%\

copy .\test\*.pas %SYNEDIT%\test\

PAUSE