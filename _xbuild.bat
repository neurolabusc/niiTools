chmod 777 ./_xclean.bat
./_xclean.bat

cp ./notgui.inc ./isgui.inc
lazbuild ./extract.lpr
lazbuild ./part.lpr
lazbuild ./mind.lpr

cp ./gui.inc ./isgui.inc
lazbuild ./partuilaz.lpr
lazbuild ./mindpro.lpr

