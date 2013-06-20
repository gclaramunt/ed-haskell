COMPILER="ghc"
SRC="src/*.hs"
OUT="out"
OPTIONS="-odir $OUT -hidir $OUT -o edi"

CALL="$COMPILER $SRC $OPTIONS"

$CALL
while inotifywait -qq -r -e modify .; do echo '<><><><> Starting build <><><><>';echo `date`; $CALL; echo '<><><><> Done <><><><>'; done 