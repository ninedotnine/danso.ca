{-# OPTIONS_GHC -XOverloadedStrings #-}

-- this program parses xml feeds from a file
-- and outputs them on one line, separated by "-s", for use by openring.
--
-- i started doing this in shell, something like the following:
--
-- ~~~
-- while IFS="" read -r line; do
--  if [[ "$line" ]]; then
--   echo -n "-s $line " ;
--  fi ;
-- done < feeds.txt
-- ~~~
--
-- but this is fragile, has at least one clear bug (blank lines),
-- and i was trying to shove it into the makefile,
-- and everything was making me sad.
-- sometimes, all i want is static type-checking.

import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Txt (lines, strip)
import Data.Text.IO qualified as TxtIO (interact)

main :: IO ()
main = TxtIO.interact parse

parse :: Text -> Text
parse = Txt.lines
    <&> map Txt.strip
    <&> filter (/= mempty)
    <&> intersperse " -s "
    <&> mconcat
    <&> ("-s " <>)
