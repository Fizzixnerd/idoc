module Text.IDoc.Render.Tex where

import ClassyPrelude

import Text.LaTeX as L hiding ((<>))
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Packages.AMSMath as M
import Text.LaTeX.Packages.AMSThm as T
import Text.LaTeX.Packages.Hyperref

type LIcon = LaTeX

defaultDecorator :: LaTeXC l => l -> l -> l
defaultDecorator title_ d =
  (documentclass [letterpaper] "memoir") ++

  (usepackage [] hyperref) ++
  (usepackage [] graphicx) ++
  (usepackage [] "listings") ++
  (usepackage ["usenames", "dvipsnames"] "xcolor") ++
  (usepackage [] amsmath) ++
  (usepackage [] amsthm) ++
  (usepackage [] amssymb) ++
  (usepackage [] "mdframed") ++
  (usepackage [] "filecontents") ++
  (usepackage ["backend=biber"] "biblatex-chicago") ++
  (addbibresource "refs.bib") ++

  (newtheorem  "Theorem" "Theorem") ++
  (newtheorem' ["Theorem"] "Lemma" "Lemma") ++
  (newtheorem' ["Theorem"] "Corollary" "Corollary") ++
  (newtheorem' ["Theorem"] "Proposition" "Proposition") ++
  (newtheorem' ["Theorem"] "Conjecture" "Conjecture") ++
  (newtheorem' ["Theorem"] "Axiom" "Axiom") ++

  (lstset ["basicstyle=\\footnotesize\\ttfamily"]) ++
  (theoremstyle T.Definition) ++
  (newtheorem  "Definition" "Definition") ++

  (newmdenv ["frametitlebackgroundcolor=lightgray"] "SideNote") ++
  (newmdenv ["frametitlebackgroundcolor=SkyBlue"] "Info") ++
  (newmdenv ["frametitlebackgroundcolor=SkyBlue"] "Tip") ++
  (newmdenv ["frametitlebackgroundcolor=Goldenrod"] "Caution") ++
  (newmdenv ["frametitlebackgroundcolor=BrickRed"] "Warning") ++
  (newmdenv ["frametitlebackgroundcolor=SkyBlue"] "Intuition") ++
  (setcounter "tocdepth" "2") ++

  (document $
    title title_ ++
    author "ILS Contributors" ++
    date today ++
    maketitle ++
    tableofcontents ++
    d ++
    printbibliography
  )

mkCode :: LaTeXC l => l -> l -> l
mkCode lang t = L.between t (raw "\\begin{lstlisting}[frame=single,framesep=5mm,breaklines,language={" ++ lang ++ raw "}]\n") (raw "\n\\end{lstlisting}\n")

mkBlock :: LaTeXC l => l -> l -> l -> l
mkBlock name btitle t = L.between t (raw "\\begin{" ++ name ++ raw "}" ++ 
                                     raw "[frametitle=" ++ btitle ++ raw "]") 
                                    (raw "\n\\end{" ++ name ++ raw "}\n")

fileContents :: LaTeXC l => FilePath -> l -> l
fileContents fname contents = L.between contents (raw "\\begin{filecontents*}{" ++ 
                                                  raw (pack fname) ++
                                                  raw "}\n")
                                                 (raw "\n\\end{filecontents*}\n")



printbibliography :: LaTeXC l => l
printbibliography = raw "\\nocite{*}\n\\printbibliography"

addbibresource :: LaTeXC l => l -> l
addbibresource res = raw "\\addbibresource{" ++ res ++ raw "}"

sideNoteBlock :: LaTeXC l => l -> l -> l
sideNoteBlock = mkBlock "SideNote"

lstset :: LaTeXC l => [Text] -> l
lstset args = L.between (concatMap raw $ intersperse "," args) (raw "\\lstset{") (raw "}")

infoBlock :: LaTeXC l => l -> l -> l
infoBlock = mkBlock "Info"

tipBlock :: LaTeXC l => l -> l -> l
tipBlock = mkBlock "Tip"

intuitionBlock :: LaTeXC l => l -> l -> l
intuitionBlock = mkBlock "Intuition"

warningBlock :: LaTeXC l => l -> l -> l
warningBlock = mkBlock "Warning"

cautionBlock :: LaTeXC l => l -> l -> l
cautionBlock = mkBlock "Caution"

newmdenv :: LaTeXC l => [Text] -> Text -> l
newmdenv opts name = raw $ "\\newmdenv[" ++ (concat $ intersperse "," opts) ++ "]" ++
                           "{" ++ name ++ "}"

newtheorem' :: LaTeXC l => [Text] -> Text -> Text -> l
newtheorem' opts env caption_ = raw $ "\\newtheorem{" ++ env ++ "}" ++
                                      "[" ++ (concat $ intersperse "," opts) ++ "]" ++
                                      "{" ++ caption_ ++ "}"

hyperref' :: LaTeXC l => Text -> l -> l
hyperref' lnk caption_ = (raw $ "\\hyperref[" ++ lnk ++ "]") ++
                         raw "{" ++ caption_ ++ raw "}"

setcounter :: LaTeXC l => l -> l -> l
setcounter name_ val = raw "\\setcounter{" ++ name_ ++ raw "}{" ++ val ++ raw "}"

vectorTexy :: (Texy a, LaTeXC l) => Vector a -> l
vectorTexy vb = concatMap texy vb

bibliography :: LaTeXC m => m -> m
bibliography bibName = raw "\\bibliography{" ++ bibName ++ raw "}"

bibliographyStyle :: LaTeXC m => m -> m
bibliographyStyle styleName = raw "\\bibliographyStyle{" ++ styleName ++ raw "}"
