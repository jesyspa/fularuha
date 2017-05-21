module Assembly (
    ASM(..),
    ppASM
) where

import Text.PrettyPrint

data ASM = Label String
         | PushLabelJump String
         | PushConstant Integer
         | PushRelative Integer
         | MakeApp
         | Unwind
         | Slide Integer
         | GetRight
         | Terminate
         deriving (Eq, Ord, Read, Show)

ppASM :: ASM -> Doc
ppASM (Label xs) = text "label" <+> text xs <> char ':'
ppASM (PushLabelJump xs) = text "push goto" <+> text xs
ppASM (PushConstant i) = text "push constant" <+> integer i
ppASM (PushRelative i) = text "push relative" <+> integer i
ppASM MakeApp = text "make app"
ppASM Unwind = text "unwind"
ppASM (Slide i) = text "slide" <+> integer i
ppASM GetRight = text "get right"
ppASM Terminate = text "terminate"
