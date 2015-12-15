{
module HKC.Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  in                            { \s -> TokenIn }
  $digit+                       { \s -> TokenInt (read s) }
  \<                            { \s -> TokenExport }
  \>                            { \s -> TokenImport }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
{
-- The token type:
data Token = TokenIn
           | TokenExport
           | TokenImport
           | TokenInt Int
           | TokenSym String
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)
scanTokens = alexScanTokens
}
