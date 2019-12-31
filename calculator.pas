program Calculator;

{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.}

{$mode objfpc}{$H+}{$J-}

uses
  Crt, CalcExpression, Tokenizer, List, SysUtils, Classes;


var
  RawExpr   : String;
  TokenExpr : TTokens;
  Expr      : TAbstractExpression;
  p         : Pointer;
  res       : real;
  vars      : array[0..2] of string = ('x', 'y', 'z');
  vals      : array[0..2] of real = (1, 2, 3);
begin
  ClrScr;
  Write('> ');
  ReadLn(RawExpr);
  TokenExpr := TokenizeString(RawExpr);
  WriteLn('Tokenized: ');
  for p in TokenExpr do
  begin
    Write(TToken(p).ToString, ' ');
  end;
  WriteLn('');
  Expr := ParseExpression(TokenExpr);
  WriteLn('Parsed: ', Expr.ToString);
  res := Expr.Evaluate(vars, vals);
  WriteLn('Calculated: ', res);
  FreeList(TokenExpr, @FreeToken);
  FreeAndNil(Expr);
  ReadKey;
end.