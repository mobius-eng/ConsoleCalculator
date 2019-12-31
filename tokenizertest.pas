program tokenizertest;

{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.}

{$mode objfpc}{$H+}{$J-}

uses
  Crt, Tokenizer, List, SysUtils;

const
  expr = 'sin(x*2) +exp(cos(y - 2)) * 5';

var
  tokens : TList;
  p      : Pointer;
begin
  ClrScr;
  tokens := TokenizeString(expr);
  for p in tokens do
  begin
    Write(TToken(p).ToString, ' ');
  end;
  WriteLn('');
  FreeList(tokens, @FreeToken);
  ReadKey;
end.
