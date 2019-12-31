program strbuffertest;

{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.}

{$mode objfpc}{$H+}{$J-}

uses
  Crt, StrBuffer, SysUtils;

var
  s1, s2, s  : String;
  sb         : TStrBuffer;
  i          : Integer;
begin
  ClrScr;
  s1 := 'abcdefghij';
  s2 := '123';
  sb := TStrBuffer.Create;
  for i := 1 to Length(s1) do
  begin
    sb.Append(s1[i]);
  end;
  sb.Append(s2);
  s := sb.ToString;
  WriteLn(s);
  FreeAndNil(sb);
  ReadKey;
end.
