program ListTest;
{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.}
{$mode objfpc}{$H+}{$J-}

uses
  Crt, List, SysUtils;

  type
    PString = ^String;

  procedure FreeString(p : Pointer);
  begin
    Dispose(PString(p));
  end;

var
  lst, tmp : TList;
  s        : PString;
  i        : Integer;
  p        : Pointer;
begin
  ClrScr;
  new(s);
  s^ := '0';
  lst := TList.Create(s);
  if assigned(lst.Next) then
  begin
    WriteLn('Something wrong!');
  end;
  for i := 1 to 10 do
  begin
    new(s);
    s^ := IntToStr(i);
    lst := TList.Create(s, lst);
  end;
  tmp := lst.Reverse;
  FreeList(lst, nil);
  lst := tmp;
  for p in lst do
  begin
    WriteLn(String(p^));
  end;
  ReadKey;
  FreeList(lst, @FreeString);
  ReadKey;
end.
