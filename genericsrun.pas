program GenericsRun;

{$mode objfpc}

uses
  Crt, GenericsTest, SysUtils;

type
  TRealArray = specialize TMyArray<real>;

var
  x : TRealArray;
begin
  x := TRealArray.Create(10);
  WriteLn('x');
  FreeAndNil(x);
end.
