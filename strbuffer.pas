unit StrBuffer;
{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.

 TStfBuffer is used to incrementally construct the string.
 Currently it uses a single-linked list at the back-end, so
 it is not very efficient. However, it is sufficient for intended
 purposes.}

{$mode objfpc}{$H+}{$J-}

interface

  uses
    Classes, SysUtils, List;

  type

    { TStrBuffer }

    TStrBuffer = class
      private
        FList : TList;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Append(const c : char);
        procedure Append(const str : string); overload;
        function ToString : string; override;
  end;

implementation

  { TStrBuffer }

  constructor TStrBuffer.Create;
  begin
    inherited Create;
    FList := nil;
  end;

  procedure FreeChar(p : Pointer);
  begin
    Dispose(PChar(p));
  end;

  destructor TStrBuffer.Destroy;
  begin
    FreeList(FList, @FreeChar);
    inherited Destroy;
  end;

procedure TStrBuffer.Append(const c: char);
var
  p : ^Char;
begin
  new(p);
  p^ := c;
  FList := TList.Create(p, FList);
end;

procedure TStrBuffer.Append(const str: string);
var
  i : integer;
begin
  for i := 1 to Length(str) do Append(str[i]);
end;

function TStrBuffer.ToString: string;
var
  n, i      : integer;
  lst, tmp  : TList;
begin
  {Since the list is constructed by appending to
   the front, it needs to be reversed. This is
   very inefficient for long strings.}
  lst := FList.Reverse;
  n := lst.Length;
  SetLength(result, n);
  tmp := lst;
  for i := 1 to n do begin
      result[i] := PChar(lst.Item)^;
      lst := lst.Next;
  end;
  FreeList(tmp, nil);
end;

end.

