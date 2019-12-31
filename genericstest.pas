unit GenericsTest;

{$mode objfpc}{$H+}{$J-}

interface

  type
    generic TMyArray<T> = class
      private
        FArray : array of T;
      public
        constructor Create(n : Integer);
        function Get(i : Integer) : T;
        procedure SetItem(i : Integer; newval : T);
        property Item[i : Integer] : T read Get write SetItem;
        destructor Destroy; override;
    end;

implementation

  constructor TMyArray.Create(n : Integer);
  begin
    inherited Create;
    SetLength(FArray, n);
  end;

  function TMyArray.Get(i : Integer) : T;
  begin
    Get := FArray[i];
  end;

  procedure TMyArray.SetItem(i : Integer; newval : T);
  begin
    FArray[i] := newval;
  end;

  destructor TMyArray.Destroy;
  begin
    SetLength(FArray, 0);
    inherited Destroy;
  end;

end.
