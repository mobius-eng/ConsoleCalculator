unit List;
{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.

 Simple single-linked list implementation.
 With FPC 3.0.4 under Windows generic class
 specialization causes internal error. Thus,
 less safe alternative with pointers is chosen.

 The interface to the list is similar to LISP's:
 "CONS": TList.Create(<pointer-to-item>, <rest, can be nil>)
 "CAR" : <list-object>.Item (gives a pointer)
 "CDR" : <list-object>.Next

 Enumerator for the list is provided to access via for-in operator:

    for p in <list-object> do ...
 "p" here is a pointer, needs to be cast to more specific type.

 (Note: classes are pointers and thus can be used directly without dereferencing)

 Memory is freed with FreeList procedure:
    FreeList(<list-object>, <procedure, optional>)
 The second argument is a procedure used to free stored items. If
 it is not desired to free them, nil can be passed.}

{$mode objfpc}{$H+}{$J-}

interface

  uses
    SysUtils, Classes, Types;

  type
    TListEnumerator = class;
    TList = class
      protected
        FItem : Pointer;
        FNext : TList;
        function GetItem : Pointer;
        function GetNext : TList;
      public
        constructor Create(x : Pointer; rest : TList = nil);
        function Reverse : TList;
        function GetEnumerator : TListEnumerator;
        function Length : Integer;
        property Item : Pointer read GetItem;
        property Next : TList read GetNext;
    end;

    TListEnumerator = class
      private
        curr : TList;
        next : TList;
      public
        constructor Create(alist : TList);
        function MoveNext : Boolean;
        function GetCurrent : Pointer;
        property Current : Pointer read GetCurrent;
    end;

    {Type for the procedure to free the items stored in the list}
    TFreePointer = procedure(x :Pointer);

  procedure FreeList(alist : TList; f : TFreePointer = nil);

implementation

  constructor TList.Create(x : Pointer; rest : TList = nil);
  begin
    FItem := x;
    FNext := rest;
  end;

  function TList.GetItem : Pointer;
  begin
    GetItem := FItem;
  end;

  function TList.GetNext : TList;
  begin
    GetNext := FNext;
  end;

  function TList.GetEnumerator : TListEnumerator;
  begin
    result := TListEnumerator.Create(self);
  end;

  {Since MoveNext is called first, need a bit of a
  special strategy to initialize curr with the
  "element before first"}
  constructor TListEnumerator.Create(alist : TList);
  begin
    curr := nil;
    next := alist;
  end;

  function TListEnumerator.MoveNext : Boolean;
  begin
    if assigned(next) then
    begin
      curr := next;
      next := curr.Next;
      MoveNext := true;
    end
    else MoveNext := false;
  end;


  function TListEnumerator.GetCurrent : Pointer;
  begin
    if assigned(curr) then GetCurrent := curr.Item
    else GetCurrent := nil;
  end;

  procedure FreeList(alist : TList; f : TFreePointer = nil);
  var
    tmp : TList;
  begin
    while assigned(alist) do
    begin
      {Free stored items if required}
      if assigned(f) then f(alist.Item);
      tmp := alist.Next;
      FreeAndNil(alist);
      alist := tmp;
    end;
  end;

  function TList.Reverse : TList;
  var
    alist, tmp, newlist : TList;
    p                   : Pointer;
  begin
    alist := self;
    newlist := nil;
    while assigned(alist) do
    begin
      p := alist.Item;
      newlist := TList.Create(alist.Item, newlist);
      tmp := alist;
      alist := alist.Next;
    end;
    Reverse := newlist;
  end;

  function TList.Length : Integer;
  var
    alist : TList;
  begin
    Length := 0;
    alist := self;
    while assigned(alist) do
    begin
      inc(Length);
      alist := alist.Next;
    end;
  end;

end.
