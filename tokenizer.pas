unit Tokenizer;
{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.

 Tokenizer breaks a string into individual tokens.
 Recognised tokens:
    - Numbers
    - Arithmetic operations
    - Parentheses
    - Named functions or variables
 This tokenizer is intended for simple arithmetic expressions only.

 The main function is TokenizeString that produces a TList of tokens.
 To free the memory use
    FreeList(<list-of-tokens>, @FreeToken)
 }

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, StrBuffer, List;

type
  TToken = class

  end;

  { TNameToken }

  TNameToken = class(TToken)
  private
    f_name : string;
  public
    constructor Create(str : string);
    property Name : string read f_name;
    function ToString : string; override;
  end;

  { TNumberToken }

  TNumberToken = class(TToken)
  private
    f_number : real;
  public
    constructor Create(str : string);
    property Number : real read f_number;
    function ToString : string; override;
  end;

  { TOperationToken }

  TOperationToken = class(TToken)
  private
    f_op : string;
  public
    constructor Create(str : string);
    property Operation : string read f_op;
    function ToString : string; override;
  end;

  { TParenToken }

  TParenToken = class(TToken)
  private
    f_open : boolean;
  public
    constructor Create(open : boolean);
    function ToString : string; override;
  end;

var
  PlusToken, MinusToken, MulToken, DivToken : TOperationToken;
  OpenParen, CloseParen : TParenToken;


function TokenizeString(const str : string) : TList;
function TokenizeString(const str : string; const pos : integer; prev : TList) : TList;
procedure FreeToken(p : Pointer);

implementation

function ReadName(const str : string; const pos : integer; out newpos : integer) : string;
var
  n, rpos : integer;
  buff : TStrBuffer;
begin
  buff := TStrBuffer.Create;
  n := length(str);
  buff.Append(str[pos]);
  rpos := 2;
  newpos := pos + 1;
  while newpos <= n do begin
    case str[newpos] of
    'a'..'z', '_', '0'..'9' :
      begin
        buff.Append(str[newpos]);
        inc(rpos); inc(newpos);
      end;
    else
      break;
    end;
  end;
  Result := buff.ToString;
  FreeAndNil(buff);
end;

function ReadNumber(const str : string; const pos : integer; out newpos : integer) : string;
var
  buff : TStrBuffer;
  nstr : integer;

  procedure ReadInt;
  begin
    while newpos <= nstr do
      case str[newpos] of
      '0'..'9' :
        begin
          buff.Append(str[newpos]);
          inc(newpos);
        end;
      else
        break;
      end;
  end;

begin
  buff := TStrBuffer.Create;
  nstr := Length(str);
  newpos := pos;
  ReadInt;
  if (newpos <= nstr) and (str[newpos] = '.') then begin
     buff.Append('.');
     inc(newpos);
     ReadInt;
  end;
  if (newpos <= nstr) and (str[newpos] = 'e') then begin
     buff.Append('e');
     inc(newpos);
     if (newpos <= nstr) and ((str[newpos] = '+') or (str[newpos] = '-')) then
     begin
        buff.Append(str[newpos]);
        inc(newpos);
     end;
     ReadInt;
  end;
  Result := buff.ToString;
  FreeAndNil(buff);
end;

function TokenizeString(const str: string): TList;
var
  tmp : TList;
begin
  Result := TokenizeString(LowerCase(str), 1, nil);
  tmp := Result;
  Result := Result.Reverse;
  FreeList(tmp);
end;

function TokenizeString(const str: string; const pos: integer; prev : TList): TList;
var
  n, newpos : integer;
  token     : TList;
  s         : string;
begin
  n := length(str);
  if pos > n then begin
     Result := prev;
     exit;
  end;
  case str[pos] of
  'a'..'z', '_', '0'..'9', '-', '+', '*', '/', ')', '(' :
  begin
    case str[pos] of
    'a'..'z', '_' :
      begin
        s := ReadName(str, pos, newpos);
        Result := TList.Create(TNameToken.Create(s), prev);
      end;
    '0'..'9' :
      begin
        s := ReadNumber(str, pos, newpos);
        Result := TList.Create(TNumberToken.Create(s), prev);
      end;
    '-', '+', '*', '/', ')', '(' :
      begin
        newpos := pos + 1;
        case str[pos] of
        '-' : Result := TList.Create(MinusToken, prev);
        '+' : Result := TList.Create(PlusToken, prev);
        '*' : Result := TList.Create(MulToken, prev);
        '/' : Result := TList.Create(DivToken, prev);
        '(' : Result := TList.Create(OpenParen, prev);
        ')' : Result := TList.Create(CloseParen, prev);
        end;
      end;
    end;
    Result := TokenizeString(str, newpos, Result);
  end;
  else
    Result := TokenizeString(str, pos + 1, prev);
  end;
end;

{ TParenToken }

constructor TParenToken.Create(open: boolean);
begin
  inherited Create;
  f_open := open;
end;

function TParenToken.ToString: string;
begin
  if f_open then Result := '<(>' else Result := '<)>';
end;

{ TOperationToken }

constructor TOperationToken.Create(str: string);
begin
  inherited Create;
  f_op := str;
end;

function TOperationToken.ToString: string;
begin
  Result:= '<' + f_op + '>';
end;

{ TNameToken }

constructor TNameToken.Create(str: string);
begin
  inherited Create;
  f_name := str;
end;

function TNameToken.ToString: string;
begin
  Result:= '<' + f_name + '>';
end;

{ TNumberToken }

constructor TNumberToken.Create(str: string);
begin
  inherited Create;
  f_number := StrToFloat(str);
end;

function TNumberToken.ToString: string;
begin
  Result:= '<' + FloatToStr(f_number) + '>';
end;

procedure FreeToken(p : Pointer);
var
  t : TToken;
begin
  t := TToken(p);
  if (t = PlusToken) or (t = MinusToken) or (t = MulToken) or (t = DivToken) or
     (t = OpenParen) or (t = CloseParen) then exit;
  FreeAndNil(TToken(p));
end;


initialization
  PlusToken := TOperationToken.Create('+');
  MinusToken := TOperationToken.Create('-');
  MulToken := TOperationToken.Create('*');
  DivToken := TOperationToken.Create('/');
  OpenParen := TParenToken.Create(true);
  CloseParen := TParenToken.Create(false);
finalization
  FreeAndNil(PlusToken);
  FreeAndNil(MinusToken);
  FreeAndNil(MulToken);
  FreeAndNil(DivToken);
  FreeAndNil(OpenParen);
  FreeAndNil(CloseParen);
end.

