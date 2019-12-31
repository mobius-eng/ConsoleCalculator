unit CalcExpression;

{ Copyright (c) 2019 Alexey V. Cherkaev - All Rights Reserved
  License: GPLv3.

 This unit provides the functionality to construct the expression tree (AST)
 from the list of tokens and to implement a simple calculator. It can cope
 with the following operations:
  - Numbers
  - Variables
  - Functions: sin, cos, tan, arcsin, arccos, arctan, exp, ln
  - Binary operations: +, -, *, /
  - Unary + and -

 The main interface is function ParseExpression that creates an AST from
 the list of tokens (TList = TTokens).

 Calculator functionality is implemented via abstract method Evaluate
 that takes an array of variable names and an array of variable values
 (since FP doesn't seem to have a built-in hash table)

 The implemented grammar is as follows:

    Expression -> Term('+'|'-'Expression)?
    Term -> ('+'|'-'Factor|Factor)('*'|'/'Term)?
    Factor -> Number | Function | Variable | '('Expression')'
    Function -> Variable'('Expression')'

 Notice, the grammar has a recursion, but it terminates if no more expressions
 of terms are found. Expression and Term are separated to deal with the
 precedens rules.

 The AST is represented by one of the following:
    - TBinary: (<operation> <arg1> <arg2>), it is used for both Expression and
      Term
    - TFunction: (<function-name> <arg1>)
    - TVariable: <variable-name>
    - TNumber: <number>.

 Future improvements: better errors/exceptions; optional logging.}

{$mode objfpc}{$H+}{$J-}

interface

  uses
    SysUtils, Classes, Tokenizer, List, Math;

  type
    TAbstractExpression = class
      public
        function Evaluate(vars : array of string; vals : array of real) : real; virtual; abstract;
    end;

    TBinary = class(TAbstractExpression)
      private
        FOperation   : String;
        FArg1, FArg2 : TAbstractExpression;
      public
        constructor Create(op : String; arg1, arg2 : TAbstractExpression);
        function ToString : String; override;
        destructor Destroy; override;
        function Evaluate(vars : array of string; vals : array of real) : real; override;
        property Operation : String read FOperation;
        property Argument1 : TAbstractExpression read FArg1;
        property Argument2 : TAbstractExpression read FArg2;
    end;

    TNumber = class(TAbstractExpression)
      private
        FNum : real;
      public
        constructor Create(x : real);
        function ToString : string; override;
        function Evaluate(vars : array of string; vals : array of real) : real; override;
        property Number : real read FNum;
    end;

    TFunction = class(TAbstractExpression)
      private
        FFunName : String;
        FArg     : TAbstractExpression;
      public
        constructor Create(fname : String; arg : TAbstractExpression);
        destructor Destroy; override;
        function ToString : string; override;
        function Evaluate(vars : array of string; vals : array of real) : real; override;
        property FunctionName : String read FFunName;
        property Argument : TAbstractExpression read FArg;
    end;

    TVariable = class(TAbstractExpression)
      private
        FVarName : String;
      public
        constructor Create(varname : String);
        function ToString : string; override;
        function Evaluate(vars : array of string; vals : array of real) : real; override;
        property VariableName : String read FVarName;
    end;

    TTokens = TList;


    function ParseExpression(e : TTokens) : TAbstractExpression;
    procedure ParseTerm(e : TTokens; var t : TAbstractExpression; var re : TTokens);
    procedure ParseFactor(e : TTokens; var f : TAbstractExpression; var re : TTokens);

implementation

  {++++++++++++++++++++++++++ TBinary +++++++++++++++++++++++++++++++}

  constructor TBinary.Create(op : String; arg1, arg2 : TAbstractExpression);
  begin
    inherited Create;
    FOperation := op;
    FArg1 := arg1;
    FArg2 := arg2;
  end;

  destructor TBinary.Destroy;
  begin
    FreeAndNil(FArg1);
    FreeAndNil(FArg2);
    inherited;
  end;

  function TBinary.ToString : string;
  begin
    Result := '(' + FOperation + ' ' + FArg1.ToString + ' ' + FArg2.ToString + ')';
  end;

  function TBinary.Evaluate(vars : array of string; vals : array of real) : real;
  var
    a1, a2 : real;
  begin
    a1 := FArg1.Evaluate(vars, vals);
    a2 := FArg2.Evaluate(vars, vals);
    if FOperation = '+' then Result := a1 + a2
    else if FOperation = '-' then Result := a1 - a2
    else if FOperation = '*' then Result := a1 * a2
    else if FOperation = '/' then Result := a1 / a2
    else raise Exception.Create('Binary evaluate: wrong operation ' + FOperation);
  end;

  {++++++++++++++++++++++++++ TNumber +++++++++++++++++++++++++++++++}

  constructor TNumber.Create(x : real);
  begin
    inherited Create;
    FNum := x;
  end;

  function TNumber.ToString : string;
  begin
    Str(FNum, Result);
  end;

  function TNumber.Evaluate(vars : array of string; vals : array of real) : real;
  begin
    Result := Fnum;
  end;

  {++++++++++++++++++++++++++ TFunction +++++++++++++++++++++++++++++}

  constructor TFunction.Create(fname : String; arg : TAbstractExpression);
  begin
    inherited Create;
    FFunName := fname;
    FArg := arg;
  end;

  destructor TFunction.Destroy;
  begin
    FreeAndNil(FArg);
    inherited;
  end;

  function TFunction.ToString : string;
  begin
    Result := '(' + FFunName + ' ' + FArg.ToString + ')';
  end;

  function TFunction.Evaluate(vars : array of string; vals : array of real) : real;
  var
    a : real;
  begin
    a := FArg.Evaluate(vars, vals);
    if FFunName = 'sin' then Result := sin(a)
    else if FFunName = 'cos' then Result := cos(a)
    else if FFunName = 'tan' then Result := tan(a)
    else if FFunName = 'arcsin' then Result := arcsin(a)
    else if FFunName = 'arccos' then Result := arccos(a)
    else if FFunName = 'arctan' then Result := arctan(a)
    else if FFunName = 'exp' then Result := exp(a)
    else if FFunName = 'ln' then Result := ln(a)
    else raise Exception.Create('Unknown function: ' + FFunName);
  end;

  {++++++++++++++++++++++++++ TVariable +++++++++++++++++++++++++++++}

  constructor TVariable.Create(varname : String);
  begin
    inherited Create;
    FVarName := varname;
  end;

  function TVariable.ToString : string;
  begin
    Result := FVarName;
  end;

  function TVariable.Evaluate(vars : array of string; vals : array of real) : real;
  var
    i : integer;
  begin
    for i := 0 to Length(vars) - 1 do
      if vars[i] = FVarName then Exit(vals[i]);
    raise Exception.Create('Variable not found: ' + FVarName);
  end;

  {++++++++++++++++ Utility functions for parsing +++++++++++++++++++}

  {Checks if the operator is '+' or '-'}
  function IsExpressionOperation(token : Pointer) : Boolean;
  var
    t : TOperationToken;
  begin
    if TToken(token) is TOperationToken then
    begin
      t := TOperationToken(token);
      Result := (t.Operation = '+') or (t.Operation = '-');
    end else Result := false;
  end;

  {Checks if the operator is '*' or '/'}
  function IsTermOperation(token : Pointer) : Boolean;
  var
    t : TOperationToken;
  begin
    if TToken(token) is TOperationToken then
    begin
      t := TOperationToken(token);
      Result := (t.Operation = '*') or (t.Operation = '/');
    end else Result := false;
  end;

  {Simplifies testing}
  function IsNil(t : pointer) : Boolean;
  begin
    IsNil := not(assigned(t));
  end;

  function IsNumberToken(p : Pointer) : Boolean;
  begin
    Result := TToken(p) is TNumberToken;
  end;

  {Sub-expression is identified by an open parenthesis}
  function IsSubExpression(t : Pointer) : Boolean;
  begin
    Result := TToken(t) = OpenParen;
  end;

  {Extracts an expression (as a list of tokens) between '(' and ')'.
   Keeps track of internal parentheses.
   Note: sub needs to be freed without destroying items:
       FreeList(sub)
   Since it share items with e}
  procedure GetSubExpression(e : TTokens; var sub, rest : TTokens);
  var
    tmp, sr : TTokens;
    parens  : integer;
  begin
    tmp := e.Next;
    sr := nil;
    parens := 0;
    while not((TToken(tmp.Item) = CloseParen) and (parens = 0)) do
    begin
      sr := TList.Create(tmp.Item, sr);
      if TToken(tmp.Item) = OpenParen then inc(parens);
      if TToken(tmp.Item) = CloseParen then dec(parens);
      tmp := tmp.Next;
      if IsNil(tmp) then raise Exception.Create('Unmatched parens');
    end;
    sub := sr.Reverse;
    FreeList(sr);
    rest := tmp.Next;
  end;


  function IsVariable(t : Pointer) : Boolean;
  begin
    Result := TToken(t) is TNameToken;
  end;

  {Note that function is a variable followed by '('. Thus, the check
   for the function needs to be performed before the check for variable
   (see below in ParseFactor)}
  function IsFunction(t : Pointer; ts : TTokens) : Boolean;
  begin
    Result := IsVariable(t) and assigned(ts) and IsSubExpression(ts.Item);
  end;

  {++++++++++++++++++++++++++ ParseExpression +++++++++++++++++++++++}

  function ParseExpression(e : TTokens) : TAbstractExpression;
  var
    term : TAbstractExpression;
    rest : TTokens;
    op   : char;
  begin
    ParseTerm(e, term, rest);
    if IsNil(rest) then Result := term
    else
    begin
      if IsExpressionOperation(rest.Item) then
      begin
        WriteLn('Parsing expression operation ', TOperationToken(rest.Item).Operation);
        Result := TBinary.Create(TOperationToken(rest.Item).Operation,
                                 term,
                                 ParseExpression(rest.Next));
      end
      else raise Exception.Create('Malformed expression in ParseExpression. Last term: ' + term.ToString);
    end;
  end;


  procedure ParseTerm(e : TTokens; var t : TAbstractExpression; var re : TTokens);
  var
    f, g  : TAbstractExpression;
    r, rr : TTokens;
  begin
    WriteLn('Entering ParseTerm');
    {Check for unary minus or plus}
    if IsExpressionOperation(e.Item) then
    begin
      ParseTerm(e.Next, f, r);
      t := TBinary.Create(TOperationToken(e.Item).Operation,
                          TNumber.Create(0),
                          f);
      re := r;
      Exit;
    end;
    ParseFactor(e, f, r);
    WriteLn('Factor parsed: ', f.ToString);
    if IsNil(r) then
    begin
      t := f;
      re := nil;
    end
    else if IsTermOperation(r.Item) then
    begin
      WriteLn('Parsing term operation ', TOperationToken(r.Item).Operation);
      ParseTerm(r.Next, g, rr);
      t := TBinary.Create(TOperationToken(r.Item).Operation,
                          f,
                          g);
      re := rr;
    end
    else
    begin
      t := f;
      re := r;
    end;
  end;


  procedure ParseFactor(e : TTokens; var f : TAbstractExpression; var re : TTokens);
  var
    t         : TToken;
    sub, rest : TTokens;
  begin
    WriteLn('Entering ParseFactor');
    t := TToken(e.Item);
    WriteLn('Stating checks on ', t.ToString);
    if IsNumberToken(t) then
    begin
      f := TNumber.Create(TNumberToken(t).Number);
      re := e.Next;
    end
    else if IsSubExpression(t) then
    begin
      GetSubExpression(e, sub, rest);
      f := ParseExpression(sub);
      FreeList(sub);
      re := rest;
    end
    else if IsFunction(e.Item, e.Next) then {must check for function before checking for variable}
    begin
      WriteLn('Parsing function ', TNameToken(e.Item).Name);
      GetSubExpression(e.Next, sub, rest);
      f := TFunction.Create(TNameToken(e.Item).Name,
                            ParseExpression(sub));
      FreeList(sub);
      re := rest;
    end
    else if IsVariable(e.Item) then
    begin
      WriteLn('Parsing variable ', TNameToken(e.Item).Name);
      f := TVariable.Create(TNameToken(e.Item).Name);
      re := e.Next;
    end
    else raise Exception.Create('Malformed expression in ParseFactor for token ' + t.ToString);
  end;

end.