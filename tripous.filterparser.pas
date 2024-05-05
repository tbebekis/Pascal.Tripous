unit Tripous.FilterParser;


{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
   Classes
  ,SysUtils
  ,Variants
  ,TypInfo
  ,Contnrs
  ,StrUtils
  ,Masks
  ;


type
  TTokenKind = (
  { single character }
   toLEFT_PAREN
  ,toRIGHT_PAREN
  ,toLEFT_BRACKET    // [  not used
  ,toRIGHT_BRACKET   // ]  not used
  ,toLEFT_BRACE      // {  not used
  ,toRIGHT_BRACE     // }  not used
  ,toCOMMA
  ,toDOT
  ,toMINUS
  ,toPLUS
  ,toSEMICOLON       // ; not used
  ,toSLASH
  ,toSTAR

  { one or two characters }
  ,toNOT_EQUAL
  ,toEQUAL
  ,toGREATER
  ,toGREATER_EQUAL
  ,toLESS
  ,toLESS_EQUAL

  { literals }
  ,toIDENTIFIER
  ,toSTRING_
  ,toNUMBER
  ,toKEYWORD

  ,toEOF
  );

 TTokenKindSet = set of TTokenKind;

type
  { TToken }
  TToken = class
  public
    Kind: TTokenKind;
    Lexeme: string;
    Literal: Variant;
    Line: Integer;

    constructor Create(aKind: TTokenKind; aLexeme: string; aLiteral: Variant; aLine: Integer);

    function ToString(): string; override;
  end;

type
  TFilterErrorEvent =  procedure(Sender: TObject; ErrorMessage: string) of object;
  TFilterVariableValueEvent = procedure(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant) of object;

type
  { TFilterScanner }
  TFilterScanner = class
  private
    FOnError: TFilterErrorEvent;
    Source: string;
    TokenList: TObjectList;
    KeywordList: TStringList;

    // scan state
    Start: Integer;
    Current: Integer;
    Line: Integer;

    function IsAlpha(C: Char): Boolean;
    function IsAlphaNumeric(C: Char): Boolean;
    function IsDigit(C: Char): Boolean;

    function IsKeyword(Text: string): Boolean;
    function IsAtEnd(): Boolean;

    function Match(Expected: Char): Boolean;
    procedure ScanToken();

    function Advance(): Char;
    function Peek(): Char;
    function PeekNext(): Char;

    procedure ReadIdentifier();
    procedure ReadNumber();
    procedure ReadString();

    procedure AddToken(Kind: TTokenKind); overload;
    procedure AddToken(Kind: TTokenKind; Literal: Variant); overload;

    function SubStr(S: string; StartIndex: Integer; EndIndex: Integer): string;
    procedure Error(ErrorMessage: string);
  public
    constructor Create();
    destructor Destroy; override;

    function Scan(Text: string): TObjectList;

    property OnError: TFilterErrorEvent read FOnError write FOnError;
  end;


  type
    TFilterParser = class;
    TFilterExprVisitorBase = class;

  type
    { TFilterExpr }
    TFilterExpr = class
    public
      function Accept(Visitor: TFilterExprVisitorBase): Variant; virtual; abstract;
      function GetExprList(): TList; virtual; abstract;
      function ToString(): string; override;
    end;

  type
    { TBinaryFilterExpr }
    TBinaryFilterExpr = class(TFilterExpr)
    public
      Left: TFilterExpr;
      Right: TFilterExpr;
      Operation: TToken;

      constructor Create(aLeft: TFilterExpr; aOperation: TToken; aRight: TFilterExpr);
      destructor Destroy; override;

      function Accept(Visitor: TFilterExprVisitorBase): Variant; override;
      function GetExprList(): TList; override;
      function ToString(): string; override;
    end;

  type
    { TUnaryFilterExpr }
    TUnaryFilterExpr = class(TFilterExpr)
    public
      Right: TFilterExpr;
      Operation: TToken;

      constructor Create(aOperation: TToken; aRight: TFilterExpr);
      destructor Destroy; override;

      function Accept(Visitor: TFilterExprVisitorBase): Variant; override;
      function GetExprList(): TList; override;
      function ToString(): string; override;
    end;

  type
    { TGroupingFilterExpr }
    TGroupingFilterExpr = class(TFilterExpr)
    public
      Expr: TFilterExpr;

      constructor Create(aExpr: TFilterExpr);
      destructor Destroy; override;

      function Accept(Visitor: TFilterExprVisitorBase): Variant; override;
      function GetExprList(): TList; override;
      function ToString(): string; override;
    end;

  type
    { TLiteralFilterExpr }
    TLiteralFilterExpr = class(TFilterExpr)
    public
      Value : Variant;

      constructor Create(aValue: Variant);
      destructor Destroy; override;

      function Accept(Visitor: TFilterExprVisitorBase): Variant; override;
      function GetExprList(): TList; override;
      function ToString(): string; override;
    end;

  type
    { TVariableFilterExpr }
    TVariableFilterExpr = class(TFilterExpr)
    public
      Token : TToken;
      Value : Variant;

      constructor Create(aToken: TToken);
      destructor Destroy; override;

      function Accept(Visitor: TFilterExprVisitorBase): Variant; override;
      function GetExprList(): TList; override;
      function ToString(): string; override;
    end;

  type
    { TLogicalFilterExpr }
    TLogicalFilterExpr = class(TFilterExpr)
    public
      Left: TFilterExpr;
      Right: TFilterExpr;
      Operation: TToken;

      constructor Create(aLeft: TFilterExpr; aOperation: TToken; aRight: TFilterExpr);
      destructor Destroy; override;

      function Accept(Visitor: TFilterExprVisitorBase): Variant; override;
      function GetExprList(): TList; override;
      function ToString(): string; override;
    end;

  type
    { TListFilterExpr }
    TListFilterExpr = class(TFilterExpr)
    public
      Items: TObjectList;

      constructor Create();
      destructor Destroy; override;

      function Accept(Visitor: TFilterExprVisitorBase): Variant; override;
      function GetExprList(): TList; override;
      function ToString(): string; override;
    end;

  type
    { TFilterExprVisitorBase }
    TFilterExprVisitorBase = class
    public
      function Visit(Expr: TFilterExpr): Variant;  virtual; abstract;
    end;

  type
    { TFilterExprVisitor }
    TFilterExprVisitor = class(TFilterExprVisitorBase)
    private
      FParser: TFilterParser;

      function IsValid(V: Variant): Boolean;
      function IsFloat(V: Variant): Boolean;

      function Evaluate(Expr: TFilterExpr): Variant;

      function VisitUnary(Expr: TUnaryFilterExpr): Variant;
      function VisitBinary(Expr: TBinaryFilterExpr): Variant;
      function VisitGrouping(Expr: TGroupingFilterExpr): Variant;
      function VisitLiteral(Expr: TLiteralFilterExpr): Variant;
      function VisitLogical(Expr: TLogicalFilterExpr): Variant;
      function VisitVariable(Expr: TVariableFilterExpr): Variant;
      function VisitList(Expr: TListFilterExpr): Variant;
    public
      constructor Create(aParser: TFilterParser);

      function Visit(Expr: TFilterExpr): Variant;  override;
    end;

 type
    { TFilterParser }
    TFilterParser = class
    private
      FCaseSensitive: Boolean;
      FOnError: TFilterErrorEvent;
      FOnVariable: TFilterVariableValueEvent;
      TokenList: TObjectList;
      Current: Integer;
      LastExpr: TFilterExpr;
      Scanner: TFilterScanner;

      FClientTag: Pointer;
      FLastToken: TToken;

      function Expression(): TFilterExpr;
      function Like(): TFilterExpr;
      function Or_(): TFilterExpr;
      function And_(): TFilterExpr;
      function Equality(): TFilterExpr;
      function Comparison(): TFilterExpr;
      function Term(): TFilterExpr;
      function Factor(): TFilterExpr;
      function Unary(): TFilterExpr;
      function Primary(): TFilterExpr;

      function Peek(): TToken;
      function Previous(): TToken;

      function Advance(): TToken;
      function Consume(TokenKind: TTokenKind; ErrorMessage: string): TToken;

      function Match(TokenKinds: TTokenKindSet): Boolean;
      function Check(TokenKind: TTokenKind): Boolean;

      function KeywordMatch(Keyword: string): Boolean;
      function KeywordCheck(Keyword: string): Boolean;

      function IsAtEnd(): Boolean;
    public
      constructor Create();
      destructor Destroy; override;

      function Parse(Source: string): TFilterExpr;
      function Evaluate(ClientTag: Pointer = nil): Variant;
      function GetVisitorVariableValue(Variable: string): Variant;

      procedure Error(ErrorMessage: string);
      procedure ExprError(Expr: TFilterExpr; ErrorMessage: string);
      procedure TokenError(Token: TToken; ErrorMessage: string);

      function ToString(): string; override;

      property CaseSensitive: Boolean read FCaseSensitive  write FCaseSensitive;

      property OnVariable: TFilterVariableValueEvent read FOnVariable write FOnVariable;
      property OnError: TFilterErrorEvent read FOnError write FOnError;
    end;


implementation


var
  InvariantFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );


{ TToken }

constructor TToken.Create(aKind: TTokenKind; aLexeme: string; aLiteral: Variant;  aLine: Integer);
begin
  Self.Kind := aKind;
  Self.Lexeme := aLexeme;
  Self.Literal := aLiteral;
  Self.Line := aLine;
end;
function TToken.ToString(): string;
begin
  Result := GetEnumName(TypeInfo(TTokenKind), Ord(Kind)) + ' ' + Lexeme; //  + ' ' + VarToStr(Literal);
end;

{ TFilterScanner }

constructor TFilterScanner.Create();
begin
  TokenList := TObjectList.Create(True);
  KeywordList := TStringList.Create();
  KeywordList.CaseSensitive := False;

  KeywordList.Add('and');
  KeywordList.Add('or');
  KeywordList.Add('not');
  KeywordList.Add('is');
  KeywordList.Add('null');
  KeywordList.Add('true');
  KeywordList.Add('false');
  KeywordList.Add('like');
  KeywordList.Add('in');
end;

destructor TFilterScanner.Destroy;
begin
  inherited Destroy;
  KeywordList.Free;
  TokenList.Free;
end;
function TFilterScanner.Scan(Text: string): TObjectList;
begin
  try
    Self.Source := Text;
    Current := 0;
    TokenList.Clear();

    while not IsAtEnd() do
    begin
      // We are at the beginning of the next lexeme.
      Start := Current;
      ScanToken();
    end;

    TokenList.Add(TToken.Create(toEOF, '', Null, Line));
    Result := TokenList
  except
    on E : Exception do
    begin
      Error(E.Message);
      Result := nil;
    end;
  end;

end;
function TFilterScanner.IsAlpha(C: Char): Boolean;
begin
  Result := C in ['a'..'z', 'A'..'Z', '_'];
end;

function TFilterScanner.IsAlphaNumeric(C: Char): Boolean;
begin
  Result := IsAlpha(C) or IsDigit(C);
end;

function TFilterScanner.IsDigit(C: Char): Boolean;
begin
  Result := C in ['0'..'9'];
end;

function TFilterScanner.IsKeyword(Text: string): Boolean;
begin
  Result := KeywordList.IndexOf(Text) <> -1;
end;

function TFilterScanner.IsAtEnd(): Boolean;
begin
  Result := Current > Length(Source);
end;

function TFilterScanner.Match(Expected: Char): Boolean;
begin
  if IsAtEnd() then
     Result := False
  else if Source[Current] <> Expected then
     Result := False
  else begin
    Inc(Current);
    Result := True;
  end;
end;

procedure TFilterScanner.ScanToken();
var
  C: Char;
begin

  C := Advance();

  case C of
    '(': AddToken(toLEFT_PAREN);
    ')': AddToken(toRIGHT_PAREN);
    '[': AddToken(toLEFT_BRACKET);
    ']': AddToken(toLEFT_BRACKET);
    '{': AddToken(toLEFT_BRACE);
    '}': AddToken(toRIGHT_BRACE);
    ',': AddToken(toCOMMA);
    '.': AddToken(toDOT);
    '-': AddToken(toMINUS);
    '+': AddToken(toPLUS);
    ';': AddToken(toSEMICOLON);
    '*': AddToken(toSTAR);
    '/': AddToken(toSLASH);

    '=': AddToken(toEQUAL);

    { two-char-tokens }
    '<':      if Match('>') then AddToken(toNOT_EQUAL)
         else if Match('=') then AddToken(toLESS_EQUAL)
         else AddToken(toLESS);
    '>': if Match('=') then AddToken(toGREATER_EQUAL)
         else AddToken(toGREATER);

    { white spaces }
    ' ',
    #9 : { ignore  } ;

    #13: if Match(#10) then
           Inc(Line)
         else { ignore } ;
    #10: Inc(Line);

    { single quote }
    #39: ReadString();

    else begin
           if IsDigit(C) then ReadNumber()
      else if IsAlpha(C) then ReadIdentifier()
      else
        Error('Unexpected character.');
      ;
    end;

  end;
end;

function TFilterScanner.Advance(): Char;
begin
  Result := Source[Current];
  Inc(Current);
end;

function TFilterScanner.Peek(): Char;
begin
  if IsAtEnd() then
     Result := #0
  else
    Result := Source[Current];
end;

function TFilterScanner.PeekNext(): Char;
begin
  if Current + 1 >= Length(Source) then
     Result := #0
  else
    Result := Source[Current + 1];
end;

procedure TFilterScanner.ReadIdentifier();
var
  Text: string;
  //C : Char;
begin
  while IsAlphaNumeric(Peek()) do
    //C :=
    Advance();

  Text := SubStr(Source, Start, Current);

  if IsKeyword(Text) then
     AddToken(toKEYWORD, Text)
  else
     AddToken(toIDENTIFIER, Text);
end;

procedure TFilterScanner.ReadNumber();
var
  Text: string;
  N : Double;
begin
  while IsDigit(Peek()) do
    Advance();

  // look for a fractional part
  if (Peek() = '.') and IsDigit(PeekNext()) then
  begin
    Advance();        // consume the decimal separator

    while IsDigit(Peek()) do
     Advance()
  end;

  Text := SubStr(Source, Start, Current);
  N :=   StrToFloat(Text, InvariantFormatSettings);

  AddToken(toNUMBER, N);
end;

procedure TFilterScanner.ReadString();
var
  Text: string;
  C : Char;
  IsAdded: Boolean;
begin

  IsAdded := False;

  while True do
  begin
    C := Advance();

    if C = #39 then    // The closing quote
    begin
      Text := SubStr(Source, Start + 1, Current - 1);
      AddToken(toSTRING_, Text);
      IsAdded := True;
      Break;
    end;

    if (Peek() = #13) or (Peek() = #10) then
      Inc(Line);

    if IsAtEnd() then
       Break;
  end;

  if (not IsAdded) and IsAtEnd() then
  begin
    Error('Not terminated string.');
    Exit;
  end;

end;

procedure TFilterScanner.AddToken(Kind: TTokenKind);
begin
  AddToken(Kind, Null);
end;

procedure TFilterScanner.AddToken(Kind: TTokenKind; Literal: Variant);
var
  Text: string;
begin
  Text :=  SubStr(Source, Start, Current);
  TokenList.Add(TToken.Create(Kind, Text, Literal, Line));
end;
function TFilterScanner.SubStr(S: string; StartIndex: Integer; EndIndex: Integer): string;
var
  Count: Integer;
begin
  Count := EndIndex - StartIndex;
  Result := Copy(S, StartIndex, Count);
end;

procedure TFilterScanner.Error(ErrorMessage: string);
begin
  if Assigned(FOnError) then
  begin
    ErrorMessage += Format(' - Line: %d, Char: %d', [Line, Current]);
    FOnError(Self, ErrorMessage);
  end;
end;




{ TFilterExpr }
function TFilterExpr.ToString: string;
begin
  Result := inherited ToString;
end;

{ TBinaryFilterExpr }
constructor TBinaryFilterExpr.Create(aLeft: TFilterExpr; aOperation: TToken; aRight: TFilterExpr);
begin
  Self.Left := aLeft;
  Self.Right := aRight;
  Self.Operation := aOperation;
end;

destructor TBinaryFilterExpr.Destroy;
begin
  Left.Free;
  Right.Free;
  inherited Destroy;
end;

function TBinaryFilterExpr.Accept(Visitor: TFilterExprVisitorBase): Variant;
begin
  Result := Visitor.Visit(Self);
end;

function TBinaryFilterExpr.GetExprList(): TList;
begin
   Result := TList.Create();
   Result.Add(Left);
   Result.Add(Right);
end;

function TBinaryFilterExpr.ToString: string;
begin
  Result := '(';
  Result += Left.ToString();
  Result += ' ' + Operation.Lexeme + ' ';
  Result += Right.ToString();
  Result += ')';
end;


{ TUnaryFilterExpr }
constructor TUnaryFilterExpr.Create(aOperation: TToken; aRight: TFilterExpr);
begin
  Self.Right := aRight;
  Self.Operation := aOperation;
end;

destructor TUnaryFilterExpr.Destroy;
begin
  Right.Free;
  inherited Destroy;
end;

function TUnaryFilterExpr.Accept(Visitor: TFilterExprVisitorBase): Variant;
begin
  Result := Visitor.Visit(Self);
end;

function TUnaryFilterExpr.GetExprList(): TList;
begin
  Result := TList.Create();
  Result.Add(Right);
end;

function TUnaryFilterExpr.ToString: string;
begin
  Result := Operation.Lexeme + ' ';
  Result += Right.ToString();
end;



{ TGroupingFilterExpr }
constructor TGroupingFilterExpr.Create(aExpr: TFilterExpr);
begin
  Self.Expr := aExpr;
end;

destructor TGroupingFilterExpr.Destroy;
begin
  Expr.Free;
  inherited Destroy;
end;

function TGroupingFilterExpr.Accept(Visitor: TFilterExprVisitorBase): Variant;
begin
  Result := Visitor.Visit(Self);
end;

function TGroupingFilterExpr.GetExprList(): TList;
begin
  Result := TList.Create();
  Result.Add(Expr);
end;

function TGroupingFilterExpr.ToString: string;
begin
  Result := '(';
  Result += Expr.ToString();
  Result += ')';
end;



{ TLiteralFilterExpr }
constructor TLiteralFilterExpr.Create(aValue: Variant);
begin
  Self.Value := aValue;
end;

destructor TLiteralFilterExpr.Destroy;
begin
  Value := Null;
  inherited Destroy;
end;

function TLiteralFilterExpr.Accept(Visitor: TFilterExprVisitorBase): Variant;
begin
  Result := Visitor.Visit(Self);
end;

function TLiteralFilterExpr.GetExprList(): TList;
begin
  Result := TList.Create();
end;

function TLiteralFilterExpr.ToString: string;
begin
  if VarIsNull(Value) or VarIsEmpty(Value) or VarIsClear(Value) then
     Result := 'null'
  else
    Result := VarToStr(Value);
end;



{ TVariableFilterExpr }
constructor TVariableFilterExpr.Create(aToken: TToken);
begin
  Self.Token := aToken;
end;

destructor TVariableFilterExpr.Destroy;
begin
  inherited Destroy;
end;

function TVariableFilterExpr.Accept(Visitor: TFilterExprVisitorBase): Variant;
begin
  Result := Visitor.Visit(Self);
end;

function TVariableFilterExpr.GetExprList(): TList;
begin
  Result := TList.Create();
end;

function TVariableFilterExpr.ToString: string;
begin
  Result := Token.Lexeme;
end;



{ TLogicalFilterExpr }
constructor TLogicalFilterExpr.Create(aLeft: TFilterExpr; aOperation: TToken; aRight: TFilterExpr);
begin
  Self.Left := aLeft;
  Self.Right := aRight;
  Self.Operation := aOperation;
end;

destructor TLogicalFilterExpr.Destroy;
begin
  Left.Free;
  Right.Free;
  inherited Destroy;
end;

function TLogicalFilterExpr.Accept(Visitor: TFilterExprVisitorBase): Variant;
begin
  Result := Visitor.Visit(Self);
end;

function TLogicalFilterExpr.GetExprList(): TList;
begin
  Result := TList.Create();
  Result.Add(Left);
  Result.Add(Right);
end;

function TLogicalFilterExpr.ToString: string;
begin
  Result := '(';
  Result += Left.ToString();
  Result += ' ' + Operation.Lexeme + ' ';
  Result += Right.ToString();
  Result += ')';
end;

{ TListFilterExpr }
constructor TListFilterExpr.Create();
begin
  Items := TObjectList.Create(True);
end;

destructor TListFilterExpr.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

function TListFilterExpr.Accept(Visitor: TFilterExprVisitorBase): Variant;
begin
  Result := Visitor.Visit(Self);
end;

function TListFilterExpr.GetExprList(): TList;
var
  i : Integer;
begin
  Result := TList.Create();
  for i := 0 to Items.Count - 1 do
      Result.Add(Items[i]);
end;

function TListFilterExpr.ToString: string;
var
  i : Integer;
begin
  Result := '(';
  for i := 0 to Items.Count - 1 do
  begin
    if (Items[i] is TLiteralFilterExpr) then
       Result += VarToStr((Items[i] as TLiteralFilterExpr).Value)
    else
      Result += (Items[i] as TVariableFilterExpr).Token.Lexeme;

    if (i < Items.Count - 1) then
      Result += ', ';
  end;
  Result += ')';
end;



{ TFilterExprVisitor }
constructor TFilterExprVisitor.Create(aParser: TFilterParser);
begin
  FParser := aParser;
end;

function TFilterExprVisitor.IsValid(V: Variant): Boolean;
begin
  Result := not (VarIsNull(V) or VarIsEmpty(V) or VarIsClear(V));
end;

function TFilterExprVisitor.IsFloat(V: Variant): Boolean;
begin
  Result := (TVarData(V).vType and varTypeMask) in [varDouble, varSingle, varDecimal, varCurrency];
end;

function TFilterExprVisitor.Evaluate(Expr: TFilterExpr): Variant;
begin
  Result := Expr.Accept(Self);
end;

function TFilterExprVisitor.VisitUnary(Expr: TUnaryFilterExpr): Variant;

  function IsTrue(V: Variant): Boolean;
  begin
    if not IsValid(V) then
       Exit(False);

    if (TVarData(V).vType and varTypeMask) = varBoolean then
       Exit(Boolean(V));

    Result := True;
  end;

var
  V: Variant;
begin
  V := Evaluate(Expr.Right);

  case Expr.Operation.Kind of
    toMINUS : begin
                if not IsFloat(V) then
                  FParser.ExprError(Expr, 'Number expected');
                ;
                Exit(-Double(V));
              end;
    toKEYWORD: if AnsiSameText(Expr.Operation.Lexeme, 'not') then
                 Exit(not IsTrue(V));
  end;

   Result := Null;
end;

function TFilterExprVisitor.VisitBinary(Expr: TBinaryFilterExpr): Variant;
  {------------------------------------------}
  function IsInteger(V: Variant): Boolean;
  begin
    Result := (TVarData(V).vType and varTypeMask) in [varSmallInt, varInteger, varShortInt, varByte, varWord,varLongWord,varInt64,varQWord];
  end;
  {------------------------------------------}
  function Compare(A, B: Variant): Integer;
  begin
    // A = B   ->   0
    // A > B   ->   1
    // A < B   ->  -1

    if IsValid(A) and IsValid(B) then              // both valid
    begin
      if VarIsStr(A) and VarIsStr(B) then
      begin
         if not FParser.CaseSensitive then
            Exit(CompareText(A, B))
         else
           Exit(CompareStr(A, B));
      end else begin
        if A = B then
          Exit(0)
        else if A > B then
          Exit(1)
        else
         Exit(-1);
      end;
    end

    else if not (IsValid(A) and IsValid(B)) then   // both invalid
      Exit(0)
    else if IsValid(A) and (not IsValid(B)) then   // A, B?
      Exit(1)
    else if (not IsValid(A)) and IsValid(B) then   // A?, B
      Exit(-1)
      ;

    Result := 0;
  end;
  {------------------------------------------}
  function AreNumbers(A, B: Variant): Boolean;
  begin
    Result := (IsInteger(A) or IsFloat(A)) and (IsInteger(B) or IsFloat(B));
  end;
  {------------------------------------------}
  procedure CheckAreNumbers(A, B: Variant);
  begin
    if not AreNumbers(A, B) then
      FParser.ExprError(Expr, 'Operands must be numbers.');
  end;
  {------------------------------------------}
var
  Left       : Variant;
  Right      : Variant;
  Res        : Integer;
  S1         : string;
  S2         : string;
  i          : Integer;
  Flag       : Boolean;
begin

  Left  := Evaluate(Expr.Left);
  Right := Evaluate(Expr.Right);

  case Expr.Operation.Kind of
     toNOT_EQUAL     : Exit(Compare(Left, Right) <> 0);
     toEQUAL         : Exit(Compare(Left, Right) = 0);
     toGREATER       : Exit(Compare(Left, Right) = 1);
     toGREATER_EQUAL : Exit(Compare(Left, Right) in [0, 1]);
     toLESS          : Exit(Compare(Left, Right) = -1);
     toLESS_EQUAL    : begin
                         // compiler complains with [0, -1] set, so...
                         Res := Compare(Left, Right);
                         Exit((Res = -1) or (Res = 0));
                       end;
     toMINUS         : begin
                         CheckAreNumbers(Left, Right);
                         Exit(Double(Left) - Double(Right));
                       end;
     toPLUS          : begin
                         if AreNumbers(Left, Right) then
                            Exit(Double(Left) + Double(Right));

                         if VarIsStr(Left) and VarIsStr(Right) then
                            Exit(String(Left) + String(Right));

                         FParser.ExprError(Expr, 'Operands must be two numbers or two strings.');
                       end;

     toSLASH         : begin
                         CheckAreNumbers(Left, Right);
                         Exit(Double(Left) / Double(Right));
                       end;
     toSTAR          : begin
                         CheckAreNumbers(Left, Right);
                         Exit(Double(Left) * Double(Right));
                       end;
     toKEYWORD       : begin
                         if AnsiSameText(Expr.Operation.Lexeme, 'like') then
                         begin
                           S1      := VarToStr(Left);
                           S2      := VarToStr(Right);
                           S2      := AnsiReplaceStr(S2, '%', '*');
                           Exit(MatchesMask(S1, S2));
                         end
                         else if AnsiSameText(Expr.Operation.Lexeme, 'in') then
                         begin
                           if VarIsArray(Right) then
                           begin
                             for i := 0 to VarArrayHighBound(Right, 1) do
                             begin
                               if VarIsEmpty(Right[i]) then
                                  Exit(False);

                               Flag := (Left = Right[i]);

                               if Flag then
                                 Exit(True);
                             end;
                             Exit(False);
                           end else begin
                             Flag := Left = Right;
                             Exit(Flag);
                           end;
                         end;
                       end;

  end;

  Result := Null;
end;
function TFilterExprVisitor.VisitGrouping(Expr: TGroupingFilterExpr): Variant;
begin
  Result := Evaluate(Expr.Expr);
end;

function TFilterExprVisitor.VisitLiteral(Expr: TLiteralFilterExpr): Variant;
begin
  Result := Expr.Value;
end;

function TFilterExprVisitor.VisitLogical(Expr: TLogicalFilterExpr): Variant;
var
  Left: Variant;
  Right: Variant;

begin
  Left := Evaluate(Expr.Left);
  Right := Evaluate(Expr.Right);

  if AnsiSameText(Expr.Operation.Lexeme, 'and') then
     Result := Boolean(Left) and Boolean(Right)
  else
     Result := Boolean(Left) or Boolean(Right)
end;
function TFilterExprVisitor.VisitVariable(Expr: TVariableFilterExpr): Variant;
var
  VarName: string;
begin
  VarName := Expr.Token.Lexeme;
  Result := FParser.GetVisitorVariableValue(VarName);
end;

function TFilterExprVisitor.VisitList(Expr: TListFilterExpr): Variant;
var
  i: Integer;
  V : Variant;
  ItemExpr: TFilterExpr;
begin
  Result := VarArrayCreate([0, Expr.Items.Count - 1], varVariant);

  for i := 0 to Expr.Items.Count - 1 do
  begin
    ItemExpr := TFilterExpr(Expr.Items[i]);
    V := Visit(ItemExpr);
    VarArrayPut(Result, V, [i]);
  end;

end;

function TFilterExprVisitor.Visit(Expr: TFilterExpr): Variant;
begin
  try
    if (Expr is TUnaryFilterExpr) then
       Exit(VisitUnary(Expr as TUnaryFilterExpr));

    if (Expr is TBinaryFilterExpr) then
      Exit(VisitBinary(Expr as TBinaryFilterExpr));

    if (Expr is TGroupingFilterExpr) then
      Exit(VisitGrouping(Expr as TGroupingFilterExpr));

    if (Expr is TLiteralFilterExpr) then
      Exit(VisitLiteral(Expr as TLiteralFilterExpr));

    if (Expr is TLogicalFilterExpr) then
       Exit(VisitLogical(Expr as TLogicalFilterExpr));

    if (Expr is TVariableFilterExpr) then
      Exit(VisitVariable(Expr as TVariableFilterExpr));

    if (Expr is TListFilterExpr) then
      Exit(VisitList(Expr as TListFilterExpr));


  except
    on E : Exception do
    begin
      FParser.Error(E.Message);
      Result := Null;
    end;
  end;

end;

{ TFilterParser }
constructor TFilterParser.Create();
begin
  inherited Create();
end;

destructor TFilterParser.Destroy;
begin
  FreeAndNil(Scanner);
  FreeAndNil(LastExpr);
  inherited Destroy;
end;

function TFilterParser.Parse(Source: string): TFilterExpr;
begin
  try
    Current := 0;

    FreeAndNil(Scanner);
    FreeAndNil(LastExpr);

    Scanner := TFilterScanner.Create();
    Self.TokenList := Scanner.Scan(Source);
    FLastToken := Peek();

    LastExpr := Expression();

  Result := LastExpr;
  except
    on E : Exception do
    begin
      Error(E.Message);
      Result := nil;
    end;
  end;
end;

function TFilterParser.Evaluate(ClientTag: Pointer): Variant;
var
  Visitor: TFilterExprVisitor;
begin
  if not Assigned(LastExpr) then
    Error('Cannot evaluate. No Expression.');


  FClientTag := ClientTag;

  Visitor := TFilterExprVisitor.Create(Self);
  try
    Result := Visitor.Visit(LastExpr);
  finally
    Visitor.Free;
  end;
end;
function TFilterParser.GetVisitorVariableValue(Variable: string): Variant;
begin
  Result := Null;

  if Assigned(FOnVariable) then
    FOnVariable(Self, Variable, FClientTag, Result)
  else
    Error(Format('Cannot get variable value: %s', [Variable]));

end;

function TFilterParser.ToString(): string;
begin
  if Assigned(LastExpr) then
     Exit(LastExpr.ToString());

  Result := 'no expression';
end;
{-------------------------------------------------------------------------------

  expression     → equality ;
  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  term           → factor ( ( "-" | "+" ) factor )* ;
  factor         → unary ( ( "/" | "*" ) unary )* ;
  unary          → ( "!" | "-" ) unary
                 | primary ;
  primary        → NUMBER | STRING | "true" | "false" | "nil"
                 | "(" expression ")" ;

-------------------------------------------------------------------------------}


function TFilterParser.Expression(): TFilterExpr;
begin
  // expression   =  equality ;
  Result := Or_(); // Or_(); // Equality();
end;

function TFilterParser.Like(): TFilterExpr;
var
  Expr: TFilterExpr;
  Operation : TToken;
  Right : TFilterExpr;
begin
  Expr := Or_();

  if KeywordMatch('like') then
  begin
    Operation := Previous();
    Right := Or_();
    Expr := TBinaryFilterExpr.Create(Expr, Operation, Right);
  end;

  Result := Expr;
end;

function TFilterParser.Or_(): TFilterExpr;
var
  Expr: TFilterExpr;
  Operation : TToken;
  Right : TFilterExpr;
begin
  Expr := And_();

  while KeywordMatch('or') do
  begin
    Operation := Previous();
    Right := And_();
    Expr := TLogicalFilterExpr.Create(Expr, Operation, Right);
  end;

  Result := Expr;
end;
function TFilterParser.And_(): TFilterExpr;
var
  Expr: TFilterExpr;
  Operation : TToken;
  Right : TFilterExpr;
begin

  Expr := Equality();

  while KeywordMatch('and') do
  begin
    Operation := Previous();
    Right := Equality();
    Expr := TLogicalFilterExpr.Create(Expr, Operation, Right);
  end;

  Result := Expr;
end;
function TFilterParser.Equality(): TFilterExpr;
var
  Expr: TFilterExpr;
  Operation : TToken;
  Right : TFilterExpr;
begin
  // equality   =  comparison ( ( "<>" | "=" ) comparison )* ;

  Expr := Comparison();

  while Match([toNOT_EQUAL, toEQUAL]) do
  begin
    Operation := Previous();
    Right := Comparison();
    Expr := TBinaryFilterExpr.Create(Expr, Operation, Right);
  end;

  Result := Expr;
end;

function TFilterParser.Comparison(): TFilterExpr;
var
  Expr: TFilterExpr;
  Operation : TToken;
  Right : TFilterExpr;
begin
  // comparison  = term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

  Expr := Term();

  while Match([toGREATER, toGREATER_EQUAL, toLESS, toLESS_EQUAL]) or KeywordMatch('like') or KeywordMatch('in') do
  begin
    Operation := Previous();
    Right := Term();
    Expr := TBinaryFilterExpr.Create(Expr, Operation, Right);
  end;

  Result := Expr;
end;

function TFilterParser.Term(): TFilterExpr;
var
  Expr: TFilterExpr;
  Operation : TToken;
  Right : TFilterExpr;
begin
  // term   =  factor ( ( "-" | "+" ) factor )* ;

  Expr := Factor();

  while Match([toMINUS, toPLUS]) do
  begin
    Operation := Previous();
    Right := Factor();
    Expr := TBinaryFilterExpr.Create(Expr, Operation, Right);
  end;

  Result := Expr;
end;

function TFilterParser.Factor(): TFilterExpr;
var
  Expr: TFilterExpr;
  Operation : TToken;
  Right : TFilterExpr;
begin
  // factor  = unary ( ( "/" | "*" ) unary )* ;

  Expr := Unary();

  while Match([toSLASH, toSTAR]) do
  begin
    Operation := Previous();
    Right := Unary();
    Expr := TBinaryFilterExpr.Create(Expr, Operation, Right);
  end;

  Result := Expr;
end;

function TFilterParser.Unary(): TFilterExpr;
var
  Operation : TToken;
  Right : TFilterExpr;
begin
  // unary  = ( "not" | "-" ) unary  | primary ;

  if Match([toMINUS]) or KeywordMatch('not') then
  begin
    Operation := Previous();
    Right := Unary();
    Result := TUnaryFilterExpr.Create(Operation, Right);
    Exit;
  end;

  Result := Primary();
end;

function TFilterParser.Primary(): TFilterExpr;
var
  Expr: TFilterExpr;
  V : Variant;
  Token: TToken;
  ListExpr: TListFilterExpr;
begin
  // primary   =   NUMBER | STRING | "true" | "false" | "nil"
  //               | "(" expression ")" ;

  if KeywordMatch('False') then
    Exit(TLiteralFilterExpr.Create(False));

  if KeywordMatch('True') then
    Exit(TLiteralFilterExpr.Create(True));

  if KeywordMatch('Null') then
    Exit(TLiteralFilterExpr.Create(Null));

  if Match([toNUMBER, toSTRING_]) then
  begin
    V := Previous().Literal;
    Exit(TLiteralFilterExpr.Create(V));
  end;

  if Match([toIDENTIFIER]) then
    Exit(TVariableFilterExpr.Create(Previous()));

  { IN keyword }
  Token := FLastToken;
  if (FLastToken.Kind = toKEYWORD) and (AnsiSameText(FLastToken.Lexeme, 'in')) then
  begin
    if not Match([toLEFT_PAREN]) then
       Error('Left parenthesis "(" expected after IN keyword');

    ListExpr:= TListFilterExpr.Create();

    while True do
    begin
      if Match([toNUMBER, toSTRING_]) then
      begin
        Token := Previous();
        ListExpr.Items.Add(TLiteralFilterExpr.Create(Token.Literal));
        Advance();
      end else if Match([toIDENTIFIER]) then
      begin
        Token := Previous();
        ListExpr.Items.Add(TVariableFilterExpr.Create(Token));
        Advance();
      end else
        Break;
    end;

    if FLastToken.Kind <> toRIGHT_PAREN then
      Error('Right parenthesis ")" expected');

    Exit(ListExpr);
  end;

  { Groups, i.e. ( expression ) }
  if Match([toLEFT_PAREN]) then
  begin
    Expr := Expression();
    Consume(toRIGHT_PAREN, 'Right parenthesis ")" expected');
    Exit(TGroupingFilterExpr.Create(Expr));
  end;

  TokenError(Peek(), 'Expression expected');

end;

function TFilterParser.Peek(): TToken;
begin
  Result := TokenList[Current] as TToken;
end;

function TFilterParser.Previous(): TToken;
begin
  Result := TokenList[Current - 1] as TToken;
end;

function TFilterParser.Advance(): TToken;
begin
  if not IsAtEnd() then
  begin
    FLastToken := Peek();
    Inc(Current);
  end;

  Result := Previous();
end;

function TFilterParser.Consume(TokenKind: TTokenKind; ErrorMessage: string): TToken;
begin
  if Check(TokenKind) then
  begin
    Result := Advance();
    Exit;
  end;

  TokenError(Peek(), ErrorMessage);
end;

function TFilterParser.Match(TokenKinds: TTokenKindSet): Boolean;
var
  T: TTokenKind;
begin
  for T in TokenKinds do
  begin
    if Check(T) then begin
      Advance();
      Exit(True);
    end;
  end;

  Result := False;
end;

function TFilterParser.Check(TokenKind: TTokenKind): Boolean;
begin
  if IsAtEnd() then
  begin
    Exit(False);
  end;

  Result := Peek().Kind = TokenKind;
end;

function TFilterParser.KeywordMatch(Keyword: string): Boolean;
begin
  if KeywordCheck(Keyword) then begin
    Advance();
    Exit(True);
  end;

  Result := False;
end;
function TFilterParser.KeywordCheck(Keyword: string): Boolean;
var
  T : TToken;
begin
  if IsAtEnd() then
  begin
    Exit(False);
  end;

  T := Peek();

  Result := (T.Kind = toKEYWORD) and AnsiSameText(T.Lexeme, Keyword);
end;

function TFilterParser.IsAtEnd(): Boolean;
begin
  Result := Peek().Kind = toEOF;
end;
procedure TFilterParser.Error(ErrorMessage: string);
begin
  if Assigned(FOnError) then
  begin
    FOnError(Self, ErrorMessage);
  end else begin
    raise Exception.Create(ErrorMessage);
  end;
end;
procedure TFilterParser.TokenError(Token: TToken; ErrorMessage: string);
begin
  if Assigned(Token) then
     ErrorMessage += Format(' - Token: %s', [Token.ToString()]);

  Error(ErrorMessage);
end;
procedure TFilterParser.ExprError(Expr: TFilterExpr; ErrorMessage: string);
begin
  if Assigned(Expr) then
     ErrorMessage += Format(' - Expression: %s', [Expr.ToString()]);
  Error(ErrorMessage);
end;



end.

