unit Tripous.StringBuilder;

{$mode ObjFPC}{$H+}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}

interface

uses
  Classes, SysUtils;

type
  SBChar = UnicodeChar;
  PSBChar = ^SBChar;
  SBString = UnicodeString;
  TSBCharArray = array of SBChar;

  IStringBuilder = interface
  ['{A7B65A46-98FB-4A7F-ADCA-A1266084AA95}']
  {private}
    function  GetCapacity: Integer;
    function  GetMaxCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
    function  GetC(Index: Integer): SBChar;
    procedure SetC(Index: Integer; AValue: SBChar);
    function  GetLength: Integer; inline;
    procedure SetLength(AValue: Integer);
  {public}
    procedure Append(const AValue: Boolean);
    procedure Append(const AValue: Byte);
    procedure Append(const AValue: SBChar);
    procedure Append(const AValue: Currency);
    procedure Append(const AValue: Double);
    procedure Append(const AValue: Smallint);
    procedure Append(const AValue: LongInt);
    procedure Append(const AValue: Int64);
    procedure Append(const AValue: TObject);
    procedure Append(const AValue: Shortint);
    procedure Append(const AValue: Single);
    procedure Append(const AValue: UInt64);
    procedure Append(const AValue: TSBCharArray);
    procedure Append(const AValue: Word);
    //procedure Append(const AValue: Cardinal);
    procedure Append(const AValue: SBString);
    procedure Append(const AValue: SBChar; RepeatCount: Integer);
    procedure Append(const AValue: TSBCharArray; StartIndex: Integer; SBCharCount: Integer);
    procedure Append(const AValue: SBString; StartIndex: Integer; Count: Integer);

    procedure Append(const Fmt: SBString; const Args: array of const);
    procedure AppendFormat(const Fmt: SBString; const Args: array of const);

    procedure AppendLine;

    procedure Clear;
    procedure CopyTo(SourceIndex: Integer; var Destination: TSBCharArray; DestinationIndex: Integer; Count: Integer);
    Function EnsureCapacity(aCapacity: Integer): Integer;

    procedure Insert(Index: Integer; const AValue: Boolean);
    procedure Insert(Index: Integer; const AValue: Byte);
    procedure Insert(Index: Integer; const AValue: SBChar);
    procedure Insert(Index: Integer; const AValue: Currency);
    procedure Insert(Index: Integer; const AValue: Double);
    procedure Insert(Index: Integer; const AValue: Smallint);
    procedure Insert(Index: Integer; const AValue: LongInt);
    procedure Insert(Index: Integer; const AValue: TSBCharArray);
    procedure Insert(Index: Integer; const AValue: Int64);
    procedure Insert(Index: Integer; const AValue: TObject);
    procedure Insert(Index: Integer; const AValue: Shortint);
    procedure Insert(Index: Integer; const AValue: Single);
    procedure Insert(Index: Integer; const AValue: SBString);
    procedure Insert(Index: Integer; const AValue: Word);
    procedure Insert(Index: Integer; const AValue: Cardinal);
    procedure Insert(Index: Integer; const AValue: UInt64);
    procedure Insert(Index: Integer; const AValue: SBString; const aRepeatCount: Integer);
    procedure Insert(Index: Integer; const AValue: TSBCharArray; startIndex: Integer; SBCharCount: Integer);

    procedure Remove(StartIndex: Integer; RemLength: Integer);
    procedure Replace(const OldChar, NewChar: SBChar);
    procedure Replace(const OldChar, NewChar: SBChar; StartIndex: Integer; Count: Integer);

    function ToString: SBString; reintroduce;
    function ToString(aStartIndex: Integer; aLength: Integer): SBString; reintroduce;

    property Chars[index: Integer]: SBChar read GetC write SetC; default;
    property Length: Integer read GetLength write SetLength;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property MaxCapacity: Integer read GetMaxCapacity;
  end;


  { TTripousStringBuilder }
  TTripousStringBuilder = class(TInterfacedObject, IStringBuilder)
  private
    const
      DefaultCapacity = 64;
  private
    function  GetCapacity: Integer;
    function  GetMaxCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
    function  GetC(Index: Integer): SBChar;
    procedure SetC(Index: Integer; AValue: SBChar);
    function  GetLength: Integer; inline;
    procedure SetLength(AValue: Integer);
  protected
    FData: TSBCharArray;
    FLength: Integer;
    FMaxCapacity: Integer;
    // raise error on range check.
    procedure CheckRange(Idx,Count,MaxLen : Integer);
    procedure CheckNegative(Const AValue : Integer; Const AName: SBString);
    // All appends/inserts pass through here.
    procedure DoAppend(Const S : SBString);virtual;
    procedure DoAppend(const AValue: TSBCharArray; Idx, aCount: Integer); virtual;
    procedure DoInsert(Index: Integer; const AValue: SBString); virtual;
    procedure DoInsert(Index: Integer; const AValue: TSBCharArray; StartIndex, SBCharCount: Integer); virtual;
    procedure DoReplace(Index: Integer; const Old, New: SBString); virtual;
    procedure Grow;
    procedure Shrink;
  public
    constructor Create;
    constructor Create(aCapacity: Integer);
    constructor Create(const AValue: SBString);
    constructor Create(aCapacity: Integer; aMaxCapacity: Integer);
    constructor Create(const AValue: SBString; aCapacity: Integer);
    constructor Create(const AValue: SBString; StartIndex: Integer; aLength: Integer; aCapacity: Integer);

    procedure Append(const AValue: Boolean);
    procedure Append(const AValue: Byte);
    procedure Append(const AValue: SBChar);
    procedure Append(const AValue: Currency);
    procedure Append(const AValue: Double);
    procedure Append(const AValue: Smallint);
    procedure Append(const AValue: LongInt);
    procedure Append(const AValue: Int64);
    procedure Append(const AValue: TObject);
    procedure Append(const AValue: Shortint);
    procedure Append(const AValue: Single);
    procedure Append(const AValue: UInt64);
    procedure Append(const AValue: TSBCharArray);
    procedure Append(const AValue: Word);
    //procedure Append(const AValue: Cardinal);
    procedure Append(const AValue: SBString);
    procedure Append(const AValue: SBChar; RepeatCount: Integer);
    procedure Append(const AValue: TSBCharArray; StartIndex: Integer; SBCharCount: Integer);
    procedure Append(const AValue: SBString; StartIndex: Integer; Count: Integer);

    procedure Append(const Fmt: SBString; const Args: array of const);
    procedure AppendFormat(const Fmt: SBString; const Args: array of const);

    procedure AppendLine;

    procedure Clear;
    procedure CopyTo(SourceIndex: Integer; var Destination: TSBCharArray; DestinationIndex: Integer; Count: Integer);
    Function EnsureCapacity(aCapacity: Integer): Integer;
    Function Equals(StringBuilder: TTripousStringBuilder): Boolean; reintroduce;

    procedure Insert(Index: Integer; const AValue: Boolean);
    procedure Insert(Index: Integer; const AValue: Byte);
    procedure Insert(Index: Integer; const AValue: SBChar);
    procedure Insert(Index: Integer; const AValue: Currency);
    procedure Insert(Index: Integer; const AValue: Double);
    procedure Insert(Index: Integer; const AValue: Smallint);
    procedure Insert(Index: Integer; const AValue: LongInt);
    procedure Insert(Index: Integer; const AValue: TSBCharArray);
    procedure Insert(Index: Integer; const AValue: Int64);
    procedure Insert(Index: Integer; const AValue: TObject);
    procedure Insert(Index: Integer; const AValue: Shortint);
    procedure Insert(Index: Integer; const AValue: Single);
    procedure Insert(Index: Integer; const AValue: SBString);
    procedure Insert(Index: Integer; const AValue: Word);
    procedure Insert(Index: Integer; const AValue: Cardinal);
    procedure Insert(Index: Integer; const AValue: UInt64);
    procedure Insert(Index: Integer; const AValue: SBString; const aRepeatCount: Integer);
    procedure Insert(Index: Integer; const AValue: TSBCharArray; startIndex: Integer; SBCharCount: Integer);

    procedure Remove(StartIndex: Integer; RemLength: Integer);
    procedure Replace(const OldChar, NewChar: SBChar);
    procedure Replace(const OldChar, NewChar: SBChar; StartIndex: Integer; Count: Integer);

    function ToString: SBString; reintroduce;
    function ToString(aStartIndex: Integer; aLength: Integer): SBString; reintroduce;

    property Chars[index: Integer]: SBChar read GetC write SetC; default;
    property Length: Integer read GetLength write SetLength;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property MaxCapacity: Integer read GetMaxCapacity;
  end;

implementation

uses
  SysConst
  ;

{ TTripousStringBuilder }

constructor TTripousStringBuilder.Create;
begin
  Create(DefaultCapacity,Maxint);
end;

constructor TTripousStringBuilder.Create(const AValue: SBString; aCapacity: Integer);
begin
  Create(aCapacity,Maxint);
  if (system.Length(AValue)>0) then
    Append(AValue);
end;




constructor TTripousStringBuilder.Create(const AValue: SBString; StartIndex, Alength,
  aCapacity: Integer);
begin
  Create(Copy(AValue,StartIndex+1,Alength), aCapacity);
end;

constructor TTripousStringBuilder.Create(aCapacity, aMaxCapacity: Integer);
begin
  FMaxCapacity:=aMaxCapacity;
  Capacity:=aCapacity;
  FLength:=0;
end;

constructor TTripousStringBuilder.Create(aCapacity: Integer);
begin
  Create(aCapacity,MaxInt);
end;

constructor TTripousStringBuilder.Create(const AValue: SBString);
begin
  Create(aValue,DefaultCapacity);
end;




{ Property getter/setter }

function TTripousStringBuilder.GetLength: Integer;
begin
  Result := FLength;
end;

function TTripousStringBuilder.GetCapacity: Integer;
begin
  Result := System.Length(FData);
end;

function TTripousStringBuilder.GetMaxCapacity: Integer;
begin
  Result := FMaxCapacity;
end;

function TTripousStringBuilder.GetC(Index: Integer): SBChar;
begin
  CheckNegative(Index,'Index');
  CheckRange(Index,0,Length);
  Result := FData[Index];
end;

procedure TTripousStringBuilder.SetC(Index: Integer; AValue: SBChar);
begin
  CheckNegative(Index,'Index');
  CheckRange(Index,0,Length-1);
  FData[Index]:=AValue;
end;

procedure TTripousStringBuilder.SetLength(AValue: Integer);
begin
  CheckNegative(AValue,'AValue');
  CheckRange(AValue,0,MaxCapacity);
  While AValue>Capacity do
    Grow;
  Flength:=AValue;
end;

{ Check functions }
procedure TTripousStringBuilder.CheckRange(Idx, Count, MaxLen: Integer);
begin
  if (Idx<0) or (Idx+Count>MaxLen) then
    raise ERangeError.CreateFmt(SListIndexError,[Idx]);
end;

procedure TTripousStringBuilder.CheckNegative(const AValue: Integer; const AName: SBString);
begin
  if (AValue<0) then
    raise ERangeError.CreateFmt(SParamIsNegative,[AName])
end;

{  These do the actual Appending/Inserting }
procedure TTripousStringBuilder.DoAppend(const S: SBString);
var
  L,SL : Integer;
begin
  SL:=System.Length(S);
  if SL>0 then
    begin
    L:=Length;
    Length:=L+SL;
    Move(S[1], FData[L],SL*SizeOf(SBChar));
    end;
end;

procedure TTripousStringBuilder.DoAppend(const AValue: TSBCharArray; Idx, aCount: Integer);
var
  L : integer;
begin
  L:=Length;
  CheckRange(Idx,aCount,System.Length(AValue));
  Length:=L+aCount;
  Move(AValue[Idx],FData[L],aCount*SizeOf(SBChar));
end;

procedure TTripousStringBuilder.DoInsert(Index: Integer; const AValue: SBString);
var
  ShiftLen,LV : Integer;
begin
  CheckRange(Index,0,Length-1);
  LV:=System.Length(AValue);
  ShiftLen:=Length-Index;
  Length:=Length+LV;
  Move(FData[Index],FData[Index+LV],ShiftLen*SizeOf(SBChar));
  Move(AValue[1],FData[Index],LV*SizeOf(SBChar));
end;

procedure TTripousStringBuilder.DoInsert(Index: Integer; const AValue: TSBCharArray; StartIndex, SBCharCount: Integer);
var
  ShiftLen : Integer;
begin
  CheckRange(Index,0,Length-1);
  CheckNegative(StartIndex,'StartIndex');
  CheckNegative(SBCharCount,'SBCharCount');
  CheckRange(StartIndex,SBCharCount,System.Length(AValue));
  Length:=Length+SBCharCount;
  ShiftLen:=Length-Index;
  if ShiftLen> 0 then
    Move(FData[Index], FData[Index+SBCharCount],ShiftLen*SizeOf(SBChar));
  Move(AValue[StartIndex],FData[Index],SBCharCount*SizeOf(SBChar));
end;

{ Public routines for appending }
procedure TTripousStringBuilder.Append(const AValue: UInt64);
begin
  DoAppend(IntToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: TSBCharArray);
var
  I,L: Integer;
begin
  I:=-1;
  L:=System.Length(AValue);
  If L=0 then
    Exit();
  Repeat
    Inc(I);
  Until (I>=L) or (AValue[I]=#0);
  DoAppend(AValue,0,I);
end;

procedure TTripousStringBuilder.Append(const AValue: Single);
begin
  DoAppend(FloatToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: Word);
begin
  Append(IntToStr(AValue));
end;

{
function TTripousStringBuilder.Append(const AValue: Cardinal);
begin
  DoAppend(IntToStr(AValue));
end;
}

procedure TTripousStringBuilder.Append(const AValue: SBChar; RepeatCount: Integer);
begin
  DoAppend(StringOfChar(AValue,RepeatCount));
end;

procedure TTripousStringBuilder.Append(const AValue: Shortint);
begin
  DoAppend(IntToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: SBChar);
begin
  DoAppend(AValue);
end;

procedure TTripousStringBuilder.Append(const AValue: Currency);
begin
  DoAppend(CurrToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: Boolean);
begin
  DoAppend(BoolToStr(AValue, True));
end;

procedure TTripousStringBuilder.Append(const AValue: Byte);
begin
  DoAppend(IntToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: Double);
begin
  DoAppend(FloatToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: Int64);
begin
  DoAppend(IntToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: TObject);
begin
  DoAppend(AValue.ToString);
end;

procedure TTripousStringBuilder.Append(const AValue: Smallint);
begin
  DoAppend(IntToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: LongInt);
begin
  DoAppend(IntToStr(AValue));
end;

procedure TTripousStringBuilder.Append(const AValue: TSBCharArray; StartIndex, SBCharCount: Integer);
begin
  DoAppend(AValue,StartIndex,SBCharCount);
end;

procedure TTripousStringBuilder.Append(const AValue: SBString; StartIndex, Count: Integer);
begin
  CheckRange(StartIndex,Count,System.Length(AValue));
  DoAppend(Copy(AValue,StartIndex+1,Count));
end;

procedure TTripousStringBuilder.Append(const AValue: SBString);
begin
  DoAppend(AValue);
end;

procedure TTripousStringBuilder.AppendFormat(const Fmt: SBString; const Args: array of const);
begin
  DoAppend(Format(Fmt,Args));
end;

procedure TTripousStringBuilder.Append(const Fmt: SBString; const Args: array of const);
begin
  DoAppend(Format(Fmt,Args));
end;

procedure TTripousStringBuilder.AppendLine;
begin
  DoAppend(sLineBreak);
end;

procedure TTripousStringBuilder.Clear;
begin
  Length:=0;
  Capacity:=DefaultCapacity;
end;

procedure TTripousStringBuilder.CopyTo(SourceIndex: Integer; var Destination: TSBCharArray; DestinationIndex: Integer; Count: Integer);
begin
  CheckNegative(Count,'Count');
  CheckNegative(DestinationIndex,'DestinationIndex');
  CheckRange(DestinationIndex,Count,System.Length(Destination));
  if Count>0 then
    begin
    CheckRange(SourceIndex,Count,Length);
    Move(FData[SourceIndex],Destination[DestinationIndex],Count * SizeOf(SBChar));
    end;
end;

function TTripousStringBuilder.EnsureCapacity(aCapacity: Integer): Integer;
begin
  CheckRange(aCapacity,0,MaxCapacity);
  if Capacity<aCapacity then
    Capacity:=aCapacity;
  Result:=Capacity;
end;

function TTripousStringBuilder.Equals(StringBuilder: TTripousStringBuilder): Boolean;
begin
  Result:=(StringBuilder<>nil);
  if Result then
    Result:=(Length=StringBuilder.Length)
             and (MaxCapacity=StringBuilder.MaxCapacity)
             and CompareMem(@FData[0],@StringBuilder.FData[0],Length*SizeOf(SBChar));
end;

procedure TTripousStringBuilder.Grow;
var
  NewCapacity: SizeInt;
begin
  NewCapacity:=Capacity*2;
  if NewCapacity>MaxCapacity then
    NewCapacity:=MaxCapacity;
  Capacity:=NewCapacity;
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: TObject);
begin
  DoInsert(Index,AValue.ToString());
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Int64);
begin
  DoInsert(Index,IntToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Single);
begin
  DoInsert(Index,FloatToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: SBString);
begin
  DoInsert(Index,AValue);
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Word);
begin
  DoInsert(Index,IntToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Shortint);
begin
  DoInsert(Index, IntToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Currency);
begin
  DoInsert(Index,CurrToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: SBChar);
begin
  DoInsert(Index,AValue);
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Byte);
begin
  DoInsert(Index,IntToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Double);
begin
  DoInsert(Index,FloatToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: LongInt);
begin
  DoInsert(Index,IntToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Smallint);
begin
  DoInsert(Index,IntToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Boolean);
begin
  DoInsert(Index,BoolToStr(AValue,True));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: SBString;  const aRepeatCount: Integer);
var
  I: Integer;
begin
  for I:=0 to aRepeatCount-1 do
    DoInsert(Index,AValue);
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: TSBCharArray);
begin
  DoInsert(Index,AValue,0,System.Length(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: TSBCharArray; startIndex: Integer; SBCharCount: Integer);
begin
  DoInsert(Index,AValue,StartIndex,SBCharCount);
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: Cardinal);
begin
  DoInsert(Index,IntToStr(AValue));
end;

procedure TTripousStringBuilder.Insert(Index: Integer; const AValue: UInt64);
begin
  DoInsert(Index,IntToStr(AValue));
end;

procedure TTripousStringBuilder.Shrink;
begin
  if (Capacity div 4)>=Length then
    Capacity:=Capacity div 2;
end;

procedure TTripousStringBuilder.Remove(StartIndex: Integer; RemLength: Integer);
var
  MoveIndex : Integer;
begin
  if (RemLength=0) then
    Exit();
  CheckNegative(RemLength,'RemLength');
  CheckRange(StartIndex,0,Length);
  MoveIndex:=StartIndex+RemLength;
  CheckRange(MoveIndex,0,Length);
  if (Length-Moveindex)>0 then
    Move(FData[MoveIndex],FData[StartIndex],(Length-MoveIndex)*SizeOf(SBChar));
  Length:=Length-RemLength;
  Shrink;
end;

procedure TTripousStringBuilder.Replace(const OldChar, NewChar: SBChar;
  StartIndex: Integer; Count: Integer);
var
  I : Integer;
  Cur : PSBChar;
begin
  if Count=0 then
    Exit();
  CheckNegative(StartIndex,'StartIndex');
  CheckNegative(Count,'Count');
  CheckRange(StartIndex,Count-1,Length);
  Cur:=@FData[StartIndex];
  For I:=1 to Count do
    begin
    if Cur^=OldChar then
      Cur^:=NewChar;
    Inc(Cur);
    end;
end;

procedure TTripousStringBuilder.Replace(const OldChar, NewChar: SBChar);
begin
  Replace(OldChar,NewChar,0,Length);
end;

procedure TTripousStringBuilder.SetCapacity(AValue: Integer);
begin
  if (AValue>FMaxCapacity) then
    raise ERangeError.CreateFmt(SListCapacityError,[AValue]);
  if (AValue<Length) then
    raise ERangeError.CreateFmt(SListCapacityError,[AValue]);
  System.SetLength(FData,AValue);
end;

function TTripousStringBuilder.ToString: SBString;
begin
  Result:=ToString(0,Length);
end;

function TTripousStringBuilder.ToString(aStartIndex: Integer; aLength: Integer): SBString;
begin
  if (aLength=0) then
    Result:=''
  else
    begin
    CheckNegative(aStartIndex,'aStartIndex');
    CheckNegative(aLength,'aLength');
    CheckRange(aStartIndex,aLength,Length);
    System.SetLength(Result,aLength);
    Move(FData[aStartIndex],Result[1],aLength*SizeOf(SBChar));
    end;
end;

procedure TTripousStringBuilder.DoReplace(Index: Integer; const Old, New: SBString);
var
  NVLen,OVLen,OLen,Delta,TailStart: Integer;
begin
  NVLen:=System.Length(New);
  OVLen:=System.Length(Old);
  Delta:=NVLen-OVLen;
  if (Delta<>0) then
    begin
    OLen:=Length;
    if (Delta>0) then
      Length:=OLen+Delta;
    TailStart:=Index+OVlen;
    Move(FData[TailStart],FData[Index+NVLen],(OLen-TailStart)*SizeOf(SBChar));
    if (Delta<0) then
      Length:=OLen+Delta;
    end;
  Move(New[1],FData[Index],NVLen*SizeOf(SBChar));
end;


end.

