unit Tripous.Generics;

{$MODE DELPHI}{$H+}

interface

uses
   Classes
  ,SysUtils
  ,SyncObjs
  ,Generics.Defaults
  //,Generics.Collections

  ,Tripous
  ;

// TODO: check TGenList, TGenDictionary

type
  TGenArray<T> = array of T;

  TGetItemAtIndexEvent<T> = function(Index: SizeInt): T of object;

  TConditionMethod<T> = function (Item: T): Boolean of object;
  TConditionFunc<T> = function (Item: T): Boolean;

  TCompareMethod<T> = function(constref A, B: T): Integer of object;
  TCompareFunc<T> = function(constref A, B: T): Integer;

  { TGenEnumerator }
  TGenEnumerator<T> = class
  protected
    FLength: SizeInt;
    FPosition : SizeInt;
    FGetItemAtIndex : TGetItemAtIndexEvent<T>;

    function DoGetCurrent: T;
  public
    constructor Create(Length: SizeInt; GetItemAtIndex: TGetItemAtIndexEvent<T>);

    function MoveNext: Boolean;
    procedure Reset();

    property Current: T read DoGetCurrent;
    property Position: SizeInt read FPosition;
  end;

  IList<T> = interface
  ['{38ED2E59-BA1D-4518-88CE-15B46AAF25E6}']
  { private }
  function GetCount: SizeInt;
  function GetIsThreadSafe: Boolean;
  function GetItem(Index: SizeInt): T;
  procedure SetItem(Index: SizeInt; Item: T);
  { public }
  procedure Clear();

  procedure Add(Item: T);
  procedure Insert(Index: Integer; Item: T);
  procedure Remove(Item: T);
  procedure RemoveAt(Index: Integer);

  procedure AddRange(constref Range: array of T);
  procedure InsertRange(Index: SizeInt; constref Range: array of T);

  function Contains(Item: T): Boolean;
  function IndexOf(Item: T): Integer;

  { (Queue) - Inserts an item as the end of the list }
  procedure Enqueue(Item: T);
  { (Queue) - Removes and returns the item at index 0 }
  function Dequeue(): T;
  { (Stack) - Inserts an item at the top of the list}
  procedure Push(Item: T);
  { (Stack) - Removes and returns the item at the top of the list }
  function  Pop(): T;
  { (Queue and Stack)- Returns the item at at index 0 without removing it. }
  function Peek(): T;

  procedure Reverse();

  { Sorts the internal list }
  procedure Sort(Comparer: TCompareFunc<T>); overload;
  { Sorts the internal list }
  procedure Sort(Comparer: TCompareMethod<T>); overload;
  { Sorts the internal list }
  procedure Sort(const Comparer: IComparer<T>); overload;

  { Returns the first element that fulfils a condition, if any, or Default(T) }
  function  FirstOrDefault(Condition: TConditionFunc<T>): T; overload;
  { Returns the first element that fulfils a condition, if any, or Default(T) }
  function  FirstOrDefault(Condition: TConditionMethod<T>): T; overload;

  { Returns true if any of the elements fulfil a condition }
  function  Any(Condition: TConditionFunc<T>): Boolean; overload;
  { Returns true if any of the elements fulfil a condition }
  function  Any(Condition: TConditionMethod<T>): Boolean; overload;

  { Returns true if all the elements fulfil a condition }
  function  All(Condition: TConditionFunc<T>): Boolean; overload;
  { Returns true if all the elements fulfil a condition }
  function  All(Condition: TConditionMethod<T>): Boolean; overload;

  { Returns a list of elements that fulfil a condition }
  function  Where(Condition: TConditionFunc<T>): IList<T>; overload;
  { Returns a list of elements that fulfil a condition }
  function  Where(Condition: TConditionMethod<T>): IList<T>; overload;

  { Returns a list of all elements }
  function  ToList(): IList<T>;
  { Returns an array of all elements }
  function  ToArray(): TGenArray<T>;

  function GetEnumerator(): TGenEnumerator<T>;

  property Count: SizeInt read GetCount;
  property IsThreadSafe: Boolean read GetIsThreadSafe;
  property Items[Index: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TGenList }
  TGenList<T> = class(TInterfacedObject, IList<T>)
  protected
    // Dynamic Array Extensions: https://lists.freepascal.org/pipermail/fpc-pascal/2018-May/053892.html
    FItems       : TGenArray<T>;
    FLock        : SyncObjs.TCriticalSection;

    function GetCount: SizeInt;
    function GetIsThreadSafe: Boolean;
    function GetItem(Index: SizeInt): T;
    procedure SetItem(Index: SizeInt; Item: T);

    procedure Lock();
    procedure UnLock();

    procedure DoClear(); virtual;
    procedure DoRemoveAt(Index: Integer); virtual;

    procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt; const AComparer: IComparer<T>);
  public
    constructor Create(ThreadSafe: Boolean = False);
    destructor Destroy(); override;

    procedure Clear();

    procedure Add(Item: T);
    procedure Insert(Index: Integer; Item: T);
    procedure Remove(Item: T);
    procedure RemoveAt(Index: Integer);

    procedure AddRange(constref Range: array of T);
    procedure InsertRange(Index: SizeInt; constref Range: array of T);

    function Contains(Item: T): Boolean;
    function IndexOf(Item: T): Integer;

    { (Queue) - Inserts an item as the end of the list }
    procedure Enqueue(Item: T);
    { (Queue) - Removes and returns the item at index 0 }
    function Dequeue(): T;
    { (Stack) - Inserts an item at the top of the list}
    procedure Push(Item: T);
    { (Stack) - Removes and returns the item at the top of the list }
    function  Pop(): T;
    { (Queue and Stack)- Returns the item at at index 0 without removing it. }
    function Peek(): T;

    procedure Reverse();

    { Sorts the internal list }
    procedure Sort(Comparer: TCompareFunc<T>); overload;
    { Sorts the internal list }
    procedure Sort(Comparer: TCompareMethod<T>); overload;
    { Sorts the internal list }
    procedure Sort(const Comparer: IComparer<T>); overload;

    { Returns the first element that fulfils a condition, if any, or Default(T) }
    function  FirstOrDefault(Condition: TConditionFunc<T>): T; overload;
    { Returns the first element that fulfils a condition, if any, or Default(T) }
    function  FirstOrDefault(Condition: TConditionMethod<T>): T; overload;

    { Returns true if any of the elements fulfil a condition }
    function  Any(Condition: TConditionFunc<T>): Boolean; overload;
    { Returns true if any of the elements fulfil a condition }
    function  Any(Condition: TConditionMethod<T>): Boolean; overload;

    { Returns true if all the elements fulfil a condition }
    function  All(Condition: TConditionFunc<T>): Boolean; overload;
    { Returns true if all the elements fulfil a condition }
    function  All(Condition: TConditionMethod<T>): Boolean; overload;

    { Returns a list of elements that fulfil a condition }
    function  Where(Condition: TConditionFunc<T>): IList<T>; overload;
    { Returns a list of elements that fulfil a condition }
    function  Where(Condition: TConditionMethod<T>): IList<T>; overload;

    { Returns a list of all elements }
    function  ToList(): IList<T>;
    { Returns an array of all elements }
    function  ToArray(): TGenArray<T>;

    function GetEnumerator(): TGenEnumerator<T>;

    property Count: SizeInt read GetCount;
    property IsThreadSafe: Boolean read GetIsThreadSafe;
    property Items[Index: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TGenObjectList }

  TGenObjectList<T: class> = class(TGenList<T>)
  protected
    FOwnsObjects : Boolean;
    procedure DoClear(); override;
    procedure DoRemoveAt(Index: Integer); override;
  public
    constructor Create(AOwnsObjects: Boolean; ThreadSafe: Boolean = False);
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  { TGenKeyValue }
  TGenKeyValue<TKey, TValue> = class
  private
    FKey: TKey;
    FValue: TValue;
  public
    constructor Create(AKey: TKey; AValue: TValue);
    property Key: TKey read FKey;
    property Value: TValue read FValue;
  end;

  { TGenDictionaryEnumerator }
  TGenDictionaryEnumerator<TKey, TValue> = class
  protected
    FLength: SizeInt;
    FPosition : SizeInt;
    FGetItemAtIndex : TGetItemAtIndexEvent<TGenKeyValue<TKey, TValue>>;
    function DoGetCurrent: TGenKeyValue<TKey, TValue>;
  public
    constructor Create(Length: SizeInt; GetItemAtIndex: TGetItemAtIndexEvent<TGenKeyValue<TKey, TValue>>);
    function MoveNext: boolean;

    procedure Reset();

    property Current: TGenKeyValue<TKey, TValue> read DoGetCurrent;
    property Position: SizeInt read FPosition;
  end;


  { TGenDictionary }
  TGenDictionary<TKey, TValue> = class
  protected
    FList : Classes.TList;
    function GetCount: SizeInt;
    function  GetValue(const Key: TKey): TValue;
    procedure SetValue(const Key: TKey; Value: TValue);

    function  GetKeys: TGenArray<TKey>;
    function  GetValues: TGenArray<TValue>;

    function FindByKey(const Key: TKey): TGenKeyValue<TKey, TValue>;
    function FindByValue(const Value: TValue): TGenKeyValue<TKey, TValue>;

    function IndexOfKey(const Key: TKey): SizeInt;
    function IndexOfValue(const Value: TValue): SizeInt;

    function GetItemAtIndex(Index: SizeInt): TGenKeyValue<TKey, TValue>;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Add(Key: TKey; const Value: Variant);
    function  Remove(Key: TKey): Boolean;
    procedure Clear();

    function  ContainsKey(const Key: TKey): Boolean;
    function  ContainsValue(const Value: TValue): Boolean;

    function  GetEnumerator(): TGenDictionaryEnumerator<TKey, TValue>;

    property Count: SizeInt read GetCount;
    property Item[const Key: TKey]: TValue read GetValue write SetValue; default;
    property Keys: TGenArray<TKey> read GetKeys;
    property Values: TGenArray<TValue> read GetValues;
  end;




implementation

{
constructor TNestedFuncComparer<T>.Create();
begin
  //FComparison := AComparison;
end;

function TNestedFuncComparer<T>.Compare(constref A, B: T): Integer;
begin
  Result := FComparison(A, B);
end;
}


{ TGenEnumerator }

constructor TGenEnumerator<T>.Create(Length: SizeInt; GetItemAtIndex: TGetItemAtIndexEvent<T>);
begin
  inherited Create();
  FPosition := -1;
  FLength := Length;
  FGetItemAtIndex := GetItemAtIndex;
end;

procedure TGenEnumerator<T>.Reset();
begin
  FPosition := -1;
end;

function TGenEnumerator<T>.DoGetCurrent: T;
begin
  Result := FGetItemAtIndex(FPosition);
end;

function TGenEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FLength;
end;





{ TGenList }

constructor TGenList<T>.Create(ThreadSafe: Boolean);
begin
  inherited Create();

  if ThreadSafe then
     FLock := SyncObjs.TCriticalSection.Create();
end;

destructor TGenList<T>.Destroy();
begin
  Clear();
  if Assigned(FLock) then
     FLock.Free();
  inherited Destroy();
end;

function TGenList<T>.GetIsThreadSafe: Boolean;
begin
  Result := Assigned(FLock);
end;

procedure TGenList<T>.Lock;
begin
  if Assigned(FLock) then
     FLock.Enter();
end;

procedure TGenList<T>.UnLock;
begin
  if Assigned(FLock) then
     FLock.Leave();
end;

function TGenList<T>.GetCount: SizeInt;
begin
  Lock();
  try
    if Assigned(FItems) then
      Result := Length(FItems)
    else
      Result := 0;
  finally
    UnLock();
  end;
end;

procedure TGenList<T>.DoClear();
begin
  FItems := nil;
end;

procedure TGenList<T>.Clear();
begin
  Lock();
  try
    DoClear();
  finally
    UnLock();
  end;
end;

procedure TGenList<T>.Add(Item: T);
begin
  Insert(Count, Item);
end;

procedure TGenList<T>.Insert(Index: Integer; Item: T);
begin
  Lock();
  try
    if (Index < 0) or (Index > Count) then
      raise Exception.CreateFmt('Cannot insert at Index: %d. Index out of bounds', [Index]);

    if Index = Count then
    begin
      if not Assigned(FItems) then
      begin
        SetLength(FItems, 1);
        FItems[0] := Item;
      end else begin
         FItems := System.Concat(FItems, [Item]);
      end;
    end else begin
      System.Insert([Item], FItems, Index);
    end;
  finally
    UnLock();
  end;
end;

procedure TGenList<T>.Remove(Item: T);
begin
  RemoveAt(IndexOf(Item));
end;

procedure TGenList<T>.DoRemoveAt(Index: Integer);
begin
  System.Delete(FItems, Index, 1);
end;

procedure TGenList<T>.RemoveAt(Index: Integer);
begin
  Lock();
  try
    if (Index < 0) or (Index >= Count) then
      raise Exception.CreateFmt('Cannot remove at Index: %d. Index out of bounds', [Index]);

    DoRemoveAt(Index);

  finally
    UnLock();
  end;
end;

procedure TGenList<T>.AddRange(constref Range: array of T);
var
  Item: T;
begin
  Lock();
  try
    for Item in Range do
      Add(Item);
  finally
    UnLock();
  end;
end;

procedure TGenList<T>.InsertRange(Index: SizeInt; constref Range: array of T);
var
  Item: T;
  i: SizeInt;
begin
  Lock();
  try
    i := 0;
    for Item in Range do
    begin
      Insert(Index + i, Item);
      Inc(i);
    end;
  finally
    UnLock();
  end;
end;

function TGenList<T>.Contains(Item: T): Boolean;
begin
  Lock();
  try
    Result := IndexOf(Item) <> -1;
  finally
    UnLock();
  end;
end;

function TGenList<T>.IndexOf(Item: T): Integer;
var
  i : Integer;
begin
  Lock();
  try
    for i := Low(FItems) to High(FItems) do
    begin
      if FItems[i] = Item then
        Exit(i);
    end;

    Exit(-1);
  finally
    UnLock();
  end;
end;

procedure TGenList<T>.Enqueue(Item: T);
begin
  // (Queue) - Inserts an item as the end of the list
  Add(Item);
end;

function TGenList<T>.Dequeue(): T;
begin
  // (Queue) - Removes and returns the item at index 0
  Lock();
  try
    if Count = 0 then
      raise Exception.Create('Queue is empty');

    Result := FItems[0];
    System.Delete(FItems, 0, 1);
  finally
    UnLock();
  end;
end;

procedure TGenList<T>.Push(Item: T);
begin
  // (Stack) - Inserts an item at the top of the list
  Insert(0, Item);
end;

function TGenList<T>.Pop(): T;
begin
  //(Stack) - Removes and returns the item at the top of the list }
  Lock();
  try
    if Count = 0 then
      raise Exception.Create('Stack is empty');

    Result := FItems[0];
    System.Delete(FItems, 0, 1);
  finally
    UnLock();
  end;
end;

function TGenList<T>.Peek(): T;
begin
  // (Queue and Stack)- Returns the item at at index 0 without removing it.
  Lock();
  try
    if Count = 0 then
      raise Exception.Create('Queue or Stack is empty');

    Result := FItems[0];
  finally
    UnLock();
  end;
end;

procedure TGenList<T>.Reverse();
var
  A, B: SizeInt;
  Temp: T;
begin
  A := 0;
  B := Count - 1;
  while A < B do
  begin
    Temp := FItems[A];
    FItems[A] := FItems[B];
    FItems[B] := Temp;
    Inc(A);
    Dec(B);
  end;
end;

procedure TGenList<T>.QuickSort(var AValues: array of T; ALeft, ARight: SizeInt; const AComparer: IComparer<T>);
var
  I, J: SizeInt;
  P, Q: T;
begin
  if ((ARight - ALeft) <= 0) or (Length(AValues) = 0) then
    Exit;
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
        while AComparer.Compare(AValues[I], P) < 0 do
          Inc(I);
        while AComparer.Compare(AValues[J], P) > 0 do
          Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        QuickSort(AValues, ALeft, J, AComparer);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, AComparer);
      ARight := J;
    end;
   until ALeft >= ARight;

end;

procedure TGenList<T>.Sort(Comparer: TCompareFunc<T>);
var
  C: IComparer<T>;
begin
  C := TComparer<T>.Construct(Comparer);
  Sort(C);
end;

procedure TGenList<T>.Sort(Comparer: TCompareMethod<T>);
var
  C: IComparer<T>;
begin
  C := TComparer<T>.Construct(Comparer);
  Sort(C);
end;

procedure TGenList<T>.Sort(const Comparer: IComparer<T>);
begin
  if not Assigned(FItems) or (Count < 2) then
     Exit; // <=

  QuickSort(FItems, 0, Pred(Count), Comparer);
end;

function TGenList<T>.FirstOrDefault(Condition: TConditionFunc<T>): T;
var
  i : Integer;
  Item: T;
begin
  Lock();
  try
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if Condition(Item) then
        Exit(Item);
    end;
    Exit(Default(T));
  finally
    UnLock();
  end;
end;

function TGenList<T>.FirstOrDefault(Condition: TConditionMethod<T>): T;
var
  i : Integer;
  Item: T;
begin
  Lock();
  try
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if Condition(Item) then
        Exit(Item);
    end;
    Exit(Default(T));
  finally
    UnLock();
  end;
end;

function TGenList<T>.Any(Condition: TConditionFunc<T>): Boolean;
var
  i : Integer;
  Item: T;
begin
  Lock();
  try
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if Condition(Item) then
        Exit(True);
    end;
    Exit(False);
  finally
    UnLock();
  end;
end;

function TGenList<T>.Any(Condition: TConditionMethod<T>): Boolean;
var
  i : Integer;
  Item: T;
begin
  Lock();
  try
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if Condition(Item) then
        Exit(True);
    end;
    Exit(False);
  finally
    UnLock();
  end;

end;

function TGenList<T>.All(Condition: TConditionFunc<T>): Boolean;
var
  i : Integer;
  Item: T;
begin
  Lock();
  try
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if not Condition(Item) then
        Exit(False);
    end;
    Exit(True);
  finally
    UnLock();
  end;
end;

function TGenList<T>.All(Condition: TConditionMethod<T>): Boolean;
var
  i : Integer;
  Item: T;
begin
  Lock();
  try
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if not Condition(Item) then
        Exit(False);
    end;
    Exit(True);
  finally
    UnLock();
  end;
end;

function TGenList<T>.Where(Condition: TConditionFunc<T>): IList<T>;
var
  List: TGenList<T>;
  i : Integer;
  Item: T;
begin
  Lock();
  try
    List := TGenList<T>.Create(False);
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if Condition(Item) then
        List.Add(Item);
    end;
    Result := List;
  finally
    UnLock();
  end;
end;

function TGenList<T>.Where(Condition: TConditionMethod<T>): IList<T>;
var
  List: TGenList<T>;
  i : Integer;
  Item: T;
begin
  Lock();
  try
    List := TGenList<T>.Create(False);
    for i := Low(FItems) to High(FItems) do
    begin
      Item := FItems[i];
      if Condition(Item) then
        List.Add(Item);
    end;
    Result := List;
  finally
    UnLock();
  end;
end;

function TGenList<T>.ToList(): IList<T>;
var
  List: TGenList<T>;
begin
  Lock();
  try
    List := TGenList<T>.Create(False);
    List.AddRange(FItems);
    Result := List;
  finally
    UnLock();
  end;
end;

function TGenList<T>.ToArray(): TGenArray<T>;
begin
   Result := System.Copy(FItems, 0);
end;

function TGenList<T>.GetEnumerator(): TGenEnumerator<T>;
begin
  Result := TGenEnumerator<T>.Create(Length(FItems), GetItem);
  //Result.FItems := FItems;
end;

function TGenList<T>.GetItem(Index: SizeInt): T;
begin
  Lock();
  try
    Result := FItems[Index];
  finally
    UnLock();
  end;

end;

procedure TGenList<T>.SetItem(Index: SizeInt; Item: T);
begin
  Lock();
  try
    FItems[Index] := Item;
  finally
    UnLock();
  end;
end;



{ TGenObjectList }

constructor TGenObjectList<T>.Create(AOwnsObjects: Boolean; ThreadSafe: Boolean);
begin
  inherited Create(ThreadSafe);
  FOwnsObjects := AOwnsObjects;
end;

procedure TGenObjectList<T>.DoClear();
var
  Item: T;
begin
  if OwnsObjects then
  try
    for Item in FItems do
        Item.Free();
  except
  end;

  inherited DoClear();
end;

procedure TGenObjectList<T>.DoRemoveAt(Index: Integer);
begin
  if OwnsObjects then
  try
    FItems[Index].Free();
  except
  end;

  inherited DoRemoveAt(Index);
end;

{ TGenKeyValue }

constructor TGenKeyValue<TKey, TValue>.Create(AKey: TKey; AValue: TValue);
begin
  inherited Create();
  FKey := AKey;
  FValue := AValue;
end;

{ TGenDictionaryEnumerator }

constructor TGenDictionaryEnumerator<TKey, TValue>.Create(Length: SizeInt; GetItemAtIndex: TGetItemAtIndexEvent<TGenKeyValue<TKey, TValue>>);
begin
  inherited Create();
  FPosition := -1;
  FLength := Length;
  FGetItemAtIndex := GetItemAtIndex;
end;

function TGenDictionaryEnumerator<TKey, TValue>.DoGetCurrent: TGenKeyValue<TKey, TValue>;
begin
  Result := FGetItemAtIndex(FPosition);
end;

function TGenDictionaryEnumerator<TKey, TValue>.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FLength;
end;

procedure TGenDictionaryEnumerator<TKey, TValue>.Reset();
begin
  FPosition := -1;
end;


{ TGenDictionary }

constructor TGenDictionary<TKey, TValue>.Create();
begin
  inherited Create();
  FList := Classes.TList.Create();
end;

destructor TGenDictionary<TKey, TValue>.Destroy();
begin
  Clear();
  FList.Free();
  inherited Destroy;
end;

function TGenDictionary<TKey, TValue>.GetCount: SizeInt;
begin
  Result := FList.Count;
end;

procedure TGenDictionary<TKey, TValue>.Clear();
begin
  while (FList.Count > 0) do
  begin
    try
      TObject(FList[FList.Count - 1]).Free;
    except
    end;
    FList.Delete(FList.Count - 1);
  end;

  FList.Clear();
end;

function TGenDictionary<TKey, TValue>.IndexOfKey(const Key: TKey): SizeInt;
var
  i : Integer;
  Entry : TGenKeyValue<TKey, TValue>;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Entry := TGenKeyValue<TKey, TValue>(FList[i]);
    if Key = Entry.Key then
       Exit(i);
  end;

  Exit(-1);
end;

function TGenDictionary<TKey, TValue>.IndexOfValue(const Value: TValue): SizeInt;
var
  i : Integer;
  Entry: TGenKeyValue<TKey, TValue>;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Entry := TGenKeyValue<TKey, TValue>(FList[i]);
    if Value = Entry.Value then
       Exit(i);
  end;

  Exit(-1);
end;



function TGenDictionary<TKey, TValue>.FindByKey(const Key: TKey): TGenKeyValue<TKey, TValue>;
var
  i : Integer;
  Entry : TGenKeyValue<TKey, TValue>;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    Entry := TGenKeyValue<TKey, TValue>(FList[i]);
    if Key = Entry.Key then
       Exit(Entry);
  end;
end;

function TGenDictionary<TKey, TValue>.FindByValue(const Value: TValue): TGenKeyValue<TKey, TValue>;
var
  i : Integer;
  Entry : TGenKeyValue<TKey, TValue>;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    Entry := TGenKeyValue<TKey, TValue>(FList[i]);
    if Value = Entry.Value then
       Exit(Entry);
  end;

end;

function TGenDictionary<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Result := FindByKey(Key) <> nil;
end;

function TGenDictionary<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
begin
  Result := FindByValue(Value) <> nil;
end;

function TGenDictionary<TKey, TValue>.GetItemAtIndex(Index: SizeInt): TGenKeyValue<TKey, TValue>;
begin
  Result := TGenKeyValue<TKey, TValue>(FList[Index]);
end;

function TGenDictionary<TKey, TValue>.GetEnumerator(): TGenDictionaryEnumerator<TKey, TValue>;
begin
   Result := TGenDictionaryEnumerator<TKey, TValue>.Create(FList.Count, GetItemAtIndex);
end;

function TGenDictionary<TKey, TValue>.GetValue(const Key: TKey): TValue;
var
  Entry : TGenKeyValue<TKey, TValue>;
begin
  Entry := FindByKey(Key);
  if Assigned(Entry) then
     Exit(Entry.Value);

  raise Exception.Create('Key not found');
end;

procedure TGenDictionary<TKey, TValue>.SetValue(const Key: TKey; Value: TValue);
var
  Entry : TGenKeyValue<TKey, TValue>;
begin
  Entry := FindByKey(Key);
  if not Assigned(Entry) then
  begin
    Entry := TGenKeyValue<TKey, TValue>.Create(Key, Value);
    FList.Add(Entry);
  end;

  Entry.FValue := Value;
end;

function TGenDictionary<TKey, TValue>.GetKeys: TGenArray<TKey>;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := TGenKeyValue<TKey, TValue>(FList[i]).Key;
end;

function TGenDictionary<TKey, TValue>.GetValues: TGenArray<TValue>;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := TGenKeyValue<TKey, TValue>(FList[i]).Value;
end;

procedure TGenDictionary<TKey, TValue>.Add(Key: TKey; const Value: Variant);
begin
  Self.Item[Key] := Value;
end;

function TGenDictionary<TKey, TValue>.Remove(Key: TKey): Boolean;
var
  Index: Integer;
  Entry : TGenKeyValue<TKey, TValue>;
begin
  Result := False;
  Entry := FindByKey(Key);
  if Assigned(Entry) then
  begin
    Index := FList.IndexOf(Entry);
    Entry.Free();
    FList.Delete(Index);
    Result := True;
  end;
end;



















end.

