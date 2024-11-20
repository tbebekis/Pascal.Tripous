unit o_Test;

{$MODE DELPHI}{$H+}
//{$modeswitch duplicatelocals}

interface

uses
  Classes
  ,SysUtils
  ,Types
  ,Forms
  ,Controls
  ,Graphics
  ,Dialogs
  ,ExtCtrls
  ,StdCtrls
  ,StrUtils

  ,Generics.Defaults
  ,Generics.Collections
  ,Tripous
  ,Tripous.Generics
  ;


var
  mmoLog: TMemo;

procedure GenListTest_Clear();
procedure GenListTest_Show();
procedure GenListTest_Reverse();
procedure GenListTest_Sort();
procedure GenListTest_AddRange(sNames: string);
procedure GenListTest_AddGiantRange(HowMany: Integer);
procedure GenListTest_FirstOrDefault(StartingWith: string);
procedure GenListTest_Where(Containing: string);

implementation


type
  { TPerson }
  TPerson = class
  public
    Name: string;
    constructor Create(AName: string);
    destructor Destroy(); override;
  end;

{ TPerson }

constructor TPerson.Create(AName: string);
begin
  Name := AName;
end;

destructor TPerson.Destroy();
begin
  inherited Destroy();
end;

type
  TPersonArray = array of TPerson;

var
  PersonList: TGenObjectList<TPerson>;

function CreatePersonArray(Names: TStringDynArray): TPersonArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, Length(Names));
  for i := Low(Names) to High(Names) do
    Result[i] := TPerson.Create(Names[i]);
end;

function Split(S: string): TStringDynArray;
var
  i : Integer;
begin
  Result := SplitString(S, ',');
  for i := Low(Result) to High(Result) do
    Result[i] := Trim(Result[i]);
end;

procedure LogPersons();
var
  Item: TPerson;
begin
  mmoLog.Clear();
  if PersonList.Count > 0 then
  begin
    for Item in PersonList do
      mmoLog.Append(Item.Name);
  end else
    mmoLog.Append('List is empty');
end;

procedure GenListTest_Clear();
begin
  mmoLog.Clear();
  PersonList.Clear();
end;

procedure GenListTest_Show();
begin
  LogPersons();
end;

procedure GenListTest_Reverse();
begin
  PersonList.Reverse();
  LogPersons();
end;

function ComparePerson(constref A, B: TPerson): Integer;
begin
  Result := AnsiCompareText(A.Name, B.Name);
end;

procedure GenListTest_Sort();
begin
  PersonList.Sort(ComparePerson);
  LogPersons();
end;

procedure GenListTest_AddRange(sNames: string);
var
  A: TStringDynArray;
  PersonArray: TPersonArray;
begin
  sNames := Trim(sNames);
  A := Split(sNames);
  if Length(A) > 0 then
  begin
    PersonArray := CreatePersonArray(A);

    PersonList.AddRange(PersonArray);
    LogPersons();
  end;
end;

procedure GenListTest_AddGiantRange(HowMany: Integer);
var
  A: TStringDynArray;
  i : Integer;
  PersonArray: TPersonArray;
begin
  A := [];
  SetLength(A, HowMany);
  for i := Low(A) to High(A) do
    A[i] := 'Name ' + IntToStr(i);

  PersonArray := CreatePersonArray(A);

  PersonList.AddRange(PersonArray);
  ShowMessage('Done. Now logging...');
  LogPersons();
end;

var
  FirstOrDefaultTerm: string;

function FirstOrDefaultConditionFunc(Item: TPerson): Boolean;
begin
  Result := StartsText(FirstOrDefaultTerm, Item.Name);
end;

procedure GenListTest_FirstOrDefault(StartingWith: string);
var
  P: TPerson;
begin
  mmoLog.Clear();

  FirstOrDefaultTerm := Trim(StartingWith);
  P := PersonList.FirstOrDefault(FirstOrDefaultConditionFunc);
  if Assigned(P) then
    mmoLog.Append(Format('Found: %s', [P.Name]))
  else
    mmoLog.Append('Nothing found');
end;

var
  WhereTerm : string;

function WhereConditionFunc(Item: TPerson): Boolean;
begin
  Result := AnsiContainsText(Item.Name, WhereTerm);
end;

procedure GenListTest_Where(Containing: string);
var
  List: IList<TPerson>;
  Item: TPerson;
begin
  mmoLog.Clear();
  WhereTerm  := Trim(Containing);
  List := PersonList.Where(WhereConditionFunc);
  if List.Count > 0 then
  begin
    for Item in List do
      mmoLog.Append(Item.Name);
  end else
    mmoLog.Append('Nothing found');
end;

initialization
  PersonList := TGenObjectList<TPerson>.Create(True);

finalization
  PersonList.Free();

end.

