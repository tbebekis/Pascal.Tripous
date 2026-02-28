unit Tripous.List.Helpers;

{$mode delphi}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Contnrs;

type
  { --- Predicates / Callbacks   --- }
  TObjectPredicateMethod     = function(Item: TObject): Boolean of object;
  TObjectPredicateFunc       = function(Item: TObject): Boolean;
  TObjectPredicateNestedFunc = function(Item: TObject): Boolean is nested;

  TObjectActionMethod        = procedure(Item: TObject) of object;
  TObjectActionFunc          = procedure(Item: TObject);
  TObjectActionNestedFunc    = procedure(Item: TObject) is nested;

type
  { TCollectionHelper }
  TCollectionHelper = class helper for TCollection
  public
    function Contains(Item: TCollectionItem): Boolean;
    function IndexOf(Item: TCollectionItem): Integer;
    function Remove(Item: TCollectionItem): Boolean;

    function FirstOrDefault(Func: TObjectPredicateMethod): TCollectionItem; overload;
    function FirstOrDefault(Func: TObjectPredicateFunc): TCollectionItem; overload;
    function FirstOrDefault(Func: TObjectPredicateNestedFunc): TCollectionItem; overload;

    function Any(Func: TObjectPredicateMethod): Boolean; overload;
    function Any(Func: TObjectPredicateFunc): Boolean; overload;
    function Any(Func: TObjectPredicateNestedFunc): Boolean; overload;

    function All(Func: TObjectPredicateMethod): Boolean; overload;
    function All(Func: TObjectPredicateFunc): Boolean; overload;
    function All(Func: TObjectPredicateNestedFunc): Boolean; overload;

    procedure Where(Func: TObjectPredicateMethod; Dest: TList); overload;
    procedure Where(Func: TObjectPredicateFunc; Dest: TList); overload;
    procedure Where(Func: TObjectPredicateNestedFunc; Dest: TList); overload;

    procedure ForEach(Action: TObjectActionMethod); overload;
    procedure ForEach(Action: TObjectActionFunc); overload;
    procedure ForEach(Action: TObjectActionNestedFunc); overload;
  end;

  { TListHelper }
  TListHelper = class helper for TObjectList
  public
    function Contains(Item: TObject): Boolean;

    function FirstOrDefault(Func: TObjectPredicateMethod): TObject; overload;
    function FirstOrDefault(Func: TObjectPredicateFunc): TObject; overload;
    function FirstOrDefault(Func: TObjectPredicateNestedFunc): TObject; overload;

    function Any(Func: TObjectPredicateMethod): Boolean; overload;
    function Any(Func: TObjectPredicateFunc): Boolean; overload;
    function Any(Func: TObjectPredicateNestedFunc): Boolean; overload;

    function All(Func: TObjectPredicateMethod): Boolean; overload;
    function All(Func: TObjectPredicateFunc): Boolean; overload;
    function All(Func: TObjectPredicateNestedFunc): Boolean; overload;

    procedure Where(Func: TObjectPredicateMethod; Dest: TList); overload;
    procedure Where(Func: TObjectPredicateFunc; Dest: TList); overload;
    procedure Where(Func: TObjectPredicateNestedFunc; Dest: TList); overload;

    procedure ForEach(Action: TObjectActionMethod); overload;
    procedure ForEach(Action: TObjectActionFunc); overload;
    procedure ForEach(Action: TObjectActionNestedFunc); overload;
  end;

implementation

{ -------------------- TCollectionHelper -------------------- }

function TCollectionHelper.Contains(Item: TCollectionItem): Boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

function TCollectionHelper.IndexOf(Item: TCollectionItem): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (Self = nil) or (Item = nil) then
    Exit;

  for I := 0 to Count - 1 do
    if Items[I] = Item then
      Exit(I);
end;

function TCollectionHelper.Remove(Item: TCollectionItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
  if Result then
    Item.Free; // Free => also removes from collection
end;

function TCollectionHelper.FirstOrDefault(Func: TObjectPredicateMethod): TCollectionItem;
var
  I: Integer;
  It: TCollectionItem;
begin
  Result := nil;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    It := Items[I];
    if Func(It) then
      Exit(It);
  end;
end;

function TCollectionHelper.FirstOrDefault(Func: TObjectPredicateFunc): TCollectionItem;
var
  I: Integer;
  It: TCollectionItem;
begin
  Result := nil;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    It := Items[I];
    if Func(It) then
      Exit(It);
  end;
end;

function TCollectionHelper.FirstOrDefault(Func: TObjectPredicateNestedFunc): TCollectionItem;
var
  I: Integer;
begin
  Result := nil;
  if (Self = nil) or (not Assigned(Func)) then Exit;
  for I := 0 to Count - 1 do
    if Func(Items[I]) then
      Exit(Items[I]);
end;

function TCollectionHelper.Any(Func: TObjectPredicateMethod): Boolean;
begin
  Result := FirstOrDefault(Func) <> nil;
end;

function TCollectionHelper.Any(Func: TObjectPredicateFunc): Boolean;
begin
  Result := FirstOrDefault(Func) <> nil;
end;

function TCollectionHelper.Any(Func: TObjectPredicateNestedFunc): Boolean;
begin
  Result := FirstOrDefault(Func) <> nil;
end;

function TCollectionHelper.All(Func: TObjectPredicateMethod): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
    if not Func(Items[I]) then
      Exit(False);
end;

function TCollectionHelper.All(Func: TObjectPredicateFunc): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
    if not Func(Items[I]) then
      Exit(False);
end;

function TCollectionHelper.All(Func: TObjectPredicateNestedFunc): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
    if not Func(Items[I]) then
      Exit(False);

end;


procedure TCollectionHelper.Where(Func: TObjectPredicateMethod; Dest: TList);
var
  I: Integer;
  It: TCollectionItem;
begin
  if (Self = nil) or (Dest = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    It := Items[I];
    if Func(It) then
      Dest.Add(It);
  end;
end;

procedure TCollectionHelper.Where(Func: TObjectPredicateFunc; Dest: TList);
var
  I: Integer;
  It: TCollectionItem;
begin
  if (Self = nil) or (Dest = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    It := Items[I];
    if Func(It) then
      Dest.Add(It);
  end;
end;

procedure TCollectionHelper.Where(Func: TObjectPredicateNestedFunc; Dest: TList);
var
  I: Integer;
  It: TCollectionItem;
begin
  if (Self = nil) or (Dest = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    It := Items[I];
    if Func(It) then
      Dest.Add(It);
  end;

end;

procedure TCollectionHelper.ForEach(Action: TObjectActionMethod);
var
  I: Integer;
begin
  if (Self = nil) or (not Assigned(Action)) then
    Exit;

  for I := 0 to Count - 1 do
    Action(Items[I]);
end;

procedure TCollectionHelper.ForEach(Action: TObjectActionFunc);
var
  I: Integer;
begin
  if (Self = nil) or (not Assigned(Action)) then
    Exit;

  for I := 0 to Count - 1 do
    Action(Items[I]);
end;

procedure TCollectionHelper.ForEach(Action: TObjectActionNestedFunc);
var
  I: Integer;
begin
  if (Self = nil) or (not Assigned(Action)) then
    Exit;

  for I := 0 to Count - 1 do
    Action(Items[I]);

end;

{ -------------------- TListHelper -------------------- }

function TListHelper.Contains(Item: TObject): Boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

function TListHelper.FirstOrDefault(Func: TObjectPredicateMethod): TObject;
var
  I: Integer;
  Obj: TObject;
begin
  Result := nil;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    Obj := TObject(Items[I]);
    if Func(Obj) then
      Exit(Obj);
  end;
end;

function TListHelper.FirstOrDefault(Func: TObjectPredicateFunc): TObject;
var
  I: Integer;
  Obj: TObject;
begin
  Result := nil;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    Obj := TObject(Items[I]);
    if Func(Obj) then
      Exit(Obj);
  end;
end;

function TListHelper.FirstOrDefault(Func: TObjectPredicateNestedFunc): TObject;
var
  I: Integer;
  Obj: TObject;
begin
  Result := nil;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    Obj := TObject(Items[I]);
    if Func(Obj) then
      Exit(Obj);
  end;
end;



function TListHelper.Any(Func: TObjectPredicateMethod): Boolean;
begin
  Result := FirstOrDefault(Func) <> nil;
end;

function TListHelper.Any(Func: TObjectPredicateFunc): Boolean;
begin
  Result := FirstOrDefault(Func) <> nil;
end;

function TListHelper.Any(Func: TObjectPredicateNestedFunc): Boolean;
begin
  Result := FirstOrDefault(Func) <> nil;
end;

function TListHelper.All(Func: TObjectPredicateMethod): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
    if not Func(TObject(Items[I])) then
      Exit(False);
end;

function TListHelper.All(Func: TObjectPredicateFunc): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
    if not Func(TObject(Items[I])) then
      Exit(False);
end;

function TListHelper.All(Func: TObjectPredicateNestedFunc): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (Self = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
    if not Func(TObject(Items[I])) then
      Exit(False);

end;

procedure TListHelper.Where(Func: TObjectPredicateMethod; Dest: TList);
var
  I: Integer;
  Obj: TObject;
begin
  if (Self = nil) or (Dest = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    Obj := TObject(Items[I]);
    if Func(Obj) then
      Dest.Add(Obj);
  end;
end;

procedure TListHelper.Where(Func: TObjectPredicateFunc; Dest: TList);
var
  I: Integer;
  Obj: TObject;
begin
  if (Self = nil) or (Dest = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    Obj := TObject(Items[I]);
    if Func(Obj) then
      Dest.Add(Obj);
  end;
end;

procedure TListHelper.Where(Func: TObjectPredicateNestedFunc; Dest: TList);
var
  I: Integer;
  Obj: TObject;
begin
  if (Self = nil) or (Dest = nil) or (not Assigned(Func)) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    Obj := TObject(Items[I]);
    if Func(Obj) then
      Dest.Add(Obj);
  end;

end;

procedure TListHelper.ForEach(Action: TObjectActionMethod);
var
  I: Integer;
begin
  if (Self = nil) or (not Assigned(Action)) then
    Exit;

  for I := 0 to Count - 1 do
    Action(TObject(Items[I]));
end;

procedure TListHelper.ForEach(Action: TObjectActionFunc);
var
  I: Integer;
begin
  if (Self = nil) or (not Assigned(Action)) then
    Exit;

  for I := 0 to Count - 1 do
    Action(TObject(Items[I]));
end;

procedure TListHelper.ForEach(Action: TObjectActionNestedFunc);
var
  I: Integer;
begin
  if (Self = nil) or (not Assigned(Action)) then
    Exit;

  for I := 0 to Count - 1 do
    Action(TObject(Items[I]));

end;

end.

