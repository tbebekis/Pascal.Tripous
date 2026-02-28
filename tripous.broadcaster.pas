unit Tripous.Broadcaster;

{$mode delphi}{$H+}

interface

uses
  SysUtils, SyncObjs;

type
  { Base event args }
  TBroadcasterArgs = class
  public
    Name: string;
    Sender: TObject;
    Data: TObject;
    constructor Create(const AName: string; ASender: TObject = nil);
  end;

  { TBroadcasterTextArgs }

  TBroadcasterTextArgs = class(TBroadcasterArgs)
  public
    Value: string;
    constructor Create(const AName: string; AValue: string; ASender: TObject = nil);
  end;

  { TBroadcasterIntegerArgs }

  TBroadcasterIntegerArgs = class(TBroadcasterArgs)
  public
    Value: Integer;
    constructor Create(const AName: string; AValue: Integer; ASender: TObject = nil);
  end;

  { Everyone receives everything (mailing list) }
  TBroadcasterEvent = procedure(Args: TBroadcasterArgs) of object;

  TBroadcastToken = Integer;

  { Broadcaster }

  Broadcaster = class
  private
    type
      TEntry = record
        Id: Integer;
        Callback: TBroadcasterEvent;
      end;

  private class var
    FLock: TCriticalSection;
    FNextId: Integer;
    FList: array of TEntry;

    class function IndexOfId(AId: Integer): Integer; static;
  public
    class constructor Create;
    class destructor Destroy;

    { Register for ALL events }
    class function Register(ACallback: TBroadcasterEvent): TBroadcastToken; static;

    { Unregister }
    class procedure Unregister(AToken: TBroadcastToken); static;

    { Broadcast to ALL (Args is NOT freed here) }
    class procedure Broadcast(const Args: TBroadcasterArgs; FreeArgs: Boolean = True); overload; static;

    { Convenience: creates and frees args. Note: Data is caller responsibility to free it. }
    class procedure Broadcast(const Name: string; Sender: TObject); overload; static;
    class procedure Broadcast(const Name: string; Data: TObject; Sender: TObject); overload; static;
    class procedure Broadcast(const Name: string; const Value: string; Sender: TObject = nil); overload; static;
    class procedure Broadcast(const Name: string; const Value: Integer; Sender: TObject = nil); overload; static;

    class procedure Clear; static;
  end;

implementation

{ TBroadcasterArgs }

constructor TBroadcasterArgs.Create(const AName: string; ASender: TObject);
begin
  inherited Create;
  Name := AName;
  Sender := ASender;
end;

{ TBroadcasterTextArgs }

constructor TBroadcasterTextArgs.Create(const AName: string; AValue: string;
  ASender: TObject);
begin
  inherited Create(AName, ASender);
  Value := AValue;
end;

{ TBroadcasterIntegerArgs }

constructor TBroadcasterIntegerArgs.Create(const AName: string;
  AValue: Integer; ASender: TObject);
begin
  inherited Create(AName, ASender);
  Value := AValue;
end;

{ Broadcaster }

class constructor Broadcaster.Create;
begin
  FLock := TCriticalSection.Create;
  FNextId := 1;
  SetLength(FList, 0);
end;

class destructor Broadcaster.Destroy;
begin
  FLock.Free;
  SetLength(FList, 0);
end;

class function Broadcaster.IndexOfId(AId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FList) do
    if FList[I].Id = AId then
      Exit(I);
end;

class function Broadcaster.Register(ACallback: TBroadcasterEvent): TBroadcastToken;
var
  N: Integer;
begin
  Result := 0;
  if not Assigned(ACallback) then
    Exit;

  FLock.Acquire;
  try
    Result := FNextId;
    Inc(FNextId);

    N := Length(FList);
    SetLength(FList, N + 1);
    FList[N].Id := Result;
    FList[N].Callback := ACallback;
  finally
    FLock.Release;
  end;
end;

class procedure Broadcaster.Unregister(AToken: TBroadcastToken);
var
  Idx: Integer;
begin
  if AToken <= 0 then
    Exit;

  FLock.Acquire;
  try
    Idx := IndexOfId(AToken);
    if Idx < 0 then
      Exit;

    { remove immediately (swap with last) }
    FList[Idx] := FList[High(FList)];
    SetLength(FList, Length(FList) - 1);
  finally
    FLock.Release;
  end;
end;

class procedure Broadcaster.Broadcast(const Args: TBroadcasterArgs; FreeArgs: Boolean);
var
  Snap: array of TEntry;
  I: Integer;
  Cb: TBroadcasterEvent;
begin
  if Args = nil then
    Exit;

  { Snapshot under lock }
  FLock.Acquire;
  try
    Snap := Copy(FList, 0, Length(FList));
  finally
    FLock.Release;
  end;

  try
    { Invoke without lock (no deadlocks, handlers can register/unregister safely) }
    for I := 0 to High(Snap) do
    begin
      Cb := Snap[I].Callback;
      if Assigned(Cb) then
      begin
        try
          Cb(Args);
        except
          on E: Exception do
          begin
              { log and continue }
          end;
        end;
      end;
    end;
  finally
    if FreeArgs then
      Args.Free();
  end;

end;

class procedure Broadcaster.Broadcast(const Name: string; Sender: TObject);
var
  Args: TBroadcasterArgs;
begin
  Args := TBroadcasterArgs.Create(Name, Sender);
  Broadcast(Args, True);
end;

class procedure Broadcaster.Broadcast(const Name: string; Data: TObject; Sender: TObject);
var
  Args: TBroadcasterArgs;
begin
  Args := TBroadcasterArgs.Create(Name, Sender);
  Args.Data := Data;
  Broadcast(Args, True);
end;

class procedure Broadcaster.Broadcast(const Name: string; const Value: string; Sender: TObject);
begin
  Broadcast(TBroadcasterTextArgs.Create(Name, Value, Sender), True);
end;

class procedure Broadcaster.Broadcast(const Name: string; const Value: Integer; Sender: TObject);
begin
  Broadcast(TBroadcasterIntegerArgs.Create(Name, Value, Sender), True);
end;

class procedure Broadcaster.Clear;
begin
  FLock.Acquire;
  try
    SetLength(FList, 0);
  finally
    FLock.Release;
  end;
end;

end.

