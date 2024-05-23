unit Tripous.Logs;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
   Classes
  ,SysUtils
  ,SyncObjs
  ,Variants
  ,Forms
  ,Controls
  ,Graphics
  ,Dialogs
  ,ExtCtrls
  ,StdCtrls
  ,DBGrids
  ,DBCtrls
  ,ComCtrls
  ,DB
  ,BufDataset
  ,fgl
  ,Tripous
  ;
 
type
  TLogLevel = (
     loNone   = 0
    ,loTrace  = 1
    ,loDebug  = 2
    ,loInfo   = 4
    ,loWarn   = 8
    ,loError  = $10
    ,loFatal  = $20
  );

  TLogPropLengthDictionary = specialize TFPGMap<string, Word>;

  TLogTextProc = procedure(LogText: string) of object;

  ILogInfo = interface;

  { TLogRecord }
  TLogRecord = class
  public
    Date: string;
    Time: string;
    User: string;
    Host: string;
    Level: string;
    Source: string;
    Scope: string;
    EventId: string;
    Text: string;

    LogText: string;

    constructor Create(Info: ILogInfo);
  end;

  { ILogInfo }
  ILogInfo = interface
    ['{BA60D540-FECA-42AB-9B3F-7FC50362EE9F}']
    function GetDate: string;
    function GetEventId: string;
    function GetException: Exception;
    function GetExceptionData: string;
    function GetHost: string;
    function GetLevel: TLogLevel;
    function GetLevelText: string;
    function GetProperties: IVariantDictionary;
    function GetScopeId: string;
    function GetSource: string;
    function GetText: string;
    function GetTextTemplate: string;
    function GetTime: string;
    function GetTimeStamp: TDateTime;
    function GetTimeStampText: string;
    function GetUser: string;
    procedure SetUser(AValue: string);

    function  ToString: ansistring; override;
    procedure SaveToFile(Folder: string);

    function GetPropertiesAsSingleLine(): string;
    function GetPropertiesAsTextList(): string;
    function CreateLogRecord(): TLogRecord;

    // Returns the UTC date-time this info created
    property TimeStamp: TDateTime read  GetTimeStamp;
    property TimeStampText: string read GetTimeStampText;
    // Returns the UTC date this info created.
    property Date: string read GetDate;
    // Returns the UTC time this info created.
    property Time: string read GetTime;
    // The username of the current user of this application or the local computer
    property User: string read GetUser write SetUser;
    // The name of the local computer
    property Host: string read GetHost;

    // The level
    property Level: TLogLevel read GetLevel;
    property LevelText: string read GetLevelText;
    // The event Id
    property EventId: string read GetEventId;
    // The text before formatting params into it.
    property TextTemplate: string read GetTextTemplate;
    // The log message
    property Text: string read GetText;
    //  The scope if any
    property ScopeId: string read GetScopeId;
    // The source of this log, if any
    property Source: string read GetSource;
    // The exception if this is a log regarding an exception
    property Exception_ : Exception read GetException;
    // The exception data, if this is a log regarding an exception
    property ExceptionData: string read GetExceptionData;
    // A dictionary with params passed when the log message was formatted. For use by structured log listeners
    property Properties: IVariantDictionary read GetProperties;
  end;

  { ILogSource }
  ILogSource = interface
    ['{019F8BB3-E92F-4580-A3BD-A142FFA0B28A}']
    function  GetActive: Boolean;
    procedure SetActive(AValue: Boolean);
    function  GetName: string;
    procedure SetName(AValue: string);

    procedure EnterScope(Id: string; const ScopeParams: IVariantDictionary = nil);
    procedure ExitScope();

    procedure Log(EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(EventId: string; Level: TLogLevel; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(Level: TLogLevel; Text: string; const Params: IVariantDictionary = nil);

    procedure Debug(EventId, Text: string);
    procedure Debug(Text: string);

    procedure Info(EventId, Text: string);
    procedure Info(Text: string);

    procedure Error(EventId: string; Ex: Exception);
    procedure Error(Ex: Exception);

    { properties }
    property Name: string read GetName write SetName;
    property Active: Boolean read GetActive write SetActive;
  end;


  { TLogListener }
  { A TLogListener automatically adds itself to Logger.Listeners when created
    and automatically removes itself from Logger.Listeners when destroyed. }
  TLogListener = class
  protected
    FLock         : SyncObjs.TCriticalSection;
    FLockCount    : Integer;

    procedure Lock();
    procedure UnLock();

    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Info: ILogInfo); virtual; abstract;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
  end;

  { TFormLogListener }
  TFormLogListener = class(TLogListener)
  private
    type
    { TLogTable }
    TLogTable = class(TBufDataset)
    protected
      procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField); override;
    public
      constructor Create(AOwner: TComponent); override;
    end;
  private
    FLogTable   : TLogTable;
    FDS         : TDatasource;
    FLogForm    : TForm;
    FSafeList   : TSafeObjectList;

    procedure CallLogProc();
    procedure InitializeGrid(Grid: TDBGrid);
    procedure ClearData();
  protected
    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Info: ILogInfo); override;
  public
    constructor Create();
    destructor Destroy(); override;
  end;

  { TFileLogListener }
  TFileLogListener = class(TLogListener)
  private
    FLengths       : TLogPropLengthDictionary;
    FMaxSizeInMB   : SizeInt;
    FFilePath      : string;
    FFolder        : string;
    FLogFile       : TLogFile;

    procedure BeginFile();
    procedure WriteLine(Line: string);
  protected
    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Info: ILogInfo); override;
  public
    constructor Create(Folder: string = ''; MaxSizeInMB: SizeInt = 5);
    destructor Destroy(); override;
  end;

  { TLogToMainThreadListener }
  { Synchronizes updates to MainThread controls }
  TLogToMainThreadListener = class(TLogListener)
  private
    FLogTextList  : TStringList;
    FLogProc      : TLogTextProc;

    procedure CallLogProc();
  protected
    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Info: ILogInfo); override;
    function  GetLogInfoText(const Info: ILogInfo): string; virtual; abstract;
  public
    constructor Create(LogTextProc: TLogTextProc); virtual;
    destructor Destroy(); override;
  end;

  { TLogTextListener }
  TLogTextListener = class(TLogToMainThreadListener)
  protected
    function  GetLogInfoText(const Info: ILogInfo): string; override;
  public
    constructor Create(LogTextProc: TLogTextProc); override;
  end;

  { TLogLineListener }
  TLogLineListener = class(TLogToMainThreadListener)
  protected
    function  GetLogInfoText(const Info: ILogInfo): string; override;
  public
    constructor Create(LogLineProc: TLogTextProc); override;
    destructor Destroy(); override;
  end;

  { TLogJob }
  TLogJob = class
  private
    FListener: TLogListener;
    FInfo    : ILogInfo;
  public
    constructor Create(Listener: TLogListener; Info: ILogInfo);
  end;

  { Logger }
  Logger = class
  private class var
    FActive          : Integer;
    FLogFolder       : string;
    FLock            : SyncObjs.TCriticalSection;
    FLockCount       : Integer;
    FListeners       : Classes.TList;
    FMinLevel        : TLogLevel;
    FSafeList        : TSafeObjectList;
    FLogThread       : TThread;
    FLogJobThreadTerminated: Boolean;
    FLineLengths     : TLogPropLengthDictionary;

    class function GetActive: Boolean; static;
    class procedure SetActive(Value: Boolean); static;
    class function GetMinLevel: TLogLevel; static;
    class procedure SetMinLevel(Value: TLogLevel); static;
    class function GetLogFolder: string; static;

    class procedure Lock;
    class procedure UnLock;

    class procedure Add(Listener: TLogListener);
    class procedure Remove(Listener: TLogListener);
  public
    class constructor Create();
    class destructor Destroy();

    class function CreateLogSource(SourceName: string): ILogSource;

    { log }
    class procedure Log(const Info: ILogInfo);
    class procedure Log(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);

    class procedure Debug(Source, ScopeId, EventId, Text: string);
    class procedure Debug(Source, EventId, Text: string);
    class procedure Debug(Source, Text: string);
    class procedure Debug(Text: string);

    class procedure Info(Source, ScopeId, EventId, Text: string);
    class procedure Info(Source, EventId, Text: string);
    class procedure Info(Source, Text: string);
    class procedure Info(Text: string);

    class procedure Error(Source, ScopeId, EventId: string; Ex: Exception);
    class procedure Error(Source, EventId: string; Ex: Exception);
    class procedure Error(Source: string; Ex: Exception);
    class procedure Error(Ex: Exception);

    { helpers }
    class function  DateTimeToFileName(DT: TDateTime): string;
    class function  GetLastExceptionStackAsText(): string;
    class function  LogLevelToString(Level: TLogLevel): string;
    class function  RemoveLineEndings(Line: string): string;
    class function  RPad(Text: string; MaxLength: Integer): string;
    class function  GetHostName(): string;
    class function  FormatParams(Text: string; const Params: IVariantDictionary): string;

    class function  GetAsText(LogInfo: ILogInfo): string;
    class function  GetAsLine(LogInfo: ILogInfo): string;

    { properties }
    class property Active: Boolean read GetActive write SetActive;
    class property LogFolder: string read GetLogFolder write FLogFolder;
    class property MinLevel: TLogLevel read GetMinLevel write SetMinLevel;
  end;


implementation

uses
   LazSysUtils
  ,StrUtils
  ,DateUtils
  ,TypInfo
  {$IFDEF UNIX} ,unix{$ENDIF}
  {$IFDEF WINDOWS} ,Windows{$ENDIF}
  ;

{ TLogRecord }
constructor TLogRecord.Create(Info: ILogInfo);
begin
  inherited Create();
  Date      := Info.Date       ;
  Time      := Info.Time       ;
  User      := Info.User       ;
  Host      := Info.Host       ;
  Level     := Info.LevelText  ;
  Source    := Info.Source     ;
  Scope     := Info.ScopeId    ;
  EventId   := Info.EventId    ;
  Text      := Info.Text       ;

  LogText   := Logger.GetAsText(Info);
end;


type
  { TLogInfo }
  TLogInfo = class(TInterfacedObject, ILogInfo)
  private
    FEventId: string;
    FException: Exception;
    FExceptionData: string;
    FHost: string;
    FLevel: TLogLevel;
    FProperties: IVariantDictionary;
    FScopeId: string;
    FSource: string;
    FText: string;
    FTextTemplate: string;
    FTimeStamp: TDateTime;
    FUser: string;

    function GetDate: string;
    function GetEventId: string;
    function GetException: Exception;
    function GetExceptionData: string;
    function GetHost: string;
    function GetLevel: TLogLevel;
    function GetLevelText: string;
    function GetProperties: IVariantDictionary;
    function GetScopeId: string;
    function GetSource: string;
    function GetText: string;
    function GetTextTemplate: string;
    function GetTime: string;
    function GetTimeStamp: TDateTime;
    function GetTimeStampText: string;
    function GetUser: string;
    procedure SetUser(Value: string);
  public
    constructor Create(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    destructor Destroy(); override;

    function  ToString: ansistring; override;
    procedure SaveToFile(Folder: string = '');

    function GetPropertiesAsSingleLine(): string;
    function GetPropertiesAsTextList(): string;
    function CreateLogRecord(): TLogRecord;

    property TimeStamp: TDateTime read  GetTimeStamp;
    property TimeStampText: string read GetTimeStampText;
    property Date: string read GetDate;
    property Time: string read GetTime;
    property User: string read GetUser write SetUser;
    property Host: string read GetHost;

    property Level: TLogLevel read GetLevel;
    property LevelText: string read GetLevelText;
    property EventId: string read GetEventId;
    property TextTemplate: string read GetTextTemplate;
    property Text: string read GetText;
    property ScopeId: string read GetScopeId;
    property Source: string read GetSource;
    property Exception_ : Exception read GetException;
    property ExceptionData: string read GetExceptionData;
    property Properties: IVariantDictionary read GetProperties;
  end;







{ TLogJob }
constructor TLogJob.Create(Listener: TLogListener; Info: ILogInfo);
begin
  inherited Create();
  FListener := Listener;
  FInfo     := Info;
end;









type
  TLogSource = class;

  { TLogScope }
  TLogScope = class
  private
    FId             : string;
    FLogSource      : TLogSource;
    FProperties     : IVariantDictionary;

    function GetId: string;
    function GetProperties: IVariantDictionary;
  public
    constructor Create(Source: TLogSource; const aId: string = ''; const ScopeParams: IVariantDictionary = nil);

    property Id : string read GetId;
    property Properties: IVariantDictionary read GetProperties;
  end;

  { TLogSource }
  TLogSource = class(TInterfacedObject, ILogSource)
  private
    FName            : string;
    FActive          : Integer;
    FLock            : SyncObjs.TCriticalSection;
    FLockCount       : Integer;

    FScopes          : TList;

    function GetActive: Boolean;
    function GetCurrentScope: TLogScope;
    function GetName: string;
    procedure Lock;
    procedure SetActive(Value: Boolean);
    procedure SetName(AValue: string);
    procedure UnLock;

    property CurrentScope: TLogScope read GetCurrentScope;
  public
    constructor Create(AName: string);
    destructor Destroy; override;

    procedure  EnterScope(Id: string; const ScopeParams: IVariantDictionary = nil);
    procedure  ExitScope();

    procedure Log(EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(EventId: string; Level: TLogLevel; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(Level: TLogLevel; Text: string; const Params: IVariantDictionary = nil);

    procedure Debug(EventId, Text: string);
    procedure Debug(Text: string);

    procedure Info(EventId, Text: string);
    procedure Info(Text: string);
    procedure Error(EventId: string; Ex: Exception);
    procedure Error(Ex: Exception);

    { properties }
    property Name: string read GetName write SetName;
    property Active: Boolean read GetActive write SetActive;

  end;


{ TLogScope }
constructor TLogScope.Create(Source: TLogSource; const aId: string; const ScopeParams: IVariantDictionary);
begin
  inherited Create();
  FLogSource := Source;
  FId := aId;
  FProperties := ScopeParams;
end;

function TLogScope.GetId: string;
begin
  Result := FId;
end;

function TLogScope.GetProperties: IVariantDictionary;
begin
  Result := FProperties;
end;


{ TLogSource }
constructor TLogSource.Create(AName: string);
begin
  inherited Create();
  FLock      := SyncObjs.TCriticalSection.Create();
  FName      := AName;

  FScopes    := TList.Create();

  Active     := True;

  EnterScope('Default Scope')
end;

destructor TLogSource.Destroy;
begin
  Sys.ClearObjectList(FScopes);
  FScopes.Free();
  FLock.Free();
  inherited Destroy;
end;

procedure TLogSource.Lock;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    FLock.Enter;
end;

procedure TLogSource.UnLock;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    FLock.Leave();
  end;
end;

function TLogSource.GetActive: Boolean;
begin
  Lock();
  try
    Result := FActive > 0;
  finally
    UnLock();
  end;
end;

function TLogSource.GetCurrentScope: TLogScope;
begin
  Result := TLogScope(FScopes.Last);
end;

function TLogSource.GetName: string;
begin
  Result := FName;
end;

procedure TLogSource.SetName(AValue: string);
begin
  FName := AValue;
end;

procedure TLogSource.SetActive(Value: Boolean);
begin
  Lock();
  try
    if Value then
      Inc(FActive)
    else begin
      Dec(FActive);
      if FActive < 0 then
        FActive := 0;
    end;
  finally
    UnLock();
  end;
end;

procedure TLogSource.EnterScope(Id: string; const ScopeParams: IVariantDictionary);
var
  Scope: TLogScope;
begin
  Lock();
  try
    if Trim(Id) = '' then
      Id := Sys.CreateGuid(True);

    Scope := TLogScope.Create(Self, Id, ScopeParams);
    FScopes.Add(Scope);
  finally
    UnLock();
  end;

end;

procedure TLogSource.ExitScope();
begin
  if FScopes.Count > 1 then
  begin
    CurrentScope.Free();
    FScopes.Delete(FScopes.Count - 1);
  end;
end;

procedure TLogSource.Log(EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary);
var
  LogInfo: TLogInfo;
  Entry: TKeyValue;
  Scope: TLogScope;
begin

  Lock();
  try
     if Active then
     begin
       Scope := CurrentScope;
       LogInfo := TLogInfo.Create(Self.Name, Scope.Id, EventId, Level, Exception_, Text, Params);
       if Assigned(Scope.Properties) then
       begin
         for Entry in Scope.Properties do
           LogInfo.Properties[Entry.Key] := Entry.Value;
       end;
       Logger.Log(LogInfo);
     end;
  finally
    UnLock();
  end;
end;

procedure TLogSource.Log(Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary);
begin
  Log('0', Level, Exception_, Text, Params);
end;

procedure TLogSource.Log(EventId: string; Level: TLogLevel; Text: string; const Params: IVariantDictionary);
begin
  Log(EventId, Level, nil, Text, Params);
end;

procedure TLogSource.Log(Level: TLogLevel; Text: string; const Params: IVariantDictionary);
begin
  Log('0', Level, nil, Text, Params);
end;

procedure TLogSource.Debug(EventId, Text: string);
begin
  Log(EventId, TLogLevel.loDebug, Text);
end;

procedure TLogSource.Debug(Text: string);
begin
  Debug('0', Text);
end;

procedure TLogSource.Info(EventId, Text: string);
begin
  Log(EventId, TLogLevel.loInfo, Text);
end;

procedure TLogSource.Info(Text: string);
begin
  Info('0', Text);
end;

procedure TLogSource.Error(EventId: string; Ex: Exception);
begin
  Log(EventId, TLogLevel.loError, Ex, Ex.Message);
end;

procedure TLogSource.Error(Ex: Exception);
begin
  Error('0', Ex);
end;



{ TLogInfo }
constructor TLogInfo.Create(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary);
begin
   inherited Create;

   FHost := Logger.GetHostName();
   FUser := SysUtils.GetEnvironmentVariable('USERNAME');
   if Length(FUser) = 0 then
      FUser := SysUtils.GetEnvironmentVariable('USER');

   FTimeStamp := NowUTC();

   if Length(Source) > 0 then
      FSource := Source
   else if Assigned(Exception_) then
      FSource := Exception_.ClassName;

   FScopeId       := ScopeId;
   FEventId       := EventId;
   FLevel         := Level;
   FException     := Exception_;
   FTextTemplate  := Text;

   if Length(Text) > 0 then
      FText := Text
   else if Assigned(Exception_) then
      FText := Format('Exception Class: %s - Exception Message: %s', [Exception_.ClassName, Exception_.Message]);

   if Assigned(Exception_) then
     FExceptionData := Logger.GetLastExceptionStackAsText();

   FProperties := Params;
   if Assigned(FProperties) then
      FText := Logger.FormatParams(FText, FProperties)
   else
     FProperties := TVariantDictionary.Create();
end;

destructor TLogInfo.Destroy;
begin
  inherited Destroy;
end;

function TLogInfo.ToString: ansistring;
var
  SB: TAnsiStringBuilder;
begin

  SB := TAnsiStringBuilder.Create('');
  try
    SB.AppendLine(' ================================================================');
    SB.AppendLine('Date           : ' + TimeStampText);
    SB.AppendLine('NetUserName    : ' + User);
    SB.AppendLine('Computer       : ' + Host);
    SB.AppendLine(' ----------------------------------------------------------------');
    SB.AppendLine('Level          : ' + LevelText);
    SB.AppendLine('Source         : ' + Source);
    SB.AppendLine('Scope          : ' + ScopeId);
    SB.AppendLine('Message text   : ' + Text);

    if Length(ExceptionData) > 0 then
    begin
      SB.AppendLine();
      SB.AppendLine(ExceptionData);
    end;

    Result := SB.ToString();
  finally
    SB.Free();
  end;

end;

procedure TLogInfo.SaveToFile(Folder: string);
var
  FilePath: string;
  List: TStringList;
begin
  if Length(Folder) = 0 then
     Folder := Logger.LogFolder;

  if not DirectoryExists(Folder) then
     CreateDir(Folder);

  FilePath := ConcatPaths([Folder, Logger.DateTimeToFileName(TimeStamp) + '.txt']);

  List := TStringList.Create;
  try
    List.Add(Self.ToString());
    List.SaveToFile(FilePath);
  finally
    List.Free;
  end;

end;

function TLogInfo.GetPropertiesAsSingleLine(): string;
var
  Entry: TKeyValue;
  Count: Integer;
  i    : Integer;
begin
  Result := '';

  if Assigned(FProperties) then
  begin
    Count := FProperties.Count;
    i     := 0;
    for Entry in FProperties do
    begin
      Inc(i);
      if not (Variants.VarIsNull(Entry.Value) or Variants.VarIsEmpty(Entry.Value)) then
      begin
        Result += Entry.Key;
        Result += ' = ';
        Result += Variants.VarToStr(Entry.Value);
        if i < Count then
          Result += ', ';
      end;
    end;
  end;
end;

function TLogInfo.GetPropertiesAsTextList(): string;
var
  Entry: TKeyValue;
  Count: Integer;
  i    : Integer;
begin
  Result := '';

  if Assigned(FProperties) then
  begin
    Count := FProperties.Count;
    i     := 0;
    for Entry in FProperties do
    begin
      Inc(i);
      if not (Variants.VarIsNull(Entry.Value) or Variants.VarIsEmpty(Entry.Value)) then
      begin
        Result += Entry.Key;
        Result += ' = ';
        Result += Variants.VarToStr(Entry.Value);
        if i < Count then
          Result += LineEnding;
      end;
    end;
  end;

end;

function TLogInfo.CreateLogRecord(): TLogRecord;
begin
  Result := TLogRecord.Create(Self as ILogInfo);
end;

function TLogInfo.GetTimeStamp: TDateTime;
begin
  Result := FTimeStamp;
end;

function TLogInfo.GetTimeStampText: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', TimeStamp);
end;

function TLogInfo.GetDate: string;
begin
  Result := FormatDateTime('yyyy-mm-dd', TimeStamp);
end;

function TLogInfo.GetTime: string;
begin
  Result := FormatDateTime('hh:nn:ss.zzz', TimeStamp);
end;

function TLogInfo.GetUser: string;
begin
  Result := FUser;
end;

procedure TLogInfo.SetUser(Value: string);
begin
  FUser := Value;
end;

function TLogInfo.GetHost: string;
begin
  Result := FHost;
end;

function TLogInfo.GetLevel: TLogLevel;
begin
  Result := FLevel;
end;

function TLogInfo.GetLevelText: string;
begin
  Result := Logger.LogLevelToString(Level);
end;

function TLogInfo.GetEventId: string;
begin
   Result := FEventId;
end;

function TLogInfo.GetTextTemplate: string;
begin
  Result := FTextTemplate;
end;

function TLogInfo.GetText: string;
begin
  Result := FText;
end;

function TLogInfo.GetScopeId: string;
begin
  Result := FScopeId;
end;

function TLogInfo.GetSource: string;
begin
  Result := FSource;
end;

function TLogInfo.GetException: Exception;
begin
  Result := FException;
end;

function TLogInfo.GetExceptionData: string;
begin
  Result := FExceptionData;
end;

function TLogInfo.GetProperties: IVariantDictionary;
begin
  Result := FProperties;
end;







{ TLogListener }
constructor TLogListener.Create();
begin
  inherited Create();
  FLock        := SyncObjs.TCriticalSection.Create();
end;

destructor TLogListener.Destroy;
begin
  FLock.Free();
  inherited Destroy;
end;

procedure TLogListener.AfterConstruction;
begin
  inherited AfterConstruction;
  Logger.Add(Self);
end;

procedure TLogListener.BeforeDestruction;
begin
  Logger.Remove(Self);
  inherited BeforeDestruction;
end;

procedure TLogListener.Lock;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    FLock.Enter;
end;

procedure TLogListener.UnLock;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    FLock.Leave();
  end;
end;



type
  { TLogForm }
  TLogForm = class(TForm)
  protected
    btnClearLog: TButton;
    btnClearTable: TButton;
    Grid: TDBGrid;
    mmoLog: TMemo;
    mmoText: TDBMemo;
    Pager: TPageControl;
    pnlLog: TPanel;
    pnlTable: TPanel;
    Splitter: TSplitter;
    tabLog: TTabSheet;
    tabTable: TTabSheet;

    Listener: TFormLogListener;

    procedure AnyClick(Sender: TObject);
    procedure InitializeForm(aListener: TFormLogListener);
  public
    function CloseQuery: boolean;  override;
  end;




{ TLogForm }
procedure TLogForm.InitializeForm(aListener: TFormLogListener);

const
  SDfm =
    'object LogDialog: TLogForm                               ' +
    '  Left = 396                                               ' +
    '  Height = 204                                             ' +
    '  Top = 200                                                ' +
    '  Width = 711                                              ' +
    '  Caption = ''LogDialog''                                  ' +
    '  ClientHeight = 343                                       ' +
    '  ClientWidth = 711                                        ' +
    '  FormStyle = fsStayOnTop                                  ' +
   // '  Position = poMainFormCenter                              ' +
    '  ShowInTaskBar = stNever                                  ' +
    '  object Pager: TPageControl                               ' +
    '    Left = 0                                               ' +
    '    Height = 343                                           ' +
    '    Top = 0                                                ' +
    '    Width = 711                                            ' +
    '    ActivePage = tabLog                                    ' +
    '    Align = alClient                                       ' +
    '    TabIndex = 0                                           ' +
    '    TabOrder = 0                                           ' +
    '    object tabLog: TTabSheet                               ' +
    '      Caption = ''Log''                                      ' +
    '      ClientHeight = 315                                   ' +
    '      ClientWidth = 703                                    ' +
    '      object pnlLog: TPanel                                ' +
    '        Left = 0                                           ' +
    '        Height = 28                                        ' +
    '        Top = 0                                            ' +
    '        Width = 703                                        ' +
    '        Align = alTop                                      ' +
    '        ClientHeight = 28                                  ' +
    '        ClientWidth = 703                                  ' +
    '        TabOrder = 0                                       ' +
    '        object btnClearLog: TButton                        ' +
    '          Left = 4                                         ' +
    '          Height = 25                                      ' +
    '          Top = 1                                          ' +
    '          Width = 75                                       ' +
    '          Caption = ''Clear''                                ' +
    '          TabOrder = 0                                     ' +
    '        end                                                ' +
    '      end                                                  ' +
    '      object mmoLog: TMemo                                 ' +
    '        Left = 0                                           ' +
    '        Height = 287                                       ' +
    '        Top = 28                                           ' +
    '        Width = 703                                        ' +
    '        Align = alClient                                   ' +
    '        Font.CharSet = ANSI_CHARSET                        ' +
    '        Font.Name = ''Courier New''                          ' +
    '        Font.Pitch = fpFixed                               ' +
    '        Font.Quality = fqDraft                             ' +
    '        ParentFont = False                                 ' +
    '        ScrollBars = ssAutoBoth                            ' +
    '        TabOrder = 1                                       ' +
    '      end                                                  ' +
    '    end                                                    ' +
    '    object tabTable: TTabSheet                             ' +
    '      Caption = ''Table''                                    ' +
    '      ClientHeight = 315                                   ' +
    '      ClientWidth = 703                                    ' +
    '      object pnlTable: TPanel                              ' +
    '        Left = 0                                           ' +
    '        Height = 28                                        ' +
    '        Top = 0                                            ' +
    '        Width = 703                                        ' +
    '        Align = alTop                                      ' +
    '        ClientHeight = 28                                  ' +
    '        ClientWidth = 703                                  ' +
    '        TabOrder = 0                                       ' +
    '        object btnClearTable: TButton                      ' +
    '          Left = 4                                         ' +
    '          Height = 25                                      ' +
    '          Top = 1                                          ' +
    '          Width = 75                                       ' +
    '          Caption = ''Clear''                                ' +
    '          TabOrder = 0                                     ' +
    '        end                                                ' +
    '      end                                                  ' +
    '      object Grid: TDBGrid                                 ' +
    '        Left = 0                                           ' +
    '        Height = 79                                       ' +
    '        Top = 28                                           ' +
    '        Width = 703                                        ' +
    '        Align = alTop                                      ' +
    '        Color = clWindow                                   ' +
    '        Columns = <>                                       ' +
    '        TabOrder = 1                                       ' +
    '      end                                                  ' +
    '      object Splitter: TSplitter                           ' +
    '        Cursor = crVSplit                                  ' +
    '        Left = 0                                           ' +
    '        Height = 5                                         ' +
    '        Top = 160                                          ' +
    '        Width = 703                                        ' +
    '        Align = alTop                                      ' +
    '        ResizeAnchor = akTop                               ' +
    '      end                                                  ' +
    '      object mmoText: TDBMemo                              ' +
    '        Left = 0                                           ' +
    '        Height = 150                                       ' +
    '        Top = 165                                          ' +
    '        Width = 703                                        ' +
    '        Align = alClient                                   ' +
    '        Font.CharSet = ANSI_CHARSET                        ' +
    '        Font.Name = ''Courier New''                          ' +
    '        Font.Pitch = fpFixed                               ' +
    '        Font.Quality = fqDraft                             ' +
    '        ParentFont = False                                 ' +
    '        TabOrder = 3                                       ' +
    '      end                                                  ' +
    '    end                                                    ' +
    '  end                                                      ' +
    'end                                                        '
     ;

  procedure ReadFormTextResource(TextResource: string; Form: TCustomForm);
  var
    SS     : TStringStream;
    MS     : TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    SS := TStringStream.Create(TextResource);
    try
      SS.Position := 0;
      ObjectTextToBinary(SS, MS);
      MS.Position := 0;
      MS.ReadComponent(Form);
    finally
      SS.Free;
      MS.Free;
    end;
  end;

   function FindControl(Container: TWinControl; const ControlName: string): TControl;
   var
     i : Integer;
   begin
     Result := nil;
     for i := 0 to Container.ControlCount - 1 do
     begin
       if AnsiSameText(ControlName, Container.Controls[i].Name) then
       begin
         Result := Container.Controls[i];
         Break;
       end else if (Container.Controls[i] is TWinControl) then
       begin
         Result := FindControl(TWinControl(Container.Controls[i]), ControlName);
         if Assigned(Result) then
            Break;
       end;
     end;
   end;

begin
  Listener := aListener;

  Classes.RegisterClass(TPageControl);
  Classes.RegisterClass(TTabSheet);
  Classes.RegisterClass(TPanel);
  Classes.RegisterClass(TMemo);
  Classes.RegisterClass(TButton);
  Classes.RegisterClass(TDBGrid);
  Classes.RegisterClass(TSplitter);
  Classes.RegisterClass(TDBMemo);

  ReadFormTextResource(SDfm, Self);

  Pager           := FindChildControl('Pager') as TPageControl;

  tabLog          := FindControl(Self, 'tabLog') as TTabSheet;
  pnlLog          := FindControl(Self, 'pnlLog') as TPanel;
  btnClearLog     := FindControl(Self, 'btnClearLog') as TButton;
  mmoLog          := FindControl(Self, 'mmoLog') as TMemo;

  tabTable        := FindControl(Self, 'tabTable') as TTabSheet;
  pnlTable        := FindControl(Self, 'pnlTable') as TPanel;
  btnClearTable   := FindControl(Self, 'btnClearTable') as TButton;
  Grid            := FindControl(Self, 'Grid') as TDBGrid;
  Splitter        := FindControl(Self, 'Splitter') as TSplitter;
  mmoText         := FindControl(Self, 'mmoText') as TDBMemo;

  mmoText.DataField  := 'Text';
  mmoText.DataSource := Listener.FDS;
  mmoText.ReadOnly   := True;

  Listener.InitializeGrid(Grid);

  btnClearLog.OnClick := @AnyClick;
  btnClearTable.OnClick := @AnyClick;

  Pager.ActivePage := tabLog;

  Top  := 6;
  Left := Screen.Width - (Width + 12);
end;

function TLogForm.CloseQuery: boolean;
begin
  Result := False;
  if MessageDlg('Close', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    Result := True;
end;

procedure TLogForm.AnyClick(Sender: TObject);
begin
  if btnClearLog = Sender then
    mmoLog.Clear()
  else if btnClearTable = Sender then
    Listener.ClearData()
  ;
end;




{ TFormLogListener.TLogTable }
constructor TFormLogListener.TLogTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TFormLogListener.TLogTable.LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField);
begin
  { nothing }
end;






{ TFormLogListener }
constructor TFormLogListener.Create();
var
  Form: TLogForm;
begin
  inherited Create();
  FSafeList := TSafeObjectList.Create(True);
  FLogTable := TLogTable.Create(nil);

  FLogTable.FieldDefs.Add('Date'    , ftString, 12);
  FLogTable.FieldDefs.Add('Time'    , ftString, 12);
  FLogTable.FieldDefs.Add('User'    , ftString, 32);
  FLogTable.FieldDefs.Add('Host'    , ftString, 32);
  FLogTable.FieldDefs.Add('Level'   , ftString, 12);
  FLogTable.FieldDefs.Add('Source'  , ftString, 128);
  FLogTable.FieldDefs.Add('Scope'   , ftString, 128);
  FLogTable.FieldDefs.Add('EventId' , ftString, 128);
  FLogTable.FieldDefs.Add('Text'    , ftMemo);

  FLogTable.CreateDataset();
  FLogTable.Active := True;

  FDS := TDataSource.Create(nil);
  FDS.DataSet := FLogTable;

  Form := TLogForm.CreateNew(nil, 0);
  FLogForm := Form;

  Form.InitializeForm(Self);

  Form.Show();
end;

destructor TFormLogListener.Destroy;
begin
  FLogForm.Free();
  FDS.Free();
  FLogTable.Free();
  FSafeList.Free();
  inherited Destroy;
end;

procedure TFormLogListener.InitializeGrid(Grid: TDBGrid);
var
  Col : TColumn;
  i   : Integer;
begin
  for i := 0 to FLogTable.FieldCount - 1 do
  begin
    if not AnsiSameText('Text', FLogTable.Fields[i].FieldName) then
    begin
      Col := Grid.Columns.Add;
      Col.FieldName := FLogTable.Fields[i].FieldName;
      Col.ReadOnly := True;
      Col.Width := 100;
    end;
  end;

  Grid.DataSource := FDS;
end;

procedure TFormLogListener.ClearData();
begin
  FLogTable.DisableControls();
  try
    FLogTable.Close;
    FLogTable.Open;
  finally
    FLogTable.EnableControls();
  end;
end;

procedure TFormLogListener.ProcessLog(const Info: ILogInfo);
begin
  FSafeList.Push(TLogRecord.Create(Info));
  TThread.Synchronize(TThread.CurrentThread, Addr(CallLogProc));
end;

procedure TFormLogListener.CallLogProc();
var
  LogUnit: TLogRecord;
begin
  LogUnit := FSafeList.Pop() as TLogRecord;
  if Assigned(LogUnit) then
  begin
    FLogTable.Append();
    FLogTable.FieldByName('Date'   ).AsString := LogUnit.Date     ;
    FLogTable.FieldByName('Time'   ).AsString := LogUnit.Time     ;
    FLogTable.FieldByName('User'   ).AsString := LogUnit.User     ;
    FLogTable.FieldByName('Host'   ).AsString := LogUnit.Host     ;
    FLogTable.FieldByName('Level'  ).AsString := LogUnit.Level    ;
    FLogTable.FieldByName('Source' ).AsString := LogUnit.Source   ;
    FLogTable.FieldByName('Scope'  ).AsString := LogUnit.Scope    ;
    FLogTable.FieldByName('EventId').AsString := LogUnit.EventId  ;
    FLogTable.FieldByName('Text'   ).AsString := LogUnit.Text     ;

    TLogForm(FLogForm).mmoLog.Append(LogUnit.LogText);

    LogUnit.Free();

    FLogTable.Post();
  end;
end;





{ TLogFileListener }
constructor TFileLogListener.Create(Folder: string; MaxSizeInMB: SizeInt);
begin
  inherited Create();

  FLogFile := TLogFile.Create();

  FLengths := TLogPropLengthDictionary.Create() ;

  // prepare the FLengths table
  FLengths.Add('TimeStamp'  , 24);
  FLengths.Add('Host'       , 24);
  FLengths.Add('User'       , 24);
  FLengths.Add('Level'      , 12);
  FLengths.Add('EventId'    , 14);
  FLengths.Add('Source'     , 64);
  FLengths.Add('Scope'      , 32);

  if Length(Self.FFolder) = 0 then
    Self.FFolder := Logger.LogFolder
  else
    Self.FFolder := Folder;

  if Self.FMaxSizeInMB < 1 then
     Self.FMaxSizeInMB := 5
  else
     Self.FMaxSizeInMB := MaxSizeInMB;

  // create the first file
  BeginFile();
end;

destructor TFileLogListener.Destroy;
begin
  FLengths.Free();
  FLogFile.Free();
  inherited Destroy;
end;

procedure TFileLogListener.BeginFile();
var
  FileName : string;
  SB       : TAnsiStringBuilder;
  Line     : string;
begin
  if not DirectoryExists(FFolder) then
    CreateDir(FFolder);

  FileName := 'LOG_' + Logger.DateTimeToFileName(NowUTC()) + '.log';
  FFilePath := ConcatPaths([FFolder, FileName]);

  SB := TAnsiStringBuilder.Create('');
  try
    SB.Append(Logger.RPad('TimeStamp UTC', FLengths['TimeStamp']));
    SB.Append(Logger.RPad('Host', FLengths['Host']));
    SB.Append(Logger.RPad('User', FLengths['User']));
    SB.Append(Logger.RPad('Level', FLengths['Level']));
    SB.Append(Logger.RPad('EventId', FLengths['EventId']));
    SB.Append(Logger.RPad('Source', FLengths['Source']));
    SB.Append(Logger.RPad('Scope', FLengths['Scope']));
    SB.Append('Text');
    SB.AppendLine();

    Line :=  SB.ToString();
  finally
    SB.Free();
  end;

  FLogFile.CreateLogFile(FFilePath);

  WriteLine(Line);

end;

procedure TFileLogListener.WriteLine(Line: string);
begin
  if Length(Line) > 0 then
  begin
    if (FLogFile.Size > (1024 * 1024 * FMaxSizeInMB)) then
       BeginFile();

    Line := Trim(Line);
    FLogFile.AppendLine(Line);
  end;
end;

procedure TFileLogListener.ProcessLog(const Info: ILogInfo);
var
  SB       : TAnsiStringBuilder;
  Line     : string;
begin
  SB := TAnsiStringBuilder.Create('');
  try
    SB.Append(Logger.RPad(Info.TimeStampText, FLengths['TimeStamp']));
    SB.Append(Logger.RPad(Info.Host, FLengths['Host']));
    SB.Append(Logger.RPad(Info.User, FLengths['User']));
    SB.Append(Logger.RPad(Info.LevelText, FLengths['Level']));
    SB.Append(Logger.RPad(Info.EventId, FLengths['EventId']));
    SB.Append(Logger.RPad(Info.Source, FLengths['Source']));
    SB.Append(Logger.RPad(Info.ScopeId, FLengths['Scope']));

    if Length(Info.Text) > 0 then
       SB.Append(Logger.RemoveLineEndings(Info.Text));

    if Length(Info.ExceptionData) > 0 then
        SB.Append(Logger.RemoveLineEndings(Info.ExceptionData));

    SB.AppendLine();

    Line := SB.ToString();

    if Assigned(Info.Properties) then
    begin
      Line += ' - Properties: ';
      Line += Info.GetPropertiesAsSingleLine();
    end;

    WriteLine(Line);
  finally
    SB.Free();
  end;

end;




{ TLogToMainThreadListener }
constructor TLogToMainThreadListener.Create(LogTextProc: TLogTextProc);
begin
  inherited Create;

  FLogTextList := TStringList.Create();

  if not Assigned(LogTextProc) then
    raise Exception.CreateFmt('No callback function is provided to %s', [Self.ClassName]);

  FLogProc := LogTextProc;
end;

destructor TLogToMainThreadListener.Destroy;
begin
  FLogTextList.Free();
  inherited Destroy;
end;

procedure TLogToMainThreadListener.ProcessLog(const Info: ILogInfo);
var
  LogText: string;
begin
  Lock();
  try
    LogText := GetLogInfoText(Info);
    FLogTextList.Add(LogText);

    TThread.Synchronize(TThread.CurrentThread, Addr(CallLogProc));
  finally
    UnLock();
  end;
end;

procedure TLogToMainThreadListener.CallLogProc();
var
  LogText: string;
begin
  Lock();
  try
    if FLogTextList.Count > 0 then
    begin
      LogText := FLogTextList[0];
      FLogTextList.Delete(0);

      FLogProc(LogText);
    end;
  finally
    UnLock();
  end;
end;




{ TLogTextListener }
constructor TLogTextListener.Create(LogTextProc: TLogTextProc);
begin
  inherited Create(LogTextProc);
end;

function TLogTextListener.GetLogInfoText(const Info: ILogInfo): string;
begin
  Result := Logger.GetAsText(Info);
end;





{ TLogLineListener }
constructor TLogLineListener.Create(LogLineProc: TLogTextProc);
begin
  inherited Create(LogLineProc);
end;

destructor TLogLineListener.Destroy;
begin
  inherited Destroy;
end;

function TLogLineListener.GetLogInfoText(const Info: ILogInfo): string;
begin
  Result := Logger.GetAsLine(Info);
end;










type
  { TLogThread }
  TLogThread = class(TThread)
  protected
    procedure Execute(); override;
  public
    constructor Create();
  end;

{ TLogThread }
constructor TLogThread.Create();
begin
  { for a TThread instance to work properly we have to
    1. pass True to constructor in order to create it suspended,
       so we have to call Start() in order for the Execute() to be called
    2. make the TThread instance to free itself }
  inherited Create(True);      // initially suspended, so we have to call Start()
  FreeOnTerminate := True;     // make it free itself
end;

procedure TLogThread.Execute();
var
  Job: TLogJob;
begin
  while not Terminated do
  begin
    Job :=  Logger.FSafeList.Pop() as TLogJob;
    if Assigned(Job) then
    begin
      Job.FListener.ProcessLog(Job.FInfo);
      Job.Free();
    end else begin
      Sleep(850);
    end;
  end;

  Logger.FLogJobThreadTerminated := True;
end;



{ Logger }
class constructor Logger.Create();
begin
  FLock      := SyncObjs.TCriticalSection.Create();
  FListeners := TList.Create();
  Active     := True ;
  FSafeList  := TSafeObjectList.Create(False);

  // prepare the FLengths table
  FLineLengths := TLogPropLengthDictionary.Create();
  FLineLengths.Add('TimeStamp'  , 24);
  FLineLengths.Add('Host'       , 24);
  FLineLengths.Add('User'       , 24);
  FLineLengths.Add('Level'      , 12);
  FLineLengths.Add('EventId'    , 14);
  FLineLengths.Add('Source'     , 64);
  FLineLengths.Add('Scope'      , 32);

  FLogThread := TLogThread.Create();
  FLogThread.Start();
end;

class destructor Logger.Destroy();
begin
  FLogThread.Terminate();
  while not FLogJobThreadTerminated do
    TThread.CurrentThread.Sleep(500);

  FSafeList.Free();
  FListeners.Free();
  FLineLengths.Free();
  FLock.Free;
end;

class function Logger.CreateLogSource(SourceName: string): ILogSource;
begin
  if Trim(SourceName) = '' then
     SourceName := Sys.CreateGuid(True);

  Result := TLogSource.Create(SourceName);
end;

class function Logger.GetActive: Boolean; static;
begin
  Lock();
  try
    Result := FActive > 0;
  finally
    UnLock();
  end;
end;

class procedure Logger.SetActive(Value: Boolean); static;
begin
  Lock();
  try
    if Value then
      Inc(FActive)
    else begin
      Dec(FActive);
      if FActive < 0 then
        FActive := 0;
    end;
  finally
    UnLock();
  end;
end;

class function Logger.GetLogFolder: string; static;
var
  AppFolder : string;
begin

  if (Length(FLogFolder) = 0) or (not DirectoryExists(FLogFolder))  then
  begin
    AppFolder := ParamStr(0);
    AppFolder := ExtractFileDir(AppFolder);

    FLogFolder := ConcatPaths([AppFolder, 'Logs']);
  end;

  if (not DirectoryExists(FLogFolder)) then
      CreateDir(FLogFolder);

  Result := FLogFolder;
end;

class procedure Logger.Lock;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    FLock.Enter;
end;

class procedure Logger.UnLock;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    FLock.Leave();
  end;
end;

class function Logger.GetMinLevel: TLogLevel; static;
begin
  Lock();
  try
    Result := FMinLevel;
  finally
    UnLock();
  end;
end;

class procedure Logger.SetMinLevel(Value: TLogLevel); static;
begin
  Lock();
  try
    FMinLevel := Value;
  finally
    UnLock();
  end;
end;

class procedure Logger.Add(Listener: TLogListener);
begin
  Lock();
  try
    if FListeners.IndexOf(Listener) = -1 then
      FListeners.Add(Listener);
  finally
    UnLock();
  end;
end;

class procedure Logger.Remove(Listener: TLogListener);
begin
  Lock();
  try
    if not FListeners.IndexOf(Listener) <> -1 then
      FListeners.Remove(Listener);
  finally
    UnLock();
  end;
end;

class function Logger.DateTimeToFileName(DT: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh_nn_ss__zzz', DT);
end;

class function Logger.GetLastExceptionStackAsText(): string;

  { remove lines with just an address and remove the address in front of line info }
  function GetLine(Line: string): string;
  begin
    Result := '';

    Line := Trim(Line);

    if (Length(Line) > 9) and (Line[1] = '$') then
    begin
      Delete(Line, 1, 9);
      Line := Trim(Line);
      Result := Line;
    end;
  end;

var
  A      : CodePointer;
  i      : Integer;
  Frames : PPointer;
  Count  : LongInt;
  Line   : string;

begin
  Result := '';

  A := ExceptAddr();
  if Assigned(A) then
  begin
    Result := BackTraceStrFunc(A);
    Result := GetLine(Result);

    Frames := ExceptFrames();
    Count  := ExceptFrameCount();
    for i := 0 to Count - 1 do
    begin
      Line :=  BackTraceStrFunc(Frames[i]);
      Line := GetLine(Line);
      if Length(Line) > 0 then
        Result := Result + LineEnding + Line;
    end;
  end;
end;

class function Logger.LogLevelToString(Level: TLogLevel): string;
begin
  Result := 'None ';

  case Level of
    loTrace   : Result := 'Trace';
    loDebug   : Result := 'Debug';
    loInfo    : Result := 'Info ';
    loWarn    : Result := 'Warn ';
    loError   : Result := 'Error';
    loFatal   : Result := 'Fatal';
  end;
end;

class function Logger.RemoveLineEndings(Line: string): string;
begin
  Line := StringReplace(Line, LineEnding, ' ', [rfReplaceAll]);
  Line := StringReplace(Line, #13, ' ', [rfReplaceAll]);
  Line := StringReplace(Line, #10, ' ', [rfReplaceAll]);
  Result := Line;
end;

class function Logger.RPad(Text: string; MaxLength: Integer): string;
begin
  if Length(Text) = 0 then
    Exit(StringOfChar(' ', MaxLength));

  if Length(Text) > MaxLength then
     Exit(Text.Substring(1, MaxLength));

  Result := Text + StringOfChar(' ', MaxLength - Length(Text));
end;

class function Logger.GetHostName(): string;
{$IFDEF WINDOWS}
var
   Len: DWORD;
{$ENDIF}
begin
  Result := '';

{$IFDEF LINUX}
  Result := GetHostName;
{$ENDIF}

{$IFDEF WINDOWS}
  Len := 255;
  SetLength(Result, Len);
  GetComputerName(PChar(Result), Len);
  SetLength(Result, Len);
{$ENDIF}

{$IFDEF appleOS}

{$ENDIF}
end;

class function Logger.FormatParams(Text: string; const Params: IVariantDictionary): string;
var
  Pair: TKeyValue;
  Value : string;
  Param: string;
begin
  // The Text parameter should be something like the following:
  // 'Customer {CustomerId} order {OrderId} is completed.';
  for Pair in Params do
  begin
    if not (Variants.VarIsNull(Pair.Value) or Variants.VarIsEmpty(Pair.Value)) then
    begin
      Param := '{' + Pair.Key + '}';
      Value := Variants.VarToStr(Pair.Value);
      Text := AnsiReplaceText(Text, Param, Value);
    end;
  end;

  Result := Text;
end;

class function Logger.GetAsText(LogInfo: ILogInfo): string;

  procedure AddLine(SB:  TAnsiStringBuilder; Name, Value: string);
  begin
     if Length(Value) > 0 then
     begin
       Value := Logger.RPad(Name, 12) + ': ' + Value;
       SB.AppendLine(Value);
     end;
  end;

var
  SB       : TAnsiStringBuilder;
begin
  SB := TAnsiStringBuilder.Create('');
  try
    AddLine(SB, 'TimeStamp', LogInfo.TimeStampText);
    AddLine(SB, 'Level', LogInfo.LevelText);
    AddLine(SB, 'Source', LogInfo.Source);
    AddLine(SB, 'Scope', LogInfo.ScopeId);
    AddLine(SB, 'EventId', LogInfo.EventId);
    AddLine(SB, 'Host', LogInfo.Host);
    AddLine(SB, 'User', LogInfo.User);
    AddLine(SB, 'Text', LogInfo.Text);
    if Length(LogInfo.ExceptionData) > 0 then
      AddLine(SB, 'Stack', LineEnding + LogInfo.ExceptionData);

    if Assigned(LogInfo.Properties) then
    begin
      AddLine(SB, 'Properties', ' ');
      SB.AppendLine(LogInfo.GetPropertiesAsTextList());
    end;

    //SB.AppendLine();
    Result := SB.ToString();

  finally
    SB.Free();
  end;

end;

class function Logger.GetAsLine(LogInfo: ILogInfo): string;
var
  SB       : TAnsiStringBuilder;
begin
  SB := TAnsiStringBuilder.Create('');
  try
    SB.Append(Logger.RPad(LogInfo.TimeStampText, FLineLengths['TimeStamp']));
    SB.Append(Logger.RPad(LogInfo.Host, FLineLengths['Host']));
    SB.Append(Logger.RPad(LogInfo.User, FLineLengths['User']));
    SB.Append(Logger.RPad(LogInfo.LevelText, FLineLengths['Level']));
    SB.Append(Logger.RPad(LogInfo.EventId, FLineLengths['EventId']));
    SB.Append(Logger.RPad(LogInfo.Source, FLineLengths['Source']));
    SB.Append(Logger.RPad(LogInfo.ScopeId, FLineLengths['Scope']));

    if Length(LogInfo.Text) > 0 then
       SB.Append(Logger.RemoveLineEndings(LogInfo.Text));

    if Length(LogInfo.ExceptionData) > 0 then
        SB.Append(Logger.RemoveLineEndings(LogInfo.ExceptionData));

    //SB.AppendLine();

    Result := SB.ToString();

    if Assigned(LogInfo.Properties) then
    begin
      Result += ' - Properties: ';
      Result += LogInfo.GetPropertiesAsSingleLine();
    end;
  finally
    SB.Free();
  end;
end;

class procedure Logger.Log(const Info: ILogInfo);
var
  i          : Integer;
  Listener   : TLogListener;
  InfoLevel  : TLogLevel;
  LogJob     : TLogJob;
begin
  Lock();
  try
    InfoLevel := Info.Level;
    if Active and (FMinLevel <> TLogLevel.loNone) and (Ord(InfoLevel) >= Ord(FMinLevel)) then
    begin
      for i := 0 to FListeners.Count - 1 do
      begin
        Listener := TLogListener(FListeners[i]);
        LogJob   := TLogJob.Create(Listener, Info);
        FSafeList.Push(LogJob);
      end;
    end;
  finally
    UnLock();
  end;

end;

class procedure Logger.Log(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary);
var
  LogInfo: ILogInfo;
begin
  LogInfo := TLogInfo.Create(Source, ScopeId, EventId, Level, Exception_, Text, Params);
  Log(LogInfo);
end;

class procedure Logger.Debug(Source, ScopeId, EventId, Text: string);
var
  Level: TLogLevel; Exception_: Exception;
begin
  Level       := loDebug;
  Exception_  := nil;

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Debug(Source, EventId, Text: string);
var
  ScopeId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loDebug;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Debug(Source, Text: string);
var
  ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loDebug;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Debug(Text: string);
var
  Source, ScopeId, EventId: string;
  Level: TLogLevel;
  Exception_: Exception;
begin
  Level       := loDebug;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';
  Source      := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;


class procedure Logger.Info(Source, ScopeId, EventId, Text: string);
var
  Level: TLogLevel; Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Info(Source, EventId, Text: string);
var
  ScopeId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Info(Source, Text: string);
var
  ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Info(Text: string);
var
  Source, ScopeId, EventId: string;
  Level: TLogLevel;
  Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';
  Source      := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Error(Source, ScopeId, EventId: string; Ex: Exception);
var
  Level: TLogLevel;
  Text : string;
begin
  Level       := loError;
  Text        := '';

  Log(Source, ScopeId, EventId, Level, Ex, Text, nil);
end;

class procedure Logger.Error(Source, EventId: string; Ex: Exception);
var
  Level: TLogLevel;
  ScopeId, Text : string;
begin
  Level       := loError;
  Text        := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Ex, Text, nil);
end;

class procedure Logger.Error(Source: string; Ex: Exception);
var
  Level: TLogLevel;
  ScopeId, EventId, Text : string;
begin
  Level       := loError;
  Text        := '';
  ScopeId     := '';
  EventId     := '';

  Log(Source, ScopeId, EventId, Level, Ex, Text, nil);
end;

class procedure Logger.Error(Ex: Exception);
var
  Level: TLogLevel;
  Source, ScopeId, EventId, Text : string;
begin
  Level       := loError;
  Text        := '';
  ScopeId     := '';
  EventId     := '';
  Source      := '';

  Log(Source, ScopeId, EventId, Level, Ex, Text, nil);

end;




end.

