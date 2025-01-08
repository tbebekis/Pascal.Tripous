unit Tripous.Logs;

{$MODE DELPHI}{$H+}
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
  ,SQLDB
  ,sqlite3conn
  ,BufDataset
  //,fgl
  ,Tripous
  ;


{ 1. A TLogListener automatically adds itself to Logger.Listeners when created
     and automatically removes itself from Logger.Listeners when destroyed.
  2. TLogTextListener and TLogLineListener inherit from TLogToMainThreadListener
     which synchronizes updates to MainThread controls
  3. The TFormLogListener shows a Form in the upper right screen corner
     where it displays log information
  4. The TSqlDbLogListener saves log information in a database.
     Using the CreateSQLite() it makes it to use a SQLite database,
     so the property sqlite3.dll should be in the project folder. }
 
type
  TLogLevel =  (
     loNone      = 0
    ,loTrace     = 1
    ,loDebug     = 2
    ,loInfo      = 4
    ,loWarning   = 8
    ,loError     = $10
    ,loFatal     = $20
  );

  //TLogPropLengthDictionary = specialize TFPGMap<string, Word>;

  TLogTextProc = procedure(LogText: string) of object;

  ILogEntry = interface;

  { TLogRecord }
  TLogRecord = class(TPersistent)
  private
    FAsJson: string;
    FAsLine: string;
    FAsList: string;
    FId: string;
    FMessage: string;
    FProperties: string;
    FTimeStamp: TDateTime;
    FDate: string;
    FTime: string;
    FUser: string;
    FHost: string;
    FLevel: string;
    FSource: string;
    FScope: string;
    FEventId: string;
  public
    constructor Create(Entry: ILogEntry);

    property AsList: string read FAsList;
    property AsLine: string read FAsLine;
    property AsJson: string read FAsJson;
  published
    property Id         : string     read FId         write FId       ;
    property TimeStamp  : TDateTime  read FTimeStamp  write FTimeStamp;
    property Date       : string     read FDate       write FDate     ;
    property Time       : string     read FTime       write FTime     ;
    property User       : string     read FUser       write FUser     ;
    property Host       : string     read FHost       write FHost     ;
    property Level      : string     read FLevel      write FLevel    ;
    property Source     : string     read FSource     write FSource   ;
    property Scope      : string     read FScope      write FScope    ;
    property EventId    : string     read FEventId    write FEventId  ;
    property Message    : string     read FMessage    write FMessage  ;
    property Properties : string     read FProperties write FProperties;

  end;

  { ILogEntry }
  ILogEntry = interface
    ['{BA60D540-FECA-42AB-9B3F-7FC50362EE9F}']
    function GetAsJson: string;
    function GetAsLine: string;
    function GetAsList: string;
    function GetDate: string;
    function GetEventId: string;
    function GetException: Exception;
    function GetExceptionData: string;
    function GetHost: string;
    function GetId: string;
    function GetLevel: TLogLevel;
    function GetLevelText: string;
    function GetProperties: IDictionary<string, Variant>;
    function GetScopeId: string;
    function GetSource: string;
    function GetText: string;
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

    property Id: string read GetId;
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
    property Properties: IDictionary<string, Variant> read GetProperties;

    property AsList : string read GetAsList;
    property AsLine : string read GetAsLine;
    property AsJson : string read GetAsJson;
  end;

  { ILogSource }
  ILogSource = interface
    ['{019F8BB3-E92F-4580-A3BD-A142FFA0B28A}']
    function  GetActive: Boolean;
    procedure SetActive(AValue: Boolean);
    function  GetName: string;
    procedure SetName(AValue: string);

    procedure EnterScope(Id: string; const ScopeParams: IDictionary<string, Variant> = nil);
    procedure ExitScope();

    procedure Log(EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant> = nil); overload;
    procedure Log(Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant> = nil); overload;
    procedure Log(EventId: string; Level: TLogLevel; Text: string; const Params: IDictionary<string, Variant> = nil); overload;
    procedure Log(Level: TLogLevel; Text: string; const Params: IDictionary<string, Variant> = nil); overload;

    procedure Debug(EventId, Text: string); overload;
    procedure Debug(Text: string); overload;

    procedure Info(EventId, Text: string); overload;
    procedure Info(Text: string); overload;

    procedure Warning(EventId, Text: string); overload;
    procedure Warning(Text: string); overload;

    procedure Error(EventId: string; Ex: Exception); overload;
    procedure Error(Ex: Exception); overload;

    procedure Error(EventId: string; Text: string); overload;
    procedure Error(Text: string); overload;

    { properties }
    property Name: string read GetName write SetName;
    property Active: Boolean read GetActive write SetActive;
  end;

  { TLogListener }
  { A TLogListener automatically adds itself to Logger.Listeners when created
    and automatically removes itself from Logger.Listeners when destroyed. }
  TLogListener = class
  protected
    FLock                   : SyncObjs.TCriticalSection;
    FLockCount              : Integer;
    FRetainDays             : Integer;
    FRetainSizeKiloBytes    : SizeInt;
    FRetainPolicyCounter    : Integer;

    procedure Lock();
    procedure UnLock();

    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Entry: ILogEntry); virtual; abstract;

    function  GetRetainDays: Integer; virtual;
    procedure SetRetainDays(Value: Integer); virtual;
    function  GetRetainSizeKiloBytes: SizeInt; virtual;
    procedure SetRetainSizeKiloBytes(Value: SizeInt); virtual;
    function  GetRetainPolicyCounter: Integer; virtual;
    procedure SetRetainPolicyCounter(Value: Integer); virtual;
  public
    constructor Create();
    destructor Destroy(); override;

    // Retain policy. How many days to retain in the storage medium. Defaults to 7
    property RetainDays          : Integer read GetRetainDays write SetRetainDays;
    // Retain policy. How many KB to allow a single log file to grow. Defaults to 512 KB
    property RetainSizeKiloBytes : SizeInt read GetRetainSizeKiloBytes write SetRetainSizeKiloBytes;
    // After how many writes to check whether it is time to apply the retain policy. Defaults to 100
    property RetainPolicyCounter: Integer read GetRetainPolicyCounter write SetRetainPolicyCounter;
  end;

  { TFormLogListener }
  TFormLogListener = class(TLogListener)
  private
    FLogTable      : TBufTable;
    FDS            : TDatasource;
    FLogForm       : TForm;
    FLogRecordList : TGenObjectList<TLogRecord>;

    procedure CallLogProc();
    procedure InitializeGrid(Grid: TDBGrid);
    procedure ClearData();
  protected
    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Entry: ILogEntry); override;
  public
    constructor Create();
    destructor Destroy(); override;
  end;

  { TFileLogListener }
  TFileLogListener = class(TLogListener)
  private
    FLogFile       : TWriteLineFile;
    FCounter       : Integer;

    procedure ApplyRetainPolicy();
  protected
    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Entry: ILogEntry); override;

    procedure SetRetainSizeKiloBytes(Value: SizeInt); override;
  public
    constructor Create(FilePath: string = '');
    destructor Destroy(); override;
  end;

  { TSqlDbLogListener }
  TSqlDbLogListener = class(TLogListener)
  private
    Con                   : TSQLConnector;
    Trans                 : TSQLTransaction;
    Q                     : TSQLQuery;
    FLogRecordList        : TGenObjectList<TLogRecord>;
    FCounter              : Integer;


    procedure PrepareSQLite3Connection();
    procedure ApplyRetainPolicy();
    procedure CallLogProc();
    procedure Log(LogRecord: TLogRecord);
  protected
    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Entry: ILogEntry); override;
  public
    constructor Create(ConnectorType, HostName, DatabaseName, UserName, Password: string);
    constructor CreateSQLite(DatabaseName: string = 'LogDB.db3');
    destructor Destroy(); override;

    // Returns the "CREATE TABLE" statement for the log table.
    // The caller has to pass the right text blob type depending on the database. For Firebird is BLOB SUB_TYPE TEXT
    class function GetCreateTableSql(TextBlobType: string): string;
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
    procedure ProcessLog(const Entry: ILogEntry); override;
    function  GetLogText(const Entry: ILogEntry): string; virtual; abstract;
  public
    constructor Create(LogTextProc: TLogTextProc); virtual;
    destructor Destroy(); override;
  end;

  { TLogTextListener }
  TLogTextListener = class(TLogToMainThreadListener)
  protected
    function  GetLogText(const Entry: ILogEntry): string; override;
  public
    constructor Create(LogTextProc: TLogTextProc); override;
  end;

  { TLogLineListener }
  TLogLineListener = class(TLogToMainThreadListener)
  protected
    function  GetLogText(const Entry: ILogEntry): string; override;
  public
    constructor Create(LogLineProc: TLogTextProc); override;
    destructor Destroy(); override;
  end;

  { TLogJob }
  TLogJob = class
  private
    FListener : TLogListener;
    FEntry    : ILogEntry;
  public
    constructor Create(Listener: TLogListener; Entry: ILogEntry);
  end;

 { Logger }
 Logger = class
 class var
    FActive                  : Integer;
    FLogFolder               : string;
    FLock                    : SyncObjs.TCriticalSection;
    FLockCount               : Integer;
    FListeners               : Classes.TList;
    FMinLevel                : TLogLevel;
    FLogJobList              : TGenObjectList<TLogJob>;
    FLogThread               : TThread;
    FLogJobThreadTerminated  : Boolean;
    FLineLengths             : TGenDictionary<string, Word>; // TLogPropLengthDictionary;
    FRetainDays              : Integer;
    FRetainSizeKiloBytes     : SizeInt;
    FRetainPolicyCounter     : Integer;

    class function  GetActive: Boolean; static;
    class function  GetRetainDays: Integer; static;
    class function  GetRetainPolicyCounter: Integer; static;
    class function  GetRetainSizeKiloBytes: SizeInt; static;
    class procedure SetActive(Value: Boolean); static;
    class function  GetMinLevel: TLogLevel; static;
    class procedure SetMinLevel(Value: TLogLevel); static;
    class function  GetLogFolder: string; static;

    class procedure Lock;
    class procedure UnLock;

    class procedure Add(Listener: TLogListener);
    class procedure Remove(Listener: TLogListener);

    class procedure Log(const Entry: ILogEntry); overload;
  public
    class constructor Create();
    class destructor Destroy();

    class function CreateLogSource(SourceName: string): ILogSource;

    { log }
    class procedure Log(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant> = nil); overload;

    class procedure Debug(Source, ScopeId, EventId, Text: string); overload;
    class procedure Debug(Source, EventId, Text: string); overload;
    class procedure Debug(Source, Text: string); overload;
    class procedure Debug(Text: string); overload;

    class procedure Info(Source, ScopeId, EventId, Text: string); overload;
    class procedure Info(Source, EventId, Text: string);  overload;
    class procedure Info(Source, Text: string); overload;
    class procedure Info(Text: string); overload;

    class procedure Warning(Source, ScopeId, EventId, Text: string); overload;
    class procedure Warning(Source, EventId, Text: string); overload;
    class procedure Warning(Source, Text: string); overload;
    class procedure Warning(Text: string); overload;

    class procedure Error(Source, ScopeId, EventId: string; Ex: Exception); overload;
    class procedure Error(Source, EventId: string; Ex: Exception); overload;
    class procedure Error(Source: string; Ex: Exception); overload;
    class procedure Error(Ex: Exception); overload;

    class procedure Error(Source, ScopeId, EventId: string; Text: string); overload;
    class procedure Error(Source, EventId: string; Text: string); overload;
    class procedure Error(EventId: string; Text: string); overload;
    class procedure Error(Text: string); overload;

    { helpers }
    class function  DateTimeToFileName(DT: TDateTime): string;
    class function  GetLastExceptionStackAsText(): string;
    class function  LogLevelToString(Level: TLogLevel): string;
    class function  RemoveLineEndings(Line: string): string;
    class function  RPad(Text: string; MaxLength: Integer): string;
    class function  GetHostName(): string;
    class function  FormatParams(Text: string; const Params: IDictionary<string, Variant>): string;

    class function  GetAsJson(const LogEntry: ILogEntry): string;
    class function  GetAsList(const LogEntry: ILogEntry): string;
    class function  GetAsLine(const LogEntry: ILogEntry): string;
    class function  GetLineCaptions(): string;

    { properties }
    class property Active: Boolean read GetActive write SetActive;
    class property LogFolder: string read GetLogFolder write FLogFolder;
    class property MinLevel: TLogLevel read GetMinLevel write SetMinLevel;

    // Retain policy. How many days to retain in the storage medium. Defaults to 7
    class property RetainDays          : Integer read GetRetainDays write FRetainDays;
    // Retain policy. How many KB to allow a single log file to grow. Defaults to 512 KB
    class property RetainSizeKiloBytes : SizeInt read GetRetainSizeKiloBytes write FRetainSizeKiloBytes;
    // After how many writes to check whether it is time to apply the retain policy. Defaults to 100
    class property RetainPolicyCounter: Integer read GetRetainPolicyCounter write FRetainPolicyCounter;
  end;

  { LogBox }

  LogBox = class
  class var
    FLock                   : SyncObjs.TCriticalSection;
    FLockCount              : Integer;

    mmoLog: TCustomMemo;
    LogLineListener: TLogLineListener;
    FLogTextList  : TStringList;

    class procedure Lock();
    class procedure UnLock();

    class procedure DoClear();
    class procedure DoLog();
  public
    class constructor Create();
    class destructor Destroy();

    class procedure Initialize(Memo: TCustomMemo; UseLogListenerToo: Boolean = True);
    class procedure Clear();
    class procedure Append(Text: string);
    class procedure AppendLine(Text: string); overload;
    class procedure AppendLine(Ex: Exception); overload;
    class procedure AppendLine(); overload;
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
constructor TLogRecord.Create(Entry: ILogEntry);
begin
  inherited Create();

  Id        := Entry.Id         ;
  TimeStamp := Entry.TimeStamp  ;
  Date      := Entry.Date       ;
  Time      := Entry.Time       ;
  User      := Entry.User       ;
  Host      := Entry.Host       ;
  Level     := Entry.LevelText  ;
  Source    := Entry.Source     ;
  Scope     := Entry.ScopeId    ;
  EventId   := Entry.EventId    ;
  Message   := Entry.Text       ;

 if Assigned(Entry.Properties) and (Entry.Properties.Count > 0) then
   Properties := Entry.GetPropertiesAsSingleLine();

 FAsList := Entry.AsList;
 FAsLine := Entry.AsLine;
 FAsJson := Entry.AsJson;
end;




type
  { TLogEntry }
  TLogEntry = class(TInterfacedObject, ILogEntry)
  private
    FAsJson: string;
    FAsLine: string;
    FAsList: string;

    FId: string;
    FEventId: string;
    FException: Exception;
    FExceptionData: string;
    FHost: string;
    FLevel: TLogLevel;
    FProperties: IDictionary<string, Variant>;
    FScopeId: string;
    FSource: string;
    FText: string;
    FTimeStamp: TDateTime;
    FUser: string;

    function GetAsJson: string;
    function GetAsLine: string;
    function GetAsList: string;
    function GetDate: string;
    function GetEventId: string;
    function GetException: Exception;
    function GetExceptionData: string;
    function GetHost: string;
    function GetId: string;
    function GetLevel: TLogLevel;
    function GetLevelText: string;
    function GetProperties: IDictionary<string, Variant>;
    function GetScopeId: string;
    function GetSource: string;
    function GetText: string;
    function GetTime: string;
    function GetTimeStamp: TDateTime;
    function GetTimeStampText: string;
    function GetUser: string;
    procedure SetUser(Value: string);
  public
    constructor Create(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant> = nil);
    destructor Destroy(); override;

    function  ToString: ansistring; override;
    procedure SaveToFile(Folder: string = '');

    function GetPropertiesAsSingleLine(): string;
    function GetPropertiesAsTextList(): string;
    function CreateLogRecord(): TLogRecord;

    property Id: string read GetId;
    property TimeStamp: TDateTime read  GetTimeStamp;
    property TimeStampText: string read GetTimeStampText;
    property Date: string read GetDate;
    property Time: string read GetTime;
    property User: string read GetUser write SetUser;
    property Host: string read GetHost;

    property Level: TLogLevel read GetLevel;
    property LevelText: string read GetLevelText;
    property Source: string read GetSource;
    property ScopeId: string read GetScopeId;
    property EventId: string read GetEventId;
    property Text: string read GetText;
    property Exception_ : Exception read GetException;
    property ExceptionData: string read GetExceptionData;
    property Properties: IDictionary<string, Variant> read GetProperties;

    property AsList : string read GetAsList;
    property AsLine : string read GetAsLine;
    property AsJson : string read GetAsJson;
  end;







{ TLogJob }
constructor TLogJob.Create(Listener: TLogListener; Entry: ILogEntry);
begin
  inherited Create();
  FListener := Listener;
  FEntry     := Entry;
end;









type
  TLogSource = class;

  { TLogScope }
  TLogScope = class
  private
    FId             : string;
    FLogSource      : TLogSource;
    FProperties     : IDictionary<string, Variant>;

    function GetId: string;
    function GetProperties: IDictionary<string, Variant>;
  public
    constructor Create(Source: TLogSource; const aId: string = ''; const ScopeParams: IDictionary<string, Variant> = nil);

    property Id : string read GetId;
    property Properties: IDictionary<string, Variant> read GetProperties;
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

    procedure  EnterScope(Id: string; const ScopeParams: IDictionary<string, Variant> = nil);
    procedure  ExitScope();

    procedure Log(EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant> = nil); overload;
    procedure Log(Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant> = nil); overload;
    procedure Log(EventId: string; Level: TLogLevel; Text: string; const Params: IDictionary<string, Variant> = nil); overload;
    procedure Log(Level: TLogLevel; Text: string; const Params: IDictionary<string, Variant> = nil); overload;

    procedure Debug(EventId, Text: string); overload;
    procedure Debug(Text: string); overload;

    procedure Info(EventId, Text: string); overload;
    procedure Info(Text: string); overload;

    procedure Warning(EventId, Text: string); overload;
    procedure Warning(Text: string); overload;

    procedure Error(EventId: string; Ex: Exception); overload;
    procedure Error(Ex: Exception); overload;

    procedure Error(EventId, Text: string); overload;
    procedure Error(Text: string); overload;

    { properties }
    property Name: string read GetName write SetName;
    property Active: Boolean read GetActive write SetActive;

  end;


{ TLogScope }
constructor TLogScope.Create(Source: TLogSource; const aId: string; const ScopeParams: IDictionary<string, Variant>);
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

function TLogScope.GetProperties: IDictionary<string, Variant>;
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

procedure TLogSource.EnterScope(Id: string; const ScopeParams: IDictionary<string, Variant>);
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

procedure TLogSource.Log(EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant>);
var
  LogEntry: TLogEntry;
  Entry: TGenKeyValue<string, Variant>;
  Scope: TLogScope;
begin

  Lock();
  try
     if Active then
     begin
       Scope := CurrentScope;
       LogEntry := TLogEntry.Create(Self.Name, Scope.Id, EventId, Level, Exception_, Text, Params);
       if Assigned(Scope.Properties) then
       begin
         for Entry in Scope.Properties do
           LogEntry.Properties[Entry.Key] := Entry.Value;
       end;
       Logger.Log(LogEntry);
     end;
  finally
    UnLock();
  end;
end;

procedure TLogSource.Log(Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant>);
begin
  Log('0', Level, Exception_, Text, Params);
end;

procedure TLogSource.Log(EventId: string; Level: TLogLevel; Text: string; const Params: IDictionary<string, Variant>);
begin
  Log(EventId, Level, nil, Text, Params);
end;

procedure TLogSource.Log(Level: TLogLevel; Text: string; const Params: IDictionary<string, Variant>);
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

procedure TLogSource.Warning(EventId, Text: string);
begin
  Log(EventId, TLogLevel.loWarning, Text);
end;

procedure TLogSource.Warning(Text: string);
begin
  Warning('0', Text);
end;

procedure TLogSource.Error(EventId: string; Ex: Exception);
begin
  Log(EventId, TLogLevel.loError, Ex, Ex.Message);
end;

procedure TLogSource.Error(Ex: Exception);
begin
  Error('0', Ex);
end;

procedure TLogSource.Error(EventId, Text: string);
begin
  Log(EventId, loError, Text);
end;

procedure TLogSource.Error(Text: string);
var
  EventId: string;
begin
  EventId := '0';
  Log(EventId, loError, Text);
end;



{ TLogInfo }
constructor TLogEntry.Create(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant>);
var
  Guid: TGuid;
begin
   inherited Create;

  if SysUtils.CreateGUID(Guid) <> 0 then
     raise Exception.Create('Failed to create GUID');

   FId   := GUIDToString(Guid);

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
     FProperties := TGenDictionary<string, Variant>.Create(); // TVariantDictionary.Create();



  FAsList := Logger.GetAsList(Self);
  FAsLine := Logger.GetAsLine(Self);
  FAsJson := Logger.GetAsJson(Self);
end;

destructor TLogEntry.Destroy;
begin
  inherited Destroy;
end;

function TLogEntry.ToString: ansistring;
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

procedure TLogEntry.SaveToFile(Folder: string);
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

function TLogEntry.GetPropertiesAsSingleLine(): string;
var
  Entry: TGenKeyValue<string, Variant>;
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

function TLogEntry.GetPropertiesAsTextList(): string;
var
  Entry: TGenKeyValue<string, Variant>;
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

function TLogEntry.CreateLogRecord(): TLogRecord;
begin
  Result := TLogRecord.Create(Self as ILogEntry);
end;

function TLogEntry.GetTimeStamp: TDateTime;
begin
  Result := FTimeStamp;
end;

function TLogEntry.GetTimeStampText: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', TimeStamp);
end;

function TLogEntry.GetDate: string;
begin
  Result := FormatDateTime('yyyy-mm-dd', TimeStamp);
end;

function TLogEntry.GetAsJson: string;
begin
  Result := FAsJson;
end;

function TLogEntry.GetAsLine: string;
begin
  Result := FAsLine;
end;

function TLogEntry.GetAsList: string;
begin
  Result := FAsList;
end;

function TLogEntry.GetTime: string;
begin
  Result := FormatDateTime('hh:nn:ss.zzz', TimeStamp);
end;

function TLogEntry.GetUser: string;
begin
  Result := FUser;
end;

procedure TLogEntry.SetUser(Value: string);
begin
  FUser := Value;
end;

function TLogEntry.GetHost: string;
begin
  Result := FHost;
end;

function TLogEntry.GetId: string;
begin
  Result := FId;
end;

function TLogEntry.GetLevel: TLogLevel;
begin
  Result := FLevel;
end;

function TLogEntry.GetLevelText: string;
begin
  Result := Logger.LogLevelToString(Level);
end;

function TLogEntry.GetEventId: string;
begin
   Result := FEventId;
end;

function TLogEntry.GetText: string;
begin
  Result := FText;
end;

function TLogEntry.GetScopeId: string;
begin
  Result := FScopeId;
end;

function TLogEntry.GetSource: string;
begin
  Result := FSource;
end;

function TLogEntry.GetException: Exception;
begin
  Result := FException;
end;

function TLogEntry.GetExceptionData: string;
begin
  Result := FExceptionData;
end;

function TLogEntry.GetProperties: IDictionary<string, Variant>;
begin
  Result := FProperties;
end;







{ TLogListener }
constructor TLogListener.Create();
begin
  inherited Create();
  FLock        := SyncObjs.TCriticalSection.Create();

  FRetainDays           := Logger.RetainDays;
  FRetainSizeKiloBytes  := Logger.RetainSizeKiloBytes;

  Logger.Add(Self);
end;

destructor TLogListener.Destroy;
begin
  Logger.Remove(Self);
  FLock.Free();
  inherited Destroy;
end;

function TLogListener.GetRetainDays: Integer;
begin
  if FRetainDays >= Logger.RetainDays then
    Result := FRetainDays
  else
    Result := Logger.RetainDays;
end;

procedure TLogListener.SetRetainDays(Value: Integer);
begin
  FRetainDays := Value;
end;

function TLogListener.GetRetainSizeKiloBytes: SizeInt;
begin
  if FRetainSizeKiloBytes >= Logger.RetainSizeKiloBytes then
    Result := FRetainSizeKiloBytes
  else
    Result := Logger.RetainSizeKiloBytes;
end;

procedure TLogListener.SetRetainSizeKiloBytes(Value: SizeInt);
begin
  FRetainSizeKiloBytes := Value;
end;

function TLogListener.GetRetainPolicyCounter: Integer;
begin
  if FRetainPolicyCounter >= Logger.RetainPolicyCounter then
    Result := FRetainPolicyCounter
  else
    Result := Logger.RetainPolicyCounter;
end;

procedure TLogListener.SetRetainPolicyCounter(Value: Integer);
begin
  FRetainPolicyCounter := Value;
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
    '  Height = 360                                             ' +
    '  Top = 200                                                ' +
    '  Width = 711                                              ' +
    '  Caption = ''LogDialog''                                  ' +
    '  ClientHeight = 343                                       ' +
    '  ClientWidth = 711                                        ' +
    '  FormStyle = fsStayOnTop                                  ' +
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
    '        Height = 140                                       ' +
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

  btnClearLog.OnClick := AnyClick;
  btnClearTable.OnClick := AnyClick;

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










{ TFormLogListener }
constructor TFormLogListener.Create();
var
  Form: TLogForm;
begin
  inherited Create();
  FLogRecordList := TGenObjectList<TLogRecord>.Create(True);    // TSafeObjectList.Create(True);
  FLogTable := TBufTable.Create(nil);

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
  FLogRecordList.Free();
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

procedure TFormLogListener.ProcessLog(const Entry: ILogEntry);
begin
  FLogRecordList.Push(TLogRecord.Create(Entry));
  TThread.Synchronize(TThread.CurrentThread, CallLogProc);   // Addr(CallLogProc)
end;

procedure TFormLogListener.CallLogProc();
var
  LogRecord: TLogRecord;
begin
  LogRecord := FLogRecordList.Pop() as TLogRecord;
  if Assigned(LogRecord) then
  begin
    FLogTable.Append();
    FLogTable.FieldByName('Date'   ).AsString := LogRecord.Date     ;
    FLogTable.FieldByName('Time'   ).AsString := LogRecord.Time     ;
    FLogTable.FieldByName('User'   ).AsString := LogRecord.User     ;
    FLogTable.FieldByName('Host'   ).AsString := LogRecord.Host     ;
    FLogTable.FieldByName('Level'  ).AsString := LogRecord.Level    ;
    FLogTable.FieldByName('Source' ).AsString := LogRecord.Source   ;
    FLogTable.FieldByName('Scope'  ).AsString := LogRecord.Scope    ;
    FLogTable.FieldByName('EventId').AsString := LogRecord.EventId  ;
    FLogTable.FieldByName('Text'   ).AsString := LogRecord.AsJson   ;

    TLogForm(FLogForm).mmoLog.Append(LogRecord.AsList);
    LogRecord.Free();

    FLogTable.Post();
  end;
end;





{ TLogFileListener }
constructor TFileLogListener.Create(FilePath: string);
var
  ColumnLine : string;
begin
  inherited Create();

  ColumnLine := Logger.GetLineCaptions();
  FLogFile   := TWriteLineFile.Create(FilePath, ColumnLine);
end;

destructor TFileLogListener.Destroy;
begin
  FLogFile.Free();
  inherited Destroy;
end;

procedure TFileLogListener.ProcessLog(const Entry: ILogEntry);
var
  Line     : string;
begin
  Line := Logger.GetAsLine(Entry);
  FLogFile.WriteLine(Line);

  Inc(FCounter);
  ApplyRetainPolicy();
end;

procedure TFileLogListener.SetRetainSizeKiloBytes(Value: SizeInt);
begin
  FRetainSizeKiloBytes := Value;
  FLogFile.RetainSizeKiloBytes := GetRetainSizeKiloBytes();
end;

procedure TFileLogListener.ApplyRetainPolicy();
begin
  if FCounter > RetainPolicyCounter then
  begin
    FCounter := 0;
    FLogFile.DeleteFilesOlderThan(RetainDays);
  end;
end;


{ TSqlDbLogListener }
constructor TSqlDbLogListener.Create(ConnectorType, HostName, DatabaseName, UserName, Password: string);
begin
  inherited Create();

  FLogRecordList := TGenObjectList<TLogRecord>.Create(True);    //     TSafeObjectList.Create(True);

  Con := TSQLConnector.Create(nil);

  Con.ConnectorType := ConnectorType;
  Con.HostName      := HostName;
  Con.DatabaseName  := DatabaseName;
  Con.UserName      := UserName;
  Con.Password      := Password;

  Trans := TSQLTransaction.Create(nil);
  Q     := TSQLQuery.Create(nil);

  Con.Transaction := Trans;
  Q.DataBase := Con;

  PrepareSQLite3Connection();
end;

constructor TSqlDbLogListener.CreateSQLite(DatabaseName: string);
begin
  Create('SQLite3', 'localhost', DatabaseName, '', '');
end;

destructor TSqlDbLogListener.Destroy;
begin
  Q.Free();
  Trans.Free();
  Con.Free();
  FLogRecordList.Free();
  inherited Destroy();
end;

procedure TSqlDbLogListener.Log(LogRecord: TLogRecord);
var
  InsertSql : string;
begin
  Inc(FCounter);

  InsertSql :=
  'insert into AppLog (    ' +
  '   Id                    ' +
  '  ,Stamp                 ' +
  '  ,Date                  ' +
  '  ,Time                  ' +
  '  ,UserName              ' +
  '  ,HostName              ' +
  '  ,Level                 ' +
  '  ,Source                ' +
  '  ,Scope                 ' +
  '  ,EventId               ' +
  '  ,LogText               ' +
  ') values (               ' +
  '    :Id                  ' +
  '  , :Stamp               ' +
  '  , :Date                ' +
  '  , :Time                ' +
  '  , :UserName            ' +
  '  , :HostName            ' +
  '  , :Level               ' +
  '  , :Source              ' +
  '  , :Scope               ' +
  '  , :EventId             ' +
  '  , :LogText             ' +
  ')                        '
  ;

  Con.Open();
  try
    Q.SQL.Text := InsertSql;

    Q.ParamByName('Id'         ).AsString   := LogRecord.Id          ;
    Q.ParamByName('Stamp'      ).AsDateTime := LogRecord.TimeStamp   ;
    Q.ParamByName('Date'       ).AsString   := LogRecord.Date        ;
    Q.ParamByName('Time'       ).AsString   := LogRecord.Time        ;
    Q.ParamByName('UserName'   ).AsString   := LogRecord.User        ;
    Q.ParamByName('HostName'   ).AsString   := LogRecord.Host        ;
    Q.ParamByName('Level'      ).AsString   := LogRecord.Level       ;
    Q.ParamByName('Source'     ).AsString   := LogRecord.Source      ;
    Q.ParamByName('Scope'      ).AsString   := LogRecord.Scope       ;
    Q.ParamByName('EventId'    ).AsString   := LogRecord.EventId     ;
    Q.ParamByName('LogText'    ).AsString   := LogRecord.AsJson      ;

    if not Trans.Active then;
       Trans.StartTransaction();
    Q.ExecSQL();
    Trans.Commit();

    Q.Close();
  finally
    Con.Close();
  end;

  ApplyRetainPolicy();

end;

class function TSqlDbLogListener.GetCreateTableSql(TextBlobType: string): string;
begin
  Result :=
  'create table AppLog (                   ' +
  '   Id         nvarchar(40) not null     ' +
  '  ,Stamp      DateTime not null         ' +
  '  ,Date       nvarchar(12) not null     ' +
  '  ,Time       nvarchar(12) not null     ' +
  '  ,UserName   nvarchar(32)              ' +
  '  ,HostName   nvarchar(32)              ' +
  '  ,Level      nvarchar(32)              ' +
  '  ,Source     nvarchar(128)             ' +
  '  ,Scope      nvarchar(128)             ' +
  '  ,EventId    nvarchar(128)             ' +
  '  ,LogText    %s not null               ' +
  ')                                       '
  ;

  Result := Format(Result, [TextBlobType]);
end;

procedure TSqlDbLogListener.PrepareSQLite3Connection();
var
  SQLite3Con: TSQLite3Connection;
  SQLiteTrans : TSQLTransaction;
  SqlText : string;
begin
  if (Con.ConnectorType = 'SQLite3') and (not FileExists(Con.DatabaseName)) then
  begin
    SQLite3Con := TSQLite3Connection.Create(nil);
    SQLiteTrans := TSQLTransaction.Create(nil);
    try
      SQLite3Con.HostName     := Con.HostName;
      SQLite3Con.DatabaseName := Con.DatabaseName;
      SQLite3Con.UserName     := Con.UserName;
      SQLite3Con.Password     := Con.Password;

      SQLite3Con.Transaction := SQLiteTrans;
      SQLite3Con.CreateDB();

      SqlText := GetCreateTableSql('text');
      SQLite3Con.ExecuteDirect(SqlText);

      SQLite3Con.ExecuteDirect('CREATE UNIQUE INDEX AppLogIndex ON AppLog(Id)');
      SQLiteTrans.Commit();
    finally
      SQLiteTrans.Free();
      SQLite3Con.Free();
    end;
  end;
end;

procedure TSqlDbLogListener.ApplyRetainPolicy();
var
  DT   : TDateTime;
  sDate: string;
begin
  if FCounter > RetainPolicyCounter then
  begin
    FCounter := 0;

    Con.Open();
    try
      DT := IncDay(NowUTC(), -RetainDays);
      sDate := FormatDateTime('yyyy-mm-dd', DT);

      Q.SQL.Text := 'delete from AppLog where Date < :Date ';
      Q.ParamByName('Date').AsString   := sDate;

      if not Trans.Active then;
         Trans.StartTransaction();
      Q.ExecSQL();
      Trans.Commit();

      Q.Close();
    finally
      Con.Close();
    end;

  end;

end;

procedure TSqlDbLogListener.ProcessLog(const Entry: ILogEntry);
begin
  FLogRecordList.Push(TLogRecord.Create(Entry));
  TThread.Synchronize(TThread.CurrentThread, CallLogProc);   // Addr(CallLogProc)
end;

procedure TSqlDbLogListener.CallLogProc();
var
  LogRecord: TLogRecord;
begin
  LogRecord := FLogRecordList.Pop() as TLogRecord;
  if Assigned(LogRecord) then
  begin
    Log(LogRecord);
    LogRecord.Free();
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

procedure TLogToMainThreadListener.ProcessLog(const Entry: ILogEntry);
var
  LogText: string;
begin
  Lock();
  try
    LogText := GetLogText(Entry);
    FLogTextList.Add(LogText);

    TThread.Synchronize(TThread.CurrentThread, CallLogProc);  // Addr(CallLogProc)
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

function TLogTextListener.GetLogText(const Entry: ILogEntry): string;
begin
  Result := Logger.GetAsList(Entry);
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

function TLogLineListener.GetLogText(const Entry: ILogEntry): string;
begin
  Result := Logger.GetAsLine(Entry);
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
    if Logger.FLogJobList.Count > 0 then
    begin
      Job :=  Logger.FLogJobList.Pop() as TLogJob;
      Job.FListener.ProcessLog(Job.FEntry);
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
  FLogJobList  := TGenObjectList<TLogJob>.Create(False); // TSafeObjectList.Create(False);

  // prepare the FLengths table
  FLineLengths := TGenDictionary<string, Word>.Create(); // TLogPropLengthDictionary.Create();
  FLineLengths.Add('Id'         , 40);
  FLineLengths.Add('TimeStamp'  , 24);
  FLineLengths.Add('Host'       , 24);
  FLineLengths.Add('User'       , 24);
  FLineLengths.Add('Level'      , 12);
  FLineLengths.Add('EventId'    , 14);
  FLineLengths.Add('Source'     , 64);
  FLineLengths.Add('Scope'      , 64);

  FLogThread := TLogThread.Create();
  FLogThread.Start();
end;

class destructor Logger.Destroy();
begin
  FLogThread.Terminate();
  while not FLogJobThreadTerminated do
    TThread.CurrentThread.Sleep(500);

  FLogJobList.Free();
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

class function Logger.GetRetainDays: Integer; static;
begin
  if FRetainDays >= 1 then
    Result := FRetainDays
  else
    Result := 7;
end;

class function Logger.GetRetainSizeKiloBytes: SizeInt; static;
begin
  if FRetainSizeKiloBytes >= 512 then
    Result := FRetainSizeKiloBytes
  else
    Result := 512;
end;

class function Logger.GetRetainPolicyCounter: Integer; static;
begin
  if FRetainPolicyCounter >= 100 then
    Result := FRetainPolicyCounter
  else
    Result := 100;
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
    if Ord(FMinLevel) >= Ord(TLogLevel.loTrace) then
      Result := FMinLevel
    else
      Result := TLogLevel.loTrace;
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
    loWarning    : Result := 'Warn ';
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

class function Logger.FormatParams(Text: string; const Params: IDictionary<string, Variant>): string;
var
  Pair: TGenKeyValue<string, Variant>;
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

class function Logger.GetAsJson(const LogEntry: ILogEntry): string;
var
  LogRecord: TLogRecord;
begin
  LogRecord := TLogRecord.Create(LogEntry);
  try
    Result := Json.Serialize(LogRecord);
  finally
    LogRecord.Free();
  end;
end;

class function Logger.GetAsList(const LogEntry: ILogEntry): string;

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
    AddLine(SB, 'Id', LogEntry.Id);
    AddLine(SB, 'TimeStamp', LogEntry.TimeStampText);
    AddLine(SB, 'Level', LogEntry.LevelText);
    AddLine(SB, 'Source', LogEntry.Source);
    AddLine(SB, 'Scope', LogEntry.ScopeId);
    AddLine(SB, 'EventId', LogEntry.EventId);
    AddLine(SB, 'Host', LogEntry.Host);
    AddLine(SB, 'User', LogEntry.User);
    AddLine(SB, 'Text', LogEntry.Text);
    if Length(LogEntry.ExceptionData) > 0 then
      AddLine(SB, 'Stack', LineEnding + LogEntry.ExceptionData);

    if Assigned(LogEntry.Properties) and (LogEntry.Properties.Count > 0) then
    begin
      AddLine(SB, 'Properties', ' ');
      SB.AppendLine(LogEntry.GetPropertiesAsTextList());
    end;

    //SB.AppendLine();
    Result := SB.ToString();

  finally
    SB.Free();
  end;

end;

class function Logger.GetAsLine(const LogEntry: ILogEntry): string;
var
  SB       : TAnsiStringBuilder;
begin
  SB := TAnsiStringBuilder.Create('');
  try
    SB.Append(Logger.RPad(LogEntry.Id, FLineLengths['Id']));
    SB.Append(Logger.RPad(LogEntry.TimeStampText, FLineLengths['TimeStamp']));
    SB.Append(Logger.RPad(LogEntry.Host, FLineLengths['Host']));
    SB.Append(Logger.RPad(LogEntry.User, FLineLengths['User']));
    SB.Append(Logger.RPad(LogEntry.LevelText, FLineLengths['Level']));
    SB.Append(Logger.RPad(LogEntry.EventId, FLineLengths['EventId']));
    SB.Append(Logger.RPad(LogEntry.Source, FLineLengths['Source']));
    SB.Append(Logger.RPad(LogEntry.ScopeId, FLineLengths['Scope']));

    if Length(LogEntry.Text) > 0 then
       SB.Append(Logger.RemoveLineEndings(LogEntry.Text));

    if Length(LogEntry.ExceptionData) > 0 then
        SB.Append(Logger.RemoveLineEndings(LogEntry.ExceptionData));

    //SB.AppendLine();

    Result := SB.ToString();

    if Assigned(LogEntry.Properties) and (LogEntry.Properties.Count > 0) then
    begin
      Result += ' - Properties: ';
      Result += LogEntry.GetPropertiesAsSingleLine();
    end;
  finally
    SB.Free();
  end;
end;

class function Logger.GetLineCaptions(): string;
var
  SB       : TAnsiStringBuilder;
begin
  SB := TAnsiStringBuilder.Create('');
  try
    SB.Append(Logger.RPad('Id', FLineLengths['Id']));
    SB.Append(Logger.RPad('TimeStamp UTC', FLineLengths['TimeStamp']));
    SB.Append(Logger.RPad('Host', FLineLengths['Host']));
    SB.Append(Logger.RPad('User', FLineLengths['User']));
    SB.Append(Logger.RPad('Level', FLineLengths['Level']));
    SB.Append(Logger.RPad('EventId', FLineLengths['EventId']));
    SB.Append(Logger.RPad('Source', FLineLengths['Source']));
    SB.Append(Logger.RPad('Scope', FLineLengths['Scope']));
    SB.Append('Text');
    SB.AppendLine();

    Result :=  SB.ToString();
  finally
    SB.Free();
  end;
end;

class procedure Logger.Log(const Entry: ILogEntry);
var
  i          : Integer;
  Listener   : TLogListener;
  InfoLevel  : TLogLevel;
  LogJob     : TLogJob;
begin
  Lock();
  try
    InfoLevel := Entry.Level;
    if Active and (MinLevel <> TLogLevel.loNone) and (Ord(InfoLevel) >= Ord(MinLevel)) then
    begin
      for i := 0 to FListeners.Count - 1 do
      begin
        Listener := TLogListener(FListeners[i]);
        LogJob   := TLogJob.Create(Listener, Entry);
        FLogJobList.Push(LogJob);
      end;
    end;
  finally
    UnLock();
  end;
end;

class procedure Logger.Log(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IDictionary<string, Variant>);
var
  LogEntry: ILogEntry;
begin
  Lock();
  try
    if Active and (MinLevel <> TLogLevel.loNone) and (Ord(Level) >= Ord(MinLevel)) then
    begin
      LogEntry := TLogEntry.Create(Source, ScopeId, EventId, Level, Exception_, Text, Params);
      Log(LogEntry);
    end;
  finally
    UnLock();
  end;
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
  EventId     := '0';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Debug(Source, Text: string);
var
  ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loDebug;
  Exception_  := nil;
  EventId     := '0';
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
  EventId     := '0';
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
  EventId     := '0';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Info(Source, Text: string);
var
  ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;
  EventId     := '0';
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
  EventId     := '0';
  ScopeId     := '';
  Source      := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Warning(Source, ScopeId, EventId, Text: string);
var
  Level: TLogLevel; Exception_: Exception;
begin
  Level       := loWarning;
  Exception_  := nil;

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Warning(Source, EventId, Text: string);
var
  ScopeId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loWarning;
  Exception_  := nil;
  EventId     := '0';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Warning(Source, Text: string);
var
  ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loWarning;
  Exception_  := nil;
  EventId     := '0';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, nil);
end;

class procedure Logger.Warning(Text: string);
var
  Source, ScopeId, EventId: string;
  Level: TLogLevel;
  Exception_: Exception;
begin
  Level       := loWarning;
  Exception_  := nil;
  EventId     := '0';
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
  EventId     := '0';

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
  EventId     := '0';
  Source      := '';

  Log(Source, ScopeId, EventId, Level, Ex, Text, nil);
end;

class procedure Logger.Error(Source, ScopeId, EventId: string; Text: string);
var
  Ex: Exception;
begin
  Ex := nil;
  Log(Source, ScopeId, EventId, loError, Ex, Text, nil);
end;

class procedure Logger.Error(Source, EventId: string; Text: string);
var
  Ex: Exception;
  ScopeId : string;
begin
  Ex := nil;
  ScopeId := '';
  Log(Source, ScopeId, EventId, loError, Ex, Text, nil);
end;

class procedure Logger.Error(EventId: string; Text: string);
var
  Ex: Exception;
  Source : string;
  ScopeId : string;
begin
  Ex := nil;
  Source := '';
  ScopeId := '';
  Log(Source, ScopeId, EventId, loError, Ex, Text, nil);
end;

class procedure Logger.Error(Text: string);
var
  Ex: Exception;
  Source : string;
  ScopeId : string;
  EventId : string;
begin
  Ex := nil;
  Source := '';
  ScopeId := '';
  EventId := '0';
  Log(Source, ScopeId, EventId, loError, Ex, Text, nil);
end;


type

  { TLogBoxLineListener }

  TLogBoxLineListener = class(TLogLineListener)
  private
     procedure LogProc(LogText: string);
  public
    constructor Create(LogLineProc: TLogTextProc); override;
  end;

{ TLogBoxLineListener }
constructor TLogBoxLineListener.Create(LogLineProc: TLogTextProc);
begin
  inherited Create(LogProc);
end;

procedure TLogBoxLineListener.LogProc(LogText: string);
begin
  if Assigned(LogBox.mmoLog) then
     LogBox.mmoLog.Append(Trim(LogText) + sLineBreak);
end;



{ LogBox }
class constructor LogBox.Create();
begin
  FLock           := SyncObjs.TCriticalSection.Create();
  FLogTextList    := TStringList.Create();
end;

class destructor LogBox.Destroy();
begin
  FLogTextList.Free();
  FLock.Free();
end;

class procedure LogBox.Lock;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    FLock.Enter;
end;

class procedure LogBox.UnLock;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    FLock.Leave();
  end;
end;

class procedure LogBox.Initialize(Memo: TCustomMemo; UseLogListenerToo: Boolean);
begin
  if not Assigned(mmoLog) then
     mmoLog := Memo;

  if UseLogListenerToo and not Assigned(LogLineListener) then
    LogLineListener := TLogBoxLineListener.Create(nil);
end;

class procedure LogBox.DoClear();
begin
  if Assigned(LogBox.mmoLog) then
     LogBox.mmoLog.Clear();
end;

class procedure LogBox.DoLog();
var
  Text: string;
begin
  if Assigned(LogBox.mmoLog) then
  begin
    if FLogTextList.Count > 0 then
    begin
      Text := FLogTextList[0];
      FLogTextList.Delete(0);
      LogBox.mmoLog.Append(Text);
    end;
  end;
end;

class procedure LogBox.Clear();
begin
  Lock();
  try
    TThread.Synchronize(TThread.CurrentThread, DoClear);  // Addr(DoClear)
  finally
    UnLock();
  end;
end;

class procedure LogBox.Append(Text: string);
begin
  Lock();
  try
    FLogTextList.Add(Text);
    TThread.Synchronize(TThread.CurrentThread, DoLog);  // Addr(DoLog)
  finally
    UnLock();
  end;
end;

class procedure LogBox.AppendLine(Text: string);
begin
  Text := sLineBreak + Text;
  Append(Text);
end;

class procedure LogBox.AppendLine(Ex: Exception);
var
  Text: string;
begin
  Text := Ex.ToString(); // Ex.ClassName + ': ' + Ex.Message;
  AppendLine(Text);
end;

class procedure LogBox.AppendLine();
var
  Text: string;
begin
  Text := '-------------------------------------------------------------------';
  AppendLine(Text);
end;




end.

