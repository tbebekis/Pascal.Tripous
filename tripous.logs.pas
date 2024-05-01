unit Tripous.Logs;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
   Classes
  ,SysUtils
  ,SyncObjs
  ,Variants
  ,Contnrs
  //,Generics.Collections
  ,FGL
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

  TLogInfoDictionary = specialize TFPGMap<string, Variant>;     // TFPGMap    TDictionary
  TLogPropLengthDictionary = specialize TFPGMap<string, Word>;

  TLogTextProc = procedure(LogText: string) of object;

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
    function GetProperties: TLogInfoDictionary;
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
    property Properties: TLogInfoDictionary read GetProperties;
  end;

  {
  ILogSource = interface
    ['{019F8BB3-E92F-4580-A3BD-A142FFA0B28A}']
  end;
  }

  { TLogListener }
  { A TLogListener automatically adds itself to Logger.Listeners when created
    and automatically removes itself from Logger.Listeners when destroyed. }
  TLogListener = class
  protected
    { CAUTION: ProcessLog() is always called from inside a secondary thread. }
    procedure ProcessLog(const Info: ILogInfo); virtual; abstract;
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
  end;


  { TLogFile }
  TLogFile = class
  private
    IsClosed : Boolean;
    F        : TextFile;
    Size     : SizeInt;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure CreateLogFile(FilePath: string);
    procedure CloseLogFile();

    procedure AppendLine(Line: string);
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
    FLock         : SyncObjs.TCriticalSection;
    FLockCount    : Integer;

    procedure Lock();
    procedure UnLock();

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
  private
    FLengths       : TLogPropLengthDictionary;
  protected
    function  GetLogInfoText(const Info: ILogInfo): string; override;
  public
    constructor Create(LogLineProc: TLogTextProc); override;
    destructor Destroy(); override;
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

    { log }
    class procedure Log(const Info: ILogInfo);
    class procedure Log(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; Params: array of const);

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

    { properties }
    class property Active: Boolean read GetActive write SetActive;
    class property LogFolder: string read GetLogFolder write FLogFolder;
    class property MinLevel: TLogLevel read GetMinLevel write SetMinLevel;
  end;


implementation

uses
  LazSysUtils
  ,DateUtils
  ,TypInfo
  ,LCLProc
  ,LazTracer
  {$IFDEF UNIX} ,unix{$ENDIF}
  {$IFDEF WINDOWS} ,Windows{$ENDIF}
  ;


type

  { TLogInfo }
  TLogInfo = class(TInterfacedObject, ILogInfo)
  private
    FEventId: string;
    FException: Exception;
    FExceptionData: string;
    FHost: string;
    FLevel: TLogLevel;
    FProperties: TLogInfoDictionary;
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
    function GetProperties: TLogInfoDictionary;
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
    constructor Create(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; Params: array of const);
    destructor Destroy(); override;

    function  ToString: ansistring; override;
    procedure SaveToFile(Folder: string = '');

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
    property Properties: TLogInfoDictionary read GetProperties;
  end;








{ TLogFile }
constructor TLogFile.Create();
begin
  inherited Create();
  IsClosed := True;
end;

destructor TLogFile.Destroy;
begin
  CloseLogFile();
  inherited Destroy;
end;

procedure TLogFile.CloseLogFile();
begin
  if not IsClosed then
  begin
    Close(F);
    IsClosed := True;
    Size     := 0;
  end;
end;

procedure TLogFile.CreateLogFile(FilePath: string);
begin
  CloseLogFile();
  Assign(F, FilePath);

  {$I-} // without this, if rewrite fails then a runtime error will be generated
  Rewrite(F);
  {$I+}
  if IOResult <> 0 then
    raise Exception.CreateFmt('Cannot create a log file: %s', [FilePath]);

  IsClosed := False;
end;

procedure TLogFile.AppendLine(Line: string);
begin
  System.WriteLn(F, Line);
  Size := Size + Length(Line);
  Flush(F);
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

    WriteLine(Line);
  finally
    SB.Free();
  end;

{ TODO: if (Info.Properties != null && Info.Properties.Count > 0)
   Text = Text + " Properties = " + Json.Serialize(Info.Properties);
}
end;

{ TLogToMainThreadListener }

constructor TLogToMainThreadListener.Create(LogTextProc: TLogTextProc);
begin
  inherited Create;

  FLock        := SyncObjs.TCriticalSection.Create();
  FLogTextList := TStringList.Create();

  if not Assigned(LogTextProc) then
    raise Exception.CreateFmt('No callback function is provided to %s', [Self.ClassName]);

  FLogProc := LogTextProc;
end;

destructor TLogToMainThreadListener.Destroy;
begin
  FLogTextList.Free();
  FLock.Free();
  inherited Destroy;
end;

procedure TLogToMainThreadListener.Lock;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    FLock.Enter;
end;

procedure TLogToMainThreadListener.UnLock;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    FLock.Leave();
  end;
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
    AddLine(SB, 'TimeStamp', Info.TimeStampText);
    AddLine(SB, 'Level', Info.LevelText);
    AddLine(SB, 'Source', Info.Source);
    AddLine(SB, 'Scope', Info.ScopeId);
    AddLine(SB, 'EventId', Info.EventId);
    AddLine(SB, 'Host', Info.Host);
    AddLine(SB, 'User', Info.User);
    AddLine(SB, 'Text', Info.Text);
    if Length(Info.ExceptionData) > 0 then
      AddLine(SB, 'Stack', LineEnding + Info.ExceptionData);

    SB.AppendLine();

    Result := SB.ToString();
  finally
    SB.Free();
  end;

end;





{ TLogLineListener }

constructor TLogLineListener.Create(LogLineProc: TLogTextProc);
begin
  inherited Create(LogLineProc);

  FLengths := TLogPropLengthDictionary.Create();

  // prepare the FLengths table
  FLengths.Add('TimeStamp'  , 24);
  FLengths.Add('Host'       , 24);
  FLengths.Add('User'       , 24);
  FLengths.Add('Level'      , 12);
  FLengths.Add('EventId'    , 14);
  FLengths.Add('Source'     , 64);
  FLengths.Add('Scope'      , 32);
end;

destructor TLogLineListener.Destroy;
begin
  FLengths.Free;
  inherited Destroy;
end;

function TLogLineListener.GetLogInfoText(const Info: ILogInfo): string;
var
  SB       : TAnsiStringBuilder;
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

    Result := SB.ToString();
  finally
    SB.Free();
  end;

{ TODO: if (Info.Properties != null && Info.Properties.Count > 0)
   Text = Text + " Properties = " + Json.Serialize(Info.Properties);
}

end;










{ TLogInfo }
constructor TLogInfo.Create(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; Params: array of const);
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

{
  TODO: this.Properties = Logger.FormatParams(ref Text, Params);
if (this.Properties == null)
    this.Properties = new Dictionary<string, object>();

    SEE: https://www.freepascal.org/docs-html/ref/refsu69.html
}

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

function TLogInfo.GetProperties: TLogInfoDictionary;
begin
  Result := FProperties;
end;




{ TLogListener }
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





{ Logger }

class constructor Logger.Create();
begin
  FLock      := SyncObjs.TCriticalSection.Create();
  FListeners := TList.Create();
end;

class destructor Logger.Destroy();
begin
  FListeners.Free;
  FLock.Free;
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


type
  { TLogThread }
  TLogThread = class(TThread)
  protected
    FListener : TLogListener;
    FInfo     : ILogInfo;
    procedure Execute(); override;
  public
    constructor Create(Listener: TLogListener; const Info: ILogInfo);
  end;

{ TLogThread }
constructor TLogThread.Create(Listener: TLogListener; const Info: ILogInfo);
begin
  { for a TThread instance to work properly we have to
    1. pass True to constructor in order to create it suspended,
       so we have to call Start() in order for the Execute() to be called
    2. make the TThread instance to free itself }
  inherited Create(True);      // initially suspended, so we have to call Start()
  FreeOnTerminate := True;     // make it free itself

  FListener       := Listener;
  FInfo           := Info;
end;

procedure TLogThread.Execute();
begin
  try
    //Sleep(1000 * 3);
    FListener.ProcessLog(FInfo);
  except
  end;
end;


class procedure Logger.Log(const Info: ILogInfo);
var
  i          : Integer;
  Listener   : TLogListener;
  InfoLevel  : TLogLevel;
begin
  Lock();
  try
    InfoLevel := Info.Level;
    if Active and (FMinLevel <> TLogLevel.loNone) and (Ord(InfoLevel) >= Ord(FMinLevel)) then
    begin
      for i := 0 to FListeners.Count - 1 do
      begin
        Listener := TLogListener(FListeners[i]);
        TLogThread.Create(Listener, Info).Start();
      end;
    end;
  finally
    UnLock();
  end;

end;

class procedure Logger.Log(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; Params: array of const);
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

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
end;

class procedure Logger.Debug(Source, EventId, Text: string);
var
  ScopeId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loDebug;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
end;

class procedure Logger.Debug(Source, Text: string);
var
  ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loDebug;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
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

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
end;


class procedure Logger.Info(Source, ScopeId, EventId, Text: string);
var
  Level: TLogLevel; Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
end;

class procedure Logger.Info(Source, EventId, Text: string);
var
  ScopeId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
end;

class procedure Logger.Info(Source, Text: string);
var
  ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception;
begin
  Level       := loInfo;
  Exception_  := nil;
  EventId     := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
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

  Log(Source, ScopeId, EventId, Level, Exception_, Text, []);
end;

class procedure Logger.Error(Source, ScopeId, EventId: string; Ex: Exception);
var
  Level: TLogLevel;
  Text : string;
begin
  Level       := loError;
  Text        := '';

  Log(Source, ScopeId, EventId, Level, Ex, Text, []);
end;

class procedure Logger.Error(Source, EventId: string; Ex: Exception);
var
  Level: TLogLevel;
  ScopeId, Text : string;
begin
  Level       := loError;
  Text        := '';
  ScopeId     := '';

  Log(Source, ScopeId, EventId, Level, Ex, Text, []);
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

  Log(Source, ScopeId, EventId, Level, Ex, Text, []);
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

  Log(Source, ScopeId, EventId, Level, Ex, Text, []);

end;




end.

