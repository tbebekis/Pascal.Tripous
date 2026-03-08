unit Tripous.Cli;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Process, Pipes, LazSysUtils;

type
  TCliResult = record
    CommandLine: string;
    ExitCode: Integer;
    StdOut: string;
    StdErr: string;
    TimedOut: Boolean;
    DurationMS: Int64;
    function Success: Boolean;
    function ToText: string;
  end;

  TCliResultArray = array of TCliResult;

  TCli = class
  private
    fDefaultTimeoutMs: Integer;
    fWorkingDirectory: string;
    fShellExecutable: string;
    fShellSwitch: string;
    fEnvironment: TStringList;
  private
    function GetSystemEnvironment: TStringList;
    function GetEffectiveTimeoutMs(TimeoutMs: Integer): Integer;
    function QuoteArg(const S: string): string;
    function BuildCommandLine(const ExePath: string; const Args: array of string): string;
    function MergeEnvironment(BaseEnv, ExtraEnv: TStrings): TStringList;
    function NormalizeExitCode(AExitCode: Integer): Integer;
    procedure AppendAvailableText(AStream: TInputPipeStream; var S: string);
    function RunProcess(const ExePath: string; const Args: array of string; const InputText: string; CloseInputAfterWrite: Boolean; TimeoutMs: Integer; ExtraEnv: TStrings = nil): TCliResult;
  public
    { Creates a CLI executor with default environment and settings }
    constructor Create;

    { Releases internal resources }
    destructor Destroy; override;

    { Executes an executable with argument array }
    function RunExe(const ExePath: string; const Args: array of string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult; overload;

    { Executes an executable with a raw argument string }
    function RunExe(const ExePath: string; const Args: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult; overload;

    { Executes an executable and sends text to its standard input }
    function RunExeWithInput(const ExePath: string; const Args: array of string; const InputText: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult; overload;

    { Executes an executable with input using a raw argument string }
    function RunExeWithInput(const ExePath: string; const Args: string; const InputText: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult; overload;

    { Executes multiple shell commands as a single chained command }
    function RunShellChain(const Commands: array of string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult; overload;

    { Executes a single shell command through the configured shell }
    function RunShellChain(const Commands: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult; overload;

    { Executes multiple shell commands sequentially and returns results for each }
    function RunShellBatch(const Commands: array of string; StopOnError: Boolean; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResultArray; overload;

    { Executes a single shell command as a batch operation }
    function RunShellBatch(const Commands: string; StopOnError: Boolean; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResultArray; overload;

    { Executes a command through the configured system shell }
    function RunShell(const Command: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;

    { Default operation timeout in milliseconds }
    property DefaultTimeoutMs: Integer read fDefaultTimeoutMs write fDefaultTimeoutMs;

    { Working directory used when executing processes }
    property WorkingDirectory: string read fWorkingDirectory write fWorkingDirectory;

    { Shell executable used for RunShell operations }
    property ShellExecutable: string read fShellExecutable write fShellExecutable;

    { Switch used to pass commands to the shell (e.g. -c or /C) }
    property ShellSwitch: string read fShellSwitch write fShellSwitch;

    { Custom environment variables added to the process environment }
    property Environment: TStringList read fEnvironment;
  end;

implementation

uses
  DateUtils;

function TCliResult.Success: Boolean;
begin
  Result := (not TimedOut) and (ExitCode = 0);
end;

function TCliResult.ToText: string;
begin
  Result :=
    '> ' + CommandLine + LineEnding +
    'ExitCode: ' + IntToStr(ExitCode) + LineEnding +
    'TimedOut: ' + BoolToStr(TimedOut, True) + LineEnding +
    'Duration: ' + IntToStr(DurationMs) + ' ms' + LineEnding + LineEnding +
    '--- StdOut ---' + LineEnding +
    StdOut + LineEnding +
    '--- StdErr ---' + LineEnding +
    StdErr;
end;

constructor TCli.Create;
begin
  inherited Create;
  fEnvironment := TStringList.Create;
  fEnvironment.NameValueSeparator := '=';

  {$IFDEF WINDOWS}
  fShellExecutable := 'cmd.exe';
  fShellSwitch := '/C';
  {$ELSE}
  fShellExecutable := '/bin/bash';
  fShellSwitch := '-c';
  {$ENDIF}

  fDefaultTimeoutMs := 60000;
  fWorkingDirectory := '';
end;

destructor TCli.Destroy;
begin
  fEnvironment.Free;
  inherited Destroy;
end;

function TCli.GetSystemEnvironment: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.NameValueSeparator := '=';
  for i := 1 to GetEnvironmentVariableCount do
    Result.Add(GetEnvironmentString(i));
end;

function TCli.GetEffectiveTimeoutMs(TimeoutMs: Integer): Integer;
begin
  if TimeoutMs >= 0 then
    Result := TimeoutMs
  else
    Result := fDefaultTimeoutMs;
end;

function TCli.QuoteArg(const S: string): string;
begin
  if S = '' then
    Exit('""');

  {$IFDEF WINDOWS}
  if (Pos(' ', S) > 0) or (Pos('"', S) > 0) or (Pos('&', S) > 0) or (Pos('(', S) > 0) or (Pos(')', S) > 0) then
    Result := '"' + StringReplace(S, '"', '\"', [rfReplaceAll]) + '"'
  else
    Result := S;
  {$ELSE}
  if (Pos(' ', S) > 0) or (Pos('"', S) > 0) or (Pos('''', S) > 0) or (Pos('$', S) > 0) or (Pos('&', S) > 0) or (Pos(';', S) > 0) or (Pos('(', S) > 0) or (Pos(')', S) > 0) then
    Result := '''' + StringReplace(S, '''', '''"''"''', [rfReplaceAll]) + ''''
  else
    Result := S;
  {$ENDIF}
end;

function TCli.BuildCommandLine(const ExePath: string; const Args: array of string): string;
var
  i: Integer;
begin
  Result := QuoteArg(ExePath);
  for i := 0 to High(Args) do
    Result := Result + ' ' + QuoteArg(Args[i]);
end;

function TCli.MergeEnvironment(BaseEnv, ExtraEnv: TStrings): TStringList;
var
  i: Integer;
  N: string;
begin
  Result := GetSystemEnvironment;

  if Assigned(BaseEnv) then
    for i := 0 to BaseEnv.Count - 1 do
    begin
      N := BaseEnv.Names[i];
      if N <> '' then
        Result.Values[N] := BaseEnv.ValueFromIndex[i]
      else
        Result.Add(BaseEnv[i]);
    end;

  if Assigned(ExtraEnv) then
    for i := 0 to ExtraEnv.Count - 1 do
    begin
      N := ExtraEnv.Names[i];
      if N <> '' then
        Result.Values[N] := ExtraEnv.ValueFromIndex[i]
      else
        Result.Add(ExtraEnv[i]);
    end;
end;

function TCli.NormalizeExitCode(AExitCode: Integer): Integer;
begin
  {$IFDEF UNIX}
  if AExitCode > 255 then
    Result := AExitCode shr 8
  else
    Result := AExitCode;
  {$ELSE}
  Result := AExitCode;
  {$ENDIF}
end;

procedure TCli.AppendAvailableText(AStream: TInputPipeStream; var S: string);
var
  Count: LongInt;
  Buffer: array[0..4095] of Byte;
  Chunk: RawByteString;
begin
  if AStream = nil then
    Exit;

  Count := AStream.NumBytesAvailable;
  while Count > 0 do
  begin
    if Count > SizeOf(Buffer) then
      Count := SizeOf(Buffer);

    Count := AStream.Read(Buffer[0], Count);
    if Count <= 0 then
      Break;

    SetString(Chunk, PAnsiChar(@Buffer[0]), Count);
    S := S + string(Chunk);

    Count := AStream.NumBytesAvailable;
  end;
end;

function TCli.RunProcess(const ExePath: string; const Args: array of string; const InputText: string; CloseInputAfterWrite: Boolean; TimeoutMs: Integer; ExtraEnv: TStrings = nil): TCliResult;
var
  P: TProcess;
  StartUtc: TDateTime;
  EffectiveTimeout: Integer;
  EnvList: TStringList;
  i: Integer;
  BytesToWrite: RawByteString;
begin
  Result.CommandLine := BuildCommandLine(ExePath, Args);
  Result.ExitCode := -1;
  Result.StdOut := '';
  Result.StdErr := '';
  Result.TimedOut := False;
  Result.DurationMS := 0;

  EffectiveTimeout := GetEffectiveTimeoutMs(TimeoutMs);
  StartUtc := NowUTC;
  EnvList := MergeEnvironment(fEnvironment, ExtraEnv);

  P := TProcess.Create(nil);
  try
    P.Executable := ExePath;
    for i := 0 to High(Args) do
      P.Parameters.Add(Args[i]);

    if fWorkingDirectory <> '' then
      P.CurrentDirectory := fWorkingDirectory;

    P.Options := [poUsePipes];
    P.Environment.Assign(EnvList);
    P.Execute;

    if InputText <> '' then
    begin
      BytesToWrite := RawByteString(InputText);
      if Length(BytesToWrite) > 0 then
        P.Input.Write(BytesToWrite[1], Length(BytesToWrite));
    end;

    if CloseInputAfterWrite then
      P.CloseInput;

    while P.Running do
    begin
      AppendAvailableText(P.Output, Result.StdOut);
      AppendAvailableText(P.Stderr, Result.StdErr);

      Sleep(10);

      if (EffectiveTimeout >= 0) and (MilliSecondsBetween(NowUTC, StartUtc) >= EffectiveTimeout) then
      begin
        Result.TimedOut := True;
        P.Terminate(0);
        Break;
      end;
    end;

    if not Result.TimedOut then
      P.WaitOnExit;

    AppendAvailableText(P.Output, Result.StdOut);
    AppendAvailableText(P.Stderr, Result.StdErr);

    Result.ExitCode := NormalizeExitCode(P.ExitStatus);
    Result.DurationMS := MilliSecondsBetween(NowUTC, StartUtc);
  finally
    EnvList.Free;
    P.Free;
  end;
end;

function TCli.RunExe(const ExePath: string; const Args: array of string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;
begin
  Result := RunProcess(ExePath, Args, '', True, TimeoutMs, ExtraEnv);
end;

function TCli.RunExe(const ExePath: string; const Args: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;
begin
  Result := RunExe(ExePath, [Args], TimeoutMs, ExtraEnv);
end;

function TCli.RunExeWithInput(const ExePath: string; const Args: array of string; const InputText: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;
begin
  Result := RunProcess(ExePath, Args, InputText, True, TimeoutMs, ExtraEnv);
end;

function TCli.RunExeWithInput(const ExePath: string; const Args: string; const InputText: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;
begin
  Result := RunExeWithInput(ExePath, [Args], InputText, TimeoutMs, ExtraEnv);
end;

function TCli.RunShell(const Command: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;
begin
  Result := RunExe(fShellExecutable, [fShellSwitch, Command], TimeoutMs, ExtraEnv);
end;

function TCli.RunShellChain(const Commands: array of string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;
var
  i: Integer;
  S: string;
begin
  S := '';
  for i := 0 to High(Commands) do
  begin
    if S <> '' then
      {$IFDEF WINDOWS}
      S := S + ' && '
      {$ELSE}
      S := S + ' && '
      {$ENDIF};
    S := S + Commands[i];
  end;
  Result := RunShell(S, TimeoutMs, ExtraEnv);
end;

function TCli.RunShellChain(const Commands: string; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResult;
begin
  Result := RunShellChain([Commands], TimeoutMs, ExtraEnv);
end;

function TCli.RunShellBatch(const Commands: array of string; StopOnError: Boolean; TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResultArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Commands));

  for i := 0 to High(Commands) do
  begin
    Result[i] := RunShell(Commands[i], TimeoutMs, ExtraEnv);

    if StopOnError and (not Result[i].Success) then
    begin
      SetLength(Result, i + 1);
      Exit;
    end;
  end;
end;

function TCli.RunShellBatch(const Commands: string; StopOnError: Boolean;
  TimeoutMs: Integer = -1; ExtraEnv: TStrings = nil): TCliResultArray;
begin
  Result := RunShellBatch([Commands], StopOnError, TimeoutMs, ExtraEnv);
end;

end.
