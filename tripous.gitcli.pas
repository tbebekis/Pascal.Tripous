unit Tripous.GitCli;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Tripous.Cli;

type
  EGitCli = class(Exception);

  { TGitCli }

  TGitCli = class
  private
    fCli: TCli;
    fOwnsCli: Boolean;
    fRepoDir: string;
    fRemoteName: string;
    fBranch: string;
    fGitHubUserName: string;
  private
    fAutoCommitOnPush: Boolean;
    function CreateGitEnv: TStringList;
    function GetDefaultBranchArg: string;
    function GitInDir(const Args: array of string; const ADir: string): TCliResult;
  public
    { Creates a Git CLI wrapper using an existing or internal TCli instance }
    constructor Create(ACli: TCli = nil);

    { Releases resources and owned CLI instance if applicable }
    destructor Destroy; override;

    { Executes a git command using argument array }
    function Git(const Args: array of string): TCliResult; overload;

    { Executes a git command using raw argument string }
    function Git(const Args: string): TCliResult; overload;

    { Returns True if RepoDir is a valid git repository }
    function IsGitRepo: Boolean;

    { Raises exception if RepoDir is not a git repository }
    procedure EnsureIsRepo;

    { Returns True if repository contains uncommitted changes }
    function HasUncommittedChanges: Boolean;

    { Creates a commit if there are pending changes }
    function CommitIfNeeded(const MessageText: string): Boolean;

    { Pushes the current branch to the configured remote }
    function Push(const CommitMessage: string = ''): TCliResult;

    { Returns the configured global git credential helper }
    function GetGlobalCredentialHelper: string;

    { Returns True if a global credential helper is configured }
    function HasGlobalCredentialHelper: Boolean;

    { Checks whether stored GitHub credentials exist }
    function HasGitHubCredentials: Boolean;

    { Stores GitHub credentials using git credential helper }
    function EnsureGitHubCredentials(const UserName, Token: string): Boolean;

    { Removes stored GitHub credentials }
    function RejectGitHubCredentials: Boolean;

    { Underlying CLI executor used for git commands }
    property Cli: TCli read fCli;

    { Repository directory where git commands will be executed }
    property RepoDir: string read fRepoDir write fRepoDir;

    { Remote name used for push operations (e.g. origin) }
    property RemoteName: string read fRemoteName write fRemoteName;

    { Branch used for push operations }
    property Branch: string read fBranch write fBranch;

    { GitHub username used for credential operations }
    property GitHubUserName: string read fGitHubUserName write fGitHubUserName;

    { If True, automatically commits pending changes before push }
    property AutoCommitOnPush : Boolean read fAutoCommitOnPush write fAutoCommitOnPush;
  end;

implementation

function TrimAll(const S: string): string;
begin
  Result := Trim(StringReplace(StringReplace(S, #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]));
end;

constructor TGitCli.Create(ACli: TCli = nil);
begin
  inherited Create;

  if Assigned(ACli) then
  begin
    fCli := ACli;
    fOwnsCli := False;
  end
  else
  begin
    fCli := TCli.Create;
    fOwnsCli := True;
  end;

  fRepoDir := '';
  fRemoteName := 'origin';
  fBranch := '';
  fGitHubUserName := '';
end;

destructor TGitCli.Destroy;
begin
  if fOwnsCli then
    fCli.Free;
  inherited Destroy;
end;

function TGitCli.CreateGitEnv: TStringList;
begin
  Result := TStringList.Create;
  Result.NameValueSeparator := '=';
  Result.Values['GIT_TERMINAL_PROMPT'] := '0';
end;

function TGitCli.GetDefaultBranchArg: string;
begin
  if Trim(fBranch) <> '' then
    Result := fBranch
  else
    Result := 'HEAD';
end;

function TGitCli.GitInDir(const Args: array of string; const ADir: string): TCliResult;
var
  SaveDir: string;
  EnvList: TStringList;
begin
  SaveDir := fCli.WorkingDirectory;
  EnvList := CreateGitEnv;
  try
    fCli.WorkingDirectory := ADir;
    Result := fCli.RunExe('git', Args, -1, EnvList);
  finally
    fCli.WorkingDirectory := SaveDir;
    EnvList.Free;
  end;
end;

function TGitCli.Git(const Args: array of string): TCliResult;
begin
  Result := GitInDir(Args, fRepoDir);
end;

function TGitCli.Git(const Args: string): TCliResult;
begin
  Result := Git([Args]);
end;

function TGitCli.IsGitRepo: Boolean;
var
  R: TCliResult;
begin
  if Trim(fRepoDir) = '' then
    Exit(False);

  R := Git(['rev-parse', '--is-inside-work-tree']);
  Result := R.Success and SameText(Trim(R.StdOut), 'true');
end;

procedure TGitCli.EnsureIsRepo;
begin
  if not IsGitRepo then
    raise EGitCli.CreateFmt('Not a git repository: %s', [fRepoDir]);
end;

function TGitCli.HasUncommittedChanges: Boolean;
var
  R: TCliResult;
begin
  EnsureIsRepo;
  R := Git(['status', '--porcelain']);
  Result := R.Success and (Trim(R.StdOut) <> '');
end;

function TGitCli.CommitIfNeeded(const MessageText: string): Boolean;
var
  R: TCliResult;
begin
  EnsureIsRepo;

  if not HasUncommittedChanges then
    Exit(False);

  R := Git(['add', '-A']);
  if not R.Success then
    raise EGitCli.Create(TrimAll(R.StdErr));

  R := Git(['commit', '-m', MessageText]);
  if not R.Success then
    raise EGitCli.Create(TrimAll(R.StdErr));

  Result := True;
end;

function TGitCli.Push(const CommitMessage: string = ''): TCliResult;
begin
  EnsureIsRepo;

  if AutoCommitOnPush then
  begin
    if CommitMessage <> '' then
      CommitIfNeeded(CommitMessage)
    else
      CommitIfNeeded('Auto commit');
  end
  else if HasUncommittedChanges then
    raise EGitCli.Create('There are uncommitted changes. Please commit first.');

  if GetDefaultBranchArg <> '' then
    Result := Git(['push', fRemoteName, GetDefaultBranchArg])
  else
    Result := Git(['push', fRemoteName]);

  if not Result.Success then
    raise EGitCli.Create(Trim(Result.StdErr));
end;

function TGitCli.GetGlobalCredentialHelper: string;
var
  R: TCliResult;
begin
  R := GitInDir(['config', '--global', '--get', 'credential.helper'], '');
  if R.Success then
    Result := Trim(R.StdOut)
  else
    Result := '';
end;

function TGitCli.HasGlobalCredentialHelper: Boolean;
begin
  Result := Trim(GetGlobalCredentialHelper) <> '';
end;

function TGitCli.HasGitHubCredentials: Boolean;
var
  R: TCliResult;
  InputText: string;
  EnvList: TStringList;
begin
  EnvList := CreateGitEnv;
  try
    InputText := 'protocol=https' + LineEnding + 'host=github.com' + LineEnding + LineEnding;
    if Trim(fGitHubUserName) <> '' then
      InputText := 'protocol=https' + LineEnding + 'host=github.com' + LineEnding + 'username=' + fGitHubUserName + LineEnding + LineEnding;

    R := fCli.RunExeWithInput('git', ['credential', 'fill'], InputText, -1, EnvList);
    Result := R.Success and (Pos('password=', R.StdOut) > 0);
  finally
    EnvList.Free;
  end;
end;

function TGitCli.EnsureGitHubCredentials(const UserName, Token: string): Boolean;
var
  R: TCliResult;
  InputText: string;
  EnvList: TStringList;
begin
  EnvList := CreateGitEnv;
  try
    InputText := 'protocol=https' + LineEnding + 'host=github.com' + LineEnding + 'username=' + UserName + LineEnding + 'password=' + Token + LineEnding + LineEnding;
    R := fCli.RunExeWithInput('git', ['credential', 'approve'], InputText, -1, EnvList);
    if R.Success then
      fGitHubUserName := UserName;
    Result := R.Success and HasGitHubCredentials;
  finally
    EnvList.Free;
  end;
end;

function TGitCli.RejectGitHubCredentials: Boolean;
var
  R: TCliResult;
  InputText: string;
  EnvList: TStringList;
begin
  EnvList := CreateGitEnv;
  try
    InputText := 'protocol=https' + LineEnding + 'host=github.com' + LineEnding;
    if Trim(fGitHubUserName) <> '' then
      InputText := InputText + 'username=' + fGitHubUserName + LineEnding;
    InputText := InputText + LineEnding;

    R := fCli.RunExeWithInput('git', ['credential', 'reject'], InputText, -1, EnvList);
    Result := R.Success;
  finally
    EnvList.Free;
  end;
end;

end.
