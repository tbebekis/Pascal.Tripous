unit f_MainForm;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls,
  Tripous.Cli,
  Tripous.GitCli;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnRunExe: TButton;
    btnRunExeTimeout: TButton;
    btnRunExeStdErr: TButton;
    brnRunExeStdOut: TButton;
    btnRunExeQuotedArgs: TButton;
    btnRunExeManyArgs: TButton;
    btnRunExeSingleQuotes: TButton;
    btnRunExeWithInput: TButton;
    btnRunExeNoInput: TButton;
    btnRunShell: TButton;
    btnRunShellChain: TButton;
    btnRunShellBatch: TButton;
    btnRunShellBatchStopOnError: TButton;
    btnRunShellBatchOnErrorContinue: TButton;
    btnGitVersion: TButton;
    btnGitIsRepo: TButton;
    btnGitHasUncommited: TButton;
    btnGitEnsureIsRepo: TButton;
    btnGitCommitIfNeeded: TButton;
    btnGitPush: TButton;
    btnGitGetGlobalCredHelper: TButton;
    btnGitHasGlobalCredHelper: TButton;
    btnGithubHasCredentials: TButton;
    btnGithubEnsureHasCredentials: TButton;
    edtGithubUserName: TEdit;
    edtToken: TEdit;
    edtFolderPath: TEdit;
    edtLog: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Pager: TPageControl;
    Panel1: TPanel;
    splitContent: TSplitter;
    tabCli: TTabSheet;
    tabGit: TTabSheet;
  private
    Cli: TCli;
    GitCli: TGitCli;

    procedure FormInitialize();
    procedure FormFinalize();

    procedure AnyClick(Sender: TObject);
    procedure AppException(Sender: TObject; E: Exception);

    function GetFolderPath(): string;
    function ResultToText(const R: TCliResult): string;
    procedure PrepareGitCli();
    procedure LogResult(const R: TCliResult);
    procedure LogBool(const Title: string; B: Boolean);

    procedure TestCliRunExe;
    procedure TestCliRunExe_StdErr();
    procedure TestCliRunExe_StdOut();
    procedure TestCliRunExe_QuotedArgs();
    procedure TestCliRunExe_ManyArgs();
    procedure TestCliRunExe_SingleQuotes();
    procedure TestCliRunExe_WithInput();
    procedure TestCliRunExe_NoInput();
    procedure TestCliRunExe_Timeout();

    procedure TestCliRunShell();
    procedure TestCliRunShell_Chain();
    procedure TestCliRunShell_Batch();
    procedure TestCliRunShell_Batch_StopOnError();
    procedure TestCliRunShell_Batch_ContinueOnError();

    procedure TestGitCli_Version();
    procedure TestGitCli_IsRepo();
    procedure TestGitCli_HasUncommitted();
    procedure TestGitCli_EnsureRepo();
    procedure TestGitCli_CommitIfNeeded();

    procedure TestGitCli_Push();
    procedure TestGitCli_GetGlobalCredentialHelper();
    procedure TestGitCli_HasGlobalCredentialHelper();

    procedure TestGitCli_HasGithubCredentials();
    procedure TestGitCli_EnsureGithubCredentials();
  protected
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Tripous,
  Tripous.Logs;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  LogBox.Initialize(edtLog);
  Application.OnException := AppException;
  Pager.ActivePage := tabCli;

  Cli := TCli.Create();
  GitCli := TGitCli.Create(Cli);

  btnRunExe.OnClick := AnyClick;
  btnRunExeStdErr.OnClick := AnyClick;
  brnRunExeStdOut.OnClick := AnyClick;
  btnRunExeQuotedArgs.OnClick := AnyClick;
  btnRunExeManyArgs.OnClick := AnyClick;
  btnRunExeSingleQuotes.OnClick := AnyClick;

  btnRunExeWithInput.OnClick := AnyClick;
  btnRunExeNoInput.OnClick := AnyClick;
  btnRunExeTimeout.OnClick := AnyClick;

  btnRunShell.OnClick := AnyClick;
  btnRunShellChain.OnClick := AnyClick;
  btnRunShellBatch.OnClick := AnyClick;
  btnRunShellBatchStopOnError.OnClick := AnyClick;
  btnRunShellBatchOnErrorContinue.OnClick := AnyClick;

  btnGitVersion.OnClick := AnyClick;
  btnGitIsRepo.OnClick := AnyClick;
  btnGitHasUncommited.OnClick := AnyClick;
  btnGitEnsureIsRepo.OnClick := AnyClick;
  btnGitCommitIfNeeded.OnClick := AnyClick;

  btnGitPush.OnClick := AnyClick;
  btnGitGetGlobalCredHelper.OnClick := AnyClick;
  btnGitHasGlobalCredHelper.OnClick := AnyClick;

  btnGithubHasCredentials.OnClick := AnyClick;
  btnGithubEnsureHasCredentials.OnClick := AnyClick;
end;

destructor TMainForm.Destroy;
begin
  GitCli.Free();
  Cli.Free();
  FormFinalize();
  inherited Destroy;
end;

procedure TMainForm.DoShow;
begin
  inherited DoShow;
  FormInitialize();
end;

procedure TMainForm.FormInitialize();
begin
end;

procedure TMainForm.FormFinalize();
begin
  LogBox.Finalize();
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnRunExe = Sender then TestCliRunExe
  else if btnRunExeStdErr = Sender then TestCliRunExe_StdErr
  else if brnRunExeStdOut = Sender then TestCliRunExe_StdOut
  else if btnRunExeQuotedArgs = Sender then TestCliRunExe_QuotedArgs
  else if btnRunExeManyArgs = Sender then TestCliRunExe_ManyArgs
  else if btnRunExeSingleQuotes = Sender then TestCliRunExe_SingleQuotes
  else if btnRunExeWithInput = Sender then TestCliRunExe_WithInput
  else if btnRunExeNoInput = Sender then TestCliRunExe_NoInput
  else if btnRunExeTimeout = Sender then TestCliRunExe_Timeout

  else if btnRunShell = Sender then TestCliRunShell
  else if btnRunShellChain = Sender then TestCliRunShell_Chain
  else if btnRunShellBatch = Sender then TestCliRunShell_Batch
  else if btnRunShellBatchStopOnError = Sender then TestCliRunShell_Batch_StopOnError
  else if btnRunShellBatchOnErrorContinue = Sender then TestCliRunShell_Batch_ContinueOnError

  else if btnGitVersion = Sender then TestGitCli_Version
  else if btnGitIsRepo = Sender then TestGitCli_IsRepo
  else if btnGitHasUncommited = Sender then TestGitCli_HasUncommitted
  else if btnGitEnsureIsRepo = Sender then TestGitCli_EnsureRepo
  else if btnGitCommitIfNeeded = Sender then TestGitCli_CommitIfNeeded

  else if btnGitPush = Sender then TestGitCli_Push
  else if btnGitGetGlobalCredHelper = Sender then TestGitCli_GetGlobalCredentialHelper
  else if btnGitHasGlobalCredHelper = Sender then TestGitCli_HasGlobalCredentialHelper

  else if btnGithubHasCredentials = Sender then TestGitCli_HasGithubCredentials
  else if btnGithubEnsureHasCredentials = Sender then TestGitCli_EnsureGithubCredentials;
end;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
begin
  LogBox.AppendLine(E);
end;

function TMainForm.GetFolderPath(): string;
begin
  Result := Trim(edtFolderPath.Text);
  if (Result = '') or (not DirectoryExists(Result)) then
    Result := GetCurrentDir;
end;

function TMainForm.ResultToText(const R: TCliResult): string;
begin
  Result :=
    'CommandLine: ' + R.CommandLine + LineEnding +
    'ExitCode: ' + IntToStr(R.ExitCode) + LineEnding +
    'TimedOut: ' + BoolToStr(R.TimedOut, True) + LineEnding +
    'DurationMS: ' + IntToStr(R.DurationMS) + LineEnding +
    '--- StdOut ---' + LineEnding +
    R.StdOut + LineEnding +
    '--- StdErr ---' + LineEnding +
    R.StdErr;
end;

procedure TMainForm.PrepareGitCli();
begin
  GitCli.RepoDir := GetFolderPath();
  GitCli.RemoteName := 'origin';
  GitCli.Branch := '';
  GitCli.GitHubUserName := Trim(edtGithubUserName.Text);

  GitCli.Cli.WorkingDirectory := GitCli.RepoDir;
end;

procedure TMainForm.LogResult(const R: TCliResult);
begin
  LogBox.AppendLine(ResultToText(R));
end;

procedure TMainForm.LogBool(const Title: string; B: Boolean);
begin
  LogBox.AppendLine(Title + ': ' + BoolToStr(B, True));
end;

procedure TMainForm.TestCliRunExe;
var
  R: TCliResult;
begin
  R := Cli.RunExe('git', ['--version']);
  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_StdErr();
var
  R: TCliResult;
begin
  R := Cli.RunExe('git', ['not-a-real-command']);
  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_StdOut();
var
  R: TCliResult;
begin
  R := Cli.RunExe('git', ['--version']);
  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_QuotedArgs();
var
  R: TCliResult;
begin
  {$IFDEF WINDOWS}
  R := Cli.RunExe('cmd.exe', ['/C', 'echo', 'hello world']);
  {$ELSE}
  R := Cli.RunExe('/bin/echo', ['hello world']);
  {$ENDIF}
  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_ManyArgs();
var
  R: TCliResult;
begin
  {$IFDEF WINDOWS}
  R := Cli.RunExe('cmd.exe', ['/C', 'echo', 'one', 'two words', 'three']);
  {$ELSE}
  R := Cli.RunExe('/bin/echo', ['one', 'two words', 'three']);
  {$ENDIF}
  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_SingleQuotes();
var
  R: TCliResult;
begin
  {$IFDEF WINDOWS}
  R := Cli.RunExe('cmd.exe', ['/C', 'echo', '''hello world''']);
  {$ELSE}
  R := Cli.RunExe('/bin/echo', ['''hello world''']);
  {$ENDIF}
  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_WithInput();
var
  R: TCliResult;
  InputText: string;
begin
  InputText := 'alpha' + LineEnding + 'beta' + LineEnding;

  {$IFDEF WINDOWS}
  R := Cli.RunExeWithInput('more', [], InputText);
  {$ELSE}
  R := Cli.RunExeWithInput('cat', [], InputText);
  {$ENDIF}

  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_NoInput();
var
  R: TCliResult;
begin
  {$IFDEF WINDOWS}
  R := Cli.RunExeWithInput('more', [], '', 3000);
  {$ELSE}
  R := Cli.RunExeWithInput('cat', [], '', 3000);
  {$ENDIF}
  LogResult(R);
end;

procedure TMainForm.TestCliRunExe_Timeout();
var
  R: TCliResult;
begin
  {$IFDEF WINDOWS}
  R := Cli.RunExe('ping', ['127.0.0.1', '-n', '10'], 1000);
  {$ELSE}
  R := Cli.RunExe('sleep', ['10'], 1000);
  {$ENDIF}
  LogResult(R);
end;

procedure TMainForm.TestCliRunShell();
var
  R: TCliResult;
begin
  R := Cli.RunShell('git --version');
  LogResult(R);
end;

procedure TMainForm.TestCliRunShell_Chain();
var
  R: TCliResult;
begin
  R := Cli.RunShellChain([
    'echo alpha',
    'echo beta'
  ]);
  LogResult(R);
end;

procedure TMainForm.TestCliRunShell_Batch();
var
  A: TCliResultArray;
  i: Integer;
begin
  A := Cli.RunShellBatch([
    'echo alpha',
    'echo beta'
  ], False);

  for i := 0 to High(A) do
  begin
    LogBox.AppendLine('Batch item ' + IntToStr(i));
    LogResult(A[i]);
  end;
end;

procedure TMainForm.TestCliRunShell_Batch_StopOnError;
var
  A: TCliResultArray;
  i: Integer;
begin
  A := Cli.RunShellBatch([
    'echo alpha',
    'git not-a-real-command',
    'echo beta'
  ], True, 120000);

  for i := 0 to High(A) do
  begin
    LogBox.AppendLine('Batch item ' + IntToStr(i));
    LogResult(A[i]);
  end;
end;

procedure TMainForm.TestCliRunShell_Batch_ContinueOnError;
var
  A: TCliResultArray;
  i: Integer;
begin
  A := Cli.RunShellBatch([
    'echo alpha',
    'git not-a-real-command',
    'echo beta'
  ], False, 120000);

  for i := 0 to High(A) do
  begin
    LogBox.AppendLine('Batch item ' + IntToStr(i));
    LogResult(A[i]);
  end;
end;

procedure TMainForm.TestGitCli_Version;
var
  R: TCliResult;
begin
  PrepareGitCli();
  R := GitCli.Git(['--version']);
  LogResult(R);
end;

procedure TMainForm.TestGitCli_IsRepo;
var
  B: Boolean;
begin
  PrepareGitCli();
  B := GitCli.IsGitRepo();
  LogBox.AppendLine('Folder: ' + GitCli.RepoDir + ' - IsRepo: ' + BoolToStr(B, True));
end;

procedure TMainForm.TestGitCli_HasUncommitted;
var
  B: Boolean;
begin
  PrepareGitCli();
  B := GitCli.HasUncommittedChanges();
  LogBox.AppendLine('Folder: ' + GitCli.RepoDir + ' - HasUncommittedChanges: ' + BoolToStr(B, True));
end;

procedure TMainForm.TestGitCli_EnsureRepo;
begin
  PrepareGitCli();
  GitCli.EnsureIsRepo();
  LogBool('EnsureIsRepo', True);
end;

procedure TMainForm.TestGitCli_CommitIfNeeded;
var
  B: Boolean;
begin
  PrepareGitCli();
  B := GitCli.CommitIfNeeded('Test commit from GitCli test app');
  LogBox.AppendLine('Folder: ' + GitCli.RepoDir + ' - CommitIfNeeded: ' + BoolToStr(B, True));
end;

procedure TMainForm.TestGitCli_Push;
var
  R: TCliResult;
begin
  PrepareGitCli();
  R := GitCli.Push();
  LogResult(R);
  //LogBox.AppendLine('Folder: ' + GitCli.RepoDir + ' - Push: ' + BoolToStr(B, True));
end;

procedure TMainForm.TestGitCli_GetGlobalCredentialHelper;
var
  S: string;
begin
  S := GitCli.GetGlobalCredentialHelper();
  LogBox.AppendLine('GlobalCredentialHelper: ' + S);
end;

procedure TMainForm.TestGitCli_HasGlobalCredentialHelper;
var
  B: Boolean;
begin
  B := GitCli.HasGlobalCredentialHelper();
  LogBox.AppendLine('HasGlobalCredentialHelper: ' + BoolToStr(B, True));
end;

procedure TMainForm.TestGitCli_HasGithubCredentials;
var
  B: Boolean;
begin
  PrepareGitCli();
  B := GitCli.HasGitHubCredentials();
  LogBox.AppendLine('UserName: ' + GitCli.GitHubUserName + ' - HasGitHubCredentials: ' + BoolToStr(B, True));
end;

procedure TMainForm.TestGitCli_EnsureGithubCredentials;
var
  UserName: string;
  Token: string;
  B: Boolean;
begin
  PrepareGitCli();
  UserName := Trim(edtGithubUserName.Text);
  Token := Trim(edtToken.Text);
  B := GitCli.EnsureGitHubCredentials(UserName, Token);
  LogBox.AppendLine('UserName: ' + UserName + ' - EnsureGitHubCredentials: ' + BoolToStr(B, True));
end;

end.
