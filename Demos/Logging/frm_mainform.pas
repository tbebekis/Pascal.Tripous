unit frm_MainForm;
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls
  ,DBGrids, DBCtrls
  ,Tripous
  ,Tripous.Logs
  ;

type

  { TMainForm }
  TMainForm = class(TForm)
    btnLogInfo: TButton;
    btnLogError: TButton;
    btnLogSource: TButton;
    btnLogSourceAndScope: TButton;
    btnLogSourceAndScope2: TButton;
    edtInfoText: TEdit;
    lblLogFolder: TLabel;
    mmoLog: TMemo;
    Panel1: TPanel;
  private
    FLogListener: TLogListener;
    FFileLogListener: TFileLogListener;
    FFormLogListener: TFormLogListener;
    FDbLogListener: TDbLogListener;

    procedure AnyClick(Sender: TObject);
    function GetLogText(): string;

    procedure LogProc(LogLine: string);

    procedure LogInfoTest();
    procedure LogErrorTest();
    procedure LogSourceTest();
    procedure LogSourceAndScopeTest();
    procedure LogSourceAndScopeTest2();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
    procedure DoDestroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Variants
  ,c_ScopeTest
  ;

{$R *.lfm}

{ TMainForm }
procedure TMainForm.DoShow;
begin
  inherited DoShow;

  mmoLog.Clear();
  KeyPreview := True;

  btnLogInfo.OnClick  := Addr(AnyClick);
  btnLogError.OnClick := Addr(AnyClick);
  btnLogSource.OnClick := Addr(AnyClick);
  btnLogSourceAndScope.OnClick := Addr(AnyClick);
  btnLogSourceAndScope2.OnClick := Addr(AnyClick);

  Logger.MinLevel := TLogLevel.loDebug;

  { 1. A TLogListener automatically adds itself to Logger.Listeners when created
       and automatically removes itself from Logger.Listeners when destroyed.
    2. TLogTextListener and TLogLineListener inherit from TLogToMainThreadListener
       which synchronizes updates to MainThread controls
    3. The TFormLogListener shows a Form in the upper right screen corner
       where it displays log information
    4. The TDbLogListener saves log information in a database.
       Using the CreateSQLite() it makes it to use a SQLite database,
       so the property sqlite3.dll should be in the project folder. }
  FLogListener     := TLogTextListener.Create(Addr(LogProc));
  FFileLogListener := TFileLogListener.Create();
  FFormLogListener := TFormLogListener.Create();
  FDbLogListener   := TDbLogListener.CreateSQLite();


  lblLogFolder.Caption := Format('Error logs are saved at folder: %s ', [Logger.LogFolder]);
end;

procedure TMainForm.DoDestroy;
begin
  FreeAndNil(FFormLogListener);
  FreeAndNil(FFileLogListener);
  FreeAndNil(FLogListener);
  FreeAndNil(FDbLogListener);

  inherited DoDestroy;
end;

procedure TMainForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnLogInfo = Sender then
    LogInfoTest()
  else if btnLogError = Sender then
    LogErrorTest()
  else if btnLogSource = Sender then
    LogSourceTest()
  else if btnLogSourceAndScope = Sender then
    LogSourceAndScopeTest()
  else if btnLogSourceAndScope2 = Sender then
    LogSourceAndScopeTest2()
  ;
end;

function TMainForm.GetLogText(): string;
begin
  if Length(edtInfoText.Text) > 0 then
    Result := edtInfoText.Text
  else
    Result := 'No log message is provided';
end;

procedure TMainForm.LogProc(LogLine: string);
begin
  mmoLog.Lines.Add(LogLine);
end;

procedure TMainForm.LogInfoTest();
begin
  Logger.Info(Self.ClassName, GetLogText());
end;

procedure TMainForm.LogErrorTest();
begin
  try
    raise Exception.Create('This is an Exception message');
  except
    on E: Exception do
      Logger.Error(Self.ClassName, E);
  end;
end;

procedure TMainForm.LogSourceTest();
var
  LogSource: ILogSource;
  Params: IVariantDictionary;
begin
  // simple
  LogSource := Logger.CreateLogSource(Self.ClassName);
  LogSource.Info(GetLogText());

  // with params
  Params := TVariantDictionary.Create();
  Params['CustomerId'] := 'BigCo';
  Params['OrderId']    := 123;

  LogSource.Log(loInfo, 'Customer {CustomerId} Order with {OrderId} is completed.', Params);
end;

procedure TMainForm.LogSourceAndScopeTest();
var
  LogSource: ILogSource;
begin
  LogSource := Logger.CreateLogSource(Self.ClassName);

  LogSource.EnterScope('Scope 2');
  LogSource.Info(GetLogText());

  LogSource.EnterScope('Scope 3');
  LogSource.Info(GetLogText());
  LogSource.ExitScope();

  LogSource.Info(GetLogText());
  LogSource.ExitScope();

  LogSource.Info(GetLogText());
end;

procedure TMainForm.LogSourceAndScopeTest2();
var
  Dev: TDeveloper;
begin
  Dev := TDeveloper.Create();
  Dev.TestLogSource();
  Dev.Free();
end;





end.

